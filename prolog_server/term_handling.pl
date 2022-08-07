
% This module provides predicates to handle terms received from the client, compute their results and assert them with term_response/1.
% There are three main types of terms.
% For each of the types there are terms which need to be handled specially.
% The following types of terms are differentiated:
% - directives:
%   - begin_tests/1
%   - begin_tests/2
%   - end_tests/1
%   - any other directive
% - clause definitions:
%   - test(Name) :- Body
%   - test(Name, Options) :- Body
%   - Head :- Body
%   - Head --> Body
%   - Head (if the request contains more than one term)
% - queries:
%   - retry or jupyter:retry
%   - cut or jupyter:cut
%   - halt or jupyter:halt
%   - a call of jupyter:print_table/1 or jupyter:print_table/2
%   - a call of run_tests: run_tests/0, run_tests/1 or run_tests/2
%   - a call of trace: trace/0, trace/1 or trace/2
%   - a call of leash/1
%   - any term following ?-
%   - any other term which is the only one of a request


:- module(term_handling,
    [handle_term/6,            % handle_term(+Term, +IsSingleTerm, +CallRequestId, +Stack, +Bindings, -Cont)
     test_definition_end/1,    % test_definition_end(+LoadFile)
     pred_definition_specs/1,  % pred_definition_specs(PredDefinitionSpecs)
     term_response/1           % term_response(JsonResponse)
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(codesio), [write_term_to_codes/3, format_to_codes/3]).
:- use_module(library(lists), [delete/3]).
:- use_module(logging, [log/1, log/2]).
:- use_module(output, [call_with_output_to_file/3, call_query_with_output_to_file/7, redirect_output_to_file/0, exception_message/2, assert_query_start_time/0]).
:- use_module(jsonrpc, [send_error_reply/3]).
:- use_module(request_handling, [loop/3]).

:- if(sicstus).
:- use_module(library(aggregate), [forall/2]).
:- use_module(library(file_systems), [delete_file/1]).
:- use_module(variable_bindings, [term_with_stored_var_bindings/4, store_var_bindings/1]).
:- endif.


:- dynamic
  is_retry/1,               % is_retry(IsRetry)
  test_definition_stream/1, % test_definition_stream(TestDefinitionStream)
                            % TestDefinitionStream is a write stream if the current request contains a begin_tests request.
  pred_definition_specs/1,  % pred_definition_specs(PredDefinitionSpecs)
                            % PredDefinitionSpecs is a list of PredName/PredArity elements for every predicate which is defined by the current request.
  term_response/1.          % term_response(JsonResponse)


test_file_name('jupyter_tests.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle_term(+Term, +IsSingleTerm, +CallRequestId, +Stack, +Bindings, -Cont)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term Term.
% Check which type of term Term is and handle it accordingly.
% Term can be either a directive, a clause definition (which might be a DCG rule), or a query
% Directives
handle_term((:- Directive), IsSingleTerm, CallRequestId, Stack, Bindings, Cont) :- !,
  handle_directive((:- Directive), IsSingleTerm, CallRequestId, Stack, Bindings, Cont).
% Clause definitions
handle_term((Head :- Body), _IsSingleTerm, _CallRequestId, _Stack, Bindings, continue) :- !,
  handle_clause_definition_term((Head :- Body), Bindings).
handle_term((Head --> Body), _IsSingleTerm, _CallRequestId, _Stack, _Bindings, continue) :- !,
  handle_dcg((Head --> Body)).
% Queries
handle_term(?-(Query), _IsSingleTerm, CallRequestId, Stack, Bindings, Cont) :- !,
  handle_query_term(Query, false, CallRequestId, Stack, Bindings, continue, Cont).
handle_term(Query, true, CallRequestId, Stack, Bindings, Cont) :-
  handle_query_term(Query, false, CallRequestId, Stack, Bindings, continue, Cont).
% Clause definitions
handle_term(Head, false, _CallRequestId, _Stack, Bindings, continue) :-
  handle_clause_definition_term(Head, Bindings).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Directives

% The directives begin_tests and end_tests are handled specially.
% All other directives are called with output:call_with_output_to_file/3.
% The runtime of the exeuction and additional query data is not asserted as it is the case for queries.
% Furthermore, a retry is not possible and a directive's variable bindings are not sent to the client.

% handle_directive(+Term, +IsSingleTerm, +CallRequestId, +Stack, +Bindings, +Cont)
%
% begin_tests/1
handle_directive((:- begin_tests(_Unit)), true, _CallRequestId, _Stack, _Bindings, continue) :- !,
  handle_single_test_directive.
handle_directive((:- begin_tests(Unit)), _IsSingleTerm, _CallRequestId, _Stack, Bindings, continue) :- !,
  handle_begin_tests((:- begin_tests(Unit)), Bindings).
% begin_tests/2
handle_directive((:- begin_tests(_Unit, _Options)), true, _CallRequestId, _Stack, _Bindings, continue) :- !,
  handle_single_test_directive.
handle_directive((:- begin_tests(Unit, Options)), _IsSingleTerm, _CallRequestId, _Stack, Bindings, continue) :- !,
  handle_begin_tests((:- begin_tests(Unit, Options)), Bindings).
% end_tests/1
handle_directive((:- end_tests(_Unit)), true,_CallRequestId, _Stack,  _Bindings, continue) :- !,
  handle_single_test_directive.
handle_directive((:- end_tests(Unit)), _IsSingleTerm, _CallRequestId, _Stack, _Bindings, continue) :- !,
  write_to_test_definition_stream((:- end_tests(Unit)), []).
% Any other directive
handle_directive((:- Directive), _IsSingleTerm, CallRequestId, Stack, Bindings, Cont) :- !,
  handle_query_term(Directive, true, CallRequestId, Stack, Bindings, cut, Cont).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clause definitions

% In order to be able to redefine a predicate without having to remove its clauses first, previous clauses are retracted automatically.
% Whenever a clause is to be defined for a predicate for which there already are clauses, these are retracted and a message saying so is sent to the client.
% This implies that all clauses of a predicate need to be defined by one request.
% New clauses are added to the database with assertz/1.

% handle_clause_definition_term(+Clause, +Bindings)
%
% Test definitions
handle_clause_definition_term((test(Name) :- Body), Bindings) :-
  test_definition_stream(TestDefinitionStream),
  % If test_definition_stream/1 succeeds, there was a begin_tests directive
  % In that case, this test definition belongs to the unit test and is written to the test definition file
  !,
  write_to_test_definition_stream((test(Name) :- Body), Bindings, TestDefinitionStream).
handle_clause_definition_term((test(Name, Options) :- Body), Bindings) :-
  test_definition_stream(TestDefinitionStream),
  % If test_definition_stream/1 succeeds, there was a begin_tests directive
  % In that case, this test definition belongs to the unit test and is written to the test definition file
  !,
  write_to_test_definition_stream((test(Name, Options) :- Body), Bindings, TestDefinitionStream).
% Any other clause definition
handle_clause_definition_term(Clause, _Bindings) :-
  handle_clause_definition(Clause).


% handle_clause_definition(+Clause)
handle_clause_definition(Clause) :-
  module_name_expanded(Clause, Module:ClauseWithoutModule),
  clause_head(ClauseWithoutModule, Head),
  functor(Head, PredName, PredArity),
  retract_previous_clauses(Module:PredName/PredArity, RetractedClauses, Output),
  % Assert the clause and check if it was successful
  catch(assertz(Module:ClauseWithoutModule), Exception, true),
  output:exception_message(Exception, ExceptionMessage),
  ( nonvar(ExceptionMessage) ->
    assert_error_response(exception, ExceptionMessage, Output, [retracted_clauses=RetractedClauses])
  ; otherwise ->
    assert_success_response(clause_definition, [], Output, [retracted_clauses=RetractedClauses])
  ).


% doch nicht duplicate? Falls es für trace für sicstus nicht gebraucht wird
% module_name_expanded(+Term, -MTerm)
module_name_expanded((Module:Head:-Body), Module:(Head:-Body)) :- !.
module_name_expanded(Module:Term, Module:Term) :- !.
module_name_expanded(Term, user:Term).


% clause_head(+Clause, -Head)
clause_head((Head :- _Body), Head) :- !.
clause_head(Head, Head).


% retract_previous_clauses(+MPredSpec, -RetractedClauses, -Output)
retract_previous_clauses(MPredSpec, RetractedClausesJson, Output) :-
  retract_definition_specs(PredDefinitionSpecs),
  retract_previous_clauses(MPredSpec, PredDefinitionSpecs, NewPredDefinitionSpecs, RetractedClauses, Output),
  assert(pred_definition_specs(NewPredDefinitionSpecs)),
  ( var(RetractedClauses) ->
    RetractedClausesJson = json([])
  ; RetractedClausesJson = json([RetractedClauses])
  ).


% retract_definition_specs(-PredDefinitionSpecs)
%
% Retracts the Module:PredName/PredArity terms of predicates for which clauses have been asserted for the current request.
% If there are such predicates, PredDefinitionSpecs is a list containing those.
% Otherwise, PredDefinitionSpecs=[].
retract_definition_specs(PredDefinitionSpecs) :-
  pred_definition_specs(PredDefinitionSpecs),
  !.
retract_definition_specs([]).


% retract_previous_clauses(+MPredSpec, +PredDefinitionSpecs, -NewPredDefinitionSpecs, -RetractedClauses, -Output)
%
% MPredSpec is of the form Module:PredName/PredArity.
% PredDefinitionSpecs is a list of Module:PredName/PredArity elements for every predicate which is defined by the current request.
% The predicate specs need to be remembered so that the first time a clause for a new predicate is encountered, all previous clauses can be retracted.
% RetractedClauses is of the form MPredSpecAtom=ListingOutput.
%  MPredSpecAtom is an atom representing MPredSpec.
%  ListingOutput is an atom of all the previously defined clauses if there were any which had to be retracted.
%  Otherwise, RetractedClauses is not bound to anything.
% When the first clause of a predicate is asserted, Output is an atom saying which predicate is being asserted.
%  Otherwise, Output=''.
retract_previous_clauses(MPredSpec, PredDefinitionSpecs, PredDefinitionSpecs, _RetractedClauses, '') :-
  member(MPredSpec, PredDefinitionSpecs),
  % This is not the first clause of the PredName/PredArity predicate for the current request -> no clauses have to be retracted
  !.
retract_previous_clauses(Module:PredName/PredArity, PredDefinitionSpecs, [Module:PredName/PredArity|PredDefinitionSpecs], _RetractedClauses, AssertMessage) :-
  functor(Term, PredName, PredArity),
  \+ predicate_property(Module:Term, _Property),
  % The predicate does not exist yet -> no clauses have to be retracted
  !,
  produce_assert_message(Module:PredName/PredArity, AssertMessage).
retract_previous_clauses(Module:PredName/PredArity, PredDefinitionSpecs, PredDefinitionSpecs, _RetractedClauses, '') :-
  functor(Term, PredName, PredArity),
  \+ predicate_property(Module:Term, dynamic),
  % The predicate is not dynamic -> no clauses can be asserted
  % Try asserting anyway so that the corresponding error reply is sent to the client
  !.
retract_previous_clauses(Module:PredName/PredArity, PredDefinitionSpecs, [Module:PredName/PredArity|PredDefinitionSpecs], MPredSpecAtom=ListingOutput, AssertMessage) :-
  functor(Head, PredName, PredArity),
  clause(Module:Head, _Body),
  % Use listing/1 to get all the clauses that are to be retracted.
  output:call_with_output_to_file(listing(Module:PredName/PredArity), ListingOutput, ExceptionMessage),
  var(ExceptionMessage),
  !,
  % Create an atom of MPredSpec so that it is JSON parsable
  write_term_to_atom(Module:PredName/PredArity, [], MPredSpecAtom),
  % Create a new unbound term to retract all clauses
  functor(Term, PredName, PredArity),
  retractall(Module:Term),
  produce_assert_message(Module:PredName/PredArity, AssertMessage).
retract_previous_clauses(PredSpec, PredDefinitionSpecs, [PredSpec|PredDefinitionSpecs], _RetractedClauses, AssertMessage) :-
  % There are no clauses to retract
  produce_assert_message(PredSpec, AssertMessage).


% produce_assert_message(+MPredSpec, -AssertMessage)
produce_assert_message(PredSpec, AssertMessage) :-
  format_to_codes('% Asserting clauses for ~w~n', [PredSpec], AssertCodes),
  atom_codes(AssertMessage, AssertCodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DCG

% DCGs are transformed into clauses with term expansion and handled like other clause definitions with handle_clause_definition/1.

% handle_dcg(+DCG)
handle_dcg(DCG) :-
  expand_dcg_term(DCG, ExpandedDCG),
  handle_clause_definition(ExpandedDCG).


% expand_dcg_term(+DCG, -ExpandedDCG)
:- if(swi).
expand_dcg_term(DCG, ExpandedDCG) :-
  dcg_translate_rule(DCG, ExpandedDCG).
:- else.
expand_dcg_term(DCG, ExpandedDCG) :-
  expand_term(DCG, ExpandedDCG).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Queries

% In case of any other query not handled by any of the predicates defined above, the query is called by output:call_query_with_output_to_file/7.
% Before calling it, any $Var terms are replaced by corresponding values from previous queries.
% Additionally, the output of the goal and debugging messages are redirected to a file so that it can be read in and sent to the client.

% output:call_query_with_output_to_file/7 leaves a choice point.
% This way, when a 'retry' term is encountered in a future request, its failing causes the goal to be retried.

% handle_query_term(+Term, +IsDirective, +CallRequestId, +Stack, +Bindings, +LoopCont, -Cont)
handle_query_term(Term, IsDirective, CallRequestId, Stack, Bindings, LoopCont, Cont) :-
  % Before executing a query, replace any of its subterms of the form $Var by the latest value of the variable Var from a previous query.
  replace_previous_variable_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings, Exception),
  ( nonvar(Exception) ->
    output:exception_message(Exception, ExceptionMessage),
    assert_error_response(exception, ExceptionMessage, '', []),
    Cont = continue
  ; otherwise ->
    % Create a term_data(TermAtom, Bindings) term.
    % If the term is a query, the term_data term is used to assert the original term data in case terms of the form $Var were replaced.
    % The term data is needed when accessing previous queries (e.g. with jupyter:print_previous_queries/1).
    % Bindings needs to be copied so that the term can be read from the atom without any of the variables being instantiated by calling the term.
    copy_term(Bindings, BindingsCopy),
    write_term_to_atom(Term, Bindings, TermAtom),
    handle_query_term_(UpdatedTerm, IsDirective, CallRequestId, Stack, UpdatedBindings, term_data(TermAtom, BindingsCopy), LoopCont, Cont)
  ).


% replace_previous_variable_bindings(+Term, +Bindings, -UpdatedTerm, -UpdatedBindings, -Exception)
:- if(swi).
replace_previous_variable_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings, Exception) :-
  catch(toplevel_variables:expand_query(Term, UpdatedTerm, Bindings, UpdatedBindings), Exception, true).
:- else.
replace_previous_variable_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings, Exception) :-
  catch(variable_bindings:term_with_stored_var_bindings(Term, Bindings, UpdatedTerm, UpdatedBindings), Exception, true).
:- endif.


% handle_query_term_(+Query, +IsDirective, +CallRequestId, +Stack, +Bindings, +OriginalTermData, +LoopCont, -Cont)
% retry
handle_query_term_(retry, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_retry(Stack).
handle_query_term_(jupyter:retry, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_retry(Stack).
% cut
handle_query_term_(jupyter:cut, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_cut(Stack, Cont).
handle_query_term_(cut, _IsDirective, _CallRequestId, Stack, _Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_cut(Stack, Cont).
% halt
handle_query_term_(jupyter:halt, _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, done) :- !,
  % By unifying Cont=done, the loop reading and handling messages is stopped
  handle_halt.
handle_query_term_(halt, _IsDirective,_CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, done) :- !,
  % By unifying Cont=done, the loop reading and handling messages is stopped
  handle_halt.
% print_table
handle_query_term_(jupyter:print_table(Goal), _IsDirective, _CallRequestId, _Stack, Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_table_with_findall(Bindings, Goal).
handle_query_term_(jupyter:print_table(ValuesLists, VariableNames), _IsDirective, _CallRequestId, _Stack, Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_print_table(Bindings, ValuesLists, VariableNames).
% run_tests
handle_query_term_(run_tests, _IsDirective, CallRequestId, Stack, Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_run_tests(run_tests, CallRequestId, Stack, Bindings, Cont).
handle_query_term_(run_tests(Spec), _IsDirective, CallRequestId, Stack, Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_run_tests(run_tests(Spec), CallRequestId, Stack, Bindings, Cont).
handle_query_term_(run_tests(Spec, Options), _IsDirective, CallRequestId, Stack, Bindings, _OriginalTermData, _LoopCont, Cont) :- !,
  handle_run_tests(run_tests(Spec, Options), CallRequestId, Stack, Bindings, Cont).
% trace
handle_query_term_(trace, _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_trace(trace/0).
:- if(swi).
handle_query_term_(trace(_Pred), _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_trace(trace/1).
handle_query_term_(trace(_Pred, _Ports), _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  handle_trace(trace/2).
:- endif.
% leash/1
handle_query_term_(leash(_Ports), _IsDirective, _CallRequestId, _Stack, _Bindings, _OriginalTermData, _LoopCont, continue) :- !,
  output:exception_message(jupyter(leash_pred), Message),
  assert_error_response(exception, Message, '', []).
% Any other query
handle_query_term_(Query, IsDirective, CallRequestId, Stack, Bindings, OriginalTermData, LoopCont, Cont) :-
  handle_query(Query, IsDirective, CallRequestId, Stack, Bindings, OriginalTermData, LoopCont, Cont).


% handle_query(+Goal, +IsDirective, +CallRequestId, +Stack, +Bindings, +OriginalTermData, +LoopCont, -Cont)
%
% Goal is the current term of the request which is to be handled.
% IsDirective=true if the Goal was encountered as a directive.
%  In that case, no variable bindings are sent to the client.
% CallRequestId is the ID of the current call request.
%  It is needed for juypter:print_previous_queries/1.
% Stack is a list containing atoms representing the previous queries which might have exited with a choicepoint and can therefore be retried.
%  It is needed for retry/0 and cut/0 queries.
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
% LoopCont is one of continue and cut.
%  If LoopCont = cut, the recurse loop (request_handling:loop/3) will exit right away without making retrys of a term possible.
% Cont will be processed by request_handling:loop/3.
handle_query(Goal, IsDirective, CallRequestId, Stack, Bindings, OriginalTermData, LoopCont, Cont) :-
  % In order to send the goal to the client, it has to be converted to an atom
  % This has to be done before calling it causes variables to be bound
  write_term_to_atom(Goal, Bindings, GoalAtom),
  RecStack = [GoalAtom|Stack],
  retractall(is_retry(_)),
  asserta(is_retry(false)),
  % Call the goal Goal
  output:call_query_with_output_to_file(Goal, CallRequestId, Bindings, OriginalTermData, Output, ExceptionMessage, IsFailure),
  retry_message_and_output(GoalAtom, Output, RetryMessageAndOutput),
  % Exception, failure or success from Goal
  ( nonvar(ExceptionMessage) -> % Exception
    !,
    assert_error_response(exception, ExceptionMessage, RetryMessageAndOutput, []),
    Cont = continue
  ; IsFailure == true -> % Failure
    !,
    % Also happens when 'retry' message requested a new solution and found none.
    assert_query_failure_response(IsDirective, GoalAtom, RetryMessageAndOutput),
    Cont = continue
  ; otherwise -> % Success
    % Update the stored variable bindings
    remove_singleton_variables(Bindings, BindingsWithoutSingletons),
    update_variable_bindings(BindingsWithoutSingletons),
    % Convert the variable values to json parsable terms
    json_parsable_vars(BindingsWithoutSingletons, Bindings, ResultBindings),
    assert_query_success_response(IsDirective, ResultBindings, RetryMessageAndOutput),
    % Start a new recursive loop so that the current goal can be retried
    % The loop will
    % - exit right away if LoopCont=cut
    % - fail if it receives a request to retry Goal
    request_handling:loop(LoopCont, RecStack, RecCont),
    ( RecCont = cut,
      !,
      Cont = continue
    ; otherwise -> % Possibly 'done'
      Cont = RecCont
    )
  ),
  !.


% assert_query_failure_response(+IsDirective, +GoalAtom, +Output)
assert_query_failure_response(true, GoalAtom, Output) :-
  % For directives, append a message displaying the failure
  !,
  directive_failed_message(GoalAtom, FailureMessage),
  output_and_failure_message(Output, FailureMessage, OutputAndFailureMessage),
  assert_error_response(failure, '', OutputAndFailureMessage, []).
assert_query_failure_response(_IsDirective, _GoalAtom, Output) :-
  assert_error_response(failure, '', Output, []).


% directive_failed_message(+Goal, -FailureMessage)
:- if(swi).
directive_failed_message(Goal, FailureMessage) :-
  format_to_codes('ERROR: Goal (directive) failed: ~w', [Goal], FailureMessageCodes),
  atom_codes(FailureMessage, FailureMessageCodes).
:- else.
directive_failed_message(Goal, FailureMessage) :-
  format_to_codes('* ~w - goal failed', [Goal], FailureMessageCodes),
  atom_codes(FailureMessage, FailureMessageCodes).
:- endif.


% output_and_failure_message(+Output, +FailureMessage, -OutputAndFailureMessage)
output_and_failure_message('', FailureMessage, FailureMessage) :- !.
output_and_failure_message(Output, FailureMessage, OutputAndFailureMessage) :-
  atom_concat('\n', FailureMessage, FailureMessageWithNl),
  atom_concat(Output, FailureMessageWithNl, OutputAndFailureMessage).


% assert_query_success_response(+IsDirective, +ResultBindings, +Output)
assert_query_success_response(true, _ResultBindings, Output) :-
  % For directives, no bindings are output
  !,
  assert_success_response(directive, [], Output, []).
assert_query_success_response(_IsDirective, ResultBindings, Output) :-
  assert_success_response(query, ResultBindings, Output, []).


% update_variable_bindings(+BindingsWithoutSingletons)
:- if(swi).
update_variable_bindings(BindingsWithoutSingletons) :-
  toplevel_variables:expand_answer(BindingsWithoutSingletons, _NewBindings).
:- else.
update_variable_bindings(BindingsWithoutSingletons) :-
  variable_bindings:store_var_bindings(BindingsWithoutSingletons).
:- endif.


% retry_message_and_output(+GoalAtom, +Output, -RetryMessageAndOutput)
%
% If the current term was 'retry', a retry message is prepended to the output of the goal.
retry_message_and_output(GoalAtom, Output, RetryMessageAndOutput) :-
  % The Id can be from the initial 'call' request or from a subsequent 'retry' request.
  retract(is_retry(IsRetry)),
  retry_message(IsRetry, GoalAtom, RetryMessage),
  atom_concat(RetryMessage, Output, RetryMessageAndOutput).


% retry_message(+IsRetry, +GoalAtom, -RetryMessage)
%
% If the current term was a 'retry' term (IsRetry=true), a retry message is sent to the client.
% This message contains the goal which was retried.
retry_message(true, GoalAtom, RetryMessage) :-
  !,
  format_to_codes('% Retrying goal: ~w~n', [GoalAtom], RetryMessageCodes),
  atom_codes(RetryMessage, RetryMessageCodes).
retry_message(_IsRetry, _GoalAtom, '').


% remove_singleton_variables(+Bindings, -BindingsWithoutSingletons)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term currently being handled.
% BindingsWithoutSingletons contains the elements of Bindings except for (named) singleton variables starting with '_'
remove_singleton_variables([], []) :- !.
remove_singleton_variables([Name=_Var|Bindings], BindingsWithoutSingletons) :-
  % Name starts with '_'
  sub_atom(Name, 0, 1, _After, '_'),
  !,
  remove_singleton_variables(Bindings, BindingsWithoutSingletons).
remove_singleton_variables([Binding|Bindings], [Binding|BindingsWithoutSingletons]) :-
  remove_singleton_variables(Bindings, BindingsWithoutSingletons).


% json_parsable_vars(+NonParsableVars, +Bindings, -JsonParsableVars)
%
% NonParsableVars and Bindings are lists of Name=Var pairs, where Name is the name of a variable Var occurring in the term currently being handled.
% As not all of the values can be parsed to JSON (e.g. uninstantiated variables and compounds), they need to be made JSON parsable first.
% Bindings is needed in case any variable term needs to be converted to an atom and contains other variables.
% By using Bindings, the names of the variables can be preserved.
% In case of domain variables with bounded domains (lower and upper bound exist) which are not bound to a single value,
%  the value returned to the client is a list of lists where each of those lists contains a lower and upper bound of a range the variable can be in.
json_parsable_vars([], _Variables, []) :- !.
json_parsable_vars([VarName=Var|RemainingBindings], Bindings, [VarName=json([dom=DomAtom])|JsonParsableBindings]) :-
  var(Var),
  % Check if the predicate clpfd:fd_dom/2 exists
  % If it does not, the library clpfd has not been loaded
  % In that case, there cannot be any domain variables
  current_predicate(clpfd:fd_dom/2),
  clpfd:fd_dom(Var, Dom),
  write_term_to_atom(Dom, [], DomAtom),
  DomAtom \= 'inf..sup',
  % cannot compare the term unless loading the library
  !,
  % The variable is a domain variable with a bounded domain (lower and upper bound exist)
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).
json_parsable_vars([VarName=Var|RemainingBindings], Bindings, JsonParsableBindings) :-
  var(Var),
  same_var(RemainingBindings, Var),
  !,
  % The list of Name=Var pairs contains at least one element OtherName=Var where Var is uninstantiated
  % Unify the variable Var with VarName
  Var=VarName,
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).
json_parsable_vars([_VarName=Var|RemainingBindings], Bindings, JsonParsableBindings) :-
  var(Var),
  !,
  % The variable is uninstantiated and therefore not included in the result list
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).
json_parsable_vars([VarName=Var|RemainingBindings], Bindings, [VarName=VarAtom|JsonParsableBindings]) :-
  % Convert the value to an atom as it may be compound and cannot be parsed to JSON otherwise
  write_term_to_atom(Var, Bindings, VarAtom),
  json_parsable_vars(RemainingBindings, Bindings, JsonParsableBindings).


% same_var(+BindingsWithoutSingletons, +Var)
%
% BindingsWithoutSingletons is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term currently being handled.
% Fails if BindingsWithoutSingletons does not contain any elenent VarName=Var1 where Var1 and Var are identical (==).
same_var([], _Var) :- fail.
same_var([_VarName=Var1|_BindingsWithoutSingletons], Var2) :-
  Var1 == Var2, !.
same_var([_Binding|BindingsWithoutSingletons], Var) :-
  same_var(BindingsWithoutSingletons, Var).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handling the different types of queries

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retry

% If there is no active goal, an error message is sent to the client.
% Otherwise, in order to retry an active previous goal, fails into the caller (output:call_query_with_output_to_file/7).
% The goal which is retried is output.

% handle_retry(+CallRequestId, +Stack)
handle_retry(Stack) :-
  ( Stack = [_ActiveGoal|_RemainingStack] ->
    % Tell caller where to send the reply
    asserta(is_retry(true)),
    output:redirect_output_to_file,
    output:assert_query_start_time,
    fail
  ; otherwise -> % No active call
    assert_error_response(no_active_call, '', '', [])
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cut

% If there is no active goal, an error message is sent to the client.
% Otherwise, Cont=cut causes possible choice points of output:call_query_with_output_to_file/7 in handle_query/8 to be cut.
% A message informing the user is output.
% Additionally, if there is a previous goal which is now the active one, it is displayed.

% handle_cut(+Stack, -Cont)
handle_cut(Stack, Cont) :-
  log(cut),
  ( Stack = [_Active|RemainingStack] ->
    cut_message(RemainingStack, CutMessage),
    assert_success_response(cut, [], CutMessage, []),
    Cont = cut
  ; otherwise -> % No active call
    assert_error_response(no_active_call, '', '', []),
    Cont = continue
  ).


% cut_message(+RemainingStack, -CutMessage)
cut_message([], '% Successfully cut\n% There is no previous active goal') :- !.
cut_message([ActiveGoalAtom|_RemainingStack], CutMessage) :-
  format_to_codes('% Successfully cut\n% The new active goal is: ~w', [ActiveGoalAtom], MessageCodes),
  atom_codes(CutMessage, MessageCodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Halt

% If the server is to be halted, the loop reading and handling messages is stopped.
% The type of the success reply sent to the client indicates that the server was halted and needs to be restarted for the next execution request.

handle_halt :-
  assertz(term_response(json([status=halt]))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print result table

% The client requested a response which can be used to print a table.
% The client expects the result to contain a member 'print_table' of which the value is a dictionary with the following members:
% - ValuesLists: a list of lists where each list corresponds to one line of the table
% - VariableNames: a list of names used as the header for the table; one of
%   - []: if no names are provided, the header will contain capital letters as names
%   - a list of ground terms of the same length as the values lists


% handle_print_table_with_findall(+Bindings, +Goal)
%
% The values need to be computed with findall/3 for the goal Goal.
% The header of the table will contain the names of the variables occurring in Goal.
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
handle_print_table_with_findall(Bindings, Goal) :-
  ( output:call_with_output_to_file(term_handling:findall_results_and_var_names(Goal, Bindings, Results, VarNames), Output, ExceptionMessage),
    % Success or exception from findall_results_and_var_names/4
    ( nonvar(ExceptionMessage) ->
      !,
      assert_error_response(exception, ExceptionMessage, '', [])
    ; otherwise -> % success
      % Return the additional 'print_table' data
      assert_success_response(print_table, [], Output, [print_table=json(['ValuesLists'=Results, 'VariableNames'=VarNames])])
    ),
    !
  ; % findall_results_and_var_names/4 failed
    assert_error_response(failure, '', '', [])
  ).


% handle_print_table(+Bindings, +ValuesLists, +VariableNames)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
% ValuesLists is expected to be a list of lists of the same lengths.
% It contains the data which is to be printed in the table.
% VariableNames is [] or a list of ground terms which need to be of the same length as the values lists.
handle_print_table(_Bindings, [], VariableNames) :- !,
  assert_success_response(print_table, [], '', [print_table=json(['ValuesLists'=[], 'VariableNames'=VariableNames])]).
handle_print_table(Bindings, ValuesLists, VariableNames) :-
  % Get the length of the first list and make sure that all other lists have the same length
  ValuesLists = [ValuesList|RemainingValuesLists],
  length(ValuesList, Length),
  ( forall(member(List, RemainingValuesLists), length(List, Length)) ->
    % Make sure that VariableNames is valid
    ( table_variable_names(VariableNames, Length, TableVariableNames) ->
      % As not all of the values can be parsed to JSON (e.g. uninstantiated variables and compounds), they need to be made JSON parsable first by converting them to atoms
      findall(ValuesAtomList, (member(Values, ValuesLists), convert_to_atom_list(Values, Bindings, ValuesAtomList)), JsonParsableValuesLists),
      % Return the additional 'print_table' data
      assert_success_response(print_table, [], '', [print_table=json(['ValuesLists'=JsonParsableValuesLists, 'VariableNames'=TableVariableNames])])
    ; otherwise -> % The variable names are invalid
      output:exception_message(jupyter(invalid_table_variable_names), Message),
      assert_error_response(exception, Message, '', [])
    )
  ; otherwise -> % Not all lists in ValuesLists are of the same length
    output:exception_message(jupyter(invalid_table_values_lists_length), Message),
    assert_error_response(exception, Message, '', [])
  ).


% table_variable_names(+VariableNames, +Length, -TableVariableNames)
table_variable_names([], Length, TableVariableNames) :-
  % If no variable names are provided, capital letters are used instead
  Letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'],
  % TableVariableNames is a list containing the first Length letters
  length(TableVariableNames, Length),
  append(TableVariableNames, _, Letters).
table_variable_names(VariableNames, Length, VariableNames) :-
  % Check that the number of variable names is correct and that all of them are ground
  length(VariableNames, Length),
  forall(member(VariableName, VariableNames), ground(VariableName)),
  !.


% convert_to_atom_list(+List, +Bindings, -AtomList)
%
% AtomList contains the elements of List after converting them to atoms.
convert_to_atom_list(List, Bindings, AtomList) :-
  findall(ElementAtom, (member(Element, List), write_term_to_atom(Element, Bindings, ElementAtom)), AtomList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PlUnit tests

% In order to use PlUnit tests with the SICStus JSON-RPC server, the tests need to be written to a file which can be loaded.
% When a the first begin_tests directive is encountered, a file is created and opened for writing.
% The corresponding stream is asserted as test_write_stream/1 so that it can be used to write test definitions test directives to the file.
% Each following begin_tests/end_tests directive and test/1 or test/2 clause is written to the file.
% When all terms of a request were handled or a run_tests query is encountered, the file is loaded.

% All tests which are to be run at the same time need to be defined by the same request.
% Otherwise, the definition is overwritten.


% handle_single_test_directive
handle_single_test_directive :-
  output:exception_message(jupyter(single_test_directive), Message),
  assert_error_response(exception, Message, '', []).


% handle_begin_tests(+Directive, +Bindings)
handle_begin_tests(Directive, Bindings) :-
  test_definition_stream(TestDefinitionStream),
  % Not the first begin_tests directive -> write to the existing file
  !,
  write_to_test_definition_stream(Directive, Bindings, TestDefinitionStream).
handle_begin_tests(Directive, Bindings) :-
  % First begin_tests directive of the request or after a run_tests query -> create a new file
  test_file_name(TestFileName),
  open(TestFileName, write, TestDefinitionStream),
  assert(test_definition_stream(TestDefinitionStream)),
  % Load the module plunit in the file
  % Otherwise, if the module was not loaded, the loading of the test definition file fails with an existence error because of user:begin_test/1
  write_term(TestDefinitionStream, ':- use_module(library(plunit)).\n', []),
  write_to_test_definition_stream(Directive, Bindings, TestDefinitionStream).


% handle_run_tests(+Term, +CallRequestId, +Stack, +Bindings, -Cont)
%
% If in the current query tests were defined, the test definition file is loaded.
% Afterwards, this is handled the same as any other query.
handle_run_tests(Term, CallRequestId, Stack, Bindings, Cont) :-
  test_definition_end(true),
  handle_query(Term, false, CallRequestId, Stack, Bindings, _OriginalTermData, cut, Cont).


% write_to_test_definition_stream(+Term, +Bindings)
write_to_test_definition_stream(Term, Bindings) :-
  test_definition_stream(TestDefinitionStream),
  !,
  write_to_test_definition_stream(Term, Bindings, TestDefinitionStream).
% Otherwise, there was no begin_tests directive -> there is no file to write to

% write_to_test_definition_stream(+Term, +Bindings, +TestDefinitionStream)
write_to_test_definition_stream(Term, Bindings, TestDefinitionStream) :-
  write_term(TestDefinitionStream, Term, [variable_names(Bindings)]),
  write_term(TestDefinitionStream, '.\n', []).


% test_definition_end(+LoadFile)
%
% Closes and retracts the stream to which test definitions were written.
% If LoadFile=true, loads the test definition file.
test_definition_end(LoadFile) :-
  test_definition_stream(TestDefinitionStream),
  !,
  close(TestDefinitionStream),
  retractall(test_definition_stream(_)),
  test_file_name(TestFileName),
  % When loading the file, an exception or warning might be output
  ( LoadFile==true ->
    % When loading the file, an exception or warning might be output
    output:call_with_output_to_file(load_files(TestFileName), Output, Exception)
  ; otherwise ->
    Output = ''
  ),
  delete_file(TestFileName),
  ( nonvar(Exception) ->
    assert_error_response(exception, Exception, Output, [])
  ; otherwise ->
    atom_concat(Output, '\n% Loaded the test file', OutputWithLoadMessage),
    assert_success_response(directive, [], OutputWithLoadMessage, [])
  ).
test_definition_end(_LoadFile).
% The request did not contain any begin_tests directive -> there is no file or stream which has to be dealt with


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% trace

% trace/0, trace/1 and trace/2 cannot be used with this server, because the debugging messages are not printed in a way that they can be read in and sent to the client.
% Instead, jupyter:trace(Goal) can be used to print the trace of the goal Goal.

% handle_trace(+TracePredSpec)
handle_trace(TracePredSpec) :-
  output:exception_message(jupyter(trace_pred(TracePredSpec)), Message),
  assert_error_response(exception, Message, '', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print tables

% findall_results_and_var_names(+Goal, +Bindings, -Results, -VarNames)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal Goal.
% Uses findall to find all results (ResultsLists) of the goal Goal.
% ResultsLists contains lists containing values for each variable in Bindings.
% VarNames is the list of variable names from Bindings.
findall_results_and_var_names(Goal, Bindings, JsonParsableResultsLists, VarNames) :-
  var_names_and_values(Bindings, VarNames, Vars),
  findall(Vars, Goal, ResultsLists),
  json_parsable_results_lists(ResultsLists, VarNames, Bindings, JsonParsableResultsLists).


% var_names_and_values(+Bindings, -VarNames, -Vars)
var_names_and_values([], [], []) :- !.
var_names_and_values([VarName=Var|Bindings], [VarName|VarNames], [Var|Vars]) :-
  var_names_and_values(Bindings, VarNames, Vars).


% json_parsable_results_lists(+ResultsLists, +VarNames, +Bindings, -JsonParsableResultsLists)
%
% ResultsLists is a list containing lists of values of the variables with names in VarNames.
% As not all of the terms in ResultsLists can be parsed to JSON (e.g. uninstantiated variables and compounds), they need to be made JSON parsable first.
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the goal which was called to get the results.
% Bindings is needed to preserve the variable names when converting a result to an atom.
json_parsable_results_lists([], _VarNames, _Bindings, []) :- !.
json_parsable_results_lists([Results|ResultsLists], VarNames, Bindings, [JsonParsableResults|JsonParsableResultsLists]) :-
  json_parsable_results(Results, VarNames, Bindings, JsonParsableResults),
  json_parsable_results_lists(ResultsLists, VarNames, Bindings, JsonParsableResultsLists).


% json_parsable_results(+Results, +VarNames, +Bindings, -JsonParsableResult)
json_parsable_results([], _VarNames, _Bindings, []) :- !.
json_parsable_results([Result|Results], [VarName|VarNames], Bindings, [Result|JsonParsableResults]) :-
  % If the result is a variable, unify it with its name
  var(Result),
  !,
  Result = VarName,
  json_parsable_results(Results, VarNames, Bindings, JsonParsableResults).
json_parsable_results([Result|Results], [_VarName|VarNames], Bindings, [ResultAtom|JsonParsableResults]) :-
  % Convert the value to an atom as it may be compound and cannot be parsed to JSON otherwise
  write_term_to_atom(Result, Bindings, ResultAtom),
  json_parsable_results(Results, VarNames, Bindings, JsonParsableResults).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Assert the term responses

% For each term which is processed and produces a result, this result is asserted.
% This way, all results can be sent to the client when all terms of a request have been handled.

% assert_success_response(+Type, +Bindings, +Output, +AdditionalData)
%
% Type is the type of the term read from the client.
% It is one of: directive, clause_definition, query, cut, print_table
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term.
% Output is the output of the term which was executed.
% AdditionalData is a list containing Key=Value pairs providing additional data for the client.
assert_success_response(Type, Bindings, Output, AdditionalData) :-
  assertz(term_response(json([status=success, type=Type, bindings=json(Bindings), output=Output|AdditionalData]))).


% assert_error_response(+ErrorCode, +ErrorInfo, +Output, +AdditionalData)
%
% ErrorCode is one of the error codes defined by error_object_code/3 (e.g. exception).
% ErrorInfo is additional information about the error (e.g. a more specific error message).
% Output is the output of the term which was executed.
% AdditionalData is a list containing Key=Value pairs providing additional data for the client.
assert_error_response(ErrorCode, ErrorInfo, Output, AdditionalData) :-
  jsonrpc:json_error_term(ErrorCode, ErrorInfo, Output, AdditionalData, ErrorData),
  assertz(term_response(json([status=error, error=ErrorData]))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write_term_to_atom(+Term, +Bindings, -Atom)
%
% Bindings is a list of Name=Var pairs, where Name is the name of a variable Var occurring in the term Term.
write_term_to_atom(Term, Bindings, Atom) :-
  write_term_to_codes(Term, TermCodes, [variable_names(Bindings)]),
  atom_codes(Atom, TermCodes).
