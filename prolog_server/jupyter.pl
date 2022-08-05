
% This module provides special predicates which can be used in call requests by the client.
% Some of these predicates need to be the only goal of a query.
% Otherwise, they cannot be determined as special predicates and do not work as expected.


:- module(jupyter,
    [print_table/1,             % print_table(+Goal)
     print_table/2,             % print_table(+ValuesLists, +VariableNames)
     trace/1,                   % trace(+Goal)
     print_variable_bindings/0,
     previous_query_time/2,     % previous_query_time(-Goal, -Runtime)
     print_previous_queries/1   % print_previous_queries(+Ids)
    ]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(lists), [reverse/2]).
:- use_module(library(codesio), [read_term_from_codes/3, write_term_to_codes/3, format_to_codes/3]).
:- use_module(logging, [log/1, log/2]).
:- use_module(output, [query_data/4, switch_debug_mode_on_for_breakpoints/0]).
:- use_module(variable_bindings, [var_bindings/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following predicates need to appear as a single goal in a query.
% In that case, the execution is handled by the module term_handling.
% Otherwise, an error message is output.

% print_table(+Goal)
jupyter:print_table(_Goal) :-
  throw(jupyter(no_single_goal(jupyter:print_table/1))).

% print_table(+ValuesLists, +VariableNames)
jupyter:print_table(_ValuesLists, _VariableNames) :-
  throw(jupyter(no_single_goal(jupyter:print_table/2))).

% retry
jupyter:retry :-
  throw(jupyter(no_single_goal(jupyter:retry/0))).

user:retry :-
  throw(jupyter(no_single_goal(jupyter:retry/0))).

% cut
jupyter:cut :-
  throw(jupyter(no_single_goal(jupyter:cut/0))).

user:cut :-
  throw(jupyter(no_single_goal(jupyter:cut/0))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Help

% jupyter:predicate_docs(-PredDocs)
%
% PredDocs is a list with elements of the form Pred=Doc, where Pred is a predicate exported by this module and Doc is its documentation as an atom.
jupyter:predicate_docs(PredDocs) :-
  findall(Pred=Doc, predicate_doc(Pred, Doc), PredDocs).


% Prints the documentation for all predicates defined in module jupyter.
jupyter:help :-
  jupyter:predicate_docs(PredDocs),
  log(PredDocs),
  print_pred_docs(PredDocs).


print_pred_docs([]) :- !.
print_pred_docs([_Pred=Doc]) :-
  !,
  format('~w', [Doc]).
print_pred_docs([_Pred=Doc|PredDocs]) :-
  format('~w~n~n--------------------------------------------------------------------------------~n~n', [Doc]),
  print_pred_docs(PredDocs).


predicate_doc('jupyter:help/0', Doc) :-
  atom_concat([
    'jupyter:help',
    '\n\nOutputs the documentation for all predicates defined in module jupyter.'
  ], Doc).
predicate_doc('jupyter:halt/0', Doc) :-
  atom_concat([
    'jupyter:halt or halt (can be called with or without module name expansion)',
    '\n\nShuts down the running Prolog process. The next time code is to be executed, a new process is started and everything defined in the database before does not exist anymore.',
    '\n\nCorresponds to the functionality of halt/0. Has the same effect as interrupting or restarting the Jupyter kernel.'
  ], Doc).
predicate_doc('jupyter:retry/0', Doc) :-
  atom_concat([
    'jupyter:retry or retry (can be called with or without module name expansion)',
    '\n\nCauses backtracking of the latest active query.',
    '\n\nA query is active as long as retrying it did not detect that there are no further solutions or the choicepoints have been cut off with jupyter:cut/0.',
    '\n\nNeeds to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:cut/0', Doc) :-
  atom_concat([
    'jupyter:cut or cut (can be called with or without module name expansion)',
    '\n\nCuts off the choicepoints of the latest active query.',
    '\n\nA query is active as long as retrying it did not detect that there are no further solutions or the choicepoints have been cut off.',
    '\nA further call of jupyter:retry/0 causes backtracking of the previous active goal.',
    '\n\nNeeds to be the only goal of a query.'
  ], Doc).
predicate_doc('jupyter:trace/1', Doc) :-
  atom_concat([
    'jupyter:trace(+Goal)',
    '\n\nPrints the trace of the goal Goal.',
    '\n\nSwitches on trace mode, calls the goal Goal and switches off trace mode.',
    'By default, all ports are unleashed so that no user interaction is requested.',
    'All previously set breakpoints are still active.',
    '\n\nNeeds to be the only goal of a query in order to work as expected.'
  ], Doc).
predicate_doc('jupyter:print_variable_bindings', Doc) :-
  atom_concat([
    'jupyter:print_variable_bindings',
    '\n\nPrints variable bindings from previous queries. For each variable the latest value it was bound to is shown.',
    '\n\nThe variable value can be accessed with a $Var term by any query. In that case, the term is replaced by the value. If there is no previous value, an error message is printed.'
  ], Doc).
predicate_doc('jupyter:print_table/1', Doc) :-
  atom_concat([
    'jupyter:print_table(+Goal)',
    '\n\nComputes all results of the goal Goal with findall/3 and prints them in a table.',
    '\n\nNeeds to be the only goal of a query.',
    '\n\nExample: jupyter:print_table(prolog_flag(FlagName, Value)).'
  ], Doc).
predicate_doc('jupyter:print_table/2', Doc) :-
  atom_concat([
    'jupyter:print_table(+ValuesLists, +VariableNames)',
    '\n\nPrints a table of the values in ValuesLists.',
    '\n\nValuesLists is a list of lists of the same length. Each list corresponds to one line of the table.',
    '\nThe header of the table is filled with the elements in VariableNames if it is a list of ground terms of the same length as the values lists.',
    '\nIf VariableNames is bound to [], capital letters are used for the header instead.',
    '\n\nNeeds to be the only goal of a query.',
    '\n\nCan be used with a predicate like findall/3, but not directly. Instead, the binding of a previous query can be accessed with a $Var term (see jupyter:print_variable_bindings/0).',
    '\n\nExamples:',
    '\n  jupyter:print_table([[10,100],[20,400],[30,900]], [\'X\', \'Y\']).',
    '\n  jupyter:print_table($ResultLists, []).'
  ], Doc).
predicate_doc('jupyter:previous_query_time/2', Doc) :-
  atom_concat([
    'jupyter:previous_query_time(-Goal, -Runtime)',
    '\n\nGoal is the previously executed goal and Time is the time in milliseconds it took the query to complete.'
  ], Doc).
predicate_doc('jupyter:print_previous_queries/1', Doc) :-
  atom_concat([
    'jupyter:print_previous_queries(+Ids)',
    '\n\nThe previous queries which were exectued in requests with IDs in Ids are printed in a way that they can be copied to a cell and executed right away or expanded with a head to define a predicate.',
    '\n\nIf one of the queries contains a $Var term and a previous query with id in Ids contains the variable Var, the term is replaced by the variable name. Otherwise, $Var is not replaced.'
  ], Doc).


% atom_concat(+AtomList, -ResultAtom)
atom_concat(Atoms, ResultAtom) :-
  reverse(Atoms, ReversedAtoms),
  atom_concat_(ReversedAtoms, '', ResultAtom).


% atom_concat(+AtomList, +AtomSoFar, -ResultAtom)
atom_concat_([], AtomSoFar, AtomSoFar) :- !.
atom_concat_([Atom|Atoms], AtomSoFar, ResultAtom) :-
  atom_concat(Atom, AtomSoFar, NewAtomSoFar),
  atom_concat_(Atoms, NewAtomSoFar, ResultAtom).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Trace

:- if(swi).

% trace(+Goal)
%
% Switch the tracer on, call the goal Goal and stop the tracer.
% Debug mode is switched on so that any breakpoints which might exist can be activated.
% Because of user:prolog_trace_interception/4, debugging messages are printed to the current output without requesting user interaction.
trace(Goal) :-
  trace,
  call(Goal),
  !,
  notrace.


% user:prolog_trace_interception(+Port, +Frame, +Choice, -Action)
%
% Print the debugging messages to the current output.
% Action=continue corresponds ot creeping in the command line debugger, so that no user interaction is required.
user:prolog_trace_interception(_Port, Frame, _PC, continue) :-
  prolog_frame_attribute(Frame, hidden, true),
  % Do nothing for frames hidden from the user
  !.
user:prolog_trace_interception(Port, Frame, PC, continue) :-
  % Print the debugging message as output by the tracer
  current_output(OutputStream),
  port_functor(Port, PortFunctor),
  phrase('$messages':translate_message(frame(Frame, PortFunctor, PC)), TraceMessageLines),
  print_message_lines(OutputStream, '', TraceMessageLines),
  nl(OutputStream).


% port_functor(+Port, -PortFunctor)
port_functor(call, call).
port_functor(redo(_), redo).
port_functor(unify, unify).
port_functor(exit, exit).
port_functor(fail, fail).
port_functor(exception(_), exception).

:- else.

% trace(+Goal)
%
% Switches on trace mode, calls the goal Goal and switches debug mode off.
% Since the last line of the output contains the debugging message of nodebug/1, this line is removed.
% If any breakpoints exist, debug mode is switched back on again so that the debugger can stop at a breakpoint.
% All ports are unleashed so that the debugger does not stop at a breakpoint to wait for user input.
% However, breakpoints are not affected by this.
trace(Goal) :-
  module_name_expanded(Goal, MGoal),
  trace,
  call(MGoal),
  !,
  nodebug,
  output:switch_debug_mode_on_for_breakpoints.


% module_name_expanded(+Term, -MTerm)
module_name_expanded((Module:Head:-Body), Module:(Head:-Body)) :- !.
module_name_expanded(Module:Term, Module:Term) :- !.
module_name_expanded(Term, user:Term).

:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Variable bindings

% print_variable_bindings
%
% Print the previous variable bindings which can be reused with a term of the form $Var.
:- if(swi).
print_variable_bindings :-
  print_toplevel_variables. % backtracks until it fails
print_variable_bindings.
:- else.
print_variable_bindings :-
  variable_bindings:var_bindings(Bindings),
  ( Bindings == [] ->
    format('No previous variable bindings~n', [])
  ; print_variable_bindings(Bindings)
  ).

print_variable_bindings([]) :- !.
print_variable_bindings([Name=Value|Bindings]) :-
  format('$~w =~t~12|~p~n', [Name, Value]),
  print_variable_bindings(Bindings).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Previous query (time)

% previous_query_time(-Goal, -Runtime)
%
% Runtime is the runtime of the latest query, which was a call of Goal
previous_query_time(Goal, Runtime) :-
  findall(Goal-Runtime, output:query_data(_CallRequestId, Runtime, term_data(Goal, _NameVarPairs), _OriginalTermData), GoalRuntimes),
  log(GoalRuntimes),
  append(_PreviousGoalRuntimes, [Goal-Runtime], GoalRuntimes).


% print_previous_queries(+Ids)
%
% Prints the previous queries with ids in Ids in a way that they can be
% - copied to a cell and executed right away or
% - expanded with a head to define a predicate
% If a query contains a term of the form $Var and a previous query contains the variable Var, $Var is replaced by the variable name.
print_previous_queries(Ids) :-
  findall(TermData-OriginalTermData, (member(Id, Ids), output:query_data(Id, _Runtime, TermData, OriginalTermData)), QueriesData),
  print_previous_queries(QueriesData, []).


% print_previous_queries(+QueriesData, +PreviousNameVarPairs)
print_previous_queries([], _PreviousNameVarPairs) :- !.
print_previous_queries([QueryData], PreviousNameVarPairs) :-
  !,
  print_previous_query(QueryData, PreviousNameVarPairs, _NewPreviousNameVarPairs, QueryAtom),
  format('  ~w.~n', [QueryAtom]).
print_previous_queries([QueryData|QueriesData], PreviousNameVarPairs) :-
  print_previous_query(QueryData, PreviousNameVarPairs, NewPreviousNameVarPairs, QueryAtom),
  format('  ~w,~n', [QueryAtom]),
  print_previous_queries(QueriesData, NewPreviousNameVarPairs).


% print_previous_query(+QueryData, +PreviousNameVarPairs, -NewPreviousNameVarPairs, -QueryAtom)
print_previous_query(term_data(QueryAtom, NameVarPairs)-same, PreviousNameVarPairs, NewPreviousNameVarPairs, QueryAtom) :-
  % There is no $Var term in the query
  append(NameVarPairs, PreviousNameVarPairs, NewPreviousNameVarPairs),
  !.
print_previous_query(_TermData-OriginalTermData, PreviousNameVarPairs, NewPreviousNameVarPairs, ExpandedTerm) :-
  OriginalTermData = term_data(OriginalTermAtom, OriginalNameVarPairs),
  append(OriginalNameVarPairs, PreviousNameVarPairs, NewPreviousNameVarPairs),
  % Read the original term from the atom
  atom_codes(OriginalTermAtom, OriginalTermCodes),
  append(OriginalTermCodes, [46], OriginalTermCodesWithFullStop),
  read_term_from_codes(OriginalTermCodesWithFullStop, OriginalTerm, [variable_names(OriginalNameVarPairs)]),
  % Expand the term by replacing variables and terms of the form $Var
  expand_term(OriginalTerm, OriginalNameVarPairs, PreviousNameVarPairs, ExpandedTerm).


% expand_term(+Term, +NameVarPairs, +PreviousNameVarPairs, -ExpandedTerm)
%
% NameVarPairs is a list of Name=Var pairs, where Name is the name of a variable Var from the current term.
% PreviousNameVarPairs contains Name=Var pairs from previous queries.
% The term Term is expanded to ExpandedTerm in the following way:
% - If Term is a variable, it is replaced by its Name from NameVarPairs.
% - If Term is of the form $Var:
%   - If the name of the variable Var occurred in one of the previous queries (is contained in PreviousNameVarPairs), $Var is replaced by the variable name.
%   - Otherwise, $Var is replaced by $Name where Name is the name of the variable.
% - If Term is a compound term, its arguments are expanded.
expand_term(Var, NameVarPairs, _PreviousNameVarPairs, Name) :-
  var(Var),
  member(Name=Var, NameVarPairs),
  !.
expand_term(Atomic, _NameVarPairs, _PreviousNameVarPairs, Atomic) :-
  atomic(Atomic),
  !.
expand_term($(Var), NameVarPairs, PreviousNameVarPairs, ExpandedTerm) :-
  !,
  % Get the name of the variable
  var_name(NameVarPairs, Var, Name),
  ( member(Name=_VarValue, PreviousNameVarPairs) ->
    % The variable occurred in one of the previous queries
    ExpandedTerm = Name
  ; otherwise ->
    ExpandedTerm = $(Name)
  ).
expand_term(Term, NameVarPairs, PreviousNameVarPairs, ExpandedTerm) :-
  functor(Term, Name, Arity),
  !,
  functor(ExpandedTerm, Name, Arity),
  expand_args(1, NameVarPairs, PreviousNameVarPairs, Term, ExpandedTerm).


% expand_args(+ArgNum, +NameVarPairs, +PreviousNameVarPairs, +Term, +ExpandedTerm)
expand_args(ArgNum, NameVarPairs, PreviousNameVarPairs, Term, ExpandedTerm) :-
  arg(ArgNum, Term, Arg),
  arg(ArgNum, ExpandedTerm, ExpandedArg),
  !,
  NextArgNum is ArgNum + 1,
  expand_term(Arg, NameVarPairs, PreviousNameVarPairs, ExpandedArg),
  expand_args(NextArgNum, NameVarPairs, PreviousNameVarPairs, Term, ExpandedTerm).
expand_args(_ArgNum, _NameVarPairs, _PreviousNameVarPairs, _Term, _ExpandedTerm).


% var_name(+NameVarPairs, +Var, -Name)
%
% NameVarPairs is a list of Name=Var pairs, where Name is the name of a variable Var.
var_name([Name=SameVar|_NameVarPairs], Var, Name) :-
  Var == SameVar,
  !.
var_name([_NameVarPair|NameVarPairs], Var, Name) :-
  var_name(NameVarPairs, Var, Name).
