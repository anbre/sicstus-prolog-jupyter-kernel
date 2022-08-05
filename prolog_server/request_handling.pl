
% This module provides predicates to start a loop reading and handling JSON RPC requests.

% This is done by starting a loop which:
% - Reads a message from the standard input stream with jsonrpc:next_jsonrpc_message/1.
% - Checks if the message is a valid request with dispatch_message/3.
% - Checks the method of the request with dispatch_request/4, handles it accordingly and sends a response to the client.
%   There are four methods:
%   - call: execute any terms (handled by the module term_handling)
%   - version: retrieve the SICStus version
%   - predicates: find built-in and exported predicates
%   - jupyter_predicate_docs: retrieve the docs of the predicates in the module jupyter

% In case of a call request, the request might contain multiple terms.
% They are handled one by one and the remaining ones are asserted with request_data/2.
% They need to be asserted so that "retry." terms can fail into the previous call.
% If the term produces any result, it is asserted with term_handling:term_response/1.
% Once all terms of a request are handled, their results are sent to the client.


:- module(request_handling,
    [loop/3]).  % loop(+ContIn, +Stack, -ContOut)


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(library(codesio), [write_term_to_codes/3, format_to_codes/3]).
:- use_module(logging, [log/1, log/2]).
:- use_module(jsonrpc, [send_success_reply/2, send_error_reply/3, next_jsonrpc_message/1, parse_json_terms_request/3]).
:- use_module(term_handling, [handle_term/6, test_definition_end/1, pred_definition_specs/1, term_response/1]).
:- use_module(output, [send_reply_on_error/0, exception_message/2]).
:- use_module(jupyter, []).


% Assert the terms which were read from the current request so that "retry." terms can fail into the previous call
:- dynamic request_data/2.  % request_data(CallRequestId, TermsAndVariables)
                            % TermsAndVariables is a list with elements of the form Term-Bindings.
                            % Each of the terms Term can be a directive, clause definition, or query.
                            % Bindings is a list of variable name and variable mappings (of the form Name=Var) which occur in the corresponding term Term.


:- if(swi).
:- multifile user:message_hook/3.

:- dynamic user:message_hook/3.

user:message_hook(MessageTerm, error, _Lines) :-
  handle_unexpected_exception(MessageTerm).
:- else.
:- multifile user:portray_message/2.

user:portray_message(error, MessageTerm) :-
  handle_unexpected_exception(MessageTerm).
:- endif.


% handle_unexpected_exception(+MessageTerm)
%
% Handle an unexpected exception.
% Send an error reply to let the client know that the server is in a state from which it cannot recover and therefore needs to be killed and restarted.
handle_unexpected_exception(MessageTerm) :-
  output:send_reply_on_error,
  logging:log(MessageTerm),
  % Retract all data of the current request
  retract(request_data(_CallRequestId, _TermsAndVariables)),
  % Use catch/3, because no clauses might have been asserted
  catch(retractall(term_handling:pred_definition_specs(_)), _, true),
  % Delete the test definition file
  test_definition_end(false),
  % Send an error response
  output:exception_message(MessageTerm, ExceptionMessage),
  jsonrpc:send_error_reply(@(null), unhandled_exception, ExceptionMessage),
  fail.


% loop(+ContIn, +Stack, -ContOut)
%
% Read and process requests from the client.
% Called to start processing requests and after calling a goal to provide the ability to compute another solution for a goal on the stack Stack.
% Succeeds with ContOut = cut if it receives a request to cut an active goal.
% Succeeds with ContOut = done if it receives a request to quit.
% Fails if it receives a request to retry an active goal - this causes the call to compute the next solution.
loop(Cont, _Stack, _ContOut) :-
  var(Cont), !,
  fail.
loop(done, _Stack, done) :-
  !,
  send_responses.
loop(cut, _Stack, cut) :- !.
loop(continue, Stack, ContOut) :-
  handle_next_term_or_request(Stack, Cont),
  loop(Cont, Stack, ContOut).


% handle_next_term_or_request(+Stack, -Cont)
%
% Handles the next term or request.
% One call request can contain more than one term.
% Terms of the current request which have not been processed yet are asserted as request_data(CallRequestId, TermsAndVariables).
handle_next_term_or_request(Stack, Cont) :-
  request_data(CallRequestId, TermsAndVariables),
  TermsAndVariables = [Term-Variables|RemainingTermsAndVariables],
  !,
  % Continue processing terms of the current request
  retract(request_data(CallRequestId, TermsAndVariables)),
  assert(request_data(CallRequestId, RemainingTermsAndVariables)),
  term_handling:handle_term(Term, false, CallRequestId, Stack, Variables, Cont).
handle_next_term_or_request(Stack, Cont) :-
  % All terms of the current request have been processed -> send their results to the client
  request_data(_CallRequestId, []),
  !,
  send_responses,
  % Read the next request
  jsonrpc:next_jsonrpc_message(Message),
  dispatch_message(Message, Stack, Cont).
handle_next_term_or_request(Stack, Cont) :-
  % First request
  % Read and handle the next request from the client
  jsonrpc:next_jsonrpc_message(Message),
  dispatch_message(Message, Stack, Cont).


% Get all term responses which were asserted as term_response(JsonResponse).
% Send a json response object where
% - the keys are the indices of the Prolog terms from the request starting from 1
% - the values are json objects representing the result of the corresponding Prolog term
send_responses :-
  % Retract all data of the current request
  retract(request_data(CallRequestId, _)),
  % Use catch/3, because no clauses might have been asserted
  catch(retractall(term_handling:pred_definition_specs(_)), _, true),
  % If any tests were defined by the current request, load the definitions
  test_definition_end(true),
  % Collect the responses and send them to the client
  term_responses(1, TermResponses),
  send_success_reply(CallRequestId, json(TermResponses)).


% term_responses(+CurrentNum, -TermResponses)
term_responses(Num, [NumAtom=Response|TermResponses]) :-
  term_response(Response),
  retract(term_response(Response)),
  !,
  number_codes(Num, NumCodes),
  atom_codes(NumAtom, NumCodes),
  NextNum is Num+1,
  term_responses(NextNum, TermResponses).
term_responses(_Num, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Request handling

% dispatch_message(+Message, +Stack, -Cont)
%
% Checks if the message is a valid request message.
% If so, handles the request.
% Otherwise, an error response is sent.
dispatch_message(Message, Stack, Cont) :-
  Message = request(Method,_Id,_Params,_RPC), !,
  dispatch_request(Method, Message, Stack, Cont).
dispatch_message(invalid(RPC), _Stack, continue) :-
  % Malformed -> the Id must be null
  jsonrpc:send_error_reply(@(null), invalid_request, RPC).


% dispatch_request(+Method, +Message, +Stack, -Cont)
%
% Checks the request method and handles the request accordingly.
dispatch_request(call, Message, Stack, Cont) :-
  !,
  Message = request(_Method,CallRequestId,Params,RPC),
  jsonrpc:parse_json_terms_request(Params, TermsAndVariables, ParsingError),
  ( var(TermsAndVariables) ->
    !,
    % An error occurred when parsing the json request
    handle_parsing_error(ParsingError, CallRequestId, RPC),
    Cont = continue
  ; TermsAndVariables = [] ->
    !,
    % The request does not contain any term
    jsonrpc:send_success_reply(CallRequestId, ''),
    Cont = continue
  ; TermsAndVariables = [Term-Variables] ->
    !,
    % The request contains one term
    % Normally this is a goal which is to be evaluated
    assert(request_data(CallRequestId, [])),
    term_handling:handle_term(Term, true, CallRequestId, Stack, Variables, Cont)
  ; otherwise ->
    % The request contains multiple terms
    % Process the first term and assert the remaining ones
    % This is needed so that "retry." terms can fail into the previous call
    TermsAndVariables = [Term-Variables|RemainingTermsAndVariables],
    assert(request_data(CallRequestId, RemainingTermsAndVariables)),
    term_handling:handle_term(Term, false, CallRequestId, Stack, Variables, Cont)
  ).
:- if(sicstus).
dispatch_request(version, Message, _Stack, continue) :-
  !,
  % Send the SICStus version to the client
  Message = request(_Method,CallRequestId,_Params,_RPC),
  prolog_flag(version_data, sicstus(Major,Minor,Revision,_Beta,_Extra)),
  format_to_codes('~d.~d.~d', [Major, Minor, Revision], VersionCodes),
  atom_codes(VersionAtom, VersionCodes),
  jsonrpc:send_success_reply(CallRequestId, VersionAtom).
:- endif.
dispatch_request(predicates, Message, _Stack, continue) :-
  Message = request(_Method,CallRequestId,_Params,_RPC),
  !,
  % Find all callable (built-in and exported) predicates and send them to the client
  findall(Pred, generate_built_in_pred(Pred), BuiltInPreds),
  findall(Pred, generate_exported_pred(Pred), ExportedPreds),
  append(ExportedPreds, BuiltInPreds, CurrentPreds),
  % convert the predicates into atoms so that they are JSON parsable
  findall(PredAtom, (member(CurPred, CurrentPreds), predicate_atom(CurPred, PredAtom)), PredAtoms),
  jsonrpc:send_success_reply(CallRequestId, PredAtoms).
dispatch_request(jupyter_predicate_docs, Message, _Stack, continue) :-
  % Retrieve the docs of the predicates in the module jupyter and send them to the client
  Message = request(_Method,CallRequestId,_Params,_RPC),
  !,
  jupyter:predicate_docs(PredDocs),
  jsonrpc:send_success_reply(CallRequestId, json(PredDocs)).
dispatch_request(Method, Message, _Stack, continue) :-
  % Make sure that a 'retry' call can fail
  Method \= call,
  Message = request(_,Id,_Params,RPC), !,
  jsonrpc:send_error_reply(Id, method_not_found, RPC).


% handle_parsing_error(+ParsingError, +CallRequestId, +RPC)
handle_parsing_error(ParsingError, CallRequestId, _RPC) :-
  nonvar(ParsingError),
  !,
  % Parsing error when reading the terms from the request
  ParsingError = parsing_error(_Exception, ExceptionMessage),
  jsonrpc:send_error_reply(CallRequestId, exception, ExceptionMessage).
handle_parsing_error(_ParsingError, CallRequestId, RPC) :-
  % Malformed request
  jsonrpc:send_error_reply(CallRequestId, invalid_params, RPC).


% generate_built_in_pred(-PredicateHead)
:- if(swi).
generate_built_in_pred(Head) :-
  predicate_property(system:Head, built_in),
  functor(Head, Name, _Arity),
  % Exclude reserved names
  \+ sub_atom(Name, 0, _, _, $).
:- else.
generate_built_in_pred(Head) :-
  predicate_property(Head, built_in),
  functor(Head, Name, _Arity),
  % Exclude the 255 call predicates
  Name \= call.
generate_built_in_pred(call(_)).
:- endif.


% generate_exported_pred(-ModuleNameExpandedPredicateHead)
generate_exported_pred(Module:Pred) :-
  ServerModules = [jsonrpc, logging, output, request_handling, sicstus_jsonrpc_server, variable_bindings, term_handling],
  predicate_property(Module:Pred, exported),
  % Exclude exported predicates from any of the modules used for this server except for 'jupyter'
  \+ member(Module, ServerModules).


% predicate_atom(+Predicate, -PredicateAtom)
%
% PredicateAtom is an atom created from Predicate by replacing all variables in it with atoms starting from 'A'.
predicate_atom(Predicate, PredicateAtom) :-
  % Create a Name=Var pairs list as can be used for write_term_to_codes/3
  term_variables(Predicate, Variables),
  name_var_pairs(Variables, 65, Bindings), % 65 is the ASCII code for 'A'
  write_term_to_codes(Predicate, PredicateCodes, [variable_names(Bindings)]),
  atom_codes(PredicateAtom, PredicateCodes).


% name_var_pairs(+Variables, +CurrentCharacterCode, -Bindings)
name_var_pairs([], _CurrentCharacterCode, []) :- !.
name_var_pairs([Variable|Variables], CurrentCharacterCode, [NameAtom=Variable|Bindings]) :-
  atom_codes(NameAtom, [CurrentCharacterCode]),
  NextCharacterCode is CurrentCharacterCode + 1,
  name_var_pairs(Variables, NextCharacterCode, Bindings).
