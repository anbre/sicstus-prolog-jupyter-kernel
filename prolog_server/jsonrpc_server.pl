
% This is the main module of the SICStus server.
% The predicate jsonrpc_server_start/0 can be called to start the server which enters a loop handling requests from a client.
% The requests and corresponding replies are JSON-RPC 2.0 (https://www.jsonrpc.org/specification) messages sent over the standard streams.
% The handling of those is based on code from 'jsonrpc_server.pl' from SICStus 4.5.1


:- module(sicstus_jsonrpc_server,
    [jsonrpc_server_start/0]).


swi     :- catch(current_prolog_flag(dialect, swi), _, fail), !.
sicstus :- catch(current_prolog_flag(dialect, sicstus), _, fail).


:- use_module(logging, [create_log_file/0, log/1, log/2]).
:- use_module(jupyter, []).
:- use_module(request_handling, [loop/3]).


jsonrpc_server_start :-
  setup,
  logging:create_log_file,
  % Start the loop handling requests from the client
  request_handling:loop(continue, [], _ContOut).


:- if(swi).
setup.
:- else.
setup :-
  % Turn leashing off for all ports so that no user interaction is required when a breakpoint is activated
  leash(off),
  % Make sure that redefinitions are performed without user interaction and warnings are issued
  set_prolog_flag(redefine_warnings, proceed).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle error messages which might be sent by the sub modules

:- if(swi).
:- multifile prolog:message/3.
prolog:message(jupyter(JupyterMessageTerm)) --> !,
  juypter_message(JupyterMessageTerm).
:- else.
:- multifile user:generate_message_hook/3.
user:generate_message_hook(jupyter(JupyterMessageTerm)) --> !,
  juypter_message(JupyterMessageTerm).
user:generate_message_hook(existence_error(user:help,0,procedure,user:help/0,0)) --> !,
  ['Existence error in user:help/0'-[]], [nl],
  ['procedure user:help/0 does not exist'-[]], [nl],
  ['goal:  user:help'-[]], [nl],
  [''-[]], [nl],
  ['However, there is the predicate jupyter:help/0'-[]], [nl].
:- endif.


juypter_message(no_single_goal(Predicate)) --> !,
  ['~w needs to be the only goal in a term'-[Predicate]], [nl].
juypter_message(trace_pred(TracePredSpec)) --> !,
  ['~w cannot be used in a Jupyter application'-[TracePredSpec]], [nl],
  ['However, there is juypter:trace(Goal)'-[]], [nl].
juypter_message(leash_pred) --> !,
  ['The leash mode cannot be changed in a Jupyter application as no user interaction can be provided at a breakpoint'-[]], [nl].
juypter_message(invalid_table_variable_names) --> !,
  ['The list of names needs to be empty or of the same length as the values lists and contain ground terms only'-[]], [nl].
juypter_message(invalid_table_values_lists_length) --> !,
  ['The values lists need to be of the same length'-[]], [nl].
juypter_message(single_test_directive) --> !,
  ['The definition of a unit test cannot be split across multiple cells'-[]], [nl].
