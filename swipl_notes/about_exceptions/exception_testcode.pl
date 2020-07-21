% ===
% Testing code
% ===
% This is built by grepping the throwme/2 calls out of the "jpl.pl" file
% and adding them below, so that all the throwme/2 call can be tested as
% they appear in the code (within limits).
% ===

:- consult(exception_thrower).     % if the code for throwme/2 is in a separate file
:- consult(exception_descriptors). % if the code for the descriptors is in a separate file

:- debug(throwme_plunit).

% ---
% Which "context" terms and "formal" terms are acceptable?
% Note that the arguments of the terms are generally NOT those prescribed by
% the ISO Standard.
% ---

context_ok(context(Location,MsgTxt)) :-
  Location = (Name/Arity),
  atom(Name),
  integer(Arity),
  (var(MsgTxt);atom(MsgTxt)).

formal_ok(instantiation_error).
formal_ok(uninstantiation_error(_Cuplrit)).
formal_ok(type_error(_ValidDomain,_Culprit)).
formal_ok(domain_error(_ValidDomain,_Culprit)).
formal_ok(existence_error(_ObjectType,_Culprit)).
formal_ok(permission_error(_Operation,_PermissionType,_Culprit)).
formal_ok(representation_error(_Flag)).
formal_ok(resource_error(_Resource)).
formal_ok(syntax_error(_ImplDepAtom)).

% ---
% "exc_test/1" receives a Goal which is a call to "throwme/2".
% It calls that Goal and catches any exception, which, if it arrives here and
% was not caught earlier, indicates a test failure.
% ---

exc_test(Goal) :-
   % the recover is just "print & fail" because we expect a catch in "throw_and_catch_iso_exception/1"
   catch(throw_and_catch_iso_exception(Goal),
         Caught,
         (log_exception_term(Caught),fail)).

log_exception_term(Caught) :-
   debug(throwme_plunit,"Generated a non-ISO-Standard exception term: ~q",[Caught]).

% ---
% "throw_and_catch_iso_exception/1" receives a Goal which is a call to throwme/2.
% It calls that Goal and catches any exception with an exception term matching at
% least the form of an ISO-Standard, then performs more checks and possibly fails.
% ---

throw_and_catch_iso_exception(Goal) :-
   catch(Goal,error(Formal,Context),true),
   (context_ok(Context) -> true ; (debug(throwme_plunit, "Bad 'context' term: ~q", [Context]),fail)),
   (formal_ok(Formal)   -> true ; (debug(throwme_plunit, "Bad 'formal' term:  ~q", [Formal]),fail)),
   debug(throwme_plunit,"Good: Formal = ~q, Context = ~q", [Formal,Context]).

:- begin_tests(throwme).

%%%%%
%%%%% Paste output from perl program here
%%%%%
%%%%% test(44) :- exc_test(throwme(jpl_get_static,arg2_is_var)).
%%%%% test(45) :- exc_test(throwme(jpl_get_static,arg2_is_bad(_Fname))).
%%%%%

:- end_tests(throwme).



