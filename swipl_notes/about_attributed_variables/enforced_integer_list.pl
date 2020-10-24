:- module(enforced_integer_list, [ list_of_int/2 ]).

:- debug(enforced_integer_list).

% ---
% Service predicate
% ---
% Create a list of variables that can only be unified with integers.
% This is a low-effort implementation, as it covers only one way of
% how this could be called: creation of a new list of unbound variables.

list_of_int(Length,List) :-
   must_be(nonneg,Length),
   must_be(var,List),
   length(List,Length),
   maplist([X]>>put_attr(X,enforced_integer_list,make_sure_it_is_an_int),List).

% ---
% The hook called on unification
% ---

attr_unify_hook(make_sure_it_is_an_int,PUV) :-
   integer(PUV) -> case_integer(PUV) ;
   string(PUV)  -> case_string(PUV)  ;
   var(PUV)     -> case_var(PUV)     ;
   case_default.

% ---
% Unification hook catch-all success
% ---

attr_unify_hook(ATTV,_PUV) :-
   ATTV \== make_sure_it_is_an_int,
   debug(enforced_integer_list,"attr_unify_hook/2 called with unknown ATTV: ~q",[ATTV]).

% ---
% Cases
% ---

case_integer(PUV) :-
   debug(enforced_integer_list,"Trying to unify with an integer: ~q. Unification okayed!",[PUV]).

case_string(PUV) :-
   debug(enforced_integer_list,"Received string '~s'; this means throwing",[PUV]),
   type_error(integer,PUV).

case_var(PUV) :-
   debug(enforced_integer_list,"Trying to fuse two variables. Unification okayed!",[]),
   assertion(get_attr(PUV,enforced_integer_list,make_sure_it_is_an_int)).

case_default :-
   debug(enforced_integer_list,"Trying to unify with something that is neither an integer nor an unbound variable: Vetoed!",[]),
   fail.
