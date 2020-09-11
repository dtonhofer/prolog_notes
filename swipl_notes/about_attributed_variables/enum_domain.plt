:- use_module(enum_domain).
:- use_module(library(ordsets)).

:- begin_tests(attributed_variables).


%% DOES NOT WORK BUT SHOULD!

test("assign attribute with single allowed value to unbound variable", true(X == quux)) :-
   enum_domain(X,[quux]).


% ---

test("retrieve attribute from unattributed unbound variable", fail) :-
   enum_domain(_X,_OutSet).

test("retrieve attribute from unattributed bound variable", fail) :-
   X=alfa,enum_domain(X,_OutSet).

test("retrieve attribute from non-variable", fail) :-
   enum_domain(alfa,_OutSet).

% ---

test("assign attribute to unbound variable", true) :-
   enum_domain(_X,[foo,bar,baz]).

test("assign attribute to bound variable with disallowed value", fail) :-
   X=alfa,enum_domain(X,[foo,bar,baz]).

test("assign attribute to bound variable with allowed value", true) :-
   X=foo,enum_domain(X,[foo,bar,baz]). % YES IT WORKS!

test("assign attribute to non-variable with disallowed value", fail) :-
   enum_domain(alfa,[foo,bar,baz]).

test("assign attribute to non-variable with allowed value", true) :-
   enum_domain(foo,[foo,bar,baz]).

% ---

test("assign new 'enum domain' to an unbound variable, then retrieve it", true(OutSet == InSetOrd)) :-
   InSet = [foo_4,bar_4,baz_4],
   enum_domain(X,InSet),
   enum_domain(X,OutSet),
   assertion(is_ordset(OutSet)),
   list_to_ord_set(InSet, InSetOrd).

% ---

test("unify two attributed variables with the allowed values equal", true) :-
   enum_domain(X,[foo,bar,baz]),
   enum_domain(Y,[foo,bar,baz]),
   X = Y.

test("unify two attributed variables with the allowed values intersection having two values", true) :-
   enum_domain(X,[foo,bar,quux]),
   enum_domain(Y,[foo,bar,zool]),
   X = Y.

test("unify two attributed variables with the allowed values intersection having one value", true(X == foo)) :-
   enum_domain(X,[foo,bar,baz]),
   enum_domain(Y,[foo,quux,zool]),
   X = Y.

test("unify two attributed variables with the allowed values intersection being empty", fail) :-
   enum_domain(X,[foo,bar]),
   enum_domain(Y,[quux,zool]),
   X = Y.

:- end_tests(attributed_variables).

