:- use_module(enum_domain).
:- use_module(library(ordsets)).

:- debug(enum_domain_test).

:- begin_tests(attributed_variables).


%% DOES NOT WORK BUT SHOULD!

test("assign attribute with single allowed value to unbound variable", true(X == quux)) :-
   debug(enum_domain_test,"assign attribute with single allowed value to unbound variable",[]),
   enum_domain(X,[quux]). % Uses 2nd clause, but hook is not called

%% ***
%% Various other tests that pass nicely
%% ***

% ---

test("retrieve attribute from unattributed unbound variable", fail) :-
   debug(enum_domain_test,"retrieve attribute from unattributed unbound variable",[]),
   enum_domain(_X,_OutSet). % Uses 1st clause

test("retrieve attribute from unattributed bound variable", fail) :-
   debug(enum_domain_test,"retrieve attribute from unattributed bound variable",[]),
   X=alfa,
   enum_domain(X,_OutSet). % Uses 1st clause

test("retrieve attribute from non-variable", fail) :-
   debug(enum_domain_test,"retrieve attribute from non-variable",[]),
   enum_domain(alfa,_OutSet). % Uses 1st clause

% ---

test("assign attribute to unbound variable", true) :-
   debug(enum_domain_test,"assign attribute to unbound variable",[]),
   enum_domain(_X,[foo,bar,baz]). % Uses 2nd clause but hook is not called

test("assign attribute to bound variable with disallowed value", fail) :-
   debug(enum_domain_test,"assign attribute to bound variable with disallowed value",[]),
   X=alfa,
   enum_domain(X,[foo,bar,baz]). % Uses 2nd clause, and hook is called (and fails) *****************

test("assign attribute to bound variable with allowed value", true) :-
   debug(enum_domain_test,"assign attribute to bound variable with allowed value",[]),
   X=foo,
   enum_domain(X,[foo,bar,baz]). % YES IT WORKS! Uses 2nd clause, and hook is called (and succeeds) **************

test("assign attribute to non-variable with disallowed value", fail) :-
   debug(enum_domain_test,"assign attribute to non-variable with disallowed value",[]),
   enum_domain(alfa,[foo,bar,baz]). % Uses 2nd clause, and hook is called (and fails) *****************

test("assign attribute to non-variable with allowed value", true) :-
   debug(enum_domain_test,"assign attribute to non-variable with allowed value",[]),
   enum_domain(foo,[foo,bar,baz]). % YES IT WORKS! Uses 2nd clause, and hook is called (and succeeds) **************

% ---

test("assign new 'enum domain' to an unbound variable, then retrieve it", true(OutSet == InSetOrd)) :-
   debug(enum_domain_test,"assign new 'enum domain' to an unbound variable, then retrieve it",[]),
   InSet = [foo,bar,baz],
   enum_domain(X,InSet),  % Uses 2nd clause but hook is not called
   enum_domain(X,OutSet), % Uses 1st clause
   assertion(is_ordset(OutSet)),
   list_to_ord_set(InSet, InSetOrd).

% ---

test("unify two attributed variables with the allowed values equal", true(ATTS == [bar,baz,foo])) :-
   debug(enum_domain_test,"unify two attributed variables with the allowed values equal",[]),
   enum_domain(X,[foo,bar,baz]),   % Uses 2nd clause but hook is not called
   enum_domain(Y,[foo,bar,baz]),   % Uses 2nd clause but hook is not called
   X = Y,                          % Hook is called and succeeds
   get_attrs(X,att(_,ATTS,[])).    % ATTS is the ordset of [foo,bar,baz]

test("unify two attributed variables with the allowed values intersection having two values", true(ATTS == [bar,foo])) :-
   debug(enum_domain_test,"unify two attributed variables with the allowed values intersection having two values",[]),
   enum_domain(X,[foo,bar,quux]),  % Uses 2nd clause but hook is not called
   enum_domain(Y,[foo,bar,zool]),  % Uses 2nd clause but hook is not called
   X = Y,                          % Hook is called and succeeds
   get_attrs(X,att(_,ATTS,[])).    % ATTS is the ordset of [foo,bar]

test("unify two attributed variables with the allowed values intersection having one value", true(X == foo)) :-
   debug(enum_domain_test,"unify two attributed variables with the allowed values intersection having one value",[]),
   enum_domain(X,[foo,bar,baz]),   % Uses 2nd clause but hook is not called
   enum_domain(Y,[foo,quux,zool]), % Uses 2nd clause but hook is not called
   X = Y.                          % Hook is called and succeeds, and X is set to the unique value allowed

test("unify two attributed variables with the allowed values intersection being empty", fail) :-
   debug(enum_domain_test,"unify two attributed variables with the allowed values intersection being empty",[]),
   enum_domain(X,[foo,bar]),       % Uses 2nd clause but hook is not called
   enum_domain(Y,[quux,zool]),     % Uses 2nd clause but hook is not called
   X = Y.                          % Hook is called and fails as no solutions left

:- end_tests(attributed_variables).

