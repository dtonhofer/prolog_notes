:- begin_tests(various).

test("elide the option 'true'") :-
   true.

test("add the option 'true' but elide putting it into a list", true) :-
   true.

test("fully specified option list", [true]) :-
   true.

test("fully specified option list for failure", [fail]) :-
   fail.

test("fully specified option list with test expression", [true(X == alpha)]) :-
   X = alpha.

test("fully specified option list with variable holding test expression", [true(T)]) :-
   X = alpha,
   T = (X == alpha).

test("expect a certain exception by giving a matching 'formal'", [error(type_error(_,_))]) :-
   type_error(integer,"bad dog").

test("catch anything, even non ISO standard exception terms",[throws(_)]) :-
   bagof(X,throw_on_redo(X),_).

test("no redo, choicepoint left open, will warn") :-
   throw_on_redo(1).

test("no redo, choicepoint left open, warning suppressed",[nondet]) :-
   throw_on_redo(1).

test("let plunit collect them all, verify in the head",[all(X == [1,2])]) :-
   nondeterminism(X).

test("make body deterministic") :-
   once(nondeterminism(_)).

test("collect them yourself and solution-unify in the body") :-
   bagof(X,nondeterminism(X),[1,2]).

test("collect them yourself and verify separately in the body") :-
   bagof(X,nondeterminism(X),Bag),
   Bag == [1,2].

test("collect them yourself and verify in the head", [true(Bag == [1,2])]) :-
   bagof(X,nondeterminism(X),Bag).

test("collect them yourself, built a verifcation expression, verify in the head", [true(T)]) :-
   bagof(X,nondeterminism(X),Bag),
   T = (Bag == [1,2]).

test("test given in manual: collect them yourself and verify separately in the body") :-
   findall(X, member(X, [a,b,c]), Xs),
   Xs == [a,b,c].

test("test given in manual: let plunit collect them all, verify in the head", all(X == [a,b,c])) :-
   member(X, [a,b,c]).

test("fails because it needs three results, but there are two",[fail]) :-
   bagof(X,nondeterminism(X),[_,_,_]).

test("fails because it wants one result, but there are two",[fail]) :-
   bagof(X,nondeterminism(X),[_]).

test("generating only the first 6 solutions instead of many more, using limit/2",[true(T)]) :-
   bagof(X,limit(6,between(1,100,X)),Bag),
   T = (Bag == [1,2,3,4,5,6]).

% Helper predicates inside the unit testing block

throw_on_redo(X) :- X=1.
throw_on_redo(_) :- throw("Ouch!").

nondeterminism(X) :- member(X,[1,2]).

:- end_tests(various).
