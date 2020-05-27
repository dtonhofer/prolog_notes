:- module(validator_test, []).
:- use_module(validator).
:- use_module(plspec).
:- use_module(library(plunit)).
:- use_module(validator).


:- begin_tests(setup).

test(spec_predicate) :-
  spec_predicate(atom, atom),
  spec_predicate(atom(X), atom(X)),
  spec_predicate(integer, integer),
  spec_predicate(number, number),
  spec_predicate(float, float),
  spec_basic(var, var),
  spec_basic(ground, ground),
  spec_basic(nonvar, nonvar),
  spec_basic(any, true).

test(spec_exists_integer) :-
  spec_exists(integer).

test(spec_exists_list) :-
  spec_exists(list(integer)).

:- end_tests(setup).

:- begin_tests(valid).

test(any) :-
  valid(any, _),
  valid(any, 1),
  valid(any, []),
  valid(any, foo(_, _)),
  valid(any, foo).

test(ground) :-
  \+ valid(ground, _),
  valid(ground, 1),
  valid(ground, []),
  \+ valid(ground, foo(_, _)),
  valid(ground, foo(1, 2)),
  valid(ground, foo).

test(list) :-
  \+ valid([any], _),
  valid([any], []),
  valid([any], [a]),
  valid([any], [1]),
  valid([any], [_]),
  valid([any], [[]]),
  valid([any], [any]).

test(list2) :-
  valid([integer], [1,2]).

test(list_of_list) :-
  \+ valid([[any]], _),
  \+ valid([[any]], [a]),
  \+ valid([[any]], [_]),
  valid([[any]], []),
  valid([[any]], [[1]]),
  valid([[any]], [[a]]),
  valid([[any]], [[]]).

test(compounds) :-
  valid(compound(foo(any)), foo(_)),
  valid(compound(foo(any)), foo(a)),
  \+ valid(compound(foo(any)), bar(a)),
  \+ valid(compound(foo(any, any)), foo(a)),
  valid(compound(foo(any, any)), foo(a, a)),
  \+ valid(compound(foo(any, var)), foo(a, a)).

test(tuples) :-
  valid(tuple([any]), [_]),
  \+ valid(tuple([any]), []),
  \+ valid(tuple([any]), [_, _]),
  valid(tuple([any, any]), [_, _]).

test(indirection) :-
  valid(integer, 3).

test(one_of) :-
  valid(one_of([integer, atomic]), 3),
  valid(one_of([integer, atomic]), abc),
  \+ valid(one_of([integer, atomic]), [1]),
  \+ valid(one_of([integer, atomic]), _).

test(and) :-
  valid(and([integer, ground]), 3),
  \+ valid(and([integer, var]), 3).

test(valid_det) :-
  valid(int,3),
  valid(atomic,a).

test(valid_ground, [nondet]) :-
  valid(_,3).

:- end_tests(valid).

:- defspec_pred(int_oddity(X), int_oddity(X)).

int_oddity(even, X) :- integer(X), 0 is X mod 2.
int_oddity(odd, X) :- integer(X), 1 is X mod 2.

:- begin_tests(self_defined_int, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(zero_is_even) :-
  valid(int_oddity(even), 0).

test(zero_is_not_odd) :-
  \+ valid(int_oddity(odd), 0).

test(one_is_odd) :-
  valid(int_oddity(odd), 1).

test(one_is_not_even) :-
  \+ valid(int_oddity(even), 1).

:- end_tests(self_defined_int).

:- defspec(tree(X), one_of([compound(node(tree(X), X, tree(X))),
                            atom(empty)])).




:- begin_tests(tree).
test(valid_trees, [nondet]) :-
  valid(tree(int), node(empty, 1, empty)),
  valid(tree(list(atomic)), node(node(empty, [2], empty), [1], empty)),
  valid(tree(_), empty),
  valid(tree(_), node(empty, root, empty)).

test(invalid_trees) :-
  \+ valid(tree(int),1),
  \+ valid(tree(int), node(empty, no, empty)),
  \+ valid(tree(int), tree(empty, 1, empty)).

:- end_tests(tree).
