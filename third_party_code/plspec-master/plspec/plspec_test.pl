:- module(plspec_test, []).
:- use_module(plspec_core).
:- use_module(library(plunit)).
:- enable_all_spec_checks.
:- use_module('plspec_test.plspec').
:- use_module(library(clpfd)).




:- spec_pre(my_member/2,[any,[any]]).
:- spec_post(my_member/2,[any,[ground]],[ground,[ground]]).
:- spec_post(my_member/2,[any,var],[any,[any]]).
my_member(E,[E|_]).
my_member(E,[_|T]) :-
  my_member(E,T).


:- spec_pre(my_member_int/2,[int,[int]]).
my_member_int(E,[E|_]).
my_member_int(E,[_|T]) :-
  my_member_int(E,T).



:- spec_pre(my_member_specific/2, [X,list(X)]).
my_member_specific(E,[E|_]) :- !.
my_member_specific(E,[_|T]) :-
  my_member_specific(E,T).




:- begin_tests(specific_any, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).
test(valid_my_member_int, [nondet]) :-
  my_member_int(1,[1,2,3]),
  \+ my_member_int(1,[4,5]).

test(invalid_my_member_int, [throws(_)]) :-
  my_member_int(a,[1,2,3]).

test(valid_my_member_specific, [nondet]) :-
  my_member_specific(1,[1,2,3]),
  \+ my_member_specific(a,[b,c]).

test(tree_no_int, [throws(_)]) :-
  my_member_specific(node(empty, 1, empty), [1,2,3]).

test(empty_is_atomic) :- % this works, because empty and 1 are both atomic.
  \+ my_member_specific(empty, [1,2,3]).

test(tree_in_tree_out) :-
  my_member_specific(empty, [empty, node(empty, 1, empty)]),
  my_member_specific(node(empty, 1, empty), [node(empty, 1, empty), node(empty, 2, empty)]),
  \+ my_member_specific(node(empty, 1, node(empty, 2, empty)), [node(empty, 1, empty), node(empty, 2, empty)]).

test(tree_no_atom, [throws(_)]) :-
  my_member_specific(node(node(empty, a, empty), b, empty), [these, are, atoms]).

:- end_tests(specific_any).

:- begin_tests(my_member_spec, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(instantiated_call, [nondet]) :-
  my_member(a, [a, b, c]).

test(partly_instantiated, [nondet]) :-
  my_member(_, [a, b, c]).

test(partly_instantiated_not_conform, [nondet, throws(_)]) :-
  % second argument is not a list
  my_member(a, _).

test(partly_instantiated3, [nondet]) :-
  my_member(c, [a, _, c]).

test(all_variables_not_conform, [nondet, throws(_)]) :-
  my_member(_, _).

test(not_conform, [throws(_)]) :-
  my_member([], a).


:- end_tests(my_member_spec).



%% multiple pres
:- spec_pre(my_compound_foo/1, [compound(foo(int))]).
:- spec_pre(my_compound_foo/1, [compound(foo(atomic))]).
:- spec_post(my_compound_foo/1, [compound(foo(ground))], [compound(foo(ground))]).
my_compound_foo(foo(_)).



:- begin_tests(my_compound_foo, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(nonconform_call, [throws(_)]) :-
  my_compound_foo(foo(_)).

test(conform_call1, [nondet]) :-
  my_compound_foo(foo(1)).

test(conform_call2, [nondet]) :-
  my_compound_foo(foo(bar)).

test(not_conform, [throws(_)]) :-
  my_compound_foo(bar(_)).

:- end_tests(my_compound_foo).


:- spec_pre(my_tuple_with_incorrect_spec/1, [tuple([atomic, atomic])]).
:- spec_post(my_tuple_with_incorrect_spec/1, [tuple([var, var])], [tuple([atomic, atomic])]).
my_tuple_with_incorrect_spec([X, X]).


bar(A) :-
  my_tuple_with_incorrect_spec(A).

:- begin_tests(my_tuple, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(conform) :-
  my_tuple_with_incorrect_spec([a, a]).

test(conform_var, [throws(_)]) :-
  my_tuple_with_incorrect_spec([a, _]).

test(conform_var2, [throws(_)]) :-
  my_tuple_with_incorrect_spec([_, a]).

test(nonconform_both_var, [throws(_)]) :-
  my_tuple_with_incorrect_spec([_, _]).

:- end_tests(my_tuple).


:- spec_pre(atom_member/2, [atomic, [atomic]]).
atom_member(X, [X|_]) :- !.
atom_member(X, [_|T]) :-
  atom_member(X, T).


:- begin_tests(atom_member, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(conform, [nondet]) :-
  atom_member(a, [a,b,c]).

test(not_conform, [throws(_)]) :-
  atom_member(a, X), !,
  X = [a,b,c].

test(not_conform2, [throws(_)]) :-
  atom_member(a, [a,_|_]).

test(not_conform3, [throws(_)]) :-
  atom_member(a, [1,_|_]).


:- end_tests(atom_member).


:- spec_pre(my_atomic/1, [one_of([atomic, [atomic]])]).
my_atomic([_|_]) :- !, fail.
my_atomic(_).

:- begin_tests(my_atomic, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(conform) :-
  my_atomic([]).

test(conform2) :-
  \+ my_atomic([foo]).

test(conform3) :-
  my_atomic(foo).

test(conform4) :-
  my_atomic(2).

test(not_conform1, [throws(_)]) :-
  my_atomic(foo(_)).

test(not_conform2, [throws(_)]) :-
  \+ my_atomic([_]).

test(not_conform3, [throws(_)]) :-
  my_atomic([[2]]).

test(not_conform4, [throws(_)]) :-
  \+ my_atomic([_]).

:- end_tests(my_atomic).


not_my_atomic(X) :-
  \+ my_atomic(X).

if_atom_then_my_atomic(X) :-
  (atom(X) -> my_atomic(X)).

if_my_atomic_then_atom(X) :-
  (my_atomic(X) -> atom(X)).


:- spec_pre(my_or_test/1, [one_of([ground, ground])]).
my_or_test(_).

:- begin_tests(my_or_test, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(conform) :-
  my_or_test(foo).

test(nonconform, [throws(_)]) :-
  my_or_test(_).

:- end_tests(my_or_test).


:- spec_pre(my_and_test/1, [and([atomic, ground])]).
my_and_test(_).

:- begin_tests(my_and_test, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(conform) :-
  my_and_test(foo).

test(nonconform, [throws(_)]) :-
  my_and_test([1]).

test(nonconform2, [throws(_)]) :-
  my_and_test(_).

:- end_tests(my_and_test).


:- spec_pre(invariant_violator/1, [any]).
:- spec_invariant(invariant_violator/1, [atomic]).
invariant_violator(X) :-
  X = [1], X == [2]. % fail in a more sophisticated way
invariant_violator(a).

:- begin_tests(invariant_violator_test, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(conform) :-
  invariant_violator(a).

test(conform2) :-
  \+ invariant_violator(b).

test(nonconform, [throws(_)]) :-
  invariant_violator(_).

:- end_tests(invariant_violator_test).

:- spec_pre(partial_instantiator/1, [[any]]).
:- spec_invariant(partial_instantiator/1, [[int]]).
partial_instantiator([_,_]).
partial_instantiator([1,_]).
partial_instantiator([_,2]).
partial_instantiator([_,a]).

:- begin_tests(partial_invariant_instantiation, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(conform, [nondet]) :-
  partial_instantiator([_,_]).

test(conform2, [nondet]) :-
  partial_instantiator([X, _]), X == 1.

test(conform3, [nondet]) :-
  partial_instantiator([_, 2]).

test(conform4, [nondet]) :-
  partial_instantiator([1, 2]).

test(nonconform, [throws(_)]) :-
  partial_instantiator([a, 2]).

test(nonconform2, [throws(_)]) :-
  findall(A, partial_instantiator(A), _).

:- end_tests(partial_invariant_instantiation).




:- defspec(tree(X), one_of([compound(node(tree(X), X, tree(X))),
                            atom(empty)])).

:- begin_tests(trees, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(empty_is_tree) :-
  valid(tree(int), empty).

test(tree1) :-
  valid(tree(int), node(empty, 1, empty)).

test(tree2) :-
  valid(tree(int),  node(node(empty, 1, empty),
                    2,
                    node(empty, 3, empty))).

test(tree3) :-
  \+ valid(tree(int), node(node(empty, 1, empty),
                      a,
                      node(empty, 3, empty))).

test(tree3) :-
  \+ valid(tree(int), node(node(empty, 1, emppty),
                      1,
                      node(empty, 3, empty))).

:- end_tests(trees).




:- spec_pre(bind_to_zero/1, [any]).
:- spec_post(bind_to_zero/1, [any], [list(any)]).
:- spec_post(bind_to_zero/1, [var], [ground]).
bind_to_zero(0).

:- begin_tests(violated_postcondition, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(postcondition_violated, [throws(_)]) :-
  bind_to_zero(_).

:- end_tests(violated_postcondition).


this_pred_has_an_extern_spec(_).

:- begin_tests(externally_stored_spec, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(ints_are_okay) :-
  this_pred_has_an_extern_spec(1).

test(atoms_are_not, [throws(_)]) :-
  this_pred_has_an_extern_spec(a).

:- end_tests(externally_stored_spec).
