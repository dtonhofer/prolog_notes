:- use_module(domain).
:- use_module(validator).
:- use_module(plspec).
:- use_module(library(plunit)).

:- defspec(bar(X), one_of([X, atom(empty)])).
:- defspec(foo(X), bar(X)).
:- defspec(complex(X), one_of([bar(X),X])).
:- defspec(tree(X), one_of([compound(node(tree(X), X, tree(X))),
                            atom(empty)])).

:- begin_tests(atomic).


test(hierarchy) :-
  dom_intersect(int,number,integer),
  dom_intersect(int,atomic,integer),
  dom_intersect(number,atomic,number),
  dom_intersect(atom,atomic,atom).

test(any) :-
  dom_intersect(float,any,float),
  dom_intersect(any,number,number),
  dom_intersect(atom,any,atom).

test(bottom) :-
  dom_intersect(atom,int,bottom),
  dom_intersect(ground,var,bottom),
  dom_intersect(gar,nichts,bottom).

:- end_tests(atomic).

:- begin_tests(indirection).

test(simple1) :-
  dom_intersect(same(isabel),  same(isabel), same(isabel)).

test(simple2) :-
  dom_intersect(bar(number), bar(integer),one_of([integer,atom(empty)])).

test(simple3) :-
  dom_intersect(foo(float), bar(number), one_of([float, atom(empty)])).

test(bar) :-
  dom_intersect(bar(number),float,float),
  dom_intersect(float,bar(number),float),
  dom_intersect(bar(float),number,float),
  dom_intersect(number,bar(float),float).

:- end_tests(indirection).

:- begin_tests(one_of).

test(one_of_member) :-
  dom_intersect(one_of([number, atom]),int,integer),
  dom_intersect(one_of([number, atom]),atom,atom).

test(one_of_not_member) :-
  dom_intersect(one_of([integer, atom]),nothing,bottom).

test(one_of_any) :-
  dom_intersect(one_of([integer, atom]),any,one_of([integer, atom])).

test(two_one_ofs) :-
  dom_intersect(one_of([integer, atom(empty)]), one_of([number, float]), integer),
  dom_intersect(one_of([number, atom(empty)]),
                one_of([integer, atom]),
                one_of([integer, atom(empty)])).

:- end_tests(one_of).

:- begin_tests(and).

test(and_element) :-
  dom_intersect(and([number,atomic]),int,integer),
  dom_intersect(and([number,atomic]),atom,bottom),
  dom_intersect(and([number,atomic]),any,number).

test(and_and) :-
  dom_intersect(and([number, atomic]), and([ground, int]), integer).

:- end_tests(and).

:- begin_tests(tree).

test(tree) :-
  dom_intersect(tree(int), tree(number), one_of(B)),
  spec_indirection(tree(integer),one_of(A)),
  sorted(B,SB),
  sorted(A,SA),
  SA = SB.

:- end_tests(tree).
