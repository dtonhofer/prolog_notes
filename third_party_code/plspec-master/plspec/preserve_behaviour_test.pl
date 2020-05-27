:- use_module(plspec_core).
:- use_module(library(plunit)).
:- set_loglevel(error).

:- enable_all_spec_checks.

fib(N,_) :-
    N < 0, !, fail.
fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, X) :-
    M is N-1,
    K is N-2,
    fib(M,A),
    fib(K,B),
    X is A+B.

is_fib(X) :-
    integer(X),
    is_fib(0,X).
is_fib(N,X) :-
    fib(N,P),
    (P == X ->
        true
    ;
        (P < X ->
            M is N+1,
            is_fib(M,X)
        ;
            fail
        )
    ).

:- defspec_pred(fib, is_fib).

:- defspec(tree(X), one_of([compound(node(tree(X), X, tree(X))),
                            atom(empty)])).

:- spec_pre(elem_tree_to_list_tree/2, [tree(X), tree([X])]).
elem_tree_to_list_tree(empty, empty) :- !.
elem_tree_to_list_tree(node(A,X,B), node(NA,[X], NB)) :-
    elem_tree_to_list_tree(A,NA),
    elem_tree_to_list_tree(B,NB).
:- defspec(tree(X), one_of([compound(node(tree(X), X, tree(X))),
                            atom(empty)])).



:- spec_pre(next_fib/2, [number, var]).
:- spec_post(next_fib/2, [number, var], [number, fib]).
next_fib(X, Y) :-
    P is 2*X,
    between(X,P,Y),
    Y > X,
    is_fib(Y), !.

next_fib_no_annotations(X,Y) :-
    P is 2*X,
    between(X,P,Y),
    Y > X,
    is_fib(Y), !.

%:- spec_pre(golden_ratio/2, [and([ground, fib]), var]).
:- spec_post(golden_ratio/2, [and([ground, fib]), var], [fib, number]).
golden_ratio(X,R) :-
    next_fib(X,Y),
    R is Y/X.

:- spec_post(my_member/2,[X,any],[X, list(X)]).
my_member(E,[E|_]).
my_member(E,[_|T]) :-
    my_member(E,T).

my_member_bla(E,[E|_]) :- !.
my_member_bla(E,[_|T]) :-
    my_member(E,T).

:- begin_tests(contain_determinism, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).

test(normal_next_fib) :-
    next_fib_no_annotations(8,X),
    X =:= 13.

test(spec_next_fib,[true(X=:=13)]) :-
    next_fib(8,X),
    X =:= 13.

test(my_member_create_list, []) :-
    my_member(1,T),
    T = [2,2,2,1,2], !.

test(golden_ratio_one_solution, []) :-
    golden_ratio(1,R),
    R =:= 2,
    findall(X, golden_ratio(34,X), L),
    length(L,1).

:- end_tests(contain_determinism).
