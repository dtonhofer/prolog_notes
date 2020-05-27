:- use_module(plspec_core).
:- use_module(library(plunit)).
:- set_loglevel(debug).

:- enable_all_spec_checks.

:- defspec(tree(X), one_of([compound(node(tree(X), X, tree(X))),
                            atom(empty)])).

:- asserta(validator:spec_predicate(f, atom(f))).
:- asserta(validator:spec_predicate(g, atom(g))).
:- defspec(f_or_g, one_of([f,g])).

:- spec_pre(fun/2, [X,X]).
fun(_,_).

:- begin_tests(unify_order, [setup(plspec:set_error_handler(throw)), cleanup(plspec:set_error_handler(plspec_default_error_handler))]).


test(fun_test) :-
    fun(f,g).

:- end_tests(unify_order).

:- spec_post(reverse_order/2, [tuple([X,Y]), var], [tuple([X,Y]), tuple([Y,X])]).
reverse_order([H|T],S) :-
    reverse_order(T,[H],S).

reverse_order([],Res,Res) :- !.
reverse_order([H|T],Buffer,S) :-
    reverse_order(T,[H|Buffer],S).
