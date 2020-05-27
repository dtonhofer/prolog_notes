:- module(validator, [spec_predicate/2, spec_predicate_recursive/4, spec_indirection/2, spec_connective/4, spec_basic/2,
                      spec_exists/1, spec_exists/2,
                      true/1, atom/2,
                      valid/2, valid/3,
                      evaluate_spec_match/4,
                      list/4, compound/4, tuple/4,
                      spec_and/4, and/4, or/4
                     ]).

:- use_module(library(terms), [variant/2]).
:- use_module(library(lists)).
:- dynamic spec_indirection/2, spec_predicate/2, spec_predicate_recursive/4, spec_connective/4.
:- multifile spec_indirection/2, spec_predicate/2, spec_predicate_recursive/4, spec_connective/4.





spec_basic(var, var).
spec_basic(ground, ground).
spec_basic(nonvar, nonvar).
spec_basic(any, true).

% Definition of spec predicates
spec_predicate(atomic, atomic).
spec_predicate(atom, atom).
spec_predicate(atom(X), atom(X)). %for a concrete atom
spec_predicate(integer, integer).
spec_predicate(number, number).
spec_predicate(float, float).

spec_predicate_recursive(compound(X), compound(X), and, and_invariant).
spec_predicate_recursive(list(X), list(X), and, and_invariant).
spec_predicate_recursive(tuple(X), tuple(X), and, and_invariant).

spec_indirection(int, integer).
spec_indirection([X], list(X)).
spec_indirection(same(X), atom(X)).

spec_connective(and([H|T]), spec_and([H|T]), and, and_invariant).
spec_connective(one_of(X), spec_and(X), or, or_invariant).

%When does a predicate exists:
spec_exists(X) :- spec_indirection(X, _), !.
spec_exists(X) :- spec_basic(X, _), !.
spec_exists(X) :- spec_predicate(X, _), !.
spec_exists(X) :- spec_predicate_recursive(X, _, _, _), !.
spec_exists(X) :- spec_connective(X, _, _, _), !.
spec_exists(X, indirection(Y)) :- spec_indirection(X, Y), !.
spec_exists(X, predicate(Y)) :- spec_predicate(X, Y), !.
spec_exists(X, predicate_recursive(A,B,C)) :- spec_predicate_recursive(X, A, B, C), !.
spec_exists(X, connective(A,B,C)) :- spec_connective(X, A, B, C), !.

:- public true/1.
true(_).
:- public atom/2.
atom(X, Y) :- atom(Y), X = Y.

valid(Type, Spec, Val) :-
    ground(Spec),
    evaluate_spec_match(Spec, Type, Val, Success),
    Success == true, !.

valid(Type, Spec, Val) :-
    \+ ground(Spec),
    evaluate_spec_match(Spec, Type, Val, Success),
    Success == true.

valid(Spec, Val) :-
    ground(Spec),
    evaluate_spec_match(Spec, def, Val, Success),
    Success == true, !.

valid(Spec, Val) :-
    \+ ground(Spec),
    evaluate_spec_match(Spec, any, Val, Success),
    Success == true.


% checks, if the spec exists.If no, fail, if yes, call evaluate_spec_match_aux
evaluate_spec_match(Spec, _Type, _, fail(spec_not_found(spec(Spec)))) :-
    nonvar(Spec),
    \+ spec_exists(Spec), !,
    log(warning,'spec ~w not found~n', [Spec]).

evaluate_spec_match(Spec, Type, Val, Res) :-
    evaluate_spec_match_case(Spec, Type,Val, Res).

%evaluate_spec_match_aux matches the value Val against the existing spec Spec.
% There are different kinds of spec predicates:

%spec was an alias for another spec
evaluate_spec_match_aux(Spec, Type, Val, Res) :-
    spec_indirection(Spec, NewSpec),
    evaluate_spec_match(NewSpec, Type, Val, Res).
% a basic spec %TODO: find better name
evaluate_spec_match_case(Spec, def, Val, Res) :-
    spec_basic(Spec, Predicate),
    %% HACK: copy_term does weird things to co-routines
    copy_term(Val, Vali),
    (call(Predicate, Val) ->
        Res = true
    ;
        Res = fail(spec_not_matched(spec(Spec), value(Val)))
    ),
    (copy_term(Val, Valii), variant(Valii, Vali) ->
        true
    ;
      log(error,'implementation of spec ~w binds variables but should not~n', [Predicate])
    ).

% a normal spec predicate
evaluate_spec_match_case(Spec, _Type, Val, Res) :-
    spec_predicate(Spec, Predicate),
    %% HACK: copy_term does weird things to co-routines
    copy_term(Val, Vali),
    (call(Predicate, Val) ->
        Res = true
    ;
        Res = fail(spec_not_matched(spec(Spec), value(Val)))
    ),
    (copy_term(Val, Valii), variant(Valii, Vali) ->
        true
    ;
      log(
        error,
        'implementation of spec ~w binds variables but should not~n',
        [Predicate])
    ).

% a recursive spec
evaluate_spec_match_case(Spec, Type, Val, Res) :-
    spec_predicate_recursive(Spec, Predicate, MergePred, _MergePredInvariant),
    copy_term(Val, Vali),
    (call(Predicate, Val, NewSpecs, NewVals) ->
        call(MergePred, NewSpecs, Type, NewVals, Res)
    ;
        Res = fail(spec_not_matched(spec(Spec), value(Val)))
    ),
    (copy_term(Val, Valii), variant(Valii, Vali) ->
        true
    ;
        log(
            error,
            'implementation of spec ~w binds variables but should not~n',
            [Predicate]
        )
    ).

% a connective spec
evaluate_spec_match_case(Spec, Type, Val, Res) :-
    nonvar(Spec),
    spec_connective(Spec, Predicate, MergePred, _MergePredInvariant),
    copy_term(Val, Vali),
    (call(Predicate, Val, NewSpecs, NewVals) ->
        call(MergePred, NewSpecs, Type, NewVals, Res)
    ;
        Res = fail(spec_not_matched(spec(Spec), value(Val)))
    ),
    (copy_term(Val, Valii), variant(Valii, Vali) ->
        true
    ;
        log(
            error,
            'implementation of spec ~w binds variables but should not~n',
            [Predicate])
    ).

%spec was an alias for another spec
evaluate_spec_match_case(Spec, Type, Val, Res) :-
    spec_indirection(Spec, NewSpec),
    evaluate_spec_match(NewSpec, Type, Val, Res).

% built-in recursive specs
list(Spec, Val, NewSpecs, NewVals) :-
    nonvar(Val), list1(Val, Spec, NewSpecs, NewVals).
%% list1 only ensures that the value is a list.
%% The type of its members is checked later on in a seperate step.
%% list1 will return a spec for each member.
%% If a tail is a variable, the bound value should be
%% of the same type as the list itself.
list1(L, Spec, [Spec|ST], [H|VT]) :-
    nonvar(L), L = [H|T], !,
    list1(T, Spec, ST, VT).
list1(L, _, [], []) :-
    nonvar(L), L = [], !.
list1(Var, Spec, [list(Spec)], [Var]) :- var(Var).

:- public compound/4.
compound(Spec, Val, NewSpecs, NewVars) :-
    compound(Val),
    Val =.. [Functor|NewVars],
    Functor \= '[|]',
    length(NewVars, Len),
    length(NewSpecs, Len),
    Spec =.. [Functor|NewSpecs].

:- public tuple/4.
tuple(SpecList, VarList, SpecList, VarList) :-
    is_list(VarList).

spec_and(SpecList, Var, SpecList, VarRepeated) :-
    SpecList \= [],
    %% this is actually repeat
    length(SpecList,L),
    length(VarRepeated,L),
    maplist(=(Var), VarRepeated).

:- public and/4.
and([], _, [], true).
and([S|Specs], Type, [V|Vals], Res) :-
    evaluate_spec_match(S, Type, V, X),
    (X == true ->
        and(Specs, Type, Vals, Res)
    ;
        Res = fail(spec_not_matched(spec(S), value(V)))
    ).

:- public or/4.
or(Specs, Type, Vals, true) :-
    or2(Specs, Type, Vals).
or(Specs, _, Vals, fail(spec_not_matched_merge(specs(or(Specs)), values(Vals)))).

or2([HSpec|TSpec], Type, [HVal|TVal]) :-
    evaluate_spec_match(HSpec, Type, HVal, X),
    (X == true ->
        true
    ;
        or2(TSpec, Type, TVal)
    ).
