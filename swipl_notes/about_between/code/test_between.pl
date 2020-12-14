% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-05-XX
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% This is free and unencumbered software released into the public domain.
%
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
%
% For more information, please refer to <http://unlicense.org/>
% ============================================================================
% Unit testing "between/3": between(+Low, +High, ?Value)
%
% https://eu.swi-prolog.org/pldoc/doc_for?object=between/3
% ============================================================================

:- begin_tests(between_3).

test("low and high args reversed, fresh value", fail) :-
   between(1,-1, _).

test("low and high args reversed, value between", fail) :-
   between(1,-1, 0).

test("low and high args reversed, value out-of-bounds, above", fail) :-
   between(1,-1, 2).

test("low and high args reversed, value out-of-bounds, below", fail) :-
   between(1,-1,-2).

% ---

test("not instantiated enough: low & high are fresh", error(instantiation_error,_)) :-
   between(_,_,1).

test("not instantiated enough: high is fresh", error(instantiation_error,_)) :-
   between(1,_,1).

test("not instantiated enough: low is fresh", error(instantiation_error,_)) :-
  between(_,1,1).

% ---

test("generating exactly 1 value", true(Bag = [1])) :-
   bagof(X, between( 1,1,X), Bag).

test("generating two values", true(Bag = [1,2])) :-
   bagof(X, between( 1,2,X), Bag).

test("generating three values, with 0 in the middle", true(Bag = [-1,0,1])) :-
   bagof(X, between(-1,1,X), Bag).

% ---

test("low limit given as flaot", error(type_error(_,_),_)) :-
   between(0.5,1,_).

test("high limit given as float", error(type_error(_,_),_)) :-
   between(1,1.5,_).

test("high limit given as atom", error(type_error(_,_),_)) :-
   between(1,foo,_).

% ---

test("'value' is non-integer (although in range)", error(type_error(_,_),_)) :-
   between(-1,1,0.5).

% ---

test("verify value == high, 1 value interval", true) :-
   between( 1,1, 1).

test("verify value == high, multivalue interval", true) :-
   between(-1,1, 1).

test("verify value == low, multivalue interval", true) :-
   between(-1,1,-1).

test("verify value in the middle of the interval", true) :-
   between(-1,1, 0).

% ---

test("verify, value out of bounds, high", fail) :-
   between(-1,1,  2).

test("verify, value out of bounds, low", fail) :-
   between(-1,1, -2).

:- end_tests(between_3).

