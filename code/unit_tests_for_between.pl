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
% Unit testing between/3
% https://eu.swi-prolog.org/pldoc/doc_for?object=between/3
% To run it, execute predicate rt(X):
%
% ?- rt(X).
% PL-Unit: unit_tests_for_between_3 .................... done
% All 20 tests passed
% X = unit_tests_for_between_3.
% ============================================================================

rt(unit_tests_for_between_3) :- run_tests(unit_tests_for_between_3).

:- begin_tests(unit_tests_for_between_3).

% ===
% Bad order: fails
% ===

test(reversed_1, fail) :- between(1,-1,_).
test(reversed_2, fail) :- between(1,-1,0).
test(reversed_3, fail) :- between(1,-1,2).
test(reversed_4, fail) :- between(1,-1,-2).

% ===
% Not instantiated enough
% ===

test(nieng_1, error(instantiation_error,_)) :- between(_,_,1).
test(nieng_2, error(instantiation_error,_)) :- between(1,_,1).
test(nieng_3, error(instantiation_error,_)) :- between(_,1,1).

% ===
% Generating values
% ===

test(one,   true(Bag = [1]))      :- bagof(X, between( 1,1,X), Bag).
test(two,   true(Bag = [1,2]))    :- bagof(X, between( 1,2,X), Bag).
test(three, true(Bag = [-1,0,1])) :- bagof(X, between(-1,1,X), Bag).

% ===
% Non-integer parameters passed for limits
% ===

test(float_1, error(type_error(_,_),_)) :- between(0.5,1,_).
test(float_2, error(type_error(_,_),_)) :- between(1,1.5,_).
test(atom_1, error(type_error(_,_),_))  :- between(1,foo,_).

% ===
% Non-integer parameters passed for verification
% ===

test(verify_three, error(type_error(_,_),_)) :- between(-1,1,0.5).

% ===
% Verifying values
% ===

test(verify_1, true) :- between( 1,1, 1).
test(verify_2, true) :- between(-1,1, 1).
test(verify_3, true) :- between(-1,1,-1).
test(verify_4, true) :- between(-1,1, 0).

test(verify_5, fail) :- between(-1,1,  2).
test(verify_6, fail) :- between(-1,1, -2).

:- end_tests(unit_tests_for_between_3).
