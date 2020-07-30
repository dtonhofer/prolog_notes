:- use_module(library('heavycarbon/utils/replace0.pl')).

:- begin_tests(replace0).

% ---
% normal replace
% ---

test(replace0_0) :- replace0([a,b,c],0,rep(A),A,NewList), A=a, NewList = [rep(a), b, c].
test(replace0_1) :- replace0([a,b,c],1,rep(B),B,NewList), B=b, NewList = [a, rep(b), c].
test(replace0_2) :- replace0([a,b,c],2,rep(C),C,NewList), C=c, NewList = [a, b, rep(c)].

% ---
% testing error conditions
% ---

% Negative index throws.
% Positive but out of bounds index causes failure.

test(replace0_fail_0,[fail])               :- replace0([a,b,c],3,_,_,_).
test(replace0_fail_1,[fail])               :- replace0([],0,_,_,_).
test(replace0_fail_2,[throws(error(_,_))]) :- replace0([a,b,c],-1,_,_,_).

:- end_tests(replace0).


