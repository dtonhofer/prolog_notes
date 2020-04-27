
% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-XX
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
% Inspired by a discussion on
% http://computer-programming-forum.com/55-prolog/358ecf5f07f2de46.htm
% although the solution below, which uses two difference lists and
% a single scan of the original list (an no append/3), was not proposed.
% ============================================================================

% ===
% rotate_list(+List,+N,?Rotated).
% ===

rotate_list([],_,[]) :- !.

rotate_list(List,N,Rotated) :-
     length(List,Len),
     Len>0,
     !,     
     Kback is N mod Len,
     H1-T1=X-X,
     H2-T2=Y-Y,
     copy_list(List,Kback,H1,T1,H2,T2),
     Rotated=H2.

copy_list([L|Ls],C,H1,T1,H2,T2) :- C>0,!,succ(Cnext,C),T1=[L|Tnew],copy_list(Ls,Cnext,H1,Tnew,H2,T2). 
copy_list([L|Ls],0,H1,T1,H2,T2) :- !,T2=[L|Tnew],copy_list(Ls,0,H1,T1,H2,Tnew). 
copy_list([],0,H1,T1,_H2,T2)    :- T2=H1,T1=[].

% ===
% Unit tests
% ===

:- begin_tests(rotate_list).        

test(empty1) :- rotate_list([], 0,R), R=[].
test(empty2) :- rotate_list([], 1,R), R=[].
test(empty3) :- rotate_list([],-1,R), R=[].

test(three3m6) :- rotate_list([1,2,3],-6,R), rotate_list([1,2,3],0,R).
test(three3m5) :- rotate_list([1,2,3],-5,R), rotate_list([1,2,3],1,R).
test(three3m4) :- rotate_list([1,2,3],-4,R), rotate_list([1,2,3],2,R).
test(three3m3) :- rotate_list([1,2,3],-3,R), rotate_list([1,2,3],0,R).
test(three3m2) :- rotate_list([1,2,3],-2,R), rotate_list([1,2,3],1,R).
test(three3m1) :- rotate_list([1,2,3],-1,R), rotate_list([1,2,3],2,R).
test(three3z)  :- rotate_list([1,2,3],+0,R), R=[1,2,3].
test(three3p1) :- rotate_list([1,2,3],+1,R), R=[2,3,1].
test(three3p2) :- rotate_list([1,2,3],+2,R), R=[3,1,2].
test(three3p3) :- rotate_list([1,2,3],+3,R), rotate_list([1,2,3],0,R).
test(three3p4) :- rotate_list([1,2,3],+4,R), rotate_list([1,2,3],1,R).
test(three3p5) :- rotate_list([1,2,3],+5,R), rotate_list([1,2,3],2,R).
test(three3p6) :- rotate_list([1,2,3],+6,R), rotate_list([1,2,3],0,R).
test(three3p7) :- rotate_list([1,2,3],+7,R), rotate_list([1,2,3],1,R).
test(three3p8) :- rotate_list([1,2,3],+8,R), rotate_list([1,2,3],2,R).

test(three3zvar)  :- rotate_list([S0,S1,S2],+0,R), R=[S0,S1,S2].
test(three3p1var) :- rotate_list([S0,S1,S2],+1,R), R=[S1,S2,S0].
test(three3p2var) :- rotate_list([S0,S1,S2],+2,R), R=[S2,S0,S1].
test(three3p3var) :- rotate_list([S0,S1,S2],+3,R), rotate_list([S0,S1,S2],0,R).
test(three3p4var) :- rotate_list([S0,S1,S2],+4,R), rotate_list([S0,S1,S2],1,R).
test(three3p5var) :- rotate_list([S0,S1,S2],+5,R), rotate_list([S0,S1,S2],2,R).

:- end_tests(rotate_list).

rt :- run_tests(rotate_list).
