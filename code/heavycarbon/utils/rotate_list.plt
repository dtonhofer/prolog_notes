:- use_module(library('heavycarbon/utils/rotate_list.pl')).

:- begin_tests(rotate_list).

test(empty1) :- rotate_list([], 0,R), R=[].
test(empty2) :- rotate_list([], 1,R), R=[].
test(empty3) :- rotate_list([],-1,R), R=[].

test(three3m6) :- rotate_list([1,2,3],-6,R), R=[1,2,3].
test(three3m5) :- rotate_list([1,2,3],-5,R), R=[2,3,1].
test(three3m4) :- rotate_list([1,2,3],-4,R), R=[3,1,2].
test(three3m3) :- rotate_list([1,2,3],-3,R), R=[1,2,3].
test(three3m2) :- rotate_list([1,2,3],-2,R), R=[2,3,1].
test(three3m1) :- rotate_list([1,2,3],-1,R), R=[3,1,2].
test(three3z)  :- rotate_list([1,2,3],+0,R), R=[1,2,3].
test(three3p1) :- rotate_list([1,2,3],+1,R), R=[2,3,1].
test(three3p2) :- rotate_list([1,2,3],+2,R), R=[3,1,2].
test(three3p3) :- rotate_list([1,2,3],+3,R), R=[1,2,3].
test(three3p4) :- rotate_list([1,2,3],+4,R), R=[2,3,1].
test(three3p5) :- rotate_list([1,2,3],+5,R), R=[3,1,2].
test(three3p6) :- rotate_list([1,2,3],+6,R), R=[1,2,3].
test(three3p7) :- rotate_list([1,2,3],+7,R), R=[2,3,1].
test(three3p8) :- rotate_list([1,2,3],+8,R), R=[3,1,2].

test(three3zvar)  :- rotate_list([S0,S1,S2],+0,R), R=[S0,S1,S2].
test(three3p1var) :- rotate_list([S0,S1,S2],+1,R), R=[S1,S2,S0].
test(three3p2var) :- rotate_list([S0,S1,S2],+2,R), R=[S2,S0,S1].
test(three3p3var) :- rotate_list([S0,S1,S2],+3,R), rotate_list([S0,S1,S2],0,R).
test(three3p4var) :- rotate_list([S0,S1,S2],+4,R), rotate_list([S0,S1,S2],1,R).
test(three3p5var) :- rotate_list([S0,S1,S2],+5,R), rotate_list([S0,S1,S2],2,R).

:- end_tests(rotate_list).

