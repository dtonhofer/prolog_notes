:- use_module(peano).

% ===
% Testing of pm/2, the "natural number" <-> "peano number" mapper
%
% In some calls, the predicate is non-deterministic and we gather all solutions
% using "bagof".
%
% To avoid confusion due to "predicate just failing" if arguments are not in-domain,
% pm/2 throws if that is the case. 
% ===

:- begin_tests(pm).

test(pm_a0)                           :- pm(z, 0).
test(pm_a1,true(XN == 0))             :- pm(z, XN).
test(pm_a2,all(XP == [z]))            :- pm(XP, 0).

test(pm_b0)                           :- pm(s(z), 1).
test(pm_b1,[true(XN == 1),nondet])    :- pm(s(z), XN).
test(pm_b2,all(XP == [s(z)]))         :- pm(XP, 1).

test(pm_c0)                           :- pm(s(s(z)), 2).
test(pm_c2,[true(XN == 2),nondet])    :- pm(s(s(z)), XN).
test(pm_c3,all(XP == [s(s(z))]))      :- pm(XP, 2).

test(pm_d0,fail)                      :- pm(s(s(z)),1).
test(pm_d1,fail)                      :- pm(s(z),0).
test(pm_d2,error(domain_error(_,_)))  :- pm(_,-1).
test(pm_d3,error(domain_error(_,_)))  :- pm(0, _).
test(pm_d4,error(domain_error(_,_)))  :- pm(_, z).
test(pm_d5,error(domain_error(_,_)))  :- pm(a, _).
test(pm_d6,error(domain_error(_,_)))  :- pm(_, a).
test(pm_d7,error(domain_error(_,_)))  :- pm(1, s(z)).

test(pm_any)  :-
   bagof([XP,XN],limit(5,pm(XP,XN)),Bag),
   Bag == [[z,0],[s(z),1],[s(s(z)),2],[s(s(s(z))),3],[s(s(s(s(z)))),4]].

test(pm_list,nondet) :-
   pm_list([z,s(z),s(s(z)),s(z),z],L),
   L=[0,1,2,1,0].

:- end_tests(pm).

:- begin_tests(pnat).

test(pnat_a,error(domain_error(_,_)))  :- pnat(a).
test(pnat_0)                           :- pnat(z).
test(pnat_1)                           :- pnat(s(z)).
test(pnat_2)                           :- pnat(s(s(z))).
test(pnat_forever) :-
   bagof(N,limit(6,pnat(N)),Bag),
   Bag == [z,s(z),s(s(z)),s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z)))))].

:- end_tests(pnat).
   
% ===
% Testing of pnonz/1 - "nonzero peano number"
% ===
   
:- begin_tests(pnonz).

test(pnonz_0,fail)  :- pnonz(z).
test(pnonz_1)       :- pnonz(s(z)).
test(pnonz_2)       :- pnonz(s(s(z))).
test(pnonz_forever) :-
   bagof(N,limit(6,pnonz(N)),Bag),
   Bag == [s(z),s(s(z)),s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z))))),s(s(s(s(s(s(z))))))].

:- end_tests(pnonz).

% ===
% Testing of pequal/1 - "are two peano numbers equal"
% ===

:- begin_tests(pequal).

test(pequal_0,nondet)   :- pequal(z, z).
test(pequal_1,nondet)   :- pequal(s(z), s(z)).
test(pequal_2,nondet)   :- pequal(s(s(z)), s(s(z))).
test(pequal_3,fail)     :- pequal(s(z), s(s(z))).

:- end_tests(pequal).

% ===
% Testing addition
% ===

:- begin_tests(padd).

test(add_bbf_0,nondet) :- bbf_collect(padd,plus_compose,0,0,Bag,100), Bag==[0+0=0].
test(add_bbf_1,nondet) :- bbf_collect(padd,plus_compose,1,0,Bag,100), Bag==[1+0=1].
test(add_bbf_2,nondet) :- bbf_collect(padd,plus_compose,0,1,Bag,100), Bag==[0+1=1].
test(add_bbf_3,nondet) :- bbf_collect(padd,plus_compose,1,1,Bag,100), Bag==[1+1=2].
test(add_bbf_4,nondet) :- bbf_collect(padd,plus_compose,4,5,Bag,100), Bag==[4+5=9].

test(add_fbb_0,nondet) :- fbb_collect(padd,plus_compose,0,0,Bag,100), Bag==[0+0=0].
test(add_fbb_1,fail)   :- fbb_collect(padd,plus_compose,1,0,___,100).
test(add_fbb_2,nondet) :- fbb_collect(padd,plus_compose,0,1,Bag,100), Bag==[1+0=1].
test(add_fbb_3,nondet) :- fbb_collect(padd,plus_compose,1,1,Bag,100), Bag==[0+1=1].
test(add_fbb_4,fail)   :- fbb_collect(padd,plus_compose,2,1,Bag,100), Bag==[0+2=1].
test(add_fbb_5,nondet) :- fbb_collect(padd,plus_compose,1,2,Bag,100), Bag==[1+1=2].

test(add_bfb_0,nondet) :- bfb_collect(padd,plus_compose,0,0,Bag,100), Bag==[0+0=0].
test(add_bfb_1,fail)   :- bfb_collect(padd,plus_compose,1,0,___,100).
test(add_bfb_2,nondet) :- bfb_collect(padd,plus_compose,0,1,Bag,100), Bag==[0+1=1].
test(add_bfb_3,nondet) :- bfb_collect(padd,plus_compose,1,1,Bag,100), Bag==[1+0=1].
test(add_bfb_4,fail)   :- bfb_collect(padd,plus_compose,2,1,___,100).
test(add_bfb_5,nondet) :- bfb_collect(padd,plus_compose,0,2,Bag,100), Bag==[0+2=2].
test(add_bfb_6,nondet) :- bfb_collect(padd,plus_compose,1,2,Bag,100), Bag==[1+1=2].
test(add_bfb_7,nondet) :- bfb_collect(padd,plus_compose,2,2,Bag,100), Bag==[2+0=2].
test(add_bfb_8,fail)   :- bfb_collect(padd,plus_compose,3,2,___,100).

test(add_bff_0,nondet) :- bff_collect(padd,plus_compose,0,Bag,3), Bag==[0+0=0,0+1=1,0+2=2].
test(add_bff_1,nondet) :- bff_collect(padd,plus_compose,1,Bag,3), Bag==[1+0=1,1+1=2,1+2=3].
test(add_bff_2,nondet) :- bff_collect(padd,plus_compose,2,Bag,3), Bag==[2+0=2,2+1=3,2+2=4].
test(add_bff_3,nondet) :- bff_collect(padd,plus_compose,3,Bag,3), Bag==[3+0=3,3+1=4,3+2=5].

test(add_fbf_0,nondet) :- fbf_collect(padd,plus_compose,0,Bag,3), Bag==[0+0=0,1+0=1,2+0=2].
test(add_fbf_1,nondet) :- fbf_collect(padd,plus_compose,1,Bag,3), Bag==[0+1=1,1+1=2,2+1=3].
test(add_fbf_2,nondet) :- fbf_collect(padd,plus_compose,2,Bag,3), Bag==[0+2=2,1+2=3,2+2=4].
test(add_fbf_3,nondet) :- fbf_collect(padd,plus_compose,3,Bag,3), Bag==[0+3=3,1+3=4,2+3=5].

test(add_ffb_0,nondet) :- ffb_collect(padd,plus_compose,0,Bag,3), Bag==[0+0=0].
test(add_ffb_1,nondet) :- ffb_collect(padd,plus_compose,1,Bag,3), Bag==[0+1=1,1+0=1].
test(add_ffb_2,nondet) :- ffb_collect(padd,plus_compose,2,Bag,3), Bag==[0+2=2,1+1=2,2+0=2].
test(add_ffb_3,nondet) :- ffb_collect(padd,plus_compose,3,Bag,5), Bag==[0+3=3,1+2=3,2+1=3,3+0=3].

test(add_fff,nondet)   :- fff_collect(padd,plus_compose,Bag,12),  Bag==[0+0=0,0+1=1,1+0=1,0+2=2,1+1=2,2+0=2,0+3=3,1+2=3,2+1=3,3+0=3,0+4=4,1+3=4].

:- end_tests(padd).

% ===
% Testing peano less-than
% ===

:- begin_tests(pless).

test(plesseq_0,nondet)        :- plesseq(z,z).
test(plesseq_1,nondet)        :- plesseq(z,s(z)).
test(plesseq_2,fail)          :- plesseq(s(z),z).
test(plesseq_3,nondet)        :- plesseq(s(s(z)),s(s(z))).
test(plesseq_4,fail)          :- plesseq(s(s(z)),s(z)).
test(plesseq_5,nondet)        :- plesseq(s(z),s(s(z))).
test(plesseq_6,all(X == [z])) :- plesseq(X,z).
test(plesseq_7)               :- bagof(X,limit(6,plesseq(z,X)),Bag),
                                 Bag = [z,s(z),s(s(z)),s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z)))))].
test(plesseq_8)               :- bagof([X,Y],limit(6,plesseq(X,Y)),Bag),
                                 Bag = [[z,z],[z,s(z)],[s(z),s(z)],[z,s(s(z))],
                                        [s(z),s(s(z))],[s(s(z)),s(s(z))]].
test(plesseq_9)               :- bagof(X,limit(6,plesseq(X,X)),Bag),
                                 Bag = [z,s(z),s(s(z)),s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z)))))].

test(pless_0,nondet)          :- pless(z,s(z)).
test(pless_1,nondet)          :- pless(s(z),s(s(z))).
test(pless_2)                 :- bagof(X,limit(6,pless(z,X)),Bag),
                                 Bag = [s(z),s(s(z)),s(s(s(z))),s(s(s(s(z)))),
                                        s(s(s(s(s(z))))),s(s(s(s(s(s(z))))))].
test(pless_3)                 :- bagof(X,limit(6,pless(s(s(z)),X)),Bag),
                                 Bag = [s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z))))),s(s(s(s(s(s(z)))))),
                                        s(s(s(s(s(s(s(z))))))),s(s(s(s(s(s(s(s(z))))))))].
test(pless_4,fail)            :- pless(_,z).
test(pless_5)                 :- bagof([X,Y],limit(6,pless(X,Y)),Bag),
                                 Bag = [[z,s(z)],[z,s(s(z))],[s(z),s(s(z))],[z,s(s(s(z)))],[s(z),s(s(s(z)))],[s(s(z)),s(s(s(z)))]].
test(pless_6,fail)            :- pless(X,X). % empty bag

:- end_tests(pless).

% ===
% Testing multiplication
% ===

:- begin_tests(pmult).

test(mult_bbf_0,nondet) :- bbf_collect(pmult,times_compose,0,0,Bag,100), Bag==[0*0=0].
test(mult_bbf_1,nondet) :- bbf_collect(pmult,times_compose,1,0,Bag,100), Bag==[1*0=0].
test(mult_bbf_2,nondet) :- bbf_collect(pmult,times_compose,0,1,Bag,100), Bag==[0*1=0].
test(mult_bbf_3,nondet) :- bbf_collect(pmult,times_compose,1,1,Bag,100), Bag==[1*1=1].
test(mult_bbf_4,nondet) :- bbf_collect(pmult,times_compose,1,2,Bag,100), Bag==[1*2=2].
test(mult_bbf_5,nondet) :- bbf_collect(pmult,times_compose,2,1,Bag,100), Bag==[2*1=2].
test(mult_bbf_6,nondet) :- bbf_collect(pmult,times_compose,2,2,Bag,100), Bag==[2*2=4].

test(mult_fbb_0,nondet) :- fbb_collect(pmult,times_compose,0,0,Bag,4  ), Bag==[0*0=0,1*0=0,2*0=0,3*0=0].
test(mult_fbb_1,nondet) :- fbb_collect(pmult,times_compose,1,0,Bag,100), Bag==[0*1=0].
test(mult_fbb_2,fail)   :- fbb_collect(pmult,times_compose,0,1,___,100).
test(mult_fbb_3,nondet) :- fbb_collect(pmult,times_compose,1,1,Bag,100), Bag==[1*1=1].
test(mult_fbb_4,nondet) :- fbb_collect(pmult,times_compose,1,2,Bag,100), Bag==[2*1=2].
test(mult_fbb_5,fail)   :- fbb_collect(pmult,times_compose,2,1,___,100).
test(mult_fbb_6,nondet) :- fbb_collect(pmult,times_compose,2,2,Bag,100), Bag==[1*2=2].

test(mult_bfb_0,nondet) :- bfb_collect(pmult,times_compose,0,0,Bag,4  ), Bag==[0*0=0,0*1=0,0*2=0,0*3=0].
test(mult_bfb_1,nondet) :- bfb_collect(pmult,times_compose,1,0,Bag,100), Bag==[1*0=0].
test(mult_bfb_2,fail)   :- bfb_collect(pmult,times_compose,0,1,___,100).
test(mult_bfb_3,nondet) :- bfb_collect(pmult,times_compose,1,1,Bag,100), Bag==[1*1=1].
test(mult_bfb_4,nondet) :- bfb_collect(pmult,times_compose,1,2,Bag,100), Bag==[1*2=2].
test(mult_bfb_5,fail)   :- bfb_collect(pmult,times_compose,2,1,___,100).
test(mult_bfb_6,nondet) :- bfb_collect(pmult,times_compose,2,2,Bag,100), Bag==[2*1=2].
test(mult_bfb_7,fail)   :- bfb_collect(pmult,times_compose,2,3,___,100).
test(mult_bfb_8,nondet) :- bfb_collect(pmult,times_compose,2,4,Bag,100), Bag==[2*2=4].

test(mult_fbf_0,nondet) :-
   time((
      fbf_collect(pmult,times_compose,0,Bag,10),
      Bag==[0*0=0,1*0=0,2*0=0,3*0=0,4*0=0,5*0=0,6*0=0,7*0=0,8*0=0,9*0=0])).

test(mult_fbf_1,nondet) :-
   time((
      fbf_collect(pmult,times_compose,1,Bag,10),
      Bag==[0*1=0,1*1=1,2*1=2,3*1=3,4*1=4,5*1=5,6*1=6,7*1=7,8*1=8,9*1=9])).

test(mult_fbf_2,nondet) :-
   time((
      fbf_collect(pmult,times_compose,2,Bag,10),
      Bag==[0*2=0,1*2=2,2*2=4,3*2=6,4*2=8,5*2=10,6*2=12,7*2=14,8*2=16,9*2=18])).

test(mult_ffb_0) :-
   time((
      ffb_collect(pmult,times_compose,0,Bag,10),
      Bag==[0*0=0,1*0=0,0*1=0,2*0=0,0*2=0,3*0=0,0*3=0,4*0=0,0*4=0,5*0=0])).

test(mult_ffb_1) :-
   time((
      ffb_collect(pmult,times_compose,1,Bag,10),
      Bag==[1*1=1])).

test(mult_ffb_2) :-
   time((
      ffb_collect(pmult,times_compose,2,Bag,10),
      Bag==[2*1=2,1*2=2])).

% The next one needs:   9,862,132 inferences for "21".
%                and   61,311,623 inferences for "30"
%                and  266,605,141 inferences for "40"
% That's ... slow (din't try tabling).

test(mult_ffb_3) :-
   time((
      ffb_collect(pmult,times_compose,21,Bag,10),
      Bag==[21*1=21,7*3=21,3*7=21,1*21=21])).

test(mult_bff_0) :-
   time((
      bff_collect(pmult,times_compose,0,Bag,10),
      Bag=[0*0=0,0*1=0,0*2=0,0*3=0,0*4=0,0*5=0,0*6=0,0*7=0,0*8=0,0*9=0])).

test(mult_bff_1) :-
   time((
      bff_collect(pmult,times_compose,1,Bag,10),
      Bag = [1*0=0,1*1=1,1*2=2,1*3=3,1*4=4,1*5=5,1*6=6,1*7=7,1*8=8,1*9=9])).

test(mult_bff_2) :-
   time((
      bff_collect(pmult,times_compose,2,Bag,10),
      Bag = [2*0=0,2*1=2,2*2=4,2*3=6,2*4=8,2*5=10,2*6=12,2*7=14,2*8=16,2*9=18])).

test(mult_fff_0) :-
   time((
      fff_collect(pmult,times_compose,Bag,27),
      Bag=[0*0=0,
           0*1=0,1*0=0,1*1=1,
           0*2=0,2*0=0,2*1=2,1*2=2,
           0*3=0,3*0=0,3*1=3,1*3=3,
           0*4=0,4*0=0,4*1=4,2*2=4,1*4=4,
           0*5=0,5*0=0,5*1=5,1*5=5,
           0*6=0,6*0=0,6*1=6,3*2=6,2*3=6,1*6=6])).

:- end_tests(pmult).

% ===
% Testing quotient and remainder computation
% ===

:- begin_tests(pqr).

test(pqr_0,error(domain_error(_,_))) :- pquotrem(z,z,z,z).
test(pqr_1,error(domain_error(_,_))) :- pm(PM,0),pm(PT,12),pquotrem(PT,PM,_,_).
test(pqr_2,nondet)        :- pm(PM,1) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,12),pm(PR,0).
test(pqr_3,nondet)        :- pm(PM,2) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,6),pm(PR,0).
test(pqr_4,nondet)        :- pm(PM,3) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,4),pm(PR,0).
test(pqr_5,nondet)        :- pm(PM,4) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,3),pm(PR,0).
test(pqr_6,nondet)        :- pm(PM,5) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,2),pm(PR,2).
test(pqr_7,nondet)        :- pm(PM,6) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,2),pm(PR,0).
test(pqr_8,nondet)        :- pm(PM,7) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,1),pm(PR,5).
test(pqr_9,nondet)        :- pm(PM,8) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,1),pm(PR,4).
test(pqr_10,nondet)       :- pm(PM,9) ,pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,1),pm(PR,3).
test(pqr_11,nondet)       :- pm(PM,10),pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,1),pm(PR,2).
test(pqr_12,nondet)       :- pm(PM,11),pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,1),pm(PR,1).
test(pqr_13,nondet)       :- pm(PM,12),pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,1),pm(PR,0).
test(pqr_14,nondet)       :- pm(PM,13),pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,0),pm(PR,12).
test(pqr_15,nondet)       :- pm(PM,14),pm(PT,12),pquotrem(PT,PM,PQ,PR),pm(PQ,0),pm(PR,12).

% Randomly constructed computations. This is very slow, so only a few

test(pqr_randomly,nondet) :- bagof(_,limit(20,
                                           (choose(PT,PM),
                                            pquotrem(PT,PM,PQ,PR),
                                            print(PT,PM,PQ,PR),
                                            verify_peano(PM,PQ,PR,PT),
                                            verify_natural(PM,PQ,PR,PT))),_Bag).

% Backtrackably generate random values

choose(PT,PM) :-
   random_between(0,500,NT),pm(PT,NT),
   random_between(1,50,NM),pm(PM,NM).
choose(PT,PM) :-
   choose(PT,PM).

% Print the expression PT = PM*PQ+PR

print(PT,PM,PQ,PR) :-
   pm_list([PT,PM,PQ,PR],[NT,NM,NQ,NR]),
   total_compose(NT,NM,NQ,NR,Expr),
   format("Found: ~q\n",[Expr]).

% Verify the expression PT = PM*PQ+PR
% in Peano Arithmetic

verify_peano(PM,PQ,PR,PT) :-
   pmult(PM,PQ,PMQ),
   padd(PR,PMQ,PT).

% Verify the expression PT = PM*PQ+PR
% in Integer Arithmetic

verify_natural(PM,PQ,PR,PT) :-
   pm_list([PT,PM,PQ,PR],[NT,NM,NQ,NR]),
   NT =:= NM*NQ+NR.

:- end_tests(pqr).

% ===
% Helper predicates used in testing
% ===

call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis) :-
   call(Op,PN0,PN1,PN2),
   pm_list([PN0,PN1,PN2], [NNO0,NNO1,NNO2]),
   call(Compose,NNO0,NNO1,NNO2,BagThis).

plus_compose(A,B,C,(A+B=C)). % syntactic composition for nice printout
times_compose(A,B,C,(A*B=C)). % syntactic composition for nice printout
total_compose(NT,NM,NQ,NR,(((NM*NQ)+NR)=NT)). % syntactic composition for nice printout

bbf_collect(Op,Compose,NN0,NN1,Bag,Count) :-
   pm_list([PN0,PN1],[NN0,NN1]),
   bagof(BagThis,
         PN2^limit(Count,call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis)),
         Bag).

fbb_collect(Op,Compose,NN1,NN2,Bag,Count) :-
   pm_list([PN1,PN2],[NN1,NN2]),
   bagof(BagThis,
         PN0^limit(Count,call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis)),
         Bag).

bfb_collect(Op,Compose,NN0,NN2,Bag,Count) :-
   pm_list([PN0,PN2],[NN0,NN2]),
   bagof(BagThis,
         PN1^limit(Count,call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis)),
         Bag).

bff_collect(Op,Compose,NN0,Bag,Count) :-
   pm_list([PN0],[NN0]),
   bagof(BagThis,
         PN1^PN2^limit(Count,call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis)),
         Bag).

fbf_collect(Op,Compose,NN1,Bag,Count) :-
   pm_list([PN1],[NN1]),
   bagof(BagThis,
         PN0^PN2^limit(Count,call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis)),
         Bag).

ffb_collect(Op,Compose,NN2,Bag,Count) :-
   pm_list([PN2],[NN2]),
   bagof(BagThis,
         PN0^PN1^limit(Count,call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis)),
         Bag).

fff_collect(Op,Compose,Bag,Count) :-
   bagof(BagThis,
         PN0^PN1^PN2^limit(Count,call_op_and_compose(Op,Compose,PN0,PN1,PN2,BagThis)),
         Bag).
