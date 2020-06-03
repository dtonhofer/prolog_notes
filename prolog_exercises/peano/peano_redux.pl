:- use_module(library(clpfd)).
:- discontiguous rt/1.

/*
    Natural Numbers ------------  peano_map(peano,int) ---------------------->  Peano Numbers
    0,1,2,3,4,.....              implements isomorphism            z, s(z), s(s(z)) s(s(s(z)))
                                                                                      |
                                                                                      |
                                                                                      |
                                                                 Peano Number x Peano Number ---> Peano Number
                                                                              peano_add(X,Y,Z)
                                                                      implements addition of peano numbers
                                                                                      |
                                                                                      |
                                                                                      |
                                                                                      V
*/

% Is this in the spirit of the Peano Axioms, which are rewriting rules (which
% may or may not terminate) over syntactic structure containing the function symbols
% s/1, */2, +/2, zero/0, whereas here, we pretend they are computation rules,
% using the intuition over similar rules in integer arithmetic. 

% ===
% On SWI Prolog, you may want to enable unlimited term depth printing first.
% ===

:- initialization(
      (set_prolog_flag(answer_write_options,[max_depth(0)]),
       set_prolog_flag(debugger_write_options,[max_depth(0)]))).

% ===
% Something to replace (frankly badly named and ugly) "var(X)" and "nonvar(X)"
% ===

ff(X) :- var(X).     % is X a variable referencing a fresh/unbound/uninstantiated term? (is X a "freshvar"?)
bb(X) :- nonvar(X).  % is X a variable referencing an nonfresh/bound/instantiated term? (is X a "boundvar"?)

% ===
% Bidirectional Mapping between Peano Numbers and Natural Numbers: pm(?PN,?NN)
% ===
% Natural Numbers are: 0,1,2,3,4, ... +oo
% Peano Numbers are:   z,s(z),s(s(z),s(s(s(z))), ... +oo
% (or alternatively    [],[s],[s,s],[s,s,s],[s,s,s,s], ... +oo but we don't use that
%                      representation here)
% 
% - We just calls this "pm/2" because it can be written in a less verbose fashion.
% - For the same reason, we use "z" instead of "zero".
% - Using CLP(FD) instead of "is" allows us to work "both ways" using the same code.
%   (although one has to pay attention to termination)
% - This predicate also generates successively larger pairs when called as pm(X,Y).
%
% It would be extremely cool to be able to set up a constraint between two 
% variables PN and NN, so that if PN is bound to a natural number, NN is automatically
% bound to a Peano Number, and vice-versa. That can probably be done with some effort
% using attributed variables.
% ===
                  
% pm(?PN,?NN)

pm(z, 0).
pm(s(N), X) :- X #> 0, X #= Y+1, pm(N, Y).

% pmlist maps a whole list; this be useful for a bit later

pmlist(PNs,NNs) :- maplist([PN,NN]>>pm(PN,NN),PNs,NNs).

% ===
% Testing of pm/2
% In some calls, the predicate is non-deterministic and we gather all solutions using "all"
% ===

:- begin_tests(pm).

test(pm_0)                      :- pm(z, 0).
test(pm_0,true(XN == 0))        :- pm(z, XN).
test(pm_0,all(XP == [z]))       :- pm(XP, 0).

test(pm_1)                      :- pm(s(z), 1).
test(pm_1,true(XN == 1))        :- pm(s(z), XN).
test(pm_1,all(XP == [s(z)]))    :- pm(XP, 1).

test(pm_2)                      :- pm(s(s(z)), 2).
test(pm_2,true(XN == 2))        :- pm(s(s(z)), XN).
test(pm_2,all(XP == [s(s(z))])) :- pm(XP, 2).

test(pm_f0,fail)                :- pm(s(s(z)), 1).
test(pm_f1,fail)                :- pm(a, 1).
test(pm_f2,fail)                :- pm(z, b).
test(pm_f3,fail)                :- pm(a, b).
test(pm_f4,fail)                :- pm(1, s(z)).

test(pm_any, all([XP,XN] == [[z,0],
                             [s(z),1],
                             [s(s(z)),2],
                             [s(s(s(z))),3],
                             [s(s(s(s(z)))),4]])) :- limit(5,pm(XP,XN)).                            
:- end_tests(pm).

rt(pm) :- run_tests(pm).


% ===
% Generator & Verifier for Peano Numbers
% ===
% This is mostly a generator ("verifying" that one has a Peano Number at hand isn't so
% interesting as we assume it has the right structure down tto "z" if it starts with
% function symbol s/1). 
%
% The Generator is needed to generate solutions for freshvars representing Peano Numbers,
% as Prolog generates representative individuals sampled from some domain making a query
% true. A general theorem prover would determine whether the query is true and not
% care about representative individuals.
%
% It is important to notice that if we have a bound X designating a Peano Number,
% we assume it is ground as in "X == s(s(z))" and not "nonzero but of unspecified magnitude": 
% as in "X = s(s(_))" -- that would demand another approach.
% ===

pnat(z).
pnat(s(X)) :- pnat(X).

% ===
% Is a Peano Number larger than zero? Alternatively, generate Peano Numbers larger than 0.
% ===

pnonz(s(X)) :- pnat(X).

% ===
% Testing of pnat/1, pnonz/1
% ===

:- begin_tests(pnat).

test(pnat_a,fail)  :- pnat(a).
test(pnat_0)       :- pnat(z).
test(pnat_1)       :- pnat(s(z)).
test(pnat_2)       :- pnat(s(s(z))).
test(pnat_forever) :- bagof(N,limit(6,pnat(N)),Bag),Bag == [z, s(z), s(s(z)), s(s(s(z))), s(s(s(s(z)))), s(s(s(s(s(z)))))].

test(pnonz_0,fail) :- pnonz(z).
test(pnonz_1)      :- pnonz(s(z)).
test(pnonz_2)      :- pnonz(s(s(z))).
test(pnat_forever) :- bagof(N,limit(5,pnat(N)),Bag),Bag == [s(z), s(s(z)), s(s(s(z))), s(s(s(s(z)))), s(s(s(s(s(z)))))].

:- end_tests(pnat).

rt(pnat) :- run_tests(pnat).

% ===
% Compare magnitudes, giving meaning to the ">" relation: X > Y. 
% After writign down all cases how this can be called (in the sense of, which variables
% received by plarger/2 are fresh, and which are bound), all ways of calling look the
% same (or nearly so) except for the case of "both variables are fresh".
% We still don't collapse the three first cases into one line, keeping things separate.
% It's useful in thinking about what's going on.
% ===

plarger(X,Y) :- bb(X),bb(Y), Y=z, X=s(_).  % X=s(_) is the same as pnonz(X), but is faster and we need not generate
plarger(X,Y) :- bb(X),bb(Y), X=s(Xs), Y=s(Ys), plarger(Xs,Ys). 

plarger(X,Y) :- ff(X),bb(Y), Y=z, pnonz(X).
plarger(X,Y) :- ff(X),bb(Y), X=s(Xs), Y=s(Ys), plarger(Xs,Ys). 

plarger(X,Y) :- bb(X),ff(Y), Y=z, pnonz(X).
plarger(X,Y) :- bb(X),ff(Y), X=s(Xs), Y=s(Ys), plarger(Xs,Ys). 

% Two freshvars.You want to generate pair of X and Y in the following order:
%
%       Y=0   Y=1   Y=2   Y=3
%  X=0  
%  X=1  (1)
%  X=2  (2)   (3)
%  X=3  (4)   (5)   (6)
%  X=4  (7)   (8)   (9)  (10)

% The trick is to generate X and Y and test them through plarger/2 in "both variables
% are bound" mode. However, the control structure is a bit problematic as we have
% to backtrack to generation of next X once plarger(X,Y) fails.

plarger(X,Y) :- ff(X),ff(Y), pnonz(X), plarger_generate_Y_then_test(X,Y).
plarger_generate_Y_then_test(X,Y) :- pnat(Y),(plarger(X,Y) -> true; (!,fail)).

% ===
% Similarly for X=Y, equality (much simpler to understand).
% ===

pequal(X,Y) :- bb(X),bb(Y), X=Y.             % == would work too, instead of unification
pequal(X,Y) :- ff(X),bb(Y), X=Y.             % unification is correct here
pequal(X,Y) :- bb(X),ff(Y), X=Y.             % unification is correct here
pequal(X,Y) :- ff(X),ff(Y), pnat(X), X=Y.    % generate X and unify

% ===
% X >= X, semi-straightforward
% ===

plarger_or_eq(X,Y) :- bb(X),bb(Y),(pequal(X,Y);plarger(X,Y)).
plarger_or_eq(X,Y) :- ff(X),bb(Y),(pequal(X,Y);plarger(X,Y)).
plarger_or_eq(X,Y) :- bb(X),ff(Y),(pequal(X,Y);plarger(X,Y)).
plarger_or_eq(X,Y) :- ff(X),ff(Y), pnat(X), plarger_or_eq_generate_Y_then_test(X,Y).
plarger_or_eq_generate_Y_then_test(X,Y) :- pnat(Y),((pequal(X,Y);plarger(X,Y)) -> true; (!,fail)).

% ===
% Testing plarger/2
% In some calls, the predicate is non-deterministic and we gather all solutions using "all"
% ==

:- begin_tests(plarger).

test(plarger_0,nondet)  :- plarger(s(s(z)), z).
test(plarger_1,nondet)  :- plarger(s(z), z).
test(plarger_2,fail)    :- plarger(z, z).
test(plarger_3,fail)    :- plarger(z, s(z)).
test(plarger_4,fail)    :- plarger(a, z).
test(plarger_5,fail)    :- plarger(a, b).
test(plarger_6,fail)    :- plarger(z, b).
test(plarger_7,fail)    :- plarger(z,Y).
test(plarger_8)         :- bagof(X,limit(4,plarger(X,z)),Bag),          Bag == [s(z),s(s(z)),s(s(s(z))),s(s(s(s(z))))].
test(plarger_9)         :- bagof(X,limit(4,plarger(X,s(z))),Bag),       Bag == [s(s(z)),s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z)))))].
test(plarger_10)        :- bagof(Y,limit(4,plarger(s(s(s(z))),Y)),Bag), Bag == [z,s(z),s(s(z))].
test(plarger_11)        :- bagof([X,Y],limit(6,plarger(X,Y)),Bag),      Bag == [[s(z),z],[s(s(z)),z],[s(s(z)),s(z)],[s(s(s(z))),z],[s(s(s(z))),s(z)],[s(s(s(z))),s(s(z))]].

test(pequal_0,nondet)   :- pequal(z, z).
test(pequal_1,nondet)   :- pequal(s(z), s(z)).
test(pequal_2,nondet)   :- pequal(s(s(z)), s(s(z))).
test(pequal_3,fail)     :- pequal(s(z), s(s(z))).

test(plarger_or_eq_1,nondet) :- plarger_or_eq(z, z).
test(plarger_or_eq_2,nondet) :- plarger_or_eq(s(z), s(z)).
test(plarger_or_eq_3,nondet) :- plarger_or_eq(s(s(z())), z).
test(plarger_or_eq_4)        :- bagof(X,limit(4,plarger_or_eq(X,z)),Bag), Bag == [z,s(z),s(s(z)),s(s(s(z)))].
test(plarger_or_eq_5)        :- bagof(Y,limit(4,plarger_or_eq(s(s(s(z))),Y)),Bag), Bag == [s(s(s(z))),z,s(z),s(s(z))].
test(plarger_or_eq_6)        :- bagof([X,Y],limit(6,plarger_or_eq(X,Y)),Bag), Bag == [[z,z],[s(z),z],[s(z),s(z)],[s(s(z)),z],[s(s(z)),s(z)],[s(s(z)),s(s(z))]].

:- end_tests(plarger).

rt(plarger) :- run_tests(plarger).

% ===
% Simple addition. All the cases have been made explicit!
% Note that Prolog does not automagically "do the right thing" in all cases of
% freshvar/boundvar situations.You have to actually think about what happens
% and handle the various cases explicitly.
% The first four cases, where at least 2 parameters are bound, are syntactically
% same code after the bound/fresh verification but they DO different things. 
% These lines could be collapsed, but keeping them separate helps verifying the 
% code. (Note that in languages lieke Jave, you may encounter the same code
% which you can't collapse due to typing constraints).
% ===

padd(X,Y,Z) :- bb(X),bb(Y),bb(Z), Y=z     , X=Z.
padd(X,Y,Z) :- bb(X),bb(Y),bb(Z), Y=s(Ys) , Z=s(Zs) , padd(X,Ys,Zs).
    
padd(X,Y,Z) :- ff(X),bb(Y),bb(Z), Y=z     , X=Z.
padd(X,Y,Z) :- ff(X),bb(Y),bb(Z), Y=s(Ys) , Z=s(Zs) , padd(X,Ys,Zs).
    
padd(X,Y,Z) :- bb(X),ff(Y),bb(Z), Y=z     , X=Z.
padd(X,Y,Z) :- bb(X),ff(Y),bb(Z), Z=s(Zs),  plarger_or_eq(Z,Y), Y=s(Ys) , padd(X,Ys,Zs).
    
padd(X,Y,Z) :- bb(X),bb(Y),ff(Z), Y=z     , X=Z.
padd(X,Y,Z) :- bb(X),bb(Y),ff(Z), Y=s(Ys) , Z=s(Zs) , padd(X,Ys,Zs).

% Hairy case: two unbound parameters

padd(X,Y,Z) :- ff(X),bb(Y),ff(Z), generate_X_add_then_test(X,Y,Z).
padd(X,Y,Z) :- ff(X),ff(Y),bb(Z), generate_X_add_then_test(X,Y,Z).                            
padd(X,Y,Z) :- bb(X),ff(Y),ff(Z), pnat(Y),padd(X,Y,Z). % guess forever over Y

% Hairier case: three unbound parameters

padd(X,Y,Z) :- ff(X),ff(Y),ff(Z), pnat(Z),padd(X,Y,Z). % guess forever over Z (not over X and Y!)

% Avoid runaway to infinity on backtracking after solution by using a 
% convoluted control construct.

generate_X_add_then_test(X,Y,Z) :- pnat(X),(padd(X,Y,Z) -> true ; (!,false)). 

% ===
% Testing addition
% ===

:- begin_tests(padd).

test(add_bbf_0,nondet) :- bbf_collect(padd,plus_compose,0,0,Bag,100), Bag==[0+0=0].
test(add_bbf_1,nondet) :- bbf_collect(padd,plus_compose,1,0,Bag,100), Bag==[1+0=1].
test(add_bbf_2,nondet) :- bbf_collect(padd,plus_compose,0,1,Bag,100), Bag==[0+1=1].
test(add_bbf_3,nondet) :- bbf_collect(padd,plus_compose,1,1,Bag,100), Bag==[1+1=2].

test(add_fbb_0,nondet) :- fbb_collect(padd,plus_compose,0,0,Bag,100), Bag==[0+0=0].
test(add_fbb_1,fail)   :- fbb_collect(padd,plus_compose,1,0,_,100).
test(add_fbb_2,nondet) :- fbb_collect(padd,plus_compose,0,1,Bag,100), Bag==[1+0=1].
test(add_fbb_3,nondet) :- fbb_collect(padd,plus_compose,1,1,Bag,100), Bag==[0+1=1].
test(add_fbb_4,fail)   :- fbb_collect(padd,plus_compose,2,1,Bag,100), Bag==[0+2=1].
test(add_fbb_5,nondet) :- fbb_collect(padd,plus_compose,1,2,Bag,100), Bag==[1+1=2].

test(add_bfb_0,nondet) :- bfb_collect(padd,plus_compose,0,0,Bag,100), Bag==[0+0=0].
test(add_bfb_1,fail)   :- bfb_collect(padd,plus_compose,1,0,_,100).
test(add_bfb_2,nondet) :- bfb_collect(padd,plus_compose,0,1,Bag,100), Bag==[1+0=1].
test(add_bfb_3,nondet) :- bfb_collect(padd,plus_compose,1,1,Bag,100), Bag==[1+0=1].
test(add_bfb_4,nondet) :- bfb_collect(padd,plus_compose,2,1,Bag,100), Bag==[2+1=1].
test(add_bfb_5,fail)   :- bfb_collect(padd,plus_compose,1,2,_,100).

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

rt(padd) :- run_tests(padd).






% ===
% Simple multiplication.
% For today, we will restrict ourselves to the case of at least 2 bound parameters.
% ===

pmult(X,Y,Z) :- bb(X),bb(Y),bb(Z), Y=z     , Z=z.
pmult(X,Y,Z) :- bb(X),bb(Y),bb(Z), Y=s(Ys) , pmult(X,Ys,Zt), padd(X,Zt,Z).
    
pmult(X,Y,Z) :- ff(X),bb(Y),bb(Z), Y=z     , Z=z.
pmult(X,Y,Z) :- ff(X),bb(Y),bb(Z), Y=s(Ys) , padd(X,Zt,Z), pmult(X,Ys,Zt).
    
pmult(X,Y,Z) :- bb(X),ff(Y),bb(Z), Y=z     , Z=z.
pmult(X,Y,Z) :- bb(X),ff(Y),bb(Z), Y=s(Ys) , padd(X,Zt,Z), pmult(X,Ys,Zt).

pmult(X,Y,Z) :- bb(X),bb(Y),ff(Z), Y=z     , Z=z.
pmult(X,Y,Z) :- bb(X),bb(Y),ff(Z), Y=s(Ys) , pmult(X,Ys,Zt), padd(X,Zt,Z).  

% Hairy case: two unbound parameters

pmult(X,Y,Z) :- ff(X),bb(Y),ff(Z), Y=z,  pnat(X),pmult(X,Y,Z). 
pmult(X,Y,Z) :- ff(X),bb(Y),ff(Z), Y=s(_), pnat(X),pmult(X,Y,Z).

pmult(X,Y,Z) :- ff(X),ff(Y),bb(Z), Z=z,  pnat(K),padd(X,Y,K). % completely artificial: generate all possible pairs (X,Y)
pmult(X,Y,Z) :- ff(X),ff(Y),bb(Z), Z=s(_), psmaller_or_eq(Y,Z),format("Trying ~q ~q ~q\n",[X,Y,Z]),pmult(X,Y,Z). % this will terminate eventually as it goes to 0, not to +oo (might not be the case if we go to +oo)

pmult(X,Y,Z) :- bb(X),ff(Y),ff(Z), pnat(Y), pmult(X,Y,Z). % guess forever over Y

% Hairier case: three unbound parameters

pmult(X,Y,Z) :- ff(X),ff(Y),ff(Z), pnat(Z),pmult(X,Y,Z).  % guess forever over Z (not over X and Y!)

% ---
% Immediately followed by tests.
% ---

:- begin_tests(pmult1).

test(mult_bbf_0,nondet) :- bbf_collect(pmult,times_compose,0,0,Bag,7), Bag==[0*0=0].
test(mult_bbf_1,nondet) :- bbf_collect(pmult,times_compose,1,0,Bag,7), Bag==[1*0=0].
test(mult_bbf_2,nondet) :- bbf_collect(pmult,times_compose,0,1,Bag,7), Bag==[0*1=0].
test(mult_bbf_3,nondet) :- bbf_collect(pmult,times_compose,1,1,Bag,7), Bag==[1*1=1].
test(mult_bbf_4,nondet) :- bbf_collect(pmult,times_compose,1,2,Bag,7), Bag==[1*2=2].
test(mult_bbf_5,nondet) :- bbf_collect(pmult,times_compose,2,1,Bag,7), Bag==[2*1=2].
test(mult_bbf_6,nondet) :- bbf_collect(pmult,times_compose,2,2,Bag,7), Bag==[2*2=4].

test(mult_fbb_0,nondet) :- fbb_collect(pmult,times_compose,0,0,Bag,4), Bag==[0*0=0,1*0=0,2*0=0,3*0=0].
test(mult_fbb_1,nondet) :- fbb_collect(pmult,times_compose,1,0,Bag,1), Bag==[0*1=0].
test(mult_fbb_2,fail)   :- fbb_collect(pmult,times_compose,0,1,Bag,1).
test(mult_fbb_3,nondet) :- fbb_collect(pmult,times_compose,1,1,Bag,1), Bag==[1*1=1].
test(mult_fbb_4,nondet) :- fbb_collect(pmult,times_compose,1,2,Bag,1), Bag==[2*1=2].
test(mult_fbb_5,fail)   :- fbb_collect(pmult,times_compose,2,1,Bag,1).
test(mult_fbb_6,nondet) :- fbb_collect(pmult,times_compose,2,2,Bag,1), Bag==[1*2=2].

:- end_tests(pmult1).

:- begin_tests(pmult2).

test(mult_bff_0,nondet) :- bff_collect(pmult,times_compose,0,Bag,10), Bag==[0*0=0,0*1=0,0*2=0,0*3=0,0*4=0,0*5=0,0*6=0,0*7=0,0*8=0,0*9=0].
test(mult_bff_1,nondet) :- bff_collect(pmult,times_compose,1,Bag,10), Bag==[1*0=0,1*1=1,1*2=2,1*3=3,1*4=4,1*5=5,1*6=6,1*7=7,1*8=8,1*9=9].
test(mult_bff_2,nondet) :- bff_collect(pmult,times_compose,2,Bag,10), Bag==[2*0=0,2*1=2,2*2=4,2*3=6,2*4=8,2*5=10,2*6=12,2*7=14,2*8=16,2*9=18].

test(mult_fbf_0,nondet) :- fbf_collect(pmult,times_compose,0,Bag,10), Bag==[0*0=0,1*0=0,2*0=0,3*0=0,4*0=0,5*0=0,6*0=0,7*0=0,8*0=0,9*0=0].
test(mult_fbf_1,nondet) :- fbf_collect(pmult,times_compose,1,Bag,10), Bag==[0*1=0,1*1=1,2*1=2,3*1=3,4*1=4,5*1=5,6*1=6,7*1=7,8*1=8,9*1=9].
test(mult_fbf_2,nondet) :- fbf_collect(pmult,times_compose,2,Bag,10), Bag==[0*2=0,1*2=2,2*2=4,3*2=6,4*2=8,5*2=10,6*2=12,7*2=14,8*2=16,9*2=18].

test(mult_ffb_0,nondet) :- ffb_collect(pmult,times_compose,0,Bag,10), Bag==[0*0=0,0*1=0,1*0=0,0*2=0,1*1=0,2*0=0,0*3=0,1*2=0,2*1=0,3*0=0].
/*
test(mult_ffb_1,nondet) :- ffb_collect(pmult,times_compose,1,Bag,10), Bag==
test(mult_ffb_2,nondet) :- ffb_collect(pmult,times_compose,2,Bag,10), Bag==
test(mult_ffb_3,nondet) :- ffb_collect(pmult,times_compose,3,Bag,10), Bag==
test(mult_ffb_4,nondet) :- ffb_collect(pmult,times_compose,4,Bag,10), Bag==
test(mult_ffb_5,nondet) :- ffb_collect(pmult,times_compose,5,Bag,10), Bag==
*/

:- end_tests(pmult2).

rt(pmult) :- run_tests(pmult1),run_tests(pmult2).














/*

:- begin_tests(peano_add).

            
:- end_tests(peano_add).

rt(peano_add) :- run_tests(peano_add).

:- begin_tests(peano_map).


test(cmp_0,fail) :- peano_larger(z,z).
test(cmp_1)      :- peano_larger(s(s(z)),z).
test(cmp_2)      :- peano_larger(s(s(s(z))),s(z)).
test(cmp_3)      :- peano_larger(s(s(s(z))),s(s(z))).
test(cmp_4,fail) :- peano_larger(s(s(s(z))),s(s(s(z)))).
test(cmp_5,fail) :- peano_larger(s(s(s(z))),s(s(s(s(z))))).

test(cmp_x, all(X == [s(z), 
                     s(s(z)), s(s(s(z))), s(s(s(s(z)))), s(s(s(s(s(z)))))] )) :- limit(5,peano_larger(X,z)).
test(cmp_x, all(X == [z,s(z),s(s(z))])) :- peano_larger(s(s(s(z))),X).

test(cmp_x, all([X,Y] == 

s(z),z ;
s(s(z)),z ;
s(s(s(z))),z ;
s(s(s(s(z)))),z 


:- end_tests(peano_map).

rt(peano_map) :- run_tests(peano_map).
*/

bbf_collect(Op,Compose,NN0,NN1,Bag,Count) :- 
   pmlist([PN0,PN1],[NN0,NN1]), bagof(CC,
                              PN2^NN2^limit(Count,
                                                    (call(Op,PN0,PN1,PN2), pmlist([PN2], [NN2]),
                                                     call(Compose,NN0,NN1,NN2,CC))),Bag).

fbb_collect(Op,Compose,NN1,NN2,Bag,Count) :- 
   pmlist([PN1,PN2],[NN1,NN2]), bagof(CC,
                              PN0^NN0^limit(Count,
                                                    (call(Op,PN0,PN1,PN2), pmlist([PN0], [NN0]),
                                                     call(Compose,NN0,NN1,NN2,CC))),Bag).

bfb_collect(Op,Compose,NN0,NN2,Bag,Count) :- 
   pmlist([PN0,PN2],[NN9,NN2]), bagof(CC,
                              PN1^NN1^limit(Count,
                                                    (call(Op,PN0,PN1,PN2), pmlist([PN1], [NN1]),
                                                     call(Compose,NN0,NN1,NN2,CC))),Bag).

bff_collect(Op,Compose,NN0,Bag,Count) :- 
   pmlist([PN0],[NN0]), bagof(CC,
                              PN1^PN2^NN1^NN2^limit(Count,
                                                    (call(Op,PN0,PN1,PN2), pmlist([PN1,PN2], [NN1,NN2]),
                                                     call(Compose,NN0,NN1,NN2,CC))),Bag).

fbf_collect(Op,Compose,NN1,Bag,Count) :- 
   pmlist([PN1],[NN1]), bagof(CC,
                              PN0^PN2^NN0^NN2^limit(Count,
                                                    (call(Op,PN0,PN1,PN2), pmlist([PN0,PN2], [NN0,NN2]),
                                                     call(Compose,NN0,NN1,NN2,CC))),Bag).

ffb_collect(Op,Compose,NN2,Bag,Count) :- 
   pmlist([PN2],[NN2]), bagof(CC,
                              PN0^PN1^NN0^NN1^limit(Count,
                                                    (call(Op,PN0,PN1,PN2), pmlist([PN0,PN1], [NN0,NN1]),
                                                     call(Compose,NN0,NN1,NN2,CC))),Bag).                                                    

fff_collect(Op,Compose,Bag,Count) :- 
                        bagof(CC,
                              PN0^PN1^PN2^NN0^NN1^NN2^limit(Count,
                                                    (call(Op,PN0,PN1,PN2), pmlist([PN0,PN1,PN2], [NN0,NN1,NN2]),
                                                     call(Compose,NN0,NN1,NN2,CC))),Bag).                                                    
                              
plus_compose(A,B,C,(A+B=C)). % syntactic composition for nice printout
times_compose(A,B,C,(A*B=C)). % syntactic composition for nice printout
