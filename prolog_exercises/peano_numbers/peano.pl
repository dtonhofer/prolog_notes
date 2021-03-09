% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-06-XX
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
% Runs on: SWI Prolog 8.1
% ============================================================================
%
%     (computer                                         (computer
%  representation of)                               representation of)
%
%   Natural Numbers ---▶--- pm(?PN,?NN) -------▶------- Peano Numbers
%     0,1,2,3,4,..          implements         z, s(z), s(s(z)), s(s(s(z)))
%                          isomorphism                     |
%                                                          ▼
%                                                          |
%                                                 |   compute using
%                        implemented              |   ⪧ padd(X,Y,Z)
%                 as (enumerating) relations -----|   ⪧ pmult(X,Y,Z)
%                      not only functions         |   ⪧ pqorem(X,Y,Q,R)
%                                                 |        |
%                                                          ▼
%                                                          |
%   Natural Numbers ---◀--- pm(?PN,?NN) -------◀----- Peano Numbers
%
% Natural Numbers are: 0,1,2,3,4, ...
% Peano Numbers are:   z,s(z),s(s(z),s(s(s(z))), ...
%
%             or are:  [],[s],[s,s],[s,s,s],[s,s,s,s], ...
%                      it's a far more practical representation for Prolog code
%
% Reading:
%
% https://en.wikipedia.org/wiki/Natural_number
% https://en.wikipedia.org/wiki/Peano_axioms
% https://en.wikipedia.org/wiki/Proofs_involving_the_addition_of_natural_numbers
%
% Thoughts:
%
% Note that the "equals" appearing in Peano Addition axioms for example is
% purely a rewriting rule over expressions containing the function symbols
% s/1, */2, +/2, zero/0:
%
% a + 0 = a            means: if you find "a + 0" you can write "a" instead
%                             if you find "a" you can write "a + 0" instead
%
% a + S(b) = S(a + b)  means: if you find "a + S(b)" you can write "S(a + b)" instead
%                             if you find "S(a + b)" you can write "a + S(b)" instead
%                             ... where a and b stand for some Peano numbers.
%
% It would be extremely cool to be able to set up a constraint between two
% variables PN and NN, so that if NN is bound to a natural number, PN is
% automatically bound to the coressponding Peano Number, and vice-versa.
% That can probably be done with some effort using attributed variables, but I 
% haven't thought about that.
%
% TODO
%
% - Would be interesting to try tabling on the padd/pmult predicates.
% ============================================================================

% ===
% The calls to the unit test sections are written as follows:
%
% rt(some_id) :- run_tests(some_id).    % rt/1 stands for "run tests".
%
% They can be called indivdually. You can then run all the tests by doing:
%
% ?- rtall.
%
% Which just calls "bagof(X,rt(X),Bag)."
%
% The rt/1 must be marked as "discontiguous".
% ===

:- discontiguous rt/1.

rtall :- bagof(X,rt(X),_).

% ===
% On SWI Prolog, you may want to enable unlimited term depth printing first.
% ===

:- initialization(
      (set_prolog_flag(answer_write_options,[max_depth(0)]),
       set_prolog_flag(debugger_write_options,[max_depth(0)]))).

% ===
% Something to replace (badly named & ugly) "var(X)" and "nonvar(X)"
% ===

% is X referencing a fresh/unbound/uninstantiated term? ("is X a freshvar"?)

ff(X) :- var(X).

% is X referencing an nonfresh/bound/instantiated term? ("is X a boundvar"?)

bb(X) :- nonvar(X).

% ===
% Bidirectional Mapping between Peano Numbers and Natural Numbers
% pm(?PN,?NN)
% ---
% - We just calls this "pm/2" because it can be written in a less verbose fashion.
% - For the same reason, we use "z" instead of "zero".
% - This predicate also generates successively larger pairs when called as pm(X,Y).
% ===

% ---
% Simple testing shortens development time:
% Acceptable natural?
% Acceptable peano number?
% It also makes immediately clear when we are moving outside of the domain,
% using a "thrown exception" as third truth value, and distinguishing an invalid
% query from a valid query that yields "false".
% ---

acc_n(X) :- ff(X),!.
acc_n(X) :- (bb(X),integer(X),X>=0)
             -> true
             ; domain_error("natural",X).

acc_p(X) :- ff(X),!.
acc_p(X) :- (bb(X),(X==z;X=s(_))) % don't check recursively!! We assume good
             -> true
             ; domain_error("peano number",X).

% The check in the first line of pm/2 is done *at every call* of pm/2.
% If throws if out-of-domain, but fails otherwise, leaving actual processing
% to be done by subsequent pm/2 clauses.

pm(PN,NN) :- acc_p(PN),acc_n(NN),fail.

% ---
% Using CLP(FD) instead of "is" allows us to work "both ways" using the same code.
% (although one has to pay attention to termination)
% ---

/*
:- use_module(library(clpfd)).

pm(z, 0).
pm(s(N), X) :- X #> 0, X #= Y+1, pm(N, Y). % No need to test ff(X) or bb(X)
*/

% ---
% Alternatively, basic Prolog instead of CLP(FD)
% ---

pm(PN,NN) :- bb(PN),bb(NN),PN==z,NN==0,!.
pm(PN,NN) :- bb(PN),bb(NN),PN=s(PNu),NN>0,!,NNu is NN-1,pm(PNu,NNu).

pm(PN,NN) :- bb(PN),ff(NN),PN==z,!,NN=0.
pm(PN,NN) :- bb(PN),ff(NN),PN=s(PNu),!,pm(PNu,NNu),NN is NNu+1.

pm(PN,NN) :- ff(PN),bb(NN),NN==0,!,PN=z.
pm(PN,NN) :- ff(PN),bb(NN),NN>0,!,NNu is NN-1,pm(PNu,NNu),PN=s(PNu).

pm(PN,NN) :- ff(PN),ff(NN),PN=z,NN=0.
pm(PN,NN) :- ff(PN),ff(NN),pm(PNu,NNu),NN is NNu+1,PN=s(PNu).

% ---
% pm_list/2 maps a whole list using maplist/3; this will be useful for a bit later
% However, we do not want it to generate or even be in the case of generation,
% just in the case of bidirectional mapping, so we check that.
% ---

% SIMPLE but also generates

% pm_list(PNs,NNs) :-
%    maplist(pm,PNs,NNs).

% Additional check to preclude generation .
% Note that the ISO Instantiation Error takes no argument (facepalm) but
% SWI Prolog provides one anyway. Good.

pm_list(PNs,NNs) :-
   maplist(pm_list_in_list,PNs,NNs).

pm_list_in_list(PN,NN) :-
   (ff(PN),ff(NN))->instantiation_error("Both PN and NN are freshvars");pm(PN,NN).

% This is an extra used for debugging

pm_list_dbg(PNs,NNs) :-
   maplist(pm_list_in_list_dbg,PNs,NNs).

pm_list_in_list_dbg(PN,NN) :-
   (ff(PN),ff(NN))->(PN='?',NN='?');pm(PN,NN).

% ===
% Testing of pm/2
% In some calls, the predicate is non-deterministic and we gather all solutions
% using "all".
% Contrary to the confusion which reigns when the predicate just fails if arguments
% are not in-domain, a predicate that throws of arguments are not in-domain brings
% great joy!
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

rt(pm) :- run_tests(pm).

% ===
% Generator & Verifier for Peano Numbers
% ===
% This is mostly a generator ("verifying" that one has a Peano Number at hand isn't so
% interesting as we assume it has the right structure down to "z" if it starts with
% function symbol s/1).
%
% The *generator* is needed to generate solutions for freshvars representing Peano Numbers,
% as Prolog generates representative individuals sampled from some domain making a query
% true. A general theorem prover would determine whether the query is true "as a whole"
% using natural deduction or otherwise and not care about representative individuals.
%
% It is important to notice that if we have a boundvar X designating a Peano Number,
% we assume it is ground as in "X == s(s(z))" and not "nonzero but of unspecified magnitude":
% as in "X = s(s(_))" -- that would demand another approach.
% ===

pnat(PN) :- acc_p(PN),fail. % Test passed parameter, then fail into the actual procedure
pnat(z).
pnat(s(X)) :- pnat(X).

% ===
% Is a Peano Number larger than zero? Alternatively: generate Peano Numbers larger than 0.
% Testing for nonzero is actually simpler: X=s(_)
% ===

pnonz(s(X)) :- pnat(X).

% ===
% Testing of pnat/1, pnonz/1
% ===

:- begin_tests(pnat).

test(pnat_a,error(domain_error(_,_)))  :- pnat(a).
test(pnat_0)                           :- pnat(z).
test(pnat_1)                           :- pnat(s(z)).
test(pnat_2)                           :- pnat(s(s(z))).
test(pnat_forever) :-
   bagof(N,limit(6,pnat(N)),Bag),
   Bag == [z,s(z),s(s(z)),s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z)))))].

test(pnonz_0,fail)  :- pnonz(z).
test(pnonz_1)       :- pnonz(s(z)).
test(pnonz_2)       :- pnonz(s(s(z))).
test(pnonz_forever) :-
   bagof(N,limit(6,pnonz(N)),Bag),
   Bag == [s(z),s(s(z)),s(s(s(z))),s(s(s(s(z)))),s(s(s(s(s(z))))),s(s(s(s(s(s(z))))))].

:- end_tests(pnat).

rt(pnat) :- run_tests(pnat).

% ===
% Equality
% ===
% When you think about it, this is equality among "representations of Peano Numbers in Prolog"
% I'm not sure equality in Peano Arithmetic is meaningful - for each number, there is only one
% number. OTOH, dis-equality in Peano Arithmetic IS meaningful, but nobody seems to talk about
% that?
%
% Here, the equality relation is basically Prolog's "=", with a special case of both sides are freshvars.
% One could also define it as: PX = PY <=> PX + z = PY
% One could also define it recursively over the s(_) chain.
% But equality is not defined in that way in Peano arithemtic, so why bother.
% ===

pequal(PX,PY) :- acc_p(PX),acc_p(PY),fail.        % Test passed parameter, then fail into the actual procedure
pequal(PX,PY) :- bb(PX),bb(PY), PX=PY.            % "==" would work too, instead of unification
pequal(PX,PY) :- ff(PX),bb(PY), PX=PY.            % unification is correct here
pequal(PX,PY) :- bb(PX),ff(PY), PX=PY.            % unification is correct here
pequal(PX,PY) :- ff(PX),ff(PY), pnat(PX), PX=PY.  % generate PX and unify

:- begin_tests(pequal).

test(pequal_0,nondet)   :- pequal(z, z).
test(pequal_1,nondet)   :- pequal(s(z), s(z)).
test(pequal_2,nondet)   :- pequal(s(s(z)), s(s(z))).
test(pequal_3,fail)     :- pequal(s(z), s(s(z))).

:- end_tests(pequal).

rt(pequal) :- run_tests(pequal).

% ===
% Addition.
% ===
% All the cases have been made explicit!
%
% Note that Prolog does NOT automagically "do the right thing" in all cases of freshvar/boundvar
% situations.You have to actually think about what happens and handle the various cases explicitly.
% Some lines could be collapsed, but keeping them separate helps verifying the code.
% ===

% ---
% Check passed parameter, then fail into the actual procedure
% ---

padd(PX,PY,PZ) :- acc_p(PX),acc_p(PY),acc_p(PZ),fail.

% ---
% Only boundvar parameters: Verification PZ ?= PX + PY
% ---

padd(PX,PY,PZ) :- bb(PX),bb(PY),bb(PZ), PY=z     ,PX=PZ.
padd(PX,PY,PZ) :- bb(PX),bb(PY),bb(PZ), PY=s(PYs),PZ=s(PZs),padd(PX,PYs,PZs).

% ---
% One freshvar parameter: Function
% ---

% Addition PZ := PX + PY

padd(PX,PY,PZ) :- bb(PX),bb(PY),ff(PZ), PY=z     ,PX=PZ.
padd(PX,PY,PZ) :- bb(PX),bb(PY),ff(PZ), PY=s(PYs),PZ=s(PZs),padd(PX,PYs,PZs).

% Subtraction PX := PZ-PY

padd(PX,PY,PZ) :- ff(PX),bb(PY),bb(PZ), PY=z     ,PX=PZ.
padd(PX,PY,PZ) :- ff(PX),bb(PY),bb(PZ), PY=s(PYs),PZ=s(PZs),padd(PX,PYs,PZs).

% Subtraction PY := PZ-PX

% If we follow the same code as above, the, if PY is free, we will wander off
% into infinity if the subtraction has no solution .

% We can use the fact that addition is provably commutative in Peano arithmetic.
% Then we can swap the first two arguments. But then we define Peano Addition
% based on properties of the Peano Addition. That sounds circular.
% Proceed anyway:

padd(PX,PY,PZ) :- bb(PX),ff(PY),bb(PZ), padd(PY,PX,PZ).

% ---
% Two freshvar parameters
% ---

% Generate valid solutions!

padd(PX,PY,PZ) :- ff(PX),bb(PY),ff(PZ), generate_X_add_then_test(PX,PY,PZ).
padd(PX,PY,PZ) :- ff(PX),ff(PY),bb(PZ), generate_X_add_then_test(PX,PY,PZ).

% guess forever over PY

padd(PX,PY,PZ) :- bb(PX),ff(PY),ff(PZ), pnat(PY),padd(PX,PY,PZ).

% ---
% Three freshvar parameters
% ---

% guess forever over Z (not over X and Y!)

padd(PX,PY,PZ) :- ff(PX),ff(PY),ff(PZ), pnat(PZ),padd(PX,PY,PZ).

% ---
% Avoid runaway to infinity on backtracking after the solution has been found
% ---
% By using a convoluted control construct. I don't like this, but it needs no
% special helper predicates to test for "larger than" (that test should be
% based on add/3, not the converse). Is there anything nicer?

generate_X_add_then_test(PX,PY,PZ) :- pnat(PX),(padd(PX,PY,PZ) -> true ; (!,false)).

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

rt(padd) :- run_tests(padd).

% ===
% plesseq/2, pless/2 : X <= Y, X < Y
% ===
% We have padd/3, now we can test for "less or equal"
% Implementation-wise, one would test "less or equal" by counting the "s" depth on both side
% but we try to stay close to Peano Arithmetic:
% For all a, b ∈ N, a ≤ b if and only if there exists some c ∈ N such that a + c = b.
% ===

plesseq(PX,PY) :- acc_p(PX),acc_p(PY),fail. % removable check for passed parameters
plesseq(PX,PY) :- padd(PX,_,PY).

pless(PX,PY) :- acc_p(PX),acc_p(PY),fail. % removable check for passed parameters

pless(PX,PY) :- bb(PX),bb(PY),padd(PX,PP,PY),PP=s(_).
pless(PX,PY) :- bb(PX),ff(PY),padd(PX,PP,PY),PP=s(_).
pless(PX,PY) :- ff(PX),bb(PY),padd(PX,PP,PY),PP=s(_).

% detecting & handling that special case for PX==PY is ugly

pless(PX,PY) :- ff(PX),ff(PY),((PX==PY)->fail;(padd(PX,PP,PY),PP=s(_))).

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

rt(pless) :- run_tests(pless).

% ===
% Simple multiplication.
% ===
% All the cases have been made explicit!

% ---
% Check passed parameter, then fail into the actual procedure
% ---

pmult(PX,PY,PZ) :- acc_p(PX),acc_p(PY),acc_p(PZ),fail.

% ---
% Only boundvar parameters: Verification PZ =? PX*PY
% ---

pmult(PX,PY,PZ) :- bb(PX),bb(PY),bb(PZ), PY=z      , PZ=z.
pmult(PX,PY,PZ) :- bb(PX),bb(PY),bb(PZ), PY=s(PYs) , pmult(PX,PYs,PZt), padd(PX,PZt,PZ).

% ---
% One freshvar parameter: Function
% ---

% Multiplication PZ := PX*PY

pmult(PX,PY,PZ) :- bb(PX),bb(PY),ff(PZ), PY=z      , PZ=z.
pmult(PX,PY,PZ) :- bb(PX),bb(PY),ff(PZ), PY=s(PYs) , pmult(PX,PYs,PZt), padd(PX,PZt,PZ).

% Division PX := PZ/PY (fails if no solution)
% We have to reorder the padd/3 and pmult/3 in defiance of the Peano axioms
% to achieve termination. I say that's ok!

pmult(PX,PY,PZ) :- ff(PX),bb(PY),bb(PZ), PY=z      , PZ=z, pnat(PX). % generate or verify
pmult(PX,PY,PZ) :- ff(PX),bb(PY),bb(PZ), PY=s(PYs) , padd(PX,PZt,PZ), pmult(PX,PYs,PZt).

% Division PY := PZ/PX (fails if no solution)
% We need to diverge from the definition of multiplication given by the Peano axioms
% to achieve termination. We can use the fact that multiplication is provably commutative
% in Peano arithmetic. Then we can swap the first two arguments. But then we define Peano
% Multiplication based on properties of the Peano Multiplication. That sounds circular.
% Proceed anyway:

pmult(PX,PY,PZ) :- bb(PX),ff(PY),bb(PZ), pmult(PY,PX,PZ).

% ---
% Two freshvar parameters
% ---

% Generate valid solutions!

pmult(PX,PY,PZ) :- ff(PX),bb(PY),ff(PZ), PY=z    , pnat(PX), pmult(PX,PY,PZ).
pmult(PX,PY,PZ) :- ff(PX),bb(PY),ff(PZ), PY=s(_) , pnat(PX), pmult(PX,PY,PZ).

% I have been searching for some pithy way to generate the numbers but once
% I thought about using "once/1", it was strangely obvious.
% Still needs a special case on 0*0=0 because one does not want to generate it twice.
% 0 as the "absorber" of multiplication is a problem!

pmult(PX,PY,PZ) :- ff(PX),ff(PY),bb(PZ), PZ=z    , ((PX=z,PY=z);(pnonz(PK), (PX=PK;PY=PK), once(pmult(PX,PY,PZ)))).
pmult(PX,PY,PZ) :- ff(PX),ff(PY),bb(PZ), PZ=s(_) , generate_Y_mult_then_test(PX,PY,PZ).

% guess forever over Y

pmult(PX,PY,PZ) :- bb(PX),ff(PY),ff(PZ), pnat(PY), pmult(PX,PY,PZ).

% ---
% Three freshvar parameters
% ---

% Weave the guessing over the X=0 line, Y=0 line and (X,Y)=/=(0,0) surface
%

pmult(X,Y,Z) :- ff(X),ff(Y),ff(Z), X=z,Y=z,pmult(X,Y,Z).
pmult(X,Y,Z) :- ff(X),ff(Y),ff(Z),
                pnonz(K),
                (pmult_xis0_line(K,X,Y,Z);pmult_yis0_line(K,X,Y,Z);pmult_xynon0_surface(K,X,Y,Z)).




% ---
% Avoid runaway to infinity on backtracking by generating using plesseq/2
% ---
% This is not fully in the spirit of the Peano Axioms, but near enough.

generate_Y_mult_then_test(PX,PY,PZ) :- plesseq(PY,PZ), pmult(PX,PY,PZ).

% ---
% Helpers for generating three values
% ---

pmult_xis0_line(K,X,Y,Z)      :- X=z,Y=K,pmult(X,Y,Z).
pmult_yis0_line(K,X,Y,Z)      :- X=K,Y=z,pmult(X,Y,Z).
pmult_xynon0_surface(K,X,Y,Z) :- Z=K,pmult(X,Y,Z).

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

rt(pmult) :- run_tests(pmult).

% ===
% Quotient and Remainder are best computed together
% ===

% PT = PM*PQ+PR, PR < PM (and PM*PQ <= PT)

pquotrem(PT,PM,PQ,PR) :-
   bb(PT),bb(PM),
   (PM==z -> domain_error('non-z peano number',z);true),
   pmult(PM,PQ,PSS),
   ((\+ plesseq(PSS,PT)) -> (!,fail) ; true),
   padd(PSS,PR,PT),
   pless(PR,PM).

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

rt(pqr) :- run_tests(pqr).

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


% ===
% UNUSED DRIFTCODE BELOW
% ===

/*
% ===
% Debug-print: Two predicates to put around others
% ===

debug_write(When,Op,VarNames,PValues) :-
   \+ \+ debug_write_shielded(When,Op,VarNames,PValues). % \+ \+ to make sure no changes occur

debug_write_shielded(When,Op,VarNames,PValues) :-
   pm_list_dbg(PValues,NValues),
   format("~s ~q: ",[When,Op]),
   maplist([VarName,NValue]>>format("~s = ~q,",[VarName,NValue]),VarNames,NValues),
   format("\n").


% ===
% Compare magnitudes, giving meaning to the ">" relation among Peano Numbers: PX > PY.

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
test(plarger_7,fail)    :- plarger(z,_).
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
*/
