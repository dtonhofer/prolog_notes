:- module(peano,
          [
           pm/2         % bidirectional mapping between Naturals and Peano Numbers
          ,pnat/1       % generate (or verify) a Peano Number
          ,pm_list/2    % apply pm/2 to a list
          ,pnonz/1      % is the argument a nonzero Peano Number
          ,pequal/2     % are two Peano Numbers equal
          ,padd/3       % add two Peano Numbers, giving a third (also works in subtraction direction)
          ,pmult/3      % multiply two Peano Numbers, giving a third (also works in division direction)
          ,pquotrem/4   % compute quotient and remainder given two Peano Numbers          
          ,plesseq/2    % relation =< between two Peano Numbers
          ,pless/2      % relation < between two Peano Numbers          
          ]).
          
% ============================================================================
% Peano Arithmetic exercise
% ============================================================================
% Located at : https://github.com/dtonhofer/prolog_notes
% Runs on    : SWI Prolog 8.3 (should be easily adaptable to other Prologs)
% ============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% ============================================================================
% 2020-06-XX Version 1.0
% 2021-03-09 Style updated with a lot of things I have learned.
% ============================================================================
% Examples
%
% Add 5 and 10 in "Peano Space":
%
% ?- pm(PA,5),pm(PB,10),padd(PA,PB,PC),pm(PC,R).
%
% Find X,Y such that X*Y=78 in "Peano Space":
%
% ?- pm(PN,78),pmult(PNX,PNY,PN),pm(PNX,X),pm(PNY,Y). 
% 
% Find Q,R such that 66=Q*7+R in "Peano Space":
%
% ?- pm(PA,66),pm(PB,7),pquotrem(PA,PB,PQ,PR),pm(PQ,Q),pm(PR,R).
%
% Enumerate the A,B such that A*B=34 in "Peano Space":
%
% ?- pm(PM,34),pmult(PA,PB,PM),pm(PA,A),pm(PB,B).
% ============================================================================
%
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
% What is the "deep" interpretation of the "equals sign" appearing in Peano Addition
% axioms? One can interprete it as syntactic equivalence of the left-hand side
% and the right-hand side:
%
% a + 0 = a            means: if you find "a + 0" you can write "a" instead
%                             if you find "a" you can write "a + 0" instead
%
% a + S(b) = S(a + b)  means: if you find "a + S(b)" you can write "S(a + b)" instead
%                             if you find "S(a + b)" you can write "a + S(b)" instead
%                             ... where a and b stand for some Peano numbers.
%
% It would be extremely nice to be able to set up a constraint between two
% variables PN and NN, so that if NN is bound to a natural number, PN is
% automatically bound to the coressponding Peano Number, and vice-versa.
% That can probably be done using attributed variables.
%
% TODO
%
% Try tabling on the padd/pmult predicates. (Actually, I tried to 
% table(peano:padd/3), but got
% ERROR: '$tbl_wkl_add_answer'/4: Not enough resources: private_table_space)
%
% On SWI Prolog, you may want to enable unlimited term depth printing first.
%
% set_prolog_flag(answer_write_options,[max_depth(0)]).
% set_prolog_flag(debugger_write_options,[max_depth(0)]).
% ============================================================================

% ===
% acc_n(@X) - acceptable natural number? Throws is not.
% acc_n(@X) - acceptable peano number? Throws if not.
%
% acc_n/1 and acc_p/1 shorten development time! They also makes immediately clear 
% when we are moving outside of the domain, using a "thrown exception" as third 
% truth value (in a sense), and distinguishing an invalid query from a valid 
% query that yields "false".
% ===

acc_n(X) :- var(X),!.
acc_n(X) :- (integer(X),X>=0) -> true ; domain_error("natural",X).

acc_p(X) :- var(X),!.
acc_p(X) :- (nonvar(X),(X==z;X=s(_))) % don't check recursively!! We assume good
             -> true
             ; domain_error("peano number",X).
                          
% ===
% Bidirectional Mapping between Peano Numbers and Natural Numbers
% pm(?PN,?NN)
% ===
% - We just calls this "pm/2" because it can be written in a less verbose fashion.
% - For the same reason, we use "z" instead of "zero".
% - This predicate also generates successively larger pairs when called as pm(X,Y).
%
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
pm(s(N), X) :- X #> 0, X #= Y+1, pm(N, Y). % No need to test var(X) or nonvar(X)
*/

% ---
% Alternatively, basic Prolog instead of CLP(FD)
% ---

% both arguments nonvar: "verifying"

pm(PN,NN) :- nonvar(PN),nonvar(NN),PN==z,NN==0,!.
pm(PN,NN) :- nonvar(PN),nonvar(NN),PN=s(PNu),NN>0,!,NNu is NN-1,pm(PNu,NNu).

% one argument nonvar: "mapping"

pm(PN,NN) :- nonvar(PN),var(NN),PN==z,!,NN=0.
pm(PN,NN) :- nonvar(PN),var(NN),PN=s(PNu),!,pm(PNu,NNu),NN is NNu+1.

pm(PN,NN) :- var(PN),nonvar(NN),NN==0,!,PN=z.
pm(PN,NN) :- var(PN),nonvar(NN),NN>0,!,NNu is NN-1,pm(PNu,NNu),PN=s(PNu).

% both arguments var: "enumerating"/"generating"

pm(PN,NN) :- var(PN),var(NN),PN=z,NN=0.
pm(PN,NN) :- var(PN),var(NN),pm(PNu,NNu),NN is NNu+1,PN=s(PNu).

% ---
% pm_list/2 maps a whole list using maplist/3; this will be useful for a bit later
% However, we do not want it to generate, just map or verify, so we check that.
%
% Note that the ISO Instantiation Error takes no argument (sad!) but
% SWI Prolog provides one anyway. Good.
% ---

pm_list(PNs,NNs) :-
   maplist(pm_list_in_list,PNs,NNs).

pm_list_in_list(PN,NN) :-
   (var(PN),var(NN))
   -> 
   instantiation_error("At least one of PN and NN must be instantiated")
   ;
   pm(PN,NN).

% This is an extra used for manual debugging

pm_list_dbg(PNs,NNs) :-
   maplist(pm_list_in_list_dbg,PNs,NNs).

pm_list_in_list_dbg(PN,NN) :-
   (var(PN),var(NN))->(PN='?',NN='?');pm(PN,NN).

% ===
% Generator & Verifier for Peano Numbers
% ===
% This is mostly a generator ("verifying" that one has a Peano Number isn't so
% interesting and we can assume it has the right structure down to "z" if it
% starts with function symbol s/1).
%
% The *generator* is needed to generate solutions for unbound variables 
% representing Peano Numbers, as Prolog generates a stream of individuals (from 
% some domain) to make a query true. A general theorem prover would determine
% whether the query is true "as a whole" and not care about representative
% individuals.
%
% It is important to notice that if we have a instantiated X designating a 
% Peano Number, we assume it is ground as in "X == s(s(z))" and not "nonzero
% but of unspecified magnitude" as in "X = s(s(_))" -- that would demand another
% approach.
% ===

pnat(PN) :- acc_p(PN),fail. % Test passed parameter, then fail into the actual procedure
pnat(z).
pnat(s(X)) :- pnat(X).

% ===
% Is a Peano Number larger than zero? Alternatively: generate Peano Numbers
% larger than 0. Testing for nonzero is actually simpler: X=s(_)
% ===

pnonz(s(X)) :- pnat(X).

% ===
% Equality
% ===
% When you think about it, this is equality among "representations of 
% Peano Numbers in Prolog" I'm not sure equality in Peano Arithmetic is meaningful
% - for each number, there is only one number. OTOH, dis-equality in Peano Arithmetic
% IS meaningful, but nobody seems to talk about that?
%
% Here, the equality relation is basically Prolog's "=", with a special case if
% both sides are unbound variables.
%
% One could also define it as: PX = PY <=> PX + z = PY
% One could also define it recursively over the s(_) chain.
%
% But equality is not defined in that way in Peano arithemtic, so why bother.
% ===

pequal(PX,PY) :- acc_p(PX),acc_p(PY),fail.             % Test passed parameter, then fail into the actual procedure
pequal(PX,PY) :- nonvar(PX),nonvar(PY), PX=PY.         % "==" would work too, instead of unification
pequal(PX,PY) :- var(PX),nonvar(PY), PX=PY.            % unification is correct here
pequal(PX,PY) :- nonvar(PX),var(PY), PX=PY.            % unification is correct here
pequal(PX,PY) :- var(PX),var(PY), pnat(PX), PX=PY.     % generate PX and unify

% ===
% Addition.
% ===
% All the cases have been made explicit!
%
% Note that Prolog does NOT automagically "do the right thing" in all cases of
% unbound variable / bound variable situations. You have to actually think 
% about what happens and handle the various cases explicitly. Some lines could 
% be collapsed, but keeping them separate helps verifying the code.
% ===

% ---
% Check passed parameter, then fail into the actual procedure
% ---

padd(PX,PY,PZ) :- acc_p(PX),acc_p(PY),acc_p(PZ),fail.

% ---
% All arguments are instantiated: Verification PZ ?= PX + PY
% ---

padd(PX,PY,PZ) :- nonvar(PX),nonvar(PY),nonvar(PZ), PY=z     ,PX=PZ.
padd(PX,PY,PZ) :- nonvar(PX),nonvar(PY),nonvar(PZ), PY=s(PYs),PZ=s(PZs),padd(PX,PYs,PZs).

% ---
% One argument is uninstantaited: Function
% ---

% Addition PZ := PX + PY

padd(PX,PY,PZ) :- nonvar(PX),nonvar(PY),var(PZ), PY=z     ,PX=PZ.
padd(PX,PY,PZ) :- nonvar(PX),nonvar(PY),var(PZ), PY=s(PYs),PZ=s(PZs),padd(PX,PYs,PZs).

% Subtraction PX := PZ-PY

padd(PX,PY,PZ) :- var(PX),nonvar(PY),nonvar(PZ), PY=z     ,PX=PZ.
padd(PX,PY,PZ) :- var(PX),nonvar(PY),nonvar(PZ), PY=s(PYs),PZ=s(PZs),padd(PX,PYs,PZs).

% Subtraction PY := PZ-PX

% If we follow the same code as above, the, if PY is uninstantaited, we will
% wander off into infinity if the subtraction has no solution.

% We can use the fact that addition is provably commutative in Peano arithmetic.
% Then we can swap the first two arguments. But then we define Peano Addition
% based on properties of the Peano Addition. That sounds circular.
% Proceed anyway:

padd(PX,PY,PZ) :- nonvar(PX),var(PY),nonvar(PZ), padd(PY,PX,PZ).

% ---
% Two uninstantiated arguments
% ---

% Generate valid solutions!

padd(PX,PY,PZ) :- var(PX),nonvar(PY),var(PZ), generate_X_add_then_test(PX,PY,PZ).
padd(PX,PY,PZ) :- var(PX),var(PY),nonvar(PZ), generate_X_add_then_test(PX,PY,PZ).

% Guess forever over PY

padd(PX,PY,PZ) :- nonvar(PX),var(PY),var(PZ),pnat(PY),padd(PX,PY,PZ).

% ---
% Three uninstantiated arguments
% ---

% guess forever over Z (not over X and Y!)

padd(PX,PY,PZ) :- var(PX),var(PY),var(PZ),pnat(PZ),padd(PX,PY,PZ).

% ---
% Avoid runaway to infinity on backtracking after the solution has been found
% ---
% By using a convoluted control construct. I don't like this, but it needs no
% special helper predicates to test for "larger than" (that test should be
% based on add/3, not the converse). Is there anything nicer?
% Update: such constructs can be encountered in code "in the wild", so I guess
% it's okay. Still weird.

generate_X_add_then_test(PX,PY,PZ) :- pnat(PX),(padd(PX,PY,PZ) -> true ; (!,fail)).

% ===
% plesseq/2, pless/2 : X <= Y, X < Y
% ===
% We have padd/3, now we can test for "less or equal"
% Implementation-wise, one would test "less or equal" by counting the "s" depth on both side
% but we try to stay close to Peano Arithmetic:
% For all a, b ∈ N, a ≤ b if and only if there exists some c ∈ N such that a + c = b.
% ===

plesseq(PX,PY) :- acc_p(PX),acc_p(PY),fail. % check passed args, then fail into the actual procedure
plesseq(PX,PY) :- padd(PX,_,PY).

% ---

pless(PX,PY) :- acc_p(PX),acc_p(PY),fail.   % check passed args, then fail into the actual procedure

pless(PX,PY) :- nonvar(PX),nonvar(PY),padd(PX,PP,PY),PP=s(_).
pless(PX,PY) :- nonvar(PX),var(PY),padd(PX,PP,PY),PP=s(_).
pless(PX,PY) :- var(PX),nonvar(PY),padd(PX,PP,PY),PP=s(_).

% detecting & handling that special case for PX==PY is ugly

pless(PX,PY) :- var(PX),var(PY),((PX==PY)->fail;(padd(PX,PP,PY),PP=s(_))).

% ===
% Simple multiplication.
% ===
% All the cases have been made explicit!

pmult(PX,PY,PZ) :- acc_p(PX),acc_p(PY),acc_p(PZ),fail. % check passed args, then fail into the actual procedure

% ---
% All arguments are instantiated: Verification PZ =? PX*PY
% ---

pmult(PX,PY,PZ) :- nonvar(PX),nonvar(PY),nonvar(PZ), PY=z      , PZ=z.
pmult(PX,PY,PZ) :- nonvar(PX),nonvar(PY),nonvar(PZ), PY=s(PYs) , pmult(PX,PYs,PZt), padd(PX,PZt,PZ).

% ---
% One argument is uninstantiated: Function
% ---

% Multiplication PZ := PX*PY

pmult(PX,PY,PZ) :- nonvar(PX),nonvar(PY),var(PZ), PY=z      , PZ=z.
pmult(PX,PY,PZ) :- nonvar(PX),nonvar(PY),var(PZ), PY=s(PYs) , pmult(PX,PYs,PZt), padd(PX,PZt,PZ).

% Division PX := PZ/PY (fails if no solution)
% We have to reorder the padd/3 and pmult/3 in defiance of the Peano axioms
% to achieve termination. I say that's ok!

pmult(PX,PY,PZ) :- var(PX),nonvar(PY),nonvar(PZ), PY=z      , PZ=z, pnat(PX). % generate or verify
pmult(PX,PY,PZ) :- var(PX),nonvar(PY),nonvar(PZ), PY=s(PYs) , padd(PX,PZt,PZ), pmult(PX,PYs,PZt).

% Division PY := PZ/PX (fails if no solution)
% We need to diverge from the definition of multiplication given by the Peano axioms
% to achieve termination. We can use the fact that multiplication is provably commutative
% in Peano arithmetic. Then we can swap the first two arguments. But then we define Peano
% Multiplication based on properties of the Peano Multiplication. That sounds circular.
% Proceed anyway:

pmult(PX,PY,PZ) :- nonvar(PX),var(PY),nonvar(PZ), pmult(PY,PX,PZ).

% ---
% Two uninstantiated arguments
% ---

% Generate valid solutions!

pmult(PX,PY,PZ) :- var(PX),nonvar(PY),var(PZ), PY=z    , pnat(PX), pmult(PX,PY,PZ).
pmult(PX,PY,PZ) :- var(PX),nonvar(PY),var(PZ), PY=s(_) , pnat(PX), pmult(PX,PY,PZ).

% I have been searching for some easy way to generate the numbers but once
% I thought about using "once/1", it was strangely obvious.
% Still needs a special case on 0*0=0 because one does not want to generate it twice.
% 0 as the "absorber" of multiplication is a problem!

pmult(PX,PY,PZ) :- var(PX),var(PY),nonvar(PZ), PZ=z    , ((PX=z,PY=z);(pnonz(PK), (PX=PK;PY=PK), once(pmult(PX,PY,PZ)))).
pmult(PX,PY,PZ) :- var(PX),var(PY),nonvar(PZ), PZ=s(_) , generate_Y_mult_then_test(PX,PY,PZ).

% guess forever over Y

pmult(PX,PY,PZ) :- nonvar(PX),var(PY),var(PZ), pnat(PY), pmult(PX,PY,PZ).

% ---
% Three uninstantiated arguments
% ---

% Weave the guessing over the X=0 line, Y=0 line and (X,Y)=/=(0,0) surface

pmult(X,Y,Z) :- var(X),var(Y),var(Z), X=z,Y=z,pmult(X,Y,Z).
pmult(X,Y,Z) :- var(X),var(Y),var(Z),
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
% Quotient and Remainder are best computed together
% ===

% PT = PM*PQ+PR, PR < PM (and PM*PQ <= PT)

pquotrem(PT,PM,PQ,PR) :-
   nonvar(PT),nonvar(PM),
   (PM==z -> domain_error('non-z peano number',z);true),
   pmult(PM,PQ,PSS),
   ((\+ plesseq(PSS,PT)) -> (!,fail) ; true),
   padd(PSS,PR,PT),
   pless(PR,PM).
     
% ===
% UNUSED DRIFTCODE BELOW
% ===

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
