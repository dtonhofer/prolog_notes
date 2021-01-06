% =============================================================================
% An Interpreter for Vivid Logic in Prolog
% =============================================================================
% From:
%
% Appendix A of 
% "Vivid Logic: Knowledge-Based Reasoning with Two Kinds of Negation"
% by Gerd Wagner
% Lecture Notes in Artificial Intelligence 764
% Springer Verlag Berlin Heidelberg 1994
%
% =============================================================================
% Adapted to SWI-Prolog 8+, rewritten, assertions added.
%
% Instead of using one-character operators (confusing, hard to read),
% operators are multi-character atoms. 
%
% The operators are defined and exported in the module declaration.
% 
% Vivid Logic (VL) formulas ar written using these operators:
%
% vlnot   Vivid Logic strong negation ("vlnot(Fo): F is definitely false")
% vlnaf   Vivid Logic weak negation ("vlnaf(Fo): there is no proof of F; there is no evidence for F; don't belive Fo")
% vland   Vivid Logic conjunction
% vlor    Vivid Logic disjunction
% vlif    Vivid Logic "conditional fact separator"; the equivalent of ":-"
%
% Prolog itself is all (and *only*) about labeling formulas TRUE, aka "proving
% in Prolog style". The way to think here is that we take a Vivid Logic
% formula and try to "label it Prolog-TRUE" (which is the case if "prove(Fo)"
% succeeds). A formula is split into subformulas which give rise to 
% individual "label subformula Prolog-TRUE" tasks.
%
% It's interesting that "prove(Fo)" is all about "labeling Fo Prolog-TRUE"
% or failing it, in spite of there being "weak" and "strong" negation. on the
% level of VL. One could also imagine a reification of the truth values,
% where the prover always succeeds and sets the truth value: strongly TRUE,
% weakly FALSE, strongly FALSE (and possibly weakly TRUE?)-
%
% Here we "map to Prolog tasks" depending on the VL formula structure. Another
% approach would be to rewrite the VL formula, by pulling not inwards for
% example.
%     
% TODO: 
%
% 1) Not sure this is correct
% 2) Can this correctly handle (a fragment of) predicate logic in addition to
%    propositional logic?
% 3) This needs lots of diagnostics and a collector of the proof tree.
% =============================================================================
% Rewrite #1 Wed 6 Jan 2021
% =============================================================================

:- module(vividlogic, 
   [
       op(100, fy  , 'vlnot' )   % highest precedence at 100, stronger than arithmetic!
      ,op(100, fy  , 'vlnaf' )   % the corresponding '\+' is 900
      ,op(110, xfy , 'vland' )   % the corresponding ',' is at 1000
      ,op(120, xfy , 'vlor'  )   % the corresponding ';' is at 1100
      ,op(130, xfy , 'vlif'  )   % the corresponding ':-' is at 1200
      ,prove/1                   % prove a Vivid Logic formula
   ]).

% -----------------------------------------------------------------------------
% "true" can directly be TRUE-labeled
% -----------------------------------------------------------------------------
% > What about false? Is it missing? 
% Maybe there is no "false"? Or Maybe there are two? Investigate!
% > What's the interaction with the CWA data used at the end of this program?

prove(true).               % (1)
prove(false) :- !, fail.   % This seems to be missing in the original code (?) 

% -----------------------------------------------------------------------------
% TRUE-labeling of vlor/vland formulas is broken up into two Prolog tasks of
% subformula TRUE-labeling.
% -----------------------------------------------------------------------------
% Extension: Iterative deepending of vlor and vland of lists of subformulas
% kept in a list!

prove(FoA vlor FoB)  :- !,(prove(FoA);prove(FoB)).  % (2)
prove(FoA vland FoB) :- !,(prove(FoA),prove(FoB)).  % (3)
  
% -----------------------------------------------------------------------------
% TRUE-labeling of vlnot°vlor,vlnot°vland formulas is broken up into two Prolog
% tasks of subformula TRUE-labeling. An application of de Morgan laws. 
% -----------------------------------------------------------------------------

prove(vlnot(FoA vlor FoB))  :- !,(prove(vlnot(FoA)),prove(vlnot(FoB))). % (4)
prove(vlnot(FoA vland FoB)) :- !,(prove(vlnot(FoA));prove(vlnot(FoB))). % (5)

% -----------------------------------------------------------------------------
% TRUE-labeling of vlnaf°vlor,vlnaf°vland formulas is broken up into two Prolog
% tasks of subformula TRUE-labeling. An application of de Morgan laws (for 
% weak negation).
% -----------------------------------------------------------------------------

prove(vlnaf(FoA vlor FoB))  :- !,(prove(vlnaf(FoA)),prove(vlnaf(FoB))). % (6)
prove(vlnaf(FoA vland FoB)) :- !,(prove(vlnaf(FoA));prove(vlnaf(FoB))). % (7)

% -----------------------------------------------------------------------------
% TRUE-labeling of vlnaf°vlnot°vlor,vlnaf°vlnot°vland formulas is broken up 
% into two Prolog tasks of vlnaf°vlnot subformula TRUE-labeling. 
% Note that the vlnaf°vlnot is *kept* for the subformulas and the formula's
% vlor is transformed into a Prolog ";" and the formula's vland is transformed
% into a Prolog ",". This is NOT de Morgan!
% -----------------------------------------------------------------------------

prove(vlnaf(vlnot(FoA vlor FoB)))  :-     % (8)
   !,
   (prove(vlnaf(vlnot(FoA)));
    prove(vlnaf(vlnot(FoB)))).
   
prove(vlnaf(vlnot(FoA vland FoB))) :-     % (9)
   !,
   (prove(vlnaf(vlnot(FoA))),
    prove(vlnaf(vlnot(FoB)))). 

% -----------------------------------------------------------------------------
% Strong°Strong negation elimination.
% "vlnot vlnot Fo" can be TRUE-labeled if "Fo" can be TRUE-labeled.
% -----------------------------------------------------------------------------

prove(vlnot vlnot Fo) :- !,prove(Fo). % (10)

% -----------------------------------------------------------------------------
% Weak°Weak negation elimination.
% "vlnaf vlnaf Fo" can be TRUE-labeled if "Fo" can be TRUE-labeled.
% -----------------------------------------------------------------------------

prove(vlnaf vlnaf Fo) :- !,prove(Fo). % (11)

% -----------------------------------------------------------------------------
% Strong°Weak negation elimination.
% "vlnot vlnaf Fo" can be TRUE-labeled if "Fo" can be TRUE-labeled.
% -----------------------------------------------------------------------------

prove(vlnot vlnaf Fo) :- !,prove(Fo). % (12)

% -----------------------------------------------------------------------------
% Triple negation elimination, part 1.
% "vlnaf vlnot vlnaf Fo" can be TRUE-labeled if "vlnaf Fo" can be TRUE-labeled.
% -----------------------------------------------------------------------------

prove(vlnaf vlnot vlnaf Fo) :- !,prove(vlnaf Fo). % (13)

% -----------------------------------------------------------------------------
% Triple negation elimination, part 2.
% "vlnot vlnot vlnaf Fo" can be TRUE-labeled if "vlnaf Fo" can be TRUE-labeled.
% This is actually subsumed by clause (10) ... so commented out!
% -----------------------------------------------------------------------------

% prove(vlnot vlnot vlnaf Fo) :- !,prove(vlnaf Fo). % (14)

% -----------------------------------------------------------------------------
% vlnaf concretion: Just pass it to Prolog!
% "vlnaf Fo" can be TRUE-labeled the TRUE-labeling of Fo by Prolog (finitely)
% fails.
% According to the code, Fo must be a positive literal (i.e. a logic atom, or
% maybe a piece of Prolog code)
% -----------------------------------------------------------------------------

prove(vlnaf Fo) :- 
   !,
   assertion(Fo \= vlnaf(_)),
   assertion(Fo \= vlnot(_)),
   assertion(Fo \= vlor(_)),
   assertion(Fo \= vland(_)),   
   \+ prove(Fo). % (15)

% -----------------------------------------------------------------------------
% The very bottom: Prove a positive or negative literal, which is either an 
% atom (in the logic sense) or a "vlnot(atom)".
%
% Here the Closed-World-Assumption is applied:
%
%   cwa(PT,NT)
%
% Where:
%
% - PT is the set of functor names about which positive things can be said
% - NT is the set of functor names about which negative things can be said
%
% The intersection of PT and NT is empty (is that so?)
% -----------------------------------------------------------------------------

% The Prolog database contains vlif/2 rules which are the "Head :- Body" of this logic
 
:- dynamic vlif/2. 

% ---
% Negative side: TRUE-labeling of vlnot(Atom) succeeds if...
% ---

prove(vlnot Atom) :-             
   !,
   assertion(Atom \= vlnot(_)),
   assertion(Atom \= vlnaf(_)),
   assertion(Atom \= vlor(_)),
   assertion(Atom \= vland(_)),
   (
      prove_via_vlif_rules(Atom)
      ;                          
      (member_of_pos_cwa(Atom),  % give it to the "positive" side if you can and fail the TRUE-labeling if that succeeds (as that means Atom is true)
       \+ prove(Atom))).         

% ---       
% Positive side: TRUE-labeling of Atom succeeds if...
% ---

prove(Atom) :-
   Atom \= vlnot(_),
   !,
   assertion(Atom \= vlnot(_)),
   assertion(Atom \= vlnaf(_)),
   assertion(Atom \= vlor(_)),
   assertion(Atom \= vland(_)),   
   (
      prove_via_vlif_rules(Atom)
      ;                          
      (member_of_neg_cwa(Atom),  % give it to the "negative" side if you can and fail the TRUE-labeling if that succeeds (as that means Atom is strongly false)
       \+ prove(vlnot Atom))).

% ---
% There a fact with vlnot(Atom) as head (includes unification of the head, but not cutting). 
% In particular: 
% "vlnot(Atom) vlif true" but not 
% "vlnot(Atom) vlif false"
% There must be at least one vlif/2 clause for this to work, or vlif/2 must have been declared dynamic!
% ---

prove_via_vlif_rules(Atom) :-
   vlnot(Atom) vlif Fo,   % call predicate vlif/2, thus matching vlnot(Atom), and retrieving Fo!
   prove(Fo).                    

       
% ===
% Atom's predicate is a member of the list of positively stated terms (there are Prolog facts / VL rules "Atom <- Foo").
% ===

member_of_pos_cwa(Atom) :-
   Atom =.. [Pred|_],            % Should maybe use the full functor?
   cwa(PT,_),                    % Retrieve! But why have a structure instead of Prolog facts? Indexing would be direct, no member/2 needed
   member(Pred,PT).              % Should maybe use memberchk instead?

% ===
% Atom's predicate is a member of the list of negatively stated terms
% ===

member_of_neg_cwa(Atom) :-
   Atom =.. [Pred|_],            % Should maybe use the full functor?
   cwa(_,NT),                    % Retrieve! But why have a structure instead of Prolog facts? Indexing would be direct, no member/2 needed
   member(Pred,NT).              % Should maybe use memberchk instead?

% ===   
% Intensional Database (Rules)
% ===

% This is a rule on Vivid Logic level, but a fact on Prolog level:
% Atom vlif Fo.
% Alternatively, one can write them as
% Head :- Body
% But not run them - instead treat them as data, retrievable with clause/N

% ===   
% Extensional Database (Facts)
% ===

% ...

% ===   
% CWA data
% ===

cwa([],[]).

:- begin_tests(vivid_logic).

test("'vlnot true' won't be labeled TRUE",fail) :- prove(vlnot true).
test("'vlnaf true' won't be labeled TRUE",fail) :- prove(vlnaf true).

:- end_tests(vivid_logic).



