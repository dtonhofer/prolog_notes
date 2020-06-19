# prolog_notes

Some notes taken while working on Prolog

## The Logic Programming Landscape

![The Logic Programming Landscape](other_notes/quick_map_of_lp_landscape/quick_map_of_lp_landscape.svg)

Missed in the above: 

- [αProlog](https://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/) (Currently frozen?)
- [GOLOG](https://en.wikipedia.org/wiki/GOLOG) (Ancient, based on Situation Calculus. What's the relationship to LPS?)
- [MProlog](https://www.mimuw.edu.pl/~nguyen/mpl.pdf) (PDF) for [Modal Logic](https://plato.stanford.edu/entries/logic-modal/)
- [HYPROLOG](http://akira.ruc.dk/~henning/hyprolog/) A Logic Programming Language with Abduction and Assumptions
- [Markov Logic Networks](https://en.wikipedia.org/wiki/Markov_logic_network) 
- More on [Logical Frameworks](https://en.wikipedia.org/wiki/Logical_framework)

## Rule-based Systems

A very low-fidelity overview, but one needs general maps.

![Rule-based Systems](other_notes/fwd_and_bwd_chaining/fwd_and_bwd_chaining.svg)

## Truth Values

Once you think about it, you notice that Prolog is both cheating in pretending that it is working in two-valued logic and also deficient in modeling capabilities by keeping to two-valued logic. One day this may be extended! See also: [Paraconsistent Logic](https://plato.stanford.edu/entries/logic-paraconsistent/).

![Truth Values](other_notes/extended_truth_values/extended_truth_values.svg)

## A we working in the right Logic?

From _"Logic Programming with Strong Negation"_ (David Pearce, Gerd Wagner, FU Berlin, 1991), appears in [Springer LNAI 475](https://link.springer.com/book/10.1007/BFb0038689): Extensions of Logic Programming, International Workshop Tübingen, FRG, December 8–10, 1989 Proceedings):

> According to the standard view, a logic program is a set of definite Horn clauses. Thus, logic programs are regarded as syntactically restricted first-order theories within the framework of classical logic. Correspondingly, the proof theory of logic programs is considered as the specialized version of classical resolution, known as SLD-resolution. This view, however, neglects the fact that a program clause, a_0 <— a_1, a_2, • • •, a_n, is an expression of a fragment of positive logic (a subsystem of intuitionistic logic) rather than an implicational formula of classical logic. The classical interpretation of logic programs, therefore, seems to be a semantical overkill.
>
> It should be clear that in order to explain the deduction mechanism of Prolog one does not have to refer to the indirect method of SLD-resolution which checks for the refutability of the contrary. It is certainly more natural to view Prolog's proof procedure as a kind of natural deduction, as, for example, in [Hallnäs & Schroeder-Heister 1987] and [Miller 1989]. This also is more in line with the intuitions of a Prolog programmer. Since Prolog is the paradigm, logic programming
semantics should take it as a point of departure. 

See also: [Logic programming with strong negation and inexact predicates (1991)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.975.3445&rank=1), where we read:

> Akama [S.Akama (1987): Resolution in constructivism, Logique et Analyse, 120, 385-392] proposed to use constructive logic for the interpretation of logic programming. He showed how the resolution calculus for definite Horn clauses can be interpreted within constructive logic. In fact, he makes the point that in the definite Horn clause setting the differences between strong negation, intuitionistic negation and classical negation do not matter. We remark that this observation is not surprising since the language of definite Horn clauses can be viewed as a fragment of positive logic which forms a common subpart of constructive, intuitionistic and classical logic.

## Depicting Terms

Being often confused by Prolog naming and concepts, my way to name & graphically represent terms (this is simpler than the one that went before)

![Term notation](other_notes/depicting_terms/prolog_term_notation.svg)

It is possible to have [Attributed Variables](https://eu.swi-prolog.org/pldoc/man?section=attvar) so that goals are scheduled when variables are bound.

## SWI-Prolog's datatypes

![swipl_datatype_tree](swipl_datatypes/swipl_datatype_tree/swipl_datatype_tree.svg) 

More here:

[SWI-Prolog datatypes](swipl_datatypes)

## External Resources

- [The first page of the SWI-Prolog manual has a large comment with pointers to resources](https://eu.swi-prolog.org/pldoc/doc_for?object=manual) ... sometimes I add something to that comment.
- [Extensive list at klaussinani's github account](https://github.com/klaussinani/awesome-prolog#resources)
- [The **Prolog Syntax Highlighting** file for KDE](https://cgit.kde.org/syntax-highlighting.git/tree/data/syntax/prolog.xml) (by Torsten Eichstädt), as used in the [Kate editor](https://docs.kde.org/trunk5/en/applications/katepart/highlight.html)
- [Bug Hunting Toolbox Wiki Page at SWI-Prolog Discourse Site](https://swi-prolog.discourse.group/t/bug-hunting-toolbox/710)
- [Frank Pfenning's Course on Logic Programming](http://www.cs.cmu.edu/~fp/courses/lp/) ([Lecture Notes as one PDF](http://www.cs.cmu.edu/~fp/courses/lp/lectures/lp-all.pdf) ... 324 pages). Hardcore!   

### "The Power of Prolog" my Markus Triska on YouTube

[https://www.youtube.com/channel/UCFFeNyzCEQDS4KCecugmotg](https://www.youtube.com/channel/UCFFeNyzCEQDS4KCecugmotg)

- Logical Foundations of Prolog
  - [Logic](https://www.youtube.com/watch?v=nlTZQ0FF2Eo) (6 Mar 2019)
  - [Predicate Logic](https://www.youtube.com/watch?v=PCO3lzGfU90) (24 Jul 2019)
  - [Horn Clauses](https://www.youtube.com/watch?v=hgw59_HBU2A) (21 Oct 2019)
- Prolog Applications
  - [Collatz Conjecture in Prolog](https://www.youtube.com/watch?v=XTrLbfl-Ag0) (12 Nov 2018)
  - [Knights and Knaves in Prolog](https://www.youtube.com/watch?v=oEAa2pQKqQU) (6 Jan 2019)
  - [Map Colouring in Prolog](https://www.youtube.com/watch?v=6XD7vBbywMc) (29 Jan 2019)
  - [Sudoku in Prolog](https://www.youtube.com/watch?v=5KUdEZTu06o) (16 Feb 2019)    
  - [N-Queens in Prolog](https://www.youtube.com/watch?v=l_tbL9RjFdo) (14 Mar 2019)
  - [Bitcoinolog: Reason about Bitcoin addresses with Prolog](https://www.youtube.com/watch?v=HDJ8E8g2zeg) (25 March 2019)
  - [Term Rewriting with Prolog](https://www.youtube.com/watch?v=b2Px7cu2a68) (23 Aug 2019) 
  - [School Timetabling with Prolog](https://www.youtube.com/watch?v=uKvS62avplE) (2 Jan 2020)
- Prolog Style and Technique
  - [Naming Prolog Predicates](https://www.youtube.com/watch?v=Uska9DgJEoo) (23 Sep 2019)
  - [Reading Prolog Code](https://www.youtube.com/watch?v=fQUVWo209WA) (31 Jan 2020)
  - [Clean vs. Defaulty Representations in Prolog](https://www.youtube.com/watch?v=KJ8uZiWpomg) (10 May 2020)
  - [Argument Indexing in Prolog](https://www.youtube.com/watch?v=FZLofckPu4A) (11 Dec 2019)
  - [Representing Strings in Prolog](https://www.youtube.com/watch?v=plvBFNi0cVw) (10 Feb 2020)
  - [Sparrows on Eagles: Delegate your work to Prolog!](https://www.youtube.com/watch?v=vdabv9EkYrY) (22 Mar 2020)
- Prolog Language: Synatx and Semantics
  - [Prolog Terms](https://www.youtube.com/watch?v=TUjQqvCTwjQ) (9 Jul 2019)
  - [Prolog Lists](https://www.youtube.com/watch?v=9JzwUnMSCdA) (16 Jul 2019)
  - [Prolog Operators](https://www.youtube.com/watch?v=DepPPfDVSpw) (20 Dec 2018)
  - [Clauses, Rules and Facts](https://www.youtube.com/watch?v=x_APqarir-k) (18 Jan 2019)
  - [Prolog Predicates](https://www.youtube.com/watch?v=GlWI9PBZD2Y) (25 Jan 2019)
  - [Prolog Queries and Answers](https://www.youtube.com/watch?v=UmGih8xOrJ4) (20 Jun 2019) 
  - [Prolog Integer Arithmetic](https://www.youtube.com/watch?v=sHo6-hk21L8) (16 Jul 2019)
  - [Meta-Predicates in Prolog](https://www.youtube.com/watch?v=-nlI33r-P70) (11 Feb 2019)
  - [Prolog Conformity Testing](https://www.youtube.com/watch?v=Ko_IrwN9yAM) (31 Mar 2019)
  - [Type tests in Prolog](https://www.youtube.com/watch?v=ZIv0G4b1xBQ) (3 Apr 2019)
  - [List differences in Prolog](https://www.youtube.com/watch?v=6egAF4-HVzw) (12 May 2019)
- Prolog Development Environment
  - [Prolog development with GNU Emacs](https://www.youtube.com/watch?v=WdWOKbTX-i4) (24 Mar 2014)
  - [ediprolog: Emacs Does Interactive Prolog](https://www.youtube.com/watch?v=jMg8sY2R930) (15 Jul 2019)
  - [Debugging Prolog Code](https://www.youtube.com/watch?v=4IWruicMd4c) (22 Jul 2019)
  
### Papers of Interest

- 1984 [Making Prolog more expressive](https://core.ac.uk/download/pdf/82550631.pdf) (J.W. Lloyd, R.W. Topor) (Journal of Logic Programming, 1984:3:225-240)
- 1989 Logic Programming with Strong Negation (Appears in Springer LNAI 475, pp 311-326) (David Pearce, Gerd Wagner)
- 1991-01 [Applying Techniques to Skeletons](https://www.researchgate.net/publication/220986744_Applying_Techniques_to_Skeletons) (Leon Sterling, Marc Kirschenbaum)
- 1991 [Logic Programming with Strong Negation and Inexact Predicates](https://academic.oup.com/logcom/article-abstract/1/6/835/943774) (Gerd Wagner) (Journal of Logic and Computation, Volume 1, Issue 6, December 1991, Pages 835–859)
- 1995-XX [Higher-order logic programming in Prolog](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.9690) (Lee Naish)
- 1991-02 [A type system for logic programs](https://www.sciencedirect.com/science/article/pii/074310669180002U) (Eyal Yardeni, Ehud Shapiro. In: _The Journal of Logic Programming_ Volume 10, Issue 2)
- 1991-XX [Uniform Proofs as a Foundation for Logic Programming](http://www.lix.polytechnique.fr/~dale/papers/apal91.pdf) (Dale Miller, Gopalan Nadathur, Frank Pfenning, Andre Scedrov) --- the Lambda-Prolog way
- 2001-04 [Logic Programming, Functional Programming, and Inductive Definitions](https://www.researchgate.net/publication/1880293_Logic_Programming_Functional_Programming_and_Inductive_Definitions) (Lawrence Paulson, Andrew W. Smith)
- 2002-07 [Abduction in Logic Programming](http://web.stanford.edu/class/cs227/Readings/Abudction%20in%20LP.pdf) (Marc Denecker, Antonis Kakas). Appears in [Computational Logic: Logic Programming and Beyond, Springer LNCS 2407](https://link.springer.com/chapter/10.1007/3-540-45628-7_16), pp 402-436
- 2003-10 [The Essence of Strategic Programming](https://www.researchgate.net/publication/277289331_The_Essence_of_Strategic_Programming) (Ralf Lämmel, Eelco Visser, Joost Visser) --- generic programming with the use of strategies
- 2009-11 [Coding Guidelines for Prolog](https://arxiv.org/abs/0911.2899) (Michael A. Covington, Roberto Bagnara, Richard A. O'Keefe, Jan Wielemaker, Simon Price)
- 2015-10 [The Inductive Constraint Programming Loop](https://arxiv.org/abs/1510.03317) (
Christian Bessiere, Luc De Raedt, Tias Guns, Lars Kotthoff, Mirco Nanni, Siegfried Nijssen, Barry O'Sullivan, Anastasia Paparrizou, Dino Pedreschi, Helmut Simonis). Appears in modified form in _IEEE Intelligent Systems_, September/October 2017.
- 2016-07 [Indexing `dif/2`](https://arxiv.org/abs/1607.01590v1) (Ulrich Neumerkel, Stefan Kral)
- 2017-09 [plspec – A Specification Language for Prolog Data](https://www.krin.gs/publication/koerner-plspec-declare17/koerner-plspec-declare17.pdf) (Philipp Körner, Sebastian Krings, _DECLARE 2017_)
- 2019-09 [Prolog Coding Guidelines: Status and Tool Support](https://arxiv.org/abs/1909.08230v1) (Falco Nogatz, Philipp Körner, Seastuan Krings, ICLP 2019
- 2020-01 [Drawing Prolog Search Trees. A Manual for Teachers and Students of Logic Programming](https://arxiv.org/abs/2001.08133v1) (Johan Bos)

### Packages of Interest

- https://github.com/shonfeder/tokenize - "A modest tokenization library for SWI-Prolog, seeking a balance between simplicity and flexibility."

## Pages in this repository

### Code grabbag

Some predicates which may be of general use can be found in the [Code grabbag](code/README.md)

- [`vector_nth0/3`](code/vector_nth0.pl): Retrieve in a list on multiple positions, by index.
- [`splinter0/5`](code/splinter0.pl): Break a list into three parts based on index.
- [`replace0/5`](code/splinter0.pl): Replace in a list based on index.
- [`rotate_list/3`](code/rotate_list.pl): Rotate a list left or right.
- [`vector_replace0/4`](code/vector_replace0.pl): Replace in a list on multiple positions, by index.

### About SWI Prolog

#### Difference List Explainer

An surprisingly large explainer on difference lists (or list differences). With illustrations. Contains code to
compute the length of a difference list.

- [Difference Lists](difflists/)
   
#### Notes on `maplist/N`

Questions on `maplist/N` are recurrent (not only on Stack Overflow), so these page collecte notes & examples:

- [maplist_2_examples.md](maplist/maplist_2_examples.md): Examples and major explainer for [`maplist/2`](https://www.swi-prolog.org/pldoc/man?predicate=maplist%2f2)
- [maplist_3_examples.md](maplist/maplist_3_examples.md): Examples for [`maplist/3`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/3)
- [maplist_4_examples.md](maplist/maplist_4_examples.md): Examples for [`maplist/4`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/4)

#### Linear `foldl` and `foldr` in Prolog

- [`foldl` and `foldr`](foldl_foldr/), Explainer and code

#### Various

These are notes on specific subjects taken while working with Prolog in general and SWI-Prolog in particular.

- [Loading a library (the lynx library in this case)](swipl_notes/loading_lynx_library.md)
- [Use of the caret ^ in` bagof/3`, `setof/3`](swipl_notes/notes_on_the_caret_used_in_bagof_goals.md)
- [Predicates for printing & formatting](swipl_notes/output_formatting.md)
- [SWI-Prolog string modes](swipl_notes/swipl_string_modes.md)

## A Tradition

![Hiroshige: Shinagawa Station](pics/various/Hiroshige_Shinagawa_Station.jpg)

From [The Fifty-three Stations of the Tōkaidō](https://en.wikipedia.org/wiki/The_Fifty-three_Stations_of_the_T%C5%8Dkaid%C5%8D) by Utagawa Hiroshige, 1832.

