# Prolog Notes

Some notes taken while working on Prolog (most SWI-Prolog)

## A Tradition

![Hiroshige: Shinagawa Station](pics/various/Hiroshige_Shinagawa_Station.jpg)

_From [The Fifty-three Stations of the Tōkaidō](https://en.wikipedia.org/wiki/The_Fifty-three_Stations_of_the_T%C5%8Dkaid%C5%8D) by Utagawa Hiroshige, 1832._

## The Logic Programming Landscape

![The Logic Programming Landscape](other_notes/quick_map_of_lp_landscape/quick_map_of_lp_landscape.svg)

So many *modeling languages* which are also *programming languages*, all a bit different.

Missed in the above: 

- [αProlog](https://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/) (Currently frozen?)
- [GOLOG](https://en.wikipedia.org/wiki/GOLOG) (Ancient, based on Situation Calculus. What's the relationship to LPS?)
- [MProlog](https://www.mimuw.edu.pl/~nguyen/mpl.pdf) (PDF) for [Modal Logic](https://plato.stanford.edu/entries/logic-modal/)
- NProlog: An extension of prolog with hypothetical implication. 
   - [part 1](https://www.sciencedirect.com/science/article/pii/0743106684900293)
   - [part 2](https://www.sciencedirect.com/science/article/pii/S0743106685800030)
- [HYPROLOG](http://akira.ruc.dk/~henning/hyprolog/) A Logic Programming Language with Abduction and Assumptions
- [LogicBlox](https://developer.logicblox.com/technology/)
- [Datalog](https://en.wikipedia.org/wiki/Datalog)
- [Logical Frameworks](https://en.wikipedia.org/wiki/Logical_framework)
- [Markov Logic Networks](https://en.wikipedia.org/wiki/Markov_logic_network) -- This seems to be hot right now
- Lambda-Prolog: [A pragmatic reconstruction of Lambda-Prolog](https://hal-enac.archives-ouvertes.fr/hal-00934033)
- There should be something here on implementation of more general theorem provers using the very restricted top-down 
  theorem prover (or rather, "enumerating consequence checker") of Prolog. And how do you direct search in a theorem prover anyway?
  The NP desert is not for the faint of heart.

## Journals

Lest I forget:

- [Theory and Practice of Logic Programming ](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming) (Cambridge UP)
- [The Journal of Logic Programming](https://www.sciencedirect.com/journal/the-journal-of-logic-programming) (Elsevier)

## Notes

- Example code for some standard problems
   - [Prolog implementations for Fibonacci Number computation](other_notes/about_fibonacci_numbers) (code is tilted towards SWI Prolog though)
   - [A discussion of a "Prolog Database" operation](swipl_notes/about_prolog_db_operations/simple_db_op.md) 
   - [Various code snippets](swipl_notes/various_code/) (testing predicates found in the SWI-Prolog manual)
- Having to do with programming constructs, some of which are specific to Prolog or to SWI-Prolog
   - [`foldl` and `foldr` in Prolog](other_notes/about_foldl_and_foldr/)   
   - [Attributed Variables](swipl_notes/about_attributed_variables/) (an addendum to the SWI-Prolog manual page with new code)
   - [Delimited continuations](swipl_notes/about_continuations/) (to be continued!)
   - [CHR: Constraint Handling Rules](swipl_notes/about_chr/) (there is nothing here yet)
   - [DCGs: Definite Clause Grammars](swipl_notes/about_dcgs/) (just some work in progress)
   - [Explaining `dif/2`](swipl_notes/about_dif) (but it's actually very simple in the end)
   - [Exceptions](swipl_notes/about_exceptions) (exceptions are useful, but the ISO standard exception are open for improvement)
      - [Catch with backtrace](swipl_notes/about_exceptions/catch_with_backtrace.md)   
      - [Throwing ISO standard exceptions](swipl_notes/about_exceptions/throwing_iso_standard_exceptions.md)
      - [Problems with the ISO standard exception terms](swipl_notes/about_exceptions/problems_with_the_iso_standard_exception_terms.md)
      - [Throwing in style: collecting exception specs in one place of the code](swipl_notes/about_exceptions/throwing_in_style.md)
   - [The `maplist/N` predicates](swipl_notes/about_maplist) 
   - [Predicates dealing with I/O and printing](swipl_notes/about_output_formatting) (it all started with this...)
   - [Predicates for analyzing/constructing terms](swipl_notes/about_term_analysis_and_construction)
   - [The use of the caret `^` in` bagof/3`, `setof/3`](swipl_notes/various/notes_on_the_caret_used_in_bagof_goals.md)
   - [SWI-Prolog string modes](swipl_notes/various/swipl_string_modes.md)  
   - [How to load a library (the lynx library in this case)](swipl_notes/various/loading_lynx_library.md)   
- Having to do with generally explaining Prolog
   - ["Byrd Box Model"](other_notes/about_byrd_box_model/)
   - [Depicting Terms](other_notes/about_depicting_terms/) (needs review)
   - [Prolog list processing idioms](other_notes/about_list_processing/)
   - [The concept of a "Prolog variable"](swipl_notes/about_concept_of_variable/) (needs review once more)
   - [Difference Lists](swipl_notes/about_difference_lists/) (another one that is actually very simple in the end)
   - [Prolog data types](swipl_notes/about_swipl_data_types) (somewhat specific to SWI Prolog)
      - Code to tag a Prolog term according to the data type decision tree: [`tagging.pl`](swipl_notes/about_swipl_data_types/code/tagging.pl)
- Having to do with [JPL](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/jpl.html%27)), the Java-Prolog Bridge
   - [Some test code](swipl_notes/about_jpl/)
- Having to do with general questions
   - [Some notes on the Logic](other_notes/about_the_logic/) (Prolog is said to be based on classical logic, but is this really true?)
   - [Some notes on the Truth Values](other_notes/about_truth_values) (Prolog is said to be based on two-valued logic, but is this really true, or advised?)
   - [Map of the Logic Programming landscape](other_notes/quick_map_of_lp_landscape) (actually quite detailed; shown above)
   - [Map of the Rule-based Systems landscape](other_notes/about_rule_based_systems) (first try)
- Freestyle Notes, undigested
   - [About Predicate Structure](swipl_notes/about_predicate_structure/)
   - [Salvaging a term out of a dropped search branch](swipl_notes/about_salvaging_a_term_out_of_a_dropped_search_branch/)
   - [Better type tests](swipl_notes/about_type_tests)

## Code grabbag

Some predicates which may be of general use can be found in the [Code grabbag](code/)

## External Resources

- [The first page of the SWI-Prolog manual has a large comment with pointers to resources](https://eu.swi-prolog.org/pldoc/doc_for?object=manual) ... sometimes I add something to that comment.
- [Extensive list at klaussinani's github account](https://github.com/klaussinani/awesome-prolog#resources)
- [The **Prolog Syntax Highlighting** file for KDE](https://cgit.kde.org/syntax-highlighting.git/tree/data/syntax/prolog.xml) (by Torsten Eichstädt), as used in the [Kate editor](https://docs.kde.org/trunk5/en/applications/katepart/highlight.html)
- [Bug Hunting Toolbox Wiki Page at SWI-Prolog Discourse Site](https://swi-prolog.discourse.group/t/bug-hunting-toolbox/710)
- [Frank Pfenning's Course on Logic Programming](http://www.cs.cmu.edu/~fp/courses/lp/) ([Lecture Notes as one PDF](http://www.cs.cmu.edu/~fp/courses/lp/lectures/lp-all.pdf) ... 324 pages). Hardcore!   

### On YouTube: "The Power of Prolog" by Markus Triska

[List of Markus Triska's "Power of Prolog" YouTube videos](other_notes/about_power_of_prolog_on_youtube)

### Papers of Interest

[Papers of interest](other_notes/about_papers_of_interest) (a small selection)

### Packages of Interest

- https://github.com/shonfeder/tokenize - "A modest tokenization library for SWI-Prolog, seeking a balance between simplicity and flexibility."

