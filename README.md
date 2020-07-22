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
- [LogicBlox](https://developer.logicblox.com/technology/)
- [Datalog](https://en.wikipedia.org/wiki/Datalog)
- More on [Logical Frameworks](https://en.wikipedia.org/wiki/Logical_framework)

## Rule-based Systems

A very low-fidelity overview, but one needs general maps.

![Rule-based Systems](other_notes/fwd_and_bwd_chaining/fwd_and_bwd_chaining.svg)

This an offshoot from this question on StackOverflow: [Forward and Backward Chaining](https://stackoverflow.com/questions/62376526/forward-and-backward-chaining). One error is that CHR does not do backtracking (it is "commited choice" - the backtracking is provided by the calling Prolog system). To be corrected!

### Are we working in the correct Logic?

Why it's not Classical Logic: [About the Logic](other_notes/about_the_logic/)

### About Truth Values

It's not really Two-Valued: [About the Truth Values](other_notes/about_truth_values/)

## Rule-based Systems

Just a general map: [About Rule-based Systems](other_notes/fwd_and_bwd_chaining/)


## Depicting Terms

Being often confused by Prolog naming and concepts, my way to name & graphically represent terms (this is simpler than the one that went before)

Remember:

- Variables are clause-local.
- Terms are global.

![Term notation](other_notes/depicting_terms/prolog_term_notation.svg)

It is possible to have [Attributed Variables](https://eu.swi-prolog.org/pldoc/man?section=attvar) so that goals are scheduled when variables are bound.

## Subjects

- [About Exceptions](swipl_notes/about_exceptions) (rather complete)
- [About the Byrd Box Model](swipl_notes/about_byrd_box_model) (rather complete)
- [About SWI-Prolog data types](swipl_notes/about_swipl_data_types) (rather complete)
- [SWI-Prolog datatypes](swipl_datatypes)
- [About output formatting in SWI-Prolog](swipl_notes/about_output_formatting) (semi-complete)
- [About DCGs](swipl_notes/about_dcgs) (just getting started)
- [About Prolog DB operations](swipl_notes/about_prolog_db_operations) (?)
- [About `dif/2`](swipl_notes/about_dif) (rather complete)
- [Difference Lists](difflists/)
- Questions on `maplist/N` are recurrent (not only on Stack Overflow), so these page collecte notes & examples:
   - [maplist_2_examples.md](maplist/maplist_2_examples.md): Examples and major explainer for [`maplist/2`](https://www.swi-prolog.org/pldoc/man?predicate=maplist%2f2)
   - [maplist_3_examples.md](maplist/maplist_3_examples.md): Examples for [`maplist/3`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/3)
   - [maplist_4_examples.md](maplist/maplist_4_examples.md): Examples for [`maplist/4`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/4)
- Linear fold in Prolog: [`foldl` and `foldr`](foldl_foldr/), Explainer and code

#### Various

These are notes on specific subjects taken while working with Prolog in general and SWI-Prolog in particular.

- [Loading a library (the lynx library in this case)](swipl_notes/loading_lynx_library.md)
- [Use of the caret ^ in` bagof/3`, `setof/3`](swipl_notes/notes_on_the_caret_used_in_bagof_goals.md)
- [Predicates for printing & formatting](swipl_notes/output_formatting.md)
- [SWI-Prolog string modes](swipl_notes/swipl_string_modes.md)


## External Resources

- [The first page of the SWI-Prolog manual has a large comment with pointers to resources](https://eu.swi-prolog.org/pldoc/doc_for?object=manual) ... sometimes I add something to that comment.
- [Extensive list at klaussinani's github account](https://github.com/klaussinani/awesome-prolog#resources)
- [The **Prolog Syntax Highlighting** file for KDE](https://cgit.kde.org/syntax-highlighting.git/tree/data/syntax/prolog.xml) (by Torsten Eichstädt), as used in the [Kate editor](https://docs.kde.org/trunk5/en/applications/katepart/highlight.html)
- [Bug Hunting Toolbox Wiki Page at SWI-Prolog Discourse Site](https://swi-prolog.discourse.group/t/bug-hunting-toolbox/710)
- [Frank Pfenning's Course on Logic Programming](http://www.cs.cmu.edu/~fp/courses/lp/) ([Lecture Notes as one PDF](http://www.cs.cmu.edu/~fp/courses/lp/lectures/lp-all.pdf) ... 324 pages). Hardcore!   

### On YouTube: "The Power of Prolog" by Markus Triska

Gotta watch them all: [List of uploads](other_notes/about_power_of_prolog_on_youtube/)
  
### Papers of Interest

A small selection: [List of papers](other_notes/about_papers_of_interest/)

### Packages of Interest

- https://github.com/shonfeder/tokenize - "A modest tokenization library for SWI-Prolog, seeking a balance between simplicity and flexibility."

### Code grabbag

Some predicates which may be of general use can be found in the [Code grabbag](code/README.md)

- [`vector_nth0/3`](code/vector_nth0.pl): Retrieve in a list on multiple positions, by index.
- [`splinter0/5`](code/splinter0.pl): Break a list into three parts based on index.
- [`replace0/5`](code/splinter0.pl): Replace in a list based on index.
- [`rotate_list/3`](code/rotate_list.pl): Rotate a list left or right.
- [`vector_replace0/4`](code/vector_replace0.pl): Replace in a list on multiple positions, by index.

## A Tradition

![Hiroshige: Shinagawa Station](pics/various/Hiroshige_Shinagawa_Station.jpg)

From [The Fifty-three Stations of the Tōkaidō](https://en.wikipedia.org/wiki/The_Fifty-three_Stations_of_the_T%C5%8Dkaid%C5%8D) by Utagawa Hiroshige, 1832.

