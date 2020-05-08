# prolog_notes
Some notes taken while working on Prolog

## The Logic Programming Landscape

![The Logic Programming Landscape](pics/quick_map_of_lp_landscape/quick_map_of_lp_landscape.png)

## A Tradition

![Hiroshige: Shinagawa Station](pics/various/Hiroshige_Shinagawa_Station.jpg)

From [The Fifty-three Stations of the Tōkaidō](https://en.wikipedia.org/wiki/The_Fifty-three_Stations_of_the_T%C5%8Dkaid%C5%8D) by Utagawa Hiroshige, 1832.

## External Resources

[Extensive list at klaussinani's github account](https://github.com/klaussinani/awesome-prolog#resources)

## Pages in this repository

### Code grabbag

Some predicates which may be of general use can be found in the [Code grabbag](blob/master/code/README.md)

- [`vector_nth0/3`](code/vector_nth0.pl): Retrieve in a list on multiple positions, by index.
- [`splinter0/5`](code/splinter0.pl): Break a list into three parts based on index.
- [`replace0/5`](code/splinter0.pl): Replace in a list based on index.
- [`rotate_list/3`](code/rotate_list.pl): Rotate a list left or right.
- [`vector_replace0/4`](code/vector_replace0.pl): Replace in a list on multiple positions, by index.

### Notes on SWI-Prolog

- [Loading a library](swipl_notes/loading_lynx_library.md)
- [Use of the caret ^ in` bagof/3`, `setof/3`](swipl_notes/notes_on_the_caret_used_in_bagof_goals.md)
- [Predicates for printing & formatting](master/swipl_notes/output_formatting.md)
- [SWI-Prolog string modes](swipl_notes/swipl_string_modes.md)

### On `maplist/N`

- [maplist_2_examples.md](maplist/maplist_2_examples.md): Examples and major explainer for [`maplist/2`](https://www.swi-prolog.org/pldoc/man?predicate=maplist%2f2)
   - Link to page aliased to https://bit.ly/2S9EWJe_prolog
- [maplist_3_examples.md](maplist/maplist_3_examples.md): Examples for [`maplist/3`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/3)
   -  Link to page aliased to https://bit.ly/2KCDpao_prolog
- [maplist_4_examples.md](maplist/maplist_4_examples.md): Examples for [`maplist/4`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/4)
   -  Link to page aliased to https://bit.ly/2x8Ml4f_prolog


