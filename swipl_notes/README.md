# Notes regarding various elements of Prolog / SWI-Prolog

Based on the SWI-Prolog manual

- [`between/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=between/3)
   - [Notes about `between/3`](about_between/), includes code for `between_with_step/4` and `between_x/3` 
   - [Website text](about_between/webmanualtxt/between.txt)

- [`findall/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=findall/3)
   - [Notes about `findall/3`](about_findall/), also about `bagof/3`.

- [`length/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=length/2)
   - [Notes about `length/2`](about_length/), includes code for `probe_length/3` and (lenient) `length/3`
   - [Website text](about_length/webmanualtxt/length.txt)

- [`maplist/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/2), [`maplist/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/3), [`maplist/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/4)
   - [Notes about `maplist/2`](about_maplist/maplist_2_examples.md) Working off a single list. This includes   
     an explainer regarding the use of [`library(yall)`](https://eu.swi-prolog.org/pldoc/man?section=yall) 
     lambda expressions and Prolog-style closures.
   - [Notes about `maplist/3`](about_maplist/maplist_3_examples.md) Relating the elements of two lists.
   - [Notes about `maplist/4`](about_maplist/maplist_4_examples.md) Relating the elements of three list
   
- [Verify type of a term](https://eu.swi-prolog.org/pldoc/man?section=typetest) and [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2)
   - [Notes](about_swipl_data_types/), includes image of the decision tree
   - [Website text](about_swipl_data_types/webmanualtxt/type_tree_in_ascii.txt)
   - [Better type tests](about_type_tests/)
   
