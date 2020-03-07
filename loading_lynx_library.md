# Loading the "lynx" library

Suppose you want to load the [lynx library](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lynx/index.html).

In directory `$SWIPROLOG/lib/swipl/library/lynx`, we find:

- File `format.pl` exporting predicate `text_format:format_paragraph/2`.
- File `html_style.pl` exporting predicates `format_style:element_css/3`,
`format_style:css_block_options/5`,
`format_style:css_inline_options/3`,
`format_style:attrs_classes/2`,
`format_style:style_css_attrs/2`.
- File `html_text.pl` exporting predicates `html_text:html_text/1`, `html_text::html_text/2`.
- File `pldoc_style.pl` with module `pldoc_style`, exporting nothing but extending `html_text:style`.

The `lynx` subdirectory of the SWI Prolog library directory is probably not in your library search path.

The files won't be found automatically, even though the library directory itself is on the search path:

````
?- bagof(X,file_search_path(library,X),L).
L = ['/usr/local/logic/swipl/lib/swipl/library',
     '/usr/local/logic/swipl/lib/swipl/library/clp',
     '/home/paquette/.local/share/swi-prolog/pack/sldnfdraw/prolog',
     pce('prolog/lib')].
````

If we take a look at the file `/usr/local/logic/swipl/lib/swipl/library/lynx/format.pl`, we see
the declaration of the module and the exports:

````
:- module(text_format,
          [ format_paragraph/2          % +Text, +Options
          ]).
````

Try to use `format_paragraph/2`

````
?- text_format:format_paragraph("hello, world",[width(50),text_align(center)]).
ERROR: Unknown procedure: text_format:format_paragraph/2 (DWIM could not correct goal)
````

So let's make accesible the predicates found in **file** `format.pl`. (**not** predicates found in module `format`).

(More precisely, "those predicates exported from some module _foo_, a part of which is defined in file `format.pl`.
This _is_ confusing.)

You can load the predicate like this:

````
use_module(library(lynx/format)).
````

or extend the file search path:

````
assertz(file_search_path(library,'/usr/local/logic/swipl/lib/swipl/library/lynx')).
use_module(library(format)).
````

or use full paths, as in:

````
use_module('/usr/local/logic/swipl/lib/swipl/library/lynx/format.pl').
````

After that:

````
?- format_paragraph("hello, world",[width(50),text_align(center)]).
                   hello, world
````

or with qualification:

````
?- text_format:format_paragraph("hello, world",[width(50),text_align(center)]).
                   hello, world
````

Perfect.

There is also a very nice predicate to examine predicate metadata,
[predicate_property/2](https://www.swi-prolog.org/pldoc/man?predicate=predicate_property%2f2):

But does it really work?

````
?- predicate_property(format_paragraph/2,imported_from(M)).
M = yall.
````

````
?- predicate_property(format_paragraph/2,file(M)).
M = '/usr/local/logic/swipl/lib/swipl/library/yall.pl'.
````

Something seems wrong.
