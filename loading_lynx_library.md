# Loading the "lynx" library

Suppose you want to load the [lynx library](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lynx/index.html).

Support `$SWIPROLOG` is the filesystem tree location where SWI Prolog has been installed (in my case, `/usr/local/logic/swipl`)

Then the relevant slice of the filesystem tree is as follows:

````
$SWIPROLOG
├── lib
│   └── swipl
│       ├── library
│       │   ├── lynx
│       │   │   ├── format.pl
│       │   │   ├── html_style.pl
│       │   │   ├── html_text.pl
│       │   │   └── pldoc_style.pl
````

In directory `$SWIPROLOG/lib/swipl/library/lynx`, we find:

- File `format.pl`
   - part of module `text_format`
   - exporting predicates
      - `format_paragraph/2`
      - `trim_line/2`
- File `html_style.pl`
   - part of module `format_style`
   - exporting predicates
      - `element_css/3`
      - `css_block_options/5`
      - `css_inline_options/3`
      - `attrs_classes/2`
      - `style_css_attrs/2`
- File `html_text.pl`
   - part of module `html_text`
   - exporting predicates
      - `html_text/1`
      - `html_text/2`
- File `pldoc_style.pl`
   - part of module `pldoc_style`
   - exporting nothing but extending `html_text:style/3` via the [`multifile`](https://eu.swi-prolog.org/pldoc/doc_for?object=(multifile)/1) directive.

## Problem

The `lynx` subdirectory of the SWI Prolog library directory is probably not in your library search path.

The files won't be found automatically, even though the library directory itself is on the search path:

````
?- bagof(X,file_search_path(library,X),L).
L = ['/usr/local/logic/swipl/lib/swipl/library',
     '/usr/local/logic/swipl/lib/swipl/library/clp',
     '/home/paquette/.local/share/swi-prolog/pack/sldnfdraw/prolog',
     pce('prolog/lib')].
````

Try to use `format_paragraph/2`, fully qualified:

````
?- text_format:format_paragraph("hello, world",[width(50),text_align(center)]).
ERROR: Unknown procedure: text_format:format_paragraph/2 (DWIM could not correct goal)
````

## Solution

Let's make the predicates found in **file** `format.pl`. (**not** predicates found in module `format`) accessible.

(More precisely, "those predicates exported from some module _foo_, a part of which is defined in file `format.pl`.
This _is_ confusing.)

You can:

**Use the relative path of a file underneath the library search path**

In interactive mode, use the predicate:

````
use_module(library(lynx/format)).
````

Inside a source file or when reading `[user]`, use the directive (actually, you can use the predicate, too -
it will just be run when you close `[user]` instead of immediately):

````
:- use_module(library(lynx/format)).
````

**Extend the `library` file search path by adding a mapping for atom `library` to a directory**

In interactive mode, use `assertz/2`:

````
assertz(file_search_path(library,'/usr/local/logic/swipl/lib/swipl/library/lynx')).
use_module(library(format)).
````

Inside a source file or when reading `[user]`, assert a new fact for `file_search_path(library,DIR)`, then 
call the `use_module/1` directive:

````
file_search_path(library,'/usr/local/logic/swipl/lib/swipl/library/lynx').
:- use_module(library(format)).
````

**Create a new file search path and search that**

In interactive mode, use `assertz/2`:

````
assertz(file_search_path(lynx,'/usr/local/logic/swipl/lib/swipl/library/lynx')).
use_module(lynx(format)).
````

Inside a source file or when reading `[user]`, assert a new fact for `file_search_apth(ALIAS,DIR)`, then 
call the `use_module/1` directive:

````
file_search_path(lynx,'/usr/local/logic/swipl/lib/swipl/library/lynx').
:- use_module(lynx(format)).
````

**Indicate full path to file**

In interactive mode:

````
use_module('/usr/local/logic/swipl/lib/swipl/library/lynx/format.pl').
````

Inside a source file or when reading `[user]`:

````
:- use_module('/usr/local/logic/swipl/lib/swipl/library/lynx/format.pl').
````

## Now the predicate is accessible!

````
?- format_paragraph("hello, world",[width(50),text_align(center)]).
                   hello, world
````

or with qualification of the predicate:

````
?- text_format:format_paragraph("hello, world",[width(50),text_align(center)]).
                   hello, world
````

Perfect.

## Addendum

There is also a very nice predicate to examine predicate metadata,
[predicate_property/2](https://www.swi-prolog.org/pldoc/man?predicate=predicate_property%2f2):

Perhaps surprisingly, the term denoting the predicate is not given in `functor/arity` style, 

````
% WRONG
?- predicate_property(format_paragraph/2,imported_from(M)).
M = yall.
````

but in the form of a "callable term":

````
% CORRECT
?- predicate_property(format_paragraph(_,_),imported_from(M)).
M = text_format.

?- predicate_property(format_paragraph(_,_),file(F)).
F = '/usr/local/logic/swipl/lib/swipl/library/lynx/format.pl'.
````

