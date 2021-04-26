# Exercise: Compile a shared library written in C for use by SWI-Prolog

**We need:**

- The shared library code: `sayhellolib.c`
- A Prolog stub module: `sayhellolib.pl`

There is no need for anything else.

The shared library code declares & defines the C function:

```none
static foreign_t pl_say_hello(term_t t, term_t enc_term)
```

and registers it as a predicate `say_hello/2` in a specially named function 

```none
install_t install_sayhellolib()
```

which is executed when the library is loaded by the SWI-Prolog runtime.


The Prolog stub module `sayhellolib.pl` just declares a module called
`sayhellolib` which exports predicate `say_hello/2`. It otherwise just
refers to the foreign library `sayhellolib.so` via the directive

```none
:- use_foreign_library(foreign(sayhellolib)).
```

**References:**

- [`use_foreign_library/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=use_foreign_library/1)
- [library(shlib): Utility library for loading foreign objects (DLLs, shared objects)](https://eu.swi-prolog.org/pldoc/man?section=shlib)

**We organize everything as follows:**

```
sayhellolib/
├── build.sh
├── lib
│   └── sayhellolib.so
├── mod
│   └── sayhellolib.pl
└── src
    └── sayhellolib.c
```

The script `build.sh` just calls [swipl-ld](https://eu.swi-prolog.org/pldoc/man?section=shlib) to compile the .so file
and put everything into the correct directories.

**Running**

Once everything has been compiled, we need to:

- Put the `mod` subdirectory on SWI-Prolog's "library path" (one could also load `sayhellolib.pl` by qualified filename).
- Put the `lib` subdirectory on SWI-Prolog's "foreign library path".
- Issue a [`use_module`](https://eu.swi-prolog.org/pldoc/doc_for?object=use_module/1) directive to pull in module `sayhellolib`.
- ...and then we can call `say_hello/2`.

And so, on a terminal which can print UTF-8 byte sequences correctly:

```
?- assertz(file_search_path(foreign,'/foo/bar/sayhellolib/lib')).
true.

?- assertz(file_search_path(library,'/foo/bar/sayhellolib/mod')).
true.

?- use_module(library('sayhellolib.pl')).
install_sayhellolib() says: Successfully registered 'say_hello/2'
true.

?- say_hello("World",utf8).
pl_say_hello() says: 'enc_term' yields 'enc_id' 20 (utf8)
Hello, World
true.

?- say_hello("вселенная",utf8).
pl_say_hello() says: 'enc_term' yields 'enc_id' 20 (utf8)
Hello, вселенная
true.
```

One can also try things which may _not_ work on an UTF-8 attuned terminal:

```
?- say_hello("вселенная",iso_latin_1).
pl_say_hello() says: 'enc_term' yields 'enc_id' 10 (iso_latin_1)
extract_text_and_say() says: Extraction of text from term failed with target encoding 'iso_latin_1'
false.

?- say_hello("вселенная",multibyte).
pl_say_hello() says: 'enc_term' yields 'enc_id' 30 (multibyte)
Hello, вселенная
true.

?- say_hello("Renée",multibyte).
pl_say_hello() says: 'enc_term' yields 'enc_id' 30 (multibyte)
Hello, Renée
true.

?- say_hello("Renée",iso_latin_1).
pl_say_hello() says: 'enc_term' yields 'enc_id' 10 (iso_latin_1)
Hello, Ren?e
true.
```
