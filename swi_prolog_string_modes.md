# SWI Prolog strings modes: How is something enclosed by double quotes `"` interpreted?

(This page is also reachable via http://bit.ly/2TtLV0H_prolog)

This is an SWI-Prolog specific extension to ISO-conforming Prolog: 

- **ISO-conforming Prolog** maps text between opening and closing double quotes `"..."` to a _list_ of character codes
  (i.e. integers >= 0).
- **SWI Prolog** has a dedicated data type _string_, and maps program text between opening and closing double
  quotes `"..."`   to a _string_ instance, by default. However, default behaviour can be changed by setting the
  flag `double_quotes`.

The flag `double_quotes` and a host of others are described in section
[Environment Control (Prolog flags)](https://www.swi-prolog.org/pldoc/man?section=flags) of the SWI-Prolog manual.

- [Description for the flag `double_quotes`](https://eu.swi-prolog.org/pldoc/man?section=flags#flag:double_quotes)
- [The string type and its double quoted syntax](https://eu.swi-prolog.org/pldoc/man?section=strings)
- [Why has the representation of double quoted text changed?](https://eu.swi-prolog.org/pldoc/man?section=ext-dquotes-motivation)

Note that a string can contain NUL characters (value 0), unlike for C strings.

## portray_text

There is also a library to set whether lists of integers are printed out as strings:

[portray_text](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/portray_text.pl). 

This makes sense only if strings are lists of integers, i.e. when one is ISO-conforming mode.
Which is no longer the SWI-Prolog way.

TODO: Does that library really do something?

## Testing this

Here is the table of tested behaviours. 

We use 

- [`string/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=string/1) to test for "stringy-ness".
- [`write/1`](https://eu.swi-prolog.org/pldoc/man?predicate=write/1) to write a string as term.

### `set_prolog_flag(double_quotes,string)` - Default mode for SWI Prolog
	
Something written as `"..."` designates an element of the particular SWI Prolog type _string_.

#### Nonempty string

It's not a list:

````
?- [F|_]="a".
false.
````

It's a _string_:

````
?- string("ab").
true.
````

#### Empty string

Also a "string" according to `string/1`!

````
?- string("").
true.
````

What happens when we term-write it?

````
?- write("").
true.
````

#### Writing the nonempty string

Term-writing it:

````
?- write("hello, ▽△ ¡ €").
hello, ▽△ ¡ €
true.
````

Writing using [`format/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=format/2) with `~w`, which calls
[`write/1`](https://eu.swi-prolog.org/pldoc/man?predicate=write/1), writing the term, same as above:

````
?- format("~w",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
true.
````

Writing using [`format/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=format/2) with `~s` (_"Output text from
a list of character codes or a string from the next argument"_):

````
?- format("~s",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
true.
````

### `set_prolog_flag(double_quotes,codes)` - Traditional, ISO-conforming Prolog
	
Something written as `"..."` designates a list of character codes (i.e. integers >= 0).

In SWI Prolog, the character codes correspond to **Unicode** character points. See: [Unicode Prolog source](https://eu.swi-prolog.org/pldoc/man?section=unicodesyntax)

#### Nonempty string

It's a list of integer:

````
?- [F|_]="a",integer(F).
F = 97.
````

It is not a _string_:

````
?- string("ab").
false.
````

#### Empty string

Term-writing it gives the empty list (because it's a list of zero characters):

````
?- write("")
[]
true.
````

Still not a _string_:

````
?- string("").
false.
````

#### Writing the nonempty string

Term-writing it:

Note that the EURO sign for example is correctly mapped to Unicode `0x8364` and not to something arcane like, say "Windows Codepage 1250" `0x80`.

````
?- [F|_] = "hello, ▽△ ¡ €", F = 104, integer(F).
F = 104.
````

````
?- write("hello, ▽△ ¡ €").
[104,101,108,108,111,44,32,9661,9651,32,161,32,8364]
````

List stays list with `format/1` and `~w`

````
?- format("~w",["hello, ▽△ ¡ €"]).
[104,101,108,108,111,44,32,9661,9651,32,161,32,8364]
````

If you leave out one level or brackets for `format/2` with `~w`:

````
?- format("~w","hello, ▽△ ¡ €").
104
````

Get the string back using `~s`:

````
?- format("~s",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
````

If you leave out one level or brackets for format/2 with `~s`:

````
?- format("~s","hello, ▽△ ¡ €").
ERROR: Illegal argument to format sequence ~s: 104
````

Note that code 0x80 for example is silently accepted (although it is not a valid Unicode code point):

````
?- format("~s",[[128]]).
true.
````

# `set_prolog_flag(double_quotes,chars)` - Useful if you want to specify lists of atoms in your code.
	
Something written as `"..."` designates a list of char.

Note that Prolog has no separate datatype _char_. It's just an integer, interpreted in context.

A single char is mapped to an _atom_ (which is a symbol that stands for itself).

## Nonempty string

It's a list of atom!

````
?- [F|_]="a",atom(F).
F = a.
````

It is not a string according to `string/1`:

````
?- string("ab").
false.
````

## Empty string

````
?- write("").
[]
true.
````

It is a string according to `string/1`

````
?- string("").
false.
````

## Nonempty string

````
?- [F|_] = "hello, ▽△ ¡ €", F = h, atom(F).
F = h.
````

````
?- write("hello, ▽△ ¡ €").
[h,e,l,l,o,,, ,▽,△, ,¡, ,€]
````

List stays list with `format/1` and `~w`

````
?- format("~w",["hello, ▽△ ¡ €"]).
[h,e,l,l,o,,, ,▽,△, ,¡, ,€]
````

If you leave out one level or brackets for `format/2` with `~w`

````
?- format("~w",["hello, ▽△ ¡ €"]).
h
````

Get the string back using `~s`

````
?- format("~s",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
````

# `set_prolog_flag(double_quotes,atom)` - _(when is this useful?)_

Something written as `"..."` designates an atom. It is not a string according to `string/1`

````
?- atom("ab").
true.

?- string("ab").
false.
````

## Empty string

Results in the _empty atom_.

````
?- write("").
true.

?- atom("").
true.
````

It is a string according to `string/1`?

````
?- string("").
false.
````

Note that in SWI-Prolog, the thing denoted by `[]` (the empty list) is not an atom:

````
?- atom([]).
false.
````

## Nonempty string

````
?- atom("hello, ▽△ ¡ €").
true.
````

Get the string back using `~w`

````
?- format("~w",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
true.
````

````
Get the string back using ~s
````

````
?- format("~s",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
````
