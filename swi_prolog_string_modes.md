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

We use 

- [`string/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=string/1) to test for "stringy-ness".
- [`write/1`](https://eu.swi-prolog.org/pldoc/man?predicate=write/1) to write a string as term.

### Default mode for SWI Prolog

`set_prolog_flag(double_quotes,string).`
	
Something written as `"..."` designates an instance of the particular SWI Prolog type _string_.

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

### Traditional: ISO-conforming Prolog

`set_prolog_flag(double_quotes,codes).`

Something written as `"..."` designates a list of character codes (i.e. integers >= 0).

In SWI Prolog, the character codes correspond to **Unicode** character points. 
See: [Unicode Prolog source](https://eu.swi-prolog.org/pldoc/man?section=unicodesyntax)

#### Nonempty string

It's a list of _integer_:

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

Term-writing it results in a list of character:

````
?- write("hello, ▽△ ¡ €").
[104,101,108,108,111,44,32,9661,9651,32,161,32,8364]
?- format("~w",["hello, ▽△ ¡ €"]).
[104,101,108,108,111,44,32,9661,9651,32,161,32,8364]
````

Note that the EURO sign for example is correctly mapped to Unicode `0x8364` and not to
something arcane like, say "Windows Codepage 1250" `0x80`.

If you leave out one level or brackets for `format/2` with `~w` by mistake:

````
?- format("~w","hello, ▽△ ¡ €").
104
````

Get the string back using `~s`:

````
?- format("~s",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
````

If you leave out one level or brackets for format/2 with `~s` by mistake:

````
?- format("~s","hello, ▽△ ¡ €").
ERROR: Illegal argument to format sequence ~s: 104
````

Note that code 0x80 for example is silently accepted (although it is not a valid Unicode code point):

````
?- format("~s",[[128]]).

true.
````

### A list of atoms

`set_prolog_flag(double_quotes,chars).` 

Something written as `"..."` designates a list of atoms. 
A single char is mapped to an _atom_ (which is a symbol that stands for itself).
Useful if you want to specify lists of atoms in your code (to avoid quoting for example).	

#### Nonempty string

It's a list of atom!

````
?- [F|_]="a",atom(F).
F = a.
````

It is not a _string_:

````
?- string("ab").
false.
````

#### Empty string

Term-writing it gives the empty list:

````
?- write("").
[]
true.
````

Still not a _string_:

````
?- string("").
false.
````

#### Nonempty string

The text withing `"..."` is decomposed into a list of atoms (are there any characters that cannot appear in an atom?):

````
?- write("hello, ▽△ ¡ €").
[h,e,l,l,o,,, ,▽,△, ,¡, ,€]
````

Same as:

````
?- format("~w",["hello, ▽△ ¡ €"]).
[h,e,l,l,o,,, ,▽,△, ,¡, ,€]
````

Get the string back using `~s`:

````
?- format("~s",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
````

### Just an atom

`set_prolog_flag(double_quotes,atom).` 

Something written as `"..."` designates a single atom (when is this useful?)

````
?- atom("ab").
true.
````

It is not a _string_:

````
?- string("ab").
false.
````

#### Empty string

Results in the _empty atom_.

````
?- write("").
true.

?- atom("").
true.

?- string("").
false.
````

#### Nonempty string

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

Get the string back using ~s

````
?- format("~s",["hello, ▽△ ¡ €"]).
hello, ▽△ ¡ €
````
