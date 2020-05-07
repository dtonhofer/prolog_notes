# SWI Prolog strings modes: How is something enclosed by double quotes `"` interpreted?

- This page is also reachable via http://bit.ly/2TtLV0H_prolog

## Intro

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

## `portray_text`

There is also a library to set whether lists of integers are printed out as strings:

[portray_text](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/portray_text.pl). 

This makes sense only if strings are lists of integers, i.e. when one is ISO-conforming mode.
Which is no longer the SWI-Prolog way.

TODO: Does that library really do something?

## Trying it out

We use 

- [`string/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=string/1) to test for "stringy-ness". This only succeeds if
  passed the SWI-Prolog specific "string" type.
. [`format/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=format/2) with `~w`, which calls  
- [`write/1`](https://eu.swi-prolog.org/pldoc/man?predicate=write/1) to write a string as term.


To transform various forms of "text representation" back into the SWI-Prolog _string_ use:

- [`text_to_string/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=text_to_string/2)

Note that it does not seem to be possible to change the `double_quotes` flag "in flight". You have to issue the
`set_prolog_flag/2` command at the toplevel, or (I imagine) as a directive in source code. If you issue it in a clause
body, it doesn't take effect. This makes sense, as the option changes the behaviour of the reader, so calling
it from a read-in and compiled program is dubious.

Below REPL ouput is preceded with `%`  so that you can copy-paste the text with wild abandon.
(Prolog's prompt is `?-` ... it should really by `%-` to allow copypastaing. And a line preceded with `|:` should
directly be interpreted as source entered at the REPL.)

### Default mode for SWI Prolog: `set_prolog_flag(double_quotes,string).`
	
Something written in between double quotes `"..."` is mapped to the SWI Prolog type _string_.

````
set_prolog_flag(double_quotes,string).
````

````
maplist([L,R]>>(string(L) -> R = 'string'; R = 'no'), 
         ["xx",'xx',xx,""], 
         Out).

% Out = [string, no, no, string].
````

````
maplist([X]>>format("string: ~s, term: ~w\n",[X,X]),["xx",'xx',xx,""]).

% string: xx, term: xx
% string: xx, term: xx
% string: xx, term: xx
% string: , term: 
````

In the snippet below, format strings are given as double-quoted strings:

````
out(I) :- format("read: ~w\n", [I]), 
          text_to_string(I,S), 
          format("out-term: ~w\nout-string: ~s\n", [S,S]).
	  
out("Hello, World!").

% read: Hello, World!
% out-term: Hello, World!
% out-string: Hello, World!
````

### Traditional: ISO-conforming Prolog: `set_prolog_flag(double_quotes,codes).`

Something written in between double quotes `"..."` is mapped to a list of character codes (i.e. integers >= 0).

In SWI Prolog, the character codes correspond to **Unicode** character points. 
See: [Unicode Prolog source](https://eu.swi-prolog.org/pldoc/man?section=unicodesyntax)

Using his is useful if you want to create a list of character codes from double-quoted quickly at the REPL. 
(But when do you need to do that?)

````
set_prolog_flag(double_quotes,codes). 
````

````
maplist([L,R]>>((L=[A|_],integer(A)) -> R = 'list-of-int'; R = 'no'), 
         ["xx",'xx',xx,""], 
         Out).
	   
% Out = ['list-of-int', no, no, no].
````

The empty double-quoted "thing" is the empty list, as expected:

````
"" = [].

% true
````

````
maplist([X]>>format('string: ~s, term: ~w\n',[X,X]),["xx",'xx',xx,""]).

% string: xx, term: [120,120]
% string: xx, term: xx
% string: xx, term: xx
% string: , term: []
````

In the snippet below, format strings are given as single-quoted strings (i.e. atoms) to avoid them being
transformed into lists, utterly confusing `format/2`:

````
out(I) :- format('read: ~w\n', [I]), 
          text_to_string(I,S), 
          format('out-term: ~w\nout-string: ~s\n', [S,S]).
	  
out("Hello, World!").

% read: [72,101,108,108,111,44,32,87,111,114,108,100,33]
% out-term: Hello, World!
% out-string: Hello, World!
````

Trying out non-ASCII:

````
write("hello, ▽△ ¡ €").

% [104,101,108,108,111,44,32,9661,9651,32,161,32,8364]

format('~w',["hello, ▽△ ¡ €"]).
% [104,101,108,108,111,44,32,9661,9651,32,161,32,8364]
````

Note that the EURO sign for example is correctly mapped to Unicode `0x8364` and not to
something arcane like, say "Windows Codepage 1250" `0x80`.

The code 0x80 for example is silently accepted (although it is not a valid Unicode code point):

````
format("~s",[[128]]).

% true.
````

### String as a list of atoms: `set_prolog_flag(double_quotes,chars).` 

Something written in between double quotes `"..."` is mapped to a list of atoms. 

````
set_prolog_flag(double_quotes,chars). 
````

````
maplist([L,R]>>((L=[A|_],atom(A)) -> R = 'list-of-atom'; R = 'no'), 
        ["xx",'xx',xx,""], 
        Out).
	
% Out = ['list-of-atom', no, no, no].	   
````

The empty double-quoted "thing" is the empty list, as expected:

````
"" = [].

% true
````

````
maplist([X]>>format('string: ~s, term: ~w\n',[X,X]),["xx",'xx',xx,""]).

string: xx, term: [x,x]
string: xx, term: xx
string: xx, term: xx
string: , term: []
````

````
out(I) :- format('read: ~w\n', [I]), 
          text_to_string(I,S), 
          format('out-term: ~w\nout-string: ~s\n', [S,S]).
	  
out("Hello, World!").

% read: [H,e,l,l,o,,, ,W,o,r,l,d,!]
% out-term: Hello, World!
% out-string: Hello, World!  
````

### String as a single atom: `set_prolog_flag(double_quotes,atom).` 

Something written in between double quotes `"..."` is mapped to a single atom.

````
set_prolog_flag(double_quotes,atom). 
````

````
maplist([L,R]>>(atom(L) -> R = 'atom'; R = 'no'), 
        ["xx",'xx',xx,""], 
        Out).

% Out = [atom, atom, atom, atom].
````

It's a bit weird that the "empty atom" even exists, but it does:

````
atom("").

% true.
````

````
maplist([X]>>format('string: ~s, term: ~w\n',[X,X]),["xx",'xx',xx,""]).

% string: xx, term: xx
% string: xx, term: xx
% string: xx, term: xx
% string: , term: 
````

````
out(I) :- format('read: ~w\n', [I]), 
          text_to_string(I,S), 
          format('out-term: ~w\nout-string: ~s\n', [S,S]).
	  
% ?- out("Hello, World!").
% read: Hello, World!
% out-term: Hello, World!
% out-string: Hello, World!
````
