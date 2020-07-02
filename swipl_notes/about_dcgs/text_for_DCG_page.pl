Introduction and tutorials:

   - [DCG Primer](https://www.metalevel.at/prolog/dcg) by Markus Triska
   - [DCG Tutorial](http://www.pathwayslms.com/swipltuts/dcg) by Anne Ogborn
   - Wikipedia: [Definite Clause Grammar](https://en.wikipedia.org/wiki/Definite_clause_grammar) (has more references)
   - In his manual: [DCG basic utilities](https://eu.swi-prolog.org/pldoc/doc/_SWI_/library/dcg/basics.pl)

Also take a look at

   - Wikipedia: [Attribute Grammar](https://en.wikipedia.org/wiki/Attribute_grammar) 

And maybe

   - [Package EDCG](https://www.swi-prolog.org/pack/list?p=edcg) which implements Van Roy's Extended DCGs from "A useful extension to Prolog’s Definite Clause Grammar notation.", SIGPLAN notices, 24(11):132–134, November 1989.

Markus Triska writes in his DCG primer:

Consider using DCGs if you are:
   - describing a list
   - reading from a file
   - passing around a state representation that only a few predicates actually use or modify.

In every serious Prolog program, you will find many opportunities to use DCGs due
to some subset of the above reasons.

## Pass through phrase!

Although currently the current implementation adds predicates names after the DCG rules with two additional arguments, one should not rely on this and call those generated predicates directly.

Markus Triska writes:

> In principle though, it would be possible to compile DCGs completely
> differently to Prolog code, or not compile them at all, and for this
> reason you should always use `phrase/[2,3]` to invoke a DCG: It keeps
> your code completely portable, no matter how DCGs are actually
> implemented in your system.

## A simple, arbitrary example

Generate or test strings obeying the Perl Regex `(ab)*`

With unit test code:

==
:- use_module(library(clpfd)).

acceptable(0) --> [].
acceptable(N1) -->  `ab`, { N #= N1-1, N #>= 0 }, acceptable(N).

phrase_acceptable(Text,N) :-
   nonvar(Text),!,                   % guard to select "recognize" case
   atom_codes(Text,Codes),           % text-to-recognize can be string or atom or numbner
   phrase(acceptable(N),Codes,[]).
   
phrase_acceptable(Text,N) :- 
   var(Text),!,                      % guard to select "generate" case
   phrase(acceptable(N),Codes,[]),
   atom_codes(Text,Codes).           % generated text is atom
      
:- begin_tests(dcg).

test(generate,[true(Truly)])  :- 
   bagof(T,phrase_acceptable(T,6),Bag),
   Truly = (Bag == ['abababababab']).
   
test(recognize,[true(Truly)]) :- 
   bagof(N,phrase_acceptable("ababab",N),Bag),
   Truly = (Bag == [3]).
   
test(enumerate5,[true(Truly)]) :- 
   bagof([T,N],limit(5,phrase_acceptable(T,N)),Bag), 
   Truly = (Bag == [['',0],['ab',1],['abab',2],['ababab',3],['abababab',4]]).
   
:- end_tests(dcg).

rt(dcg) :- run_tests(dcg).
==
   
**Generate**

```
?- phrase_acceptable(T,6).
T = abababababab ;
false.
```

**Test/Recognize**

```
?- phrase_acceptable("ababab",N).
N = 3 ;
false.
```

**Enumerate valid solutions**

```
?- phrase_acceptable(T,N).
T = '',
N = 0 ;
T = ab,
N = 1 ;
T = abab,
N = 2 ;
T = ababab,
N = 3 ;
T = abababab,
N = 4 
...
```
