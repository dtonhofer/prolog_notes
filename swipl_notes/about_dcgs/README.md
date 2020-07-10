# About DCGs

## References

- SWI Prolog manual: https://eu.swi-prolog.org/pldoc/man?section=DCG
- Introduction and tutorials:
   - https://www.metalevel.at/prolog/dcg by Markus Triska
   - http://www.pathwayslms.com/swipltuts/dcg by Anne Ogborn
   - https://en.wikipedia.org/wiki/Definite_clause_grammar (has more references)

## Extensions

- https://www.info.ucl.ac.be/~pvr/edcg.html

## Résumé of Markus Triska's tutorial

A Prolog definite clause grammar (DCG) describes a **list**.

The list can be arbitrary terms. However, when parsing, one would process lists of characters (atoms of length 1) or list of
codepoints aka. character codes, which are integers, and which are 16-bit Unicode codepoints in SWI-Prolog (how to guarantee
portability between Prologs which do not specify that codepoints must be Unicode codepoints? One cannot!)

Operationally, DCGs can be used to parse, generate and check/accept lists. 

The basic DCG rule is:

```
Head --> Body.
```

DCG rules can take additional arguments (up from 0) and arbitrary Prolog code in the body, which means you are free to do anything:

```
Head(Arg1,Arg2) --> BodyPart, { PrologCode }, BodyPart.
```

More generally, DCGs can do arbitrary computations on lists. Indeed, the list may not even be the object that is conceptually worked on.
It may be a constant-length list holding a "current state" variable that is updated by te DCG in each rule depending on
other computations.

The list can be "worked off from the left", you can "look ahead from the left" to decide which rule to apply next and you
can even "push new items onto the list" from the left, actually increasing its length.

Names for nonterminals used in DCGs have a special name: _nonterminal indicators_ `f//N`, which indicates a DCG nonterminal `f` with arity `N`. 
This is similar to the _predicate indicator_ of core Prolog, `f/N`, which indicates a predicate `f` with arity `N`.

The cut in a DCG body works in the same way as it does in a claus body: It commits to the choice made to the left for the current
DCG rule activation. When **parsing** or **recognizing**, you generally **do not** want to avoid using the cut to make parsing
fast and deterministic and avoid useless redos. When **generating** you want to avoid using the cut to generate all possibilities.

Not using the cut when parsing means you have to add more Prolog code inside of `{` `}`.  

**Example**: `abba//2` counts the occurrences `ab` and `ba` atoms in a list of atoms:

```
:- use_module(library(clpfd)).

% Try without using cuts!

abba(AB,BA) --> [ab], abba(ABm,BA), {AB #= ABm+1}. 
abba(AB,BA) --> [ba], abba(AB,BAm), {BA #= BAm+1}.
abba(AB,BA) --> [X], { \+ memberchk(X,[ab,ba]) }, abba(AB,BA).
abba(0,0)   --> [].

:- begin_tests(dcg).

test(1,[true(Truly),nondet]) :- phrase(abba(AB,BA),[ab,x,y,ba,ba,abba,z]), Truly = ([AB,BA] == [1,2]).
test(2,[true(Truly),nondet]) :- phrase(abba(AB,BA),[x,y,z]),               Truly = ([AB,BA] == [0,0]).

:- end_tests(dcg). 

rt(dcg) :- run_tests(dcg). % run as ?- rt(_).
```

**Example**: `abba//2` counts the `ab` and `ba` sequences in a list of characters. A character can belong to ons sequence only.

TBD


Transformations:

- Double-Quoted-String -> atom_codes(L,Cs) to transfrom to codepoints if you want to work with codepoints (unreadable when debugging)
- Backquoted String    -> is another way of calling atom_codes(L,Cs) on a string, so these would appear on the RHS of codepoint-processing DCG rules.
- (Mono-Quoted) Atom   -> atom_chars(A,Chs) to transfrom to characters if you want to work with characters (readable when debugging)

You can have the parser interpret a double-quoted string as a list of characters directly by using

https://eu.swi-prolog.org/pldoc/man?section=flags#flag:double_quotes

double_quotes(codes,chars,atom,string, changeable) on a per-module basis.

https://eu.swi-prolog.org/pldoc/man?section=flags#flag:back_quotes

back_quotes(codes,chars,string,symbol_char, changeable)

## Always pass through `phrase/2` or `phrase/3`

Markus Triska writes about assuming that there is a List/Rest argument for a given rule:

> Adding two arguments in the way you suggest is only one particular way
> to compile DCGs, though admittedly of course the most common one.
> In principle though, it would be possible to compile DCGs completely
> differently to Prolog code, or not compile them at all, and for this
> reason you should always use `phrase/[2,3]` to invoke a DCG: It keeps
> your code completely portable, no matter how DCGs are actually 
> implemented in your system.

## For a future extension: EDCGs

From: *Assumptive Logic Programming* by Veronica Dahl and Paul Tarau - https://www.cse.unt.edu/~tarau/research/2004/dahl_tarau_argentina04.pdf

The authors are using "accumulator" instead of "difference list" here.
Are the two views correct?

> As the apparently simple translation scheme of grammars to Prolog became 
> popular, DCGs have been assimilated by means of their preprocessor based
> implementation. When restricted to definite clauses the original DCG translation
> is indeed operationally trouble free and has a simple Herbrand semantics.
> On the other hand, mixing DCGs with full Prolog and side effects has been a 
> prototypical Pandora’s box, ever since. Cumbersome debugging in the presence
> of large list arguments of translated DCGs was another initially unobvious
> consequence, overcome in part with depth-limited term printing. The complexity
> of a well-implemented preprocessor made almost each implementation slightly
> different from all others. The lack of support for “multiple DCG streams”,
> although elegantly solved with Peter Van Roy’s Extended DCGs [13], required
> an even more complex preprocessor and extending the language with new declarations.
> Worse, proliferation of programs mixing DCG translation with direct manipulation
> of grammar arguments have worked against data abstraction and portability.
>
> Translation free, and more general than Van Roy’s Extended DCGs [13], **assumption grammars**
> consist of logic programs augmented with a) multiple implicit accumulators, useful
> in particular to make the input and output strings invisible, and b) linear and
> intuitionistic implications scoped over the current continuation (i.e., over the
> remaining AND branch of the resolution), based on a variant of linear logic [4] with
> good computational properties, affine logic [8].

- [4] J.-Y. Girard. Linear logic. Theoretical Computer Science, (50):1–102, 1987.
- [8] A. P. Kopylov. Decidability of linear affine logic. In Proceedings, Tenth
      Annual IEEE Symposium on Logic in Computer Science, pages 496–504, San Diego, 
      California, 26–29 June 1995. IEEE Computer Society Press.
- [13] Peter Van Roy. A useful extension to Prolog’s Definite Clause Grammar notation.
      SIGPLAN notices, 24(11):132–134, November 1989.

Meanwhile, there is a pack for EDCGs:

https://www.swi-prolog.org/pack/list?p=edcg




