# About DCGs

## References

- SWI Prolog manual: https://eu.swi-prolog.org/pldoc/man?section=DCG
- Introduction and tutorials:
   - https://www.metalevel.at/prolog/dcg by Markus Triska
   - http://www.pathwayslms.com/swipltuts/dcg by Anne Ogborn
   - https://en.wikipedia.org/wiki/Definite_clause_grammar (has more references)

## Always pass through phrase

Markus Triska writes about assuming that there is a List/Rest argument 
for a given rule:

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




