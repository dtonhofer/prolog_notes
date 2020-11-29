# Tabling and the Well-Founded Semantics.

I sure hope this is being discussed in modern course on Logic Programming.

**There is not much here, yet.**

- The Flora-2 manual has excellent descriptions.
- Well-Founded Semantics is often primarily discussed in papers about Negation in Logic Programming.

## Examples

Here is a case where Tabled Prolog is unsure and prints the residual program:

```
p(1) :- tnot(p(1)).

?- p(1).
% WFS residual program
    p(1) :-
        tnot(p(1)).
p(1).
```

The query, however "succeeds" (firmly saying that "computation succeeded").

How do I find out that I should be unsure about the result when I am interested in _modeling_?

This is the same problem that occurs with [`freeze/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=freeze/2).
Although at the end of a successful computation, one can ask [`frozen/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=frozen/2) to
check for residual goals (and act accordingly).

## Reading

### Well-Founded and Stationary Models of Logic Programs (1994)

- Teodor Przymusinski 
- https://link.springer.com/article/10.1007/BF01530784
- https://www.researchgate.net/publication/2299243_Well-Founded_and_Stationary_Models_of_Logic_Programs
- Appears in: Annals of Mathematics and Artificial Intelligence, 1994, vol. 12, page 141-187

Excellent Overview Paper.

### The Well-Founded Semantics for General Logic Programs

- Allen Van Gelder, Kenneth A. Ross, John S. Schlipf
- https://www.researchgate.net/publication/2742259_The_Well-Founded_Semantics_for_General_Logic_Programs
- Appears in: Journal of the ACM. Vol. 38, No. 3, July 1991, pp. 620-650

### The Flora-2 manual

Chapter 20: Negation_ (p 86 ff.) explains the extensions of Flora-2/XSB relative to Prolog

I suppose the XSB manual has a lot to say, too.

 http://flora.sourceforge.net/docs/floraManual.pdf (PDF), 

In the SWI-Prolog manual

- [Tabled execution (SLG resolution)](https://eu.swi-prolog.org/pldoc/man?section=tabling)
- XSB Prolog (David Warrne, Terrance Swift) is the nexus of development here...
- Also implemented in B-Prolog and thus availabe in Picat (which builds on B-Prolog) (Nah Feng-Zhou et al.)


   - 
   - 
   
What are "stratified programs"?

### OLD Resolution with Tabulation 

An earlier approach?

   - 1986-07
   - Taisuke Sato

### Efficiently Implementing SLG Resolution

   - 1994-01-25
   - http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.49.5979
   - Terrance Swift , David S. Warren 

> SLG is a table-oriented resolution method that is gaining increasing attention due to its ability
> to combine the deductive database, non-monotonic reasoning and logic programming paradigms. SLG
> resembles SLD in that it admits a tuple-at-a-time resolution method, so it can make use of many of
> the techniques developed in implementing SLD over the past decade. Indeed, a program can contain any
> mixture of SLG and SLD evaluated predicates. As a result SLG forms a natural basis for extending Prolog
> systems. SLG can be efficiently implemented using a WAM-style abstract machine, the SLG-WAM. The SLG-WAM
> has been implemented for stratified programs, and design is underway for extensions to general programs.
> Performance results for stratified programs are available. They indicate firstly when executing SLD,
> the overhead of the SLG-WAM compared to a similar WAM implementation is minimal, and usually less than 10%.
> Further results indicate that SLG derivation may be expected to be surprisingly competitive with SLD 
> derivation on numerous datalog programs, even when SLD terminates and contains no redundant subcomputations.
> Finally, performance comparisons indicate that the SLG-WAM is about an order of magnitude faster than current
> deductive database systems even for datalog queries. The results, taken as a whole call into question
> traditional arguments about top-down versus bottom-up evaluation effciency, and also indicate that implementation
> of SLG evaluation is a field of research worthy of general study.

## Memoing for Logic Programs

   - David S. Warren
   - Communications of the ACM 
   - March 1992
   - Vol 35, N. 3.
   - Pages 94-111
   
Discusses: OLDT - Ordered Selection Strategy with Linear Resolution for Definite Clauses for Tabling, Abstract Interpretation, Partial Deduction
