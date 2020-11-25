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
