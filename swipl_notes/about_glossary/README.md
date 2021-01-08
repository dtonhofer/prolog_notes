# Glossary Extensions

The SWI-Prolog manual has a [glossary](https://eu.swi-prolog.org/pldoc/man?section=glossary) but extensions are possible!

**goal**

A goal can be a disjunction or conjunction or a combination thereof, or more generally, anything that
can appear in a body rule. This precludes compound terms with the name `:-`.
A more appropriate name (from the logical stance) for "goal" would be "conjecture".
([Merriam-Webster](https://www.merriam-webster.com/dictionary/conjecture): _a proposition (as in mathematics) before it has been proved or disproved_)

**Pure Prolog**

The ISO Standard of 1995 says this on page 128:

> Assume first that databases and goals use user-defined procedures and conjunction ((,)/2) only, and that a predication in the body 
> of a clause cannot be a variable \[this means, no "call/N"\]. A goal or the body of a clause is a possibly empty sequence of
> predications, denoted by the conjunction.
> 
> NOTE — This is "pure" Prolog. The notion of a search-tree was introduced for "pure" Prolog in the history of logic programming in
> order to explain the resolution and the backtracking as they are fixed in Standard Prolog, and it will serve as a basis to define
> and understand the semantics of further constructs.

Pure Prolog is relatively weak, as it has no cut (!) and no negation-as-failure (\+) (both can be expressed in terms of one another)
and no _disequality_ (`dif/2`). This means a proof cannot restrict itself to only a "valid branch" by committing or testing for
disequlity - it must necessarily traverse all branches and will generate spurious proof witnesses! Moreover, Prolog does not 
restrict the domain of a variable: there are no types. This also precludes enumerating possibly values of a variable over a finite
domain, so avenues which fail if the enumeration does not provide a witness also are closed off.

You will want to add least add `dif/2` to "pure Prolog" to do something reasonable.

Interestingly, a (Universal) Turing Machine can be implemented in Pure Prolog, by reversing the "causality" of ":-":
The state at T+1 is the body and is based on the state T in the head. The program basically "proves that the machine halts":

```
turing_machine_state(T,[L|TapeLeft],[R|TapeRight],TapeReadHead,State,Halts) :- 
   lookup_tm_automaton_table(TapeReadHead,State,Write,Move,NextState),
   turing_machine_state(s(T),...).
```

But pure Prolog cannot even do list intersection in its "native format" of Prolog lists (see also: 
[Paradox? Pure Prolog is Turing-complete and yet incapable of expressing list intersection?](https://cs.stackexchange.com/questions/133895/paradox-pure-prolog-is-turing-complete-and-yet-incapable-of-expressing-list-int))

**Argument**

In standard programming languages there is a difference between a procedure parameter (abstract argument) and a procedure argument (concrete argument):

```
  public int add(int x,int y)  parameters x and y
  
  int x = add(1,2)             arguments 1 and 2
```

In Prolog, the same word, "arguments", is used throughout. Probably because one borrows
from logic and a compound term always has arguments (numbered 1, .. ,n btw), but 
parameters are something that is passed on a call, which is "imperative"

```
  p(A,B,C) :- q(A,B,C).        predicate or clause arguments
  f(1,2,3)                     term arguments
  call(p,1,2,3)                call arguments
  p(1,2,3)                     also call arguments
```

**Atomic term etc.**

Do not confuse:

- Prolog "atom" like `atom` - a symbol (in principle without inner structure although, being actually a string, it dos) that stands for itself (same as the LISP atom)
- Prolog "atomic" like `[]` - a symbol that is "atomic" but not an "atom" (the set of atom is subset of the set of atomic)
- Prolog "atomic term" - a term that is a member of the set of atomics
- Logic "atom" - a predicate symbol followed by parenthesized arguments, as many as its arity demands: p(x,y,z). If the arity is 0, the logic atom resolves to one of TRUE or FALSE
- A Prolog "constant" - a 0-ary function that resolves to soemthing (generally a number). In SWI-Prolog can be written with our without parentheses: pi or pi()
- Logic "constant" - One of the logic "functions" OR, AND, TRUE, FALSE, NOT
- A logic literal - A positive or negative (negated) logic atom. The negation is generally the "strong" (~p: it is true that p is not the case) rather than the "weak" (-p: there is no evidence that p is the case) negation

**Partial term**

- instantiated term: a term that is not an unbound variable. it may or may not be ground
- uninstanted term: a term that is an unbound variable
- partial term: an instantiated or uninstantiated term (basically, "any term" or an "unconstrained term")

(All these terms have a certain information content, with an unbound variable having content 0. How can we express this
content in a number that does not depend on context (i.e. the atom "foo" would have a high (Shannon) information content "in context"
if it is rare on a channel, but we don't have access to that context)? Something like Kolomgorov Complexity (compressibility)
sounds like the correct idea; gotta think about that)
