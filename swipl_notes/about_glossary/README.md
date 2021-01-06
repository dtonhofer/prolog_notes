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


