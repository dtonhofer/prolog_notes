# Notes about if-then-else

## If-then-else

The construct [if-then-else](https://eu.swi-prolog.org/pldoc/doc_for?object=(-%3E)/2) construct is syntactically a bit special 
because the `->/2` operator is "helped" by `;/2` operator to yield a quasi-trinary operator:

**if-then-else as written**

![if-then-else as written](pics/if_then_else_as_written.png)

**if-then-else as parsed**

`write_canonical/1` prints what it reads:

```
?- write_canonical((p->q;r)).
;(->(p,q),r)
true.

?- write_canonical((p->q);r).
;(->(p,q),r)
true.
```

In case there is no "else"

```
?- write_canonical(p->q).
->(p,q)
true.
```

So, `p->q;r` is parsed exactly the same as the "or" of `p->q` and `r`: Both expressions are indistinguishable.
As the else-less `p->q` _fails_ if `p` _fails_, this is completely correct: The if-then-else _is_ a composite of the _or_ operator `;/2` 
operator first, and an additional `->/2` operator second.

Define predicates which you can cause to succeed or fail by argument:

```
p(X) :- format("p(~q)",[X]),call(X).
q(X) :- format(" q(~q)",[X]),call(X).
r(X) :- format(" r(~q)",[X]),call(X).

```

Then:

```
?- 
p(true) -> q(true) ; r(true).

p(true) q(true)
true.
```

```
?- 
p(false) -> q(true) ; r(true).

p(false) r(true)
true.
```

```
?- 
?- p(true) -> q(false) ; r(true).

p(true) q(false)
false.
```

And this is exactly the same as:

```
?- (p(true) -> q(true)) ; r(true).

p(true) q(true)
true.
```

```
?- (p(false) -> q(false)) ; r(true).

p(false) r(true)
true.
```

```
?- (p(true) -> q(false)) ; r(true).

p(true) q(false)
false.
```

![if-then-else as parsed](pics/if_then_else_as_parsed.png)


**if-then-else as wired up in the [Byrd Box model](../other_notes/about_byrd_box_model)**

![if-then-else as wired up in the byrd box model](pics/if_then_else_as_wired_up_in_the_byrd_box_model.png)

In case the "else" condition is missing, it is replaced by a `false` and we get:

**if-then or if-then-else-false as wired up in the Byrd Box model**

![if-then-else-false wired up in the byrd box model](pics/if_then_as_wired_up_in_the_byrd_box_model.png)

## Soft-cut

Another construct, the [soft-cut](https://eu.swi-prolog.org/pldoc/doc_for?object=(*-%3E)/2), is built form `->*` and `;/2`.
Again, a trinary operator is build 



