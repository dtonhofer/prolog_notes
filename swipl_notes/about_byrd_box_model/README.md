# About the "Byrd Box Model"

The "Byrd Box Model", also called "Tracing Model" or "Procedure Box Model" or 
"4-port Model", pictures the calls to a Prolog predicate as a "box with 4 ports".

The model's idea is that the Prolog Processor traverses these ports as it
runs the program.

Debuggers use this model as conceptual basis when they generate output or
allow monitoring of "ports traversal events" either in general 
([`leash/1`](https://www.swi-prolog.org/pldoc/doc_for?object=leash/1))
or on a per-predicate basis
([`trace/1`](https://www.swi-prolog.org/pldoc/doc_for?object=trace/1), [`trace/2`](https://www.swi-prolog.org/pldoc/doc_for?object=trace/2)).

The Byrd Box model has been described first in:

- Understanding the control flow of Prolog programs, 1980, Lawrence Byrd.
  In: "Proceedings of the Logic Programming Workshop in Debrecen, Hungary."
  (this document does not seem to exist online)

And later here:

- DECSystem-10 PROLOG USER'S MANUAL version 3.47, November 10, 1982
  by D.L. Bowen (ed.), L. Byrd, F.C.N. Pereira, L.M. Pereira, D.H.D. Warren
  (p. 13 ff. "2.1 The Procedure Box Control Flow Model"). 
  Found at [CiteseerX](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.300.8430)

And is described for example here:

- Programming in Prolog, 5th edition (Springer Verlag, 2003),
  William F. Clocksin, Christopher S. Mellish, p. 194 ff.: "8.3 The Tracing Model"
- [GNU Prolog - Debugging - The Procedure Box Model](http://gprolog.univ-paris1.fr/manual/gprolog.html#sec22)

The Byrd Box Model leaves out a lot of detail - being a model, that's the idea.
But maybe it leaves out a bit too much. The Byrd-Box model says nothing about
what happens "outside the box": cuts, selection of a clause, constraints. It doesn't
even mention the clause head. In particular it says nothing about the operations on
the _term store_, which is, however, a crucial aspect. 

Note that the Byrd Box Model conflates "ports" and "events". One can say that
"port" designates both a "port on some kind of box" as well as the event 
"the execution flow traverses this port". It seems to work.

## From the DECsystem-10 User Manual

Here is the image from the 
[DECsystem-10 Prolog User's Manual](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.300.8430)
of November 1982 on page 13 ("Chapter 2: Debugging")

```text
                *--------------------------------------*
         Call   |                                      |   Exit
     ---------> +  descendant(X,Y) :- offspring(X,Y).  + --------->
                |                                      |
                |  descendant(X,Z) :-                  |
     <--------- +     offspring(X,Y), descendant(Y,Z). + <---------
         Fail   |                                      |   Redo
                *--------------------------------------*
```

The text says (some remarks added):

> During debugging the interpreter prints out a sequence of goals [atomic goals] in various
> states of instantiation in order to show the state the program [process] has reached
> in its execution. However, in order to understand what is occurring it is
> necessary to understand when and why the interpreter prints out goals.
> As in other programming languages, key points of interest are procedure [predicate] entry
> and return, but in Prolog there is the additional complexity of backtracking.
> One of the major confusions that novice Prolog programmers have to face is
> the question of what actually happens when a goal fails and the system suddenly
> starts backtracking. The Procedure Box model of Prolog execution views program
> control flow in terms of movement about the program text [that is not really the
> case, we are in 'box space', not in 'text space']. This model provides a
> basis for the debugging mechanism in the interpreter [more generally, the Prolog
> Processor], and enables the user to view the behaviour of his program in a consistent way.
>
> ...
>
> In terms of this model, the information we get about the procedure box is only
> the control flow through these four ports. This means that at this level we are
> not concerned with which clause matches, and how any subgoals are satisfied,
> but rather we only wish to know the initial goal and the final outcome. However,
> it can be seen that whenever we are trying to satisfy subgoals, what we are
> actually doing is passing through the ports of THEIR respective boxes. If we
> were to follow this, then we would have complete information about the control
> flow inside the procedure box. Note that the box we have drawn round the
> procedure should really be seen as an invocation box [i.e. an activation record].
> That is, there will be a different box for each different invocation of the procedure [predicate].
> Obviously, with something like a recursive procedure [predicate], there will be many different Calls and
> Exits in the control flow, but these will be for different invocations.
> Since this might get confusing each invocation box is given a unique integer
> identifier.

## Explainer 

Let's call that box at the center of the model a _B-Box_ for short.

Consider the Prolog Processor (_PP_) running a Prolog program. The B-Box represents a
_predicate activation record_ rather than a predicate. The latter is a specification,
an element of a Prolog program (essentially text), rather than an element of a
Prolog Processor's run. A predicate activation record is another name for a frame on
the Prolog Processor's execution stack, or a node in Prolog's search tree. Every B-Box
must first be instantiated by the PP before it can be called.

As a Prolog predicate can call other Prolog predicates, the B-Box model is
a recursive "boxes-in-boxes" model. The PP instantiates a B-Box in the context of a 
surrounding B-Box, and deletes it when it backtracks out of the B-Box. Inside a B-Box
the same happens: it's B-Box instantiations, callings and deletions all the way down. Or
at least until you reach the B-Boxes that cannot be decomposed into smaller B-Boxes.
When you think about it, these must be either unifications or operations
not involving unification, like branching decisions, function evaluations or I/O.
More on that below. 

The root B-Box, i.e. the one without any enclosing B-Box, would be the one enclosing the
B-Boxes of the original goal (which is a clause body) entered at the Prolog toplevel.

The B-Box has _internal state_. Evidently there are the currently active sub-B-Boxes, but
there is also the index of the predicate clause that is currently being processed. This
index which will be incremented when the B-Box is asked for a "redo". 

(In some papers on parallel implementations of Prolog, the predicate activation is seen 
as an "agent" in and of itself, communicating over channels (the variables) with other agents,
an interesting view).

Execution flow can be considered as a token passing into one port of an instantiated
B-Box and out of another. In a picture chaining several B-Boxes, the token is transported
over "wires" linking the ports. Whenever the token goes through one of the ports, the
corresponding event is fired and may cause the debugger/tracer to print out messages 
or query the user for interaction.

## The ports

### Traditional ports

The traditional set of ports (and events) is the following. Passing the execution token
left-to-right "grows the stack". Passing the execution token right-to-left "shrinks the
stack".

- **`call`**: Incoming, left-to-right. The B-Box is created by the PP, then called (exactly
  once, actually) through this port. If this is the start of a clause body, there must have
  been a previous box that concluded successfully: the B-Box performing the head unifications.
- **`exit`**: Outgoing, left-to-right (and it should really be called **`succeed`**): The predicate call
  succeeds, and the next B-Box can be instantiated and called. Which B-Box that is
  completely depends on the current state of the surrounding B-Box, which manages branching,
  B-Box instantiations and wiring. If this was the end of a clause body, the execution token
  is transferred to the surrounding B-Box.
- **`redo`**: Incoming, right-to-left. The B-Box line to the right has encountered 
  failure or a collection of results via a meta-predicate like `bagof/3` is ongoing, or
  the user has asked for more solutions on the toplevel after a successful conclusion.
  Passing the execution token back into the B-Box via `redo` may:
  leads to the execution token being passed to `redo` ports of lower-level boxes or
  lead to advancing the B-Box counters to the next clause. If a new
  solution can be found, the term store is updated and the token is passed out via
  `exit`. If no new solution can be found, the token is passed out via `fail`. 
- **`fail`** Outgoing, right-to-left. The B-Box cannot provide any more solutions
  given current conditions. Setting up different terms in the term store for a
  reattempt is left to the predicate instantiation to the left.

Note that the naming of the ports is less than ideal: The port for `exit` should really
be called `succeed`, in analogy to `fail` - and what does `exit` even mean? Missed the
boat on that one!

### The exception port

ISO Standard Prolog specifies exceptions, so one would expected a port like `throw` or
`exception` to be present on the B-Box, too. An execution token exiting via `exception`
will be passed to first enclosing B-Box that can _catch_ it (i.e. unify the thrown term in a 
[`catch/3`](https://eu.swi-prolog.org/pldoc/man?section=exception)). 

### Head unification

There is no mention in the model of the special B-Box performing head unifications. That
B-Box can be considered a series of unifications which must pass. Its execution token 
comes from the surrounding box through `call`, its `fail` passes the token back to the
surrounding box, and its `redo` is just short-circuited to `fail`. 

### Non-traditional ports

SWI-Prolog provides `exception` as well as the additional `unify` 
(see [Overview of the Debugger](https://www.swi-prolog.org/pldoc/man?section=debugoverview)).
`unify` can be seen as an event or as the `exit` port of the special head B-Box: 

- `unify`: allows the user to inspect the result after unification of the head.
- `exception`: shows exceptions raised by `throw/1` or one of the built-in predicates. 

Logtalk additionally distinguishes the `unify` depending on whether the PP is currently
evaluating a rule or a fact
(see [Logtalk - Debugging - Procedure box model](https://logtalk.org/manuals/userman/debugging.html#procedure-box-model)).
Again, `rule` and `fact` can be seen as an events or as the `exit` port of the special head B-Box, 
separated by special case:

- `rule`: unification success between a goal and a rule head
- `fact`: unification success between a goal and a fact
- `exception`: predicate call throws an exception

An appoximate rendering of a clause for which the PP generate the head B-Box, and two additional B-Boxes 
would thus be this:

![Byrd Box Model](byrd_box_model.svg)

## Specific predicate behaviour in the B-Box model

Note these case of predicate behaviour:

- A **deterministic** predicate always has exactly one solution and tells the PP
  that this is indeed the case, that there are no alterntive solutions and
  there is no point asking for more. In the B-Box model, this can be understood thus:
  the token exits at `succeed` but, when going left for some backtracking,
  it does not enter `redo` - it bypasses the B-Box entirely (and this bypassing
  may be chained). This implies there must be some kind of "switch" for the token
  path that is set by the B-Box at `succeed` time. On the Prolog Toplevel,
  a deterministic predicate will not accept a `;` for more solutions, but only say
  `true.` (or else `false.`). Example: `memberchk/2`.
- A **semi-deterministic** predicate may succeed several times but behaves
  deterministically on the last solution. Exploiting internal information, it can tell
  the PP there is nothing more, that it can behave deterministically the
  last time a token goes out via `succeed`. Again, the token emitted from the `redo`
  port on the right can bypass that Byrd Box entirely.
  Example: `member/2`  having had a match on the last item of a list.
- A **non-deterministic** predicate gives nothing away. It demands that the PP
  call `redo` on every backtrack. If there are no solutions after all, it will `fail`.
  This is the behaviour of predicates that have a search space that does not allow 
  to directly determine whether more solutions exist without actually looking for them
  (a problem of epistemology?) Example: `member/2` having a nonempty list left to
  traverse when it found the objectively last solution. These predicates accept `;` 
  on the toplevel but then may say `false`.

## Term Store Operations

### An execution of elementary B-Boxes

The B-Box Model does not talk about the term store updates that take place as a B-Box is active. The term
store is the versioned global (or thread-local) store holding terms, said terms being named/denoted by the
clause-local variables. If a term is no longer named (and not used by a constraint) in any predicate
activation record on the stack, it can evidently be garbage-collected as it has no influence on further
processing.

If you flatten the recursively imbricated boxes of the B-Box Model, you will end up with a chain with two types
of boxes:

- _U-Boxes_: These are boxes which perform a sequence of unifications on the global term store. That modifies the term
  store. Unification operations instantiate uninstantiated terms (fresh terms) at leaf positions of term trees or merge pairs
  of fresh terms into single fresh terms. This extends the term trees in term store downwards (term trees newer
  shrink going forward in computation). The clause-local variables that appear to the left and right of any `=` 
  name the positions of interest in those trees. In particular, clause head B-Boxes are U-Boxes.  
- _V-Boxes_: These are boxes which do not change the global term store, though they may consult it. They perform
  processing other than unification. What processing is this? Branching decisions, I/O and function evaluation,
  including comparison operation on term store elements fall under this.

Both of the above are essentially "elementary B-Boxes" whose inner structure can be disregarded.

### Term Store updates and rollbacks

If you consider a (single-thread) Prolog program execution as a sequence of U-Boxes and V-Boxes, then the U-Boxes
look like (transactional) updates of the term store:

- If any of the unifications fails, term store updates are rolled back and the preceding V-Box is reactived.
- If that V-Box fails, then backtracking rolls the term store back to the version that existed for the
  previous V-Box (or to one even earlier) and said previous V-Box is reattempted.

![Term Store Versioning](term_store_versioning.svg)

For non-elementary B-Boxes, updates on the term store may occur at any point "inside the box", however
the paths of the execution token implies some invariants:

- Going from `call` to `fail`: No updates on the term store are retained. When exiting via `fail` the term
  store is back at the same version as it had when entering via `call`.
- Going from `call` to `succeed`: arbitrary updates (including none) may be performed on the term store.
- Going from `redo` to `succeed`: An arbitrary number of term store rollbacks may be performed, followed
  by an arbitrary number of updates.
- Going from `redo` to `fail`:  When exiting via `fail` the term store is back at the same version as
  it had when entering via `call`.

The above is a direct consequence of what sub B-Boxes and the U-Boxes do, no special handling is
required. On the other hand, a bypass of the B-Box due to determinism or semi-determinism indicates
that a forced rollback of the term store to the version valid at call time must be performed, 
at least in this model.

## See also

- [SWI-Prolog: Overview of the Debugger](https://eu.swi-prolog.org/pldoc/man?section=debugoverview)
- [SWI-Prolog: Deterministic/Semi-deterministic/Non-deterministic predicates](https://www.swi-prolog.org/pldoc/man?section=testbody)

