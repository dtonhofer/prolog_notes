# About conditions

** In progress **

Michael Hendrix has written SWI-Prolog pack [`condition`](https://www.swi-prolog.org/pack/list?p=condition),
_Condition system à la Common Lisp_. This code is from 2014.

- [Michael Hendrix' page for this pack](http://packs.ndrix.com/condition/).
- The manual page generated from the annotated source: [`condition.pl`](http://packs.ndrix.com/condition/condition.html).
- The latest download URL: http://packs.ndrix.com/condition/condition-0.1.1.tgz (see below for installation)
- The 2001 paper _Condition Handling in the Lisp Language Family_ by Kent M. Pitman, who wrote the condition system for Common Lisp: 
  - appears in: [_Advances in Exception Handling Techniques_](https://www.springer.com/gp/book/9783540419525), (Springer Lecture Notes in Computer Science (LNCS) 2022), 2001.
  - in HTML [here](http://www.nhplace.com/kent/Papers/Condition-Handling-2001.html)
- Also by Kent M. Pitman (while at Symbolics Inc.), a paper from 1990, _Exceptional Situation in Lisp_:
  - appears in: _Proceedings for the First European Conference om the Practical Application of Lisp (EUROPAL'90)_. Churchill College, Cambridge, UK, March 27-29, 1990.
  - in HTML [here](https://nhplace.com/kent/Papers/Exceptional-Situations-1990.html). 

## Desc

Pack "condition" is a way to provide "implicit context information" to a thread.

The thread can ask for context information at any given point (_signal_), in particular
if a problematic or unexpected situation (an _error condition_) occurs. This can be more
appropriate than just throwing an exception: the handling of the problematic situation 
can be done at the point (in time an in code) where the problem occurs. By contrast, after
an exception has been thrown, the stack has been unwound and any handling code can only
rely on second.hand information carried by the exception description and possibly the
(now post-mortem) stacktrace.

_Handlers_ previously registered by callers higher up the stack will be called to resolve
the situation (to _provide restarts_). 

In Common Lisp, the situation is resolved when a handler _accepts the signal_ and branches
to a handling code (which may involve actions like cleaning up, invoking the debugger,
asking the user for missing information, throwing an exception etc.) If no handler 
accepts the signal, a default action (generally thrown an exception) is taken. A handler
which just returns is considered as having rejected the signal.

In (pre-Common Lisp) Lisp Machines Zetalisp, the situation is resolved by handlers 
providing _restarts_ (advice on how to proceed) and the code decides which advice to
follow (so it has some kind of deconflicting strategy). This is more indirect and was not 
taken up for Common Lisp.

Vocabulary:

- The program encounters an exceptional situation that it is not ready to handle by default (e.g. I/O error)
- A decision has to be made where in the program code to continue (these locations are restarts, and
  they are basically "continuations", a reified way on what to do next - e.g. an exception handler)

- It signals this fact (by calling a function or a predicate, in this pack, `signal/3` or `signal/2`)
- The pogram asks for help (signals) by calling an appropriate signal function.
- The system consults previously installed handlers about what to do.
- The handler transfers control to a continuation if it decides to handle the situation, otherwise
  it just returns.

From Kent Pitman's 2001 paper:

> The process of programming is about saying what to do in every circumstance. 
> In that regard, a computer has been sometimes characterized as a “relentless judge of 
> incompleteness”. When a program reaches a place where there are several possible next
> steps and the program is unwilling or incapable of choosing among them, the program has
> detected an exceptional situation.
> 
> The possible next steps are called restarts. Restarts are, effectively, named continuations.
>
> The process of asking for help in resolving the problem of selecting among the possible next steps
> is called signaling.

> The independently contributed pieces of code which are consulted during the signaling process are
> called handlers. In Common Lisp, these are functions contributed by the dynamic call chain that
> are tried in order from innermost (i.e., most specific) to outermost (i.e., most general). 
> Each handler is called with an argument that is a description of the problem situation. The handler
> will transfer control (by GO, RETURN or THROW) if it chooses to handle the problem described by 
> its argument.
> 
> Think of the process of signaling and handling as analogous to finding a
> fork in a road that you do not commonly travel. You don’t know which way to
> go, so you make known your dilemma, that is, you signal a condition. Various
> sources of wisdom (handlers) present themselves, and you consult each, placing
> your trust in them because you have no special knowledge yourself of what to do.
> Not all sources of wisdom are experts on every topics, so some may decline to
> help before you find one that is confident of its advice. When an appropriately
> confident source of wisdom is found, it will act on your behalf. The situation has
> been handled.

### Extend it?

The use of this idiom may well go beyond error handling as it is about providing 
generic call-stack context-dependent advice to a currently active procedure or predicate.

One can apply the same principle for example to:

- _Logging_: Find the currently valid logging handler, which may change during the program
  run and when descending the call stack.
  
- _I/O_: Find the currently valid input oand output streams. SWI-Prolog provides the variables
  current_input and current_ouput but one can consider getting them from a context
  
- _General search problems_. The handlers may provide heuristics valid at a given search point.

## Pack installation

I will call this a _pack_ rather than a _package_. In SWI-Prolog _packages_ are software packages that come
with the SWI-Prolog "core", rather than 3rd-party software packages: See the [packages page](https://eu.swi-prolog.org/pldoc/doc_for?object=packages).

To install the "condition" pack:

- Download the file `condition-0.1.1.tgz`
- Check its hashsum (`sha1sum condition-0.1.1.tgz` must yield `6f1204f9185773489da29ce66b40534c7c0fd32a`)
- Call [`pack_install/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=pack_install/1) with the filename as argument. 

Alternatively, you may just want to do everything in one step with `pack_install(condition).`. 

The pack contents appears in a system-dependent config folder. On my machine:

```
$HOME/.local/share/swi-prolog/pack/condition/
├── History.md            - Change log
├── pack.pl               - Pack meta-information file, represented through Prolog facts (the "MANIFEST")
├── prolog
│   └── condition.pl      - The annotated code
├── README.md
└── t
    └── examples.pl       - Tests based on package "tap"
```


The "condition" pack needs the ["lambda" pack](https://www.swi-prolog.org/pack/list?p=lambda) to process a lambda expression. SWI-Prolog
has a differing syntax for lambda expression in the autoloaded [`library(yall)`](https://eu.swi-prolog.org/pldoc/man?section=yall)
so you might want to choose to modify the "condition" pack instead of installating the "lambda" pack. 

This is easily done. Change the clause for `handle/3` to:

```
handle(Goal, Condition, Restart) :-
    handle(Goal, {Condition,Restart}/[C,R]>>(C=Condition -> R=Restart)).
```

The "condition" pack has test code that needs pack [`tap`](https://www.swi-prolog.org/pack/list?p=tap). 
It is not needed to run the `condition.pl` code.

The following Prolog predicates are exported:

- handle/2       - handle(Goal,Restarter): setup_call_cleanup/3 around Goal by adding 
                   a handler using add_handler/2, brackets a subtree with a handler (name could be chosen better)
- handle/3       - handle(Goal, Condition, Restart): setup_call_cleanup/3 around a specific
                   Restarter which unifies Condition with the siganlled condition (guard)
                   and then (->) unified Restart with the signalled Restart (so, success if
                   the signalled condition and restart unify). Useful when the restarter just
                   needs to provide a known value for a known condition.

- signal/2       - A thread in difficulty calls `signal(Condition,Restart)`, which
                   backtracks over registered rules with head `handler(Condition,Restart)`.
                   `Restart` is advice on the action to take. It is thus bound to successive values by the registered handlers 
                   which unify with Condition, latest (innermost) registered first. The signaller can
                   use the first one, consult them all etc. It might happen that the handler throws, too.
                   If there are none registered, or they all fail, `Condition`is thrown.

- signal/3       - Same as signal/2 but if Condition is thrown (i.e. if there are no handlers),
                   then a Default value (2nd argument position) is unified with Restart.
                   signal(Condition,Default,Restart).

- add_handler/2  - Add a handler entry to the local thread-local Prolog database add_handler(Restarter,Ref)
- rm_handler/1   - Remove a handler entry from the thread-local Prolog database  rm_handler(Ref)

You will want to install handlers "going down" and uninstall handlers "coming up" (N.B. you "come up"
when the subtree has been fully explored with all choicpoints closed or an exception has been generated)
using [`setup_call_cleanup/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=setup_call_cleanup/3) or
[`setup_call_catcher_cleanup/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=setup_call_catcher_cleanup/4).

signal(Condition,Restart) 
  --calls--> handler(Condition,Restart)                 (the asserted rule head)
             ----> call(Restarter,Condition,Restart)    (the asserted rule body, Restarter was given to add_handler)
                   Restarter(Condition,Restart)         Restarter may throw, or provide Restart advice to the signalling procedure via Restart, or call a continuation etc.
                                                        this is a mix of Zeta Lisp and Common Lisp approach
                                                        if restarter fails, the next restarter in line is tried (as per *->)
                                                        Note that the Restarter may not necessarily be interested in the Condition,
                                                        so there needs to be a guard first: Condition=WantedCondition (or ==)
  --calls--> handler(Condition,Restart)                 backtracking if the signalling predicate wants it: the next handler matching Condition is called (as per *->)

## Implementation

Implementaion is done by using a thread-local Prolog clause database in the "condition"
module. New clauses to find handlers are added "in priority position" (at the top of the list
of clauses) with [`asserta/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=asserta/2)) when
going "down the stack" and removed "on return", i.e. when the whole search subtree is spent (all
solutions have been generated and no choicpeoints are left or an exception has been thrown).

Coroutines running synchronously using the same thread will see the same settings.

## Why not use Delimited Continuations?

The condition pack can be rewritten using [delimited continuations](https://eu.swi-prolog.org/pldoc/man?section=delcont).

# Improvements 

The handler might want to examine the "stack of handlers" itself.


