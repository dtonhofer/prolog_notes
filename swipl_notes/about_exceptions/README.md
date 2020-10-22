# SWI-Prolog Exceptions

_References to the SWI-Prolog manual pages and more are collected at the tail end of this page._

## Throwing exceptions 

Predicate [`throw/1`](https://www.swi-prolog.org/pldoc/doc_for?object=throw/1) takes a single argument, the _exception term_:

```text
throw(+Exception).
```

User predicates are free to choose the structure of their exception terms (i.e. they can define their own conventions) but _should_
adhere to the ISO-Standard if possible, in particular for libraries.

SWI-Prolog built-in predicates throw exception terms as specified by the ISO standard unless the exception does not fit any of
the ISO standard error term definitions.

In particular [`assertion/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=assertion/1) throws a non ISO standard 
exception term `error(assertion_error(Reason,Culprit),Context)`. 

Another non-ISO standard exception term is thrown by [`dict_pairs/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_pairs/3)
(and probably other dict-handling predicates): `error(duplicate_key(Key),Context)`.

Note that the above exceptions terms use a non-standard formal term but retain the structure of the ISO standard exception term.

## "It actually works"

As the page for [`throw/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=throw/1) says:

> ISO demands that `throw/1` make a copy of `Exception`, walk up the stack to a `catch/3` call, backtrack 
> and try to unify the copy of `Exception` with `Catcher`.

It not only "makes a copy", but "makes a copy that survives backtracking to the catch point" as otherwise
one would never see the values bound to the variables that can be found in the catcher term. It's quite "un-Prolog-y" in fact.

See also: [Salvaging a term out of a dropped search branch](../about_salvaging_a_term_out_of_a_dropped_search_branch)

## Catch with backtrace

How do we get a backtrace and how does the exception term have to look to get one?

The backtrace is filled in according to SWI-Prolog conventions because the ISO Standard has nothing to say about this. 

SWI-Prolog wants the second argument of the `error/2` term (given in the ISO standard as `Imp_def`) to look
like `context(Location,Message)`. If `Location` is fresh and the catch is performed
by [`catch_with_backtrace/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=catch_with_backtrace/3) (which happens
either explicity in code or at the latest possible time at the Prolog Toplevel), `Location`
is filled with a backtrace (as implemented by `library(prolog_stack)`  in file `swipl/lib/swipl/library/prolog_stack.pl`).
The `Message` is generally a cleartext message (string or atom).

Take this program:

```prolog
call0(ExceptionTerm) :- 
   call1(ExceptionTerm).
   
call1(ExceptionTerm) :- 
   call2(ExceptionTerm).
   
call2(ExceptionTerm) :- 
   throw(ExceptionTerm).
```

Enable debugging to keep the compiler from optimizing-away stack frames. 

```text
?- debug.
```

Let's study the behaviour of "backtrace generation" by `catch_with_backtrace/3` with various forms of `ExceptionTerm`. We will let the
exception be caught at the Prolog Toplevel, which uses that predicate.

### Non-ISO-standard exception term without placeholder

No backtrace is generated, there is minimal printing at toplevel:

```text
?- call0("deep in a search tree").
ERROR: Unhandled exception: "deep in a search tree"
```

### Quasi-ISO-standard exception term `error(_,_)`

An exception term that looks like `error(_,_)` matches the ISO Standard basic format, although the requirements
regarding the formal term on the first position have to be followed too for full compliance.

The second argument is set to `context(B,_)` where `B` contains a backtrace.

The toplevel tries validly to print something in the first line, but has to admit that it found an `Unknown error term`:

```text
[debug]  ?- call0(error("deep in a search tree",Context)).

ERROR: Unknown error term: "deep in a search tree"
ERROR: In:
ERROR:   [13] throw(error("deep in a search tree",_4126))
ERROR:   [12] call2(error("deep in a search tree",_4156)) at user://1:14
ERROR:   [11] call1(error("deep in a search tree",_4186)) at user://1:11
ERROR:   [10] call0(error("deep in a search tree",_4216)) at user://1:8
ERROR:    [9] <user>
   Exception: (13) throw(error("deep in a search tree", _3266)) ? Exception details
==
```

Asking for "exception details" using `m` reveals that `Context` has been filled in with a 
term `context(prolog_stack(Frames),_)` as the exception term looks as follows:

```text
error("deep in a search tree",
      context(
         prolog_stack([
            frame(13,call(system:throw/1),throw(error("deep in a search tree",_4126))),
            frame(12,clause(<clause>(0x1a7ef30),4),call2(error("deep in a search tree",_4156))),
            frame(11,clause(<clause>(0x1a9ccd0),4),call1(error("deep in a search tree",_4186))),
            frame(10,clause(<clause>(0x1a368c0),4),call0(error("deep in a search tree",_4216))),
            frame(9,clause(<clause>(0x18f7450),3),'$toplevel':toplevel_call(user:user: ...))]),_4082))
```

### Quasi-ISO-standard exception term with SWI-Prolog context term `error(_,context(_,_))`

The same as above, we just have SWI-Prolog specific `context/2` subterm already in the
ISO-standard specific `error/2` term. 

We can put a generic message in the second argument of `context/2`. In this example, a String:

```text
[debug]  ?- call0(error("deep in a search tree",context(_,"get me outta here"))).
ERROR: Unknown error term: "deep in a search tree" (get me outta here)
```

### ISO-standard exception term with SWI-Prolog context term `error(IsoFormal,context(_,_))` 

As above, backtrace and all, except that now error message generation is correct as it is based on the 
list of valid ISO formal terms:

```text
[debug]  ?- call0(error(instantiation_error,context(_,"get me outta here"))).
ERROR: Arguments are not sufficiently instantiated (get me outta here)
ERROR: In:
ERROR:   [13] throw(error(instantiation_error,context(_3028,"get me outta here")))
ERROR:   [12] call2(error(instantiation_error,context(_3064,"get me outta here"))) at user://1:14
ERROR:   [11] call1(error(instantiation_error,context(_3100,"get me outta here"))) at user://1:11
ERROR:   [10] call0(error(instantiation_error,context(_3136,"get me outta here"))) at user://1:8
ERROR:    [9] <user>
```



## Good idea: Selecting whether to "throw or fail" at runtime

Sometimes context determines whether some code, upon encountering an identical
problematic situation, should fail or throw. For example, deterministic predicates
(always succeeding) can only throw to signal a problem (the meaning of which would
then be that a computational problem was encountered, rather a problem in any problem
modeled in logic), whereas semi-deterministic or non-deterministic predicates may choose
to fail instead.

We can add wrappers to throw or fail depending on options list contents.
If `Options` is a list containing the atom `throw`, then throw, else fail:

```
throw_existence_error(Pred,Type,Term,ExCode,Options) :
   (nonvar(Options),memberchk(throw,Options))
   ->
   throw_existence_error(Pred,Type,Term,ExCode)
   ;
   fail. % actually unnecessary to write this, but it's good for the next programmer
```
## Reading

### Coding guidelines say nothing

[Coding Guidelines for Prolog](https://arxiv.org/abs/0911.2899) offers a bit of commentary on _when_ to throw, but does not go further.

### An alternative to Exceptions

[Package 'condition'](https://eu.swi-prolog.org/pack/list?p=condition) provides a _Condition_ system as in Common Lisp. (I haven't tried it yet)
   
### Pages of interest in the SWI-Prolog manual

  - [`library(error)`](https://eu.swi-prolog.org/pldoc/man?section=error) - Has a description of exception meanings & usage
  - [Chapter 4.10: Exception handling](https://eu.swi-prolog.org/pldoc/man?section=exception)
     - [`catch/3`](https://www.swi-prolog.org/pldoc/doc_for?object=catch/3)
     - [`throw/1`](https://www.swi-prolog.org/pldoc/doc_for?object=throw/1)
     - [`catch_with_backtrace/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=catch_with_backtrace/3)
     - [Urgency of exceptions](https://eu.swi-prolog.org/pldoc/man?section=urgentexceptions)
     - [Debugging and exceptions](https://eu.swi-prolog.org/pldoc/man?section=debugexceptions)
     - [The exception term](https://eu.swi-prolog.org/pldoc/man?section=exceptterm)
  - [B.6 Hooks using the exception predicate](https://eu.swi-prolog.org/pldoc/man?section=exception3)
  - [A.14 library(debug): Print debug messages and test assertions](https://eu.swi-prolog.org/pldoc/man?section=debug)
     - [assertion/1](https://eu.swi-prolog.org/pldoc/doc_for?object=assertion/1)
  - Code which translates exception terms to cleartext messages (generally on the Prolog Toplevel), via DCG:  
     - [`term_message//1`](https://www.swi-prolog.org/pldoc/doc/_SWI_/boot/messages.pl?show=src#term_message//1).
     - This is file `${DISTRO}/lib/swipl/boot/messages.pl`
     - See also this report about `jpl.pl` by Jan Burse: [Issue#59](https://github.com/SWI-Prolog/packages-jpl/issues/59)
  - [Printing messages](https://eu.swi-prolog.org/pldoc/man?section=printmsg) from exceptions, but can be used more generally. See 
    also Anne Ogborn's [Tutorial](http://www.pathwayslms.com/swipltuts/message/index.html) on printing messages
     
## Compare with

### From the Java World: The Exception Hierarchy

![Java Exception Hierarchy](pics/Java_Exception_Hierarchy.svg)


