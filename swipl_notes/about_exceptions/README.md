# Prolog Exceptions

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

## Throwing ISO standard exceptions

[Throwing ISO standard exceptions](throwing_iso_standard_exceptions.md)

## Catch with backtrace

[Catch with backtrace](catch_with_backtrace.md)

## Problems with the ISO standard exception terms

[Problems with the ISO standard exception terms](problems_with_the_iso_standard_exception_terms.md)

## Good idea: Throwing in style and collecting exception texts in one place

[Throwing in style and collecting exception texts in one place](throwing_in_style.md)

## Good idea: Selecting whether to "throw or fail" at runtime

Sometimes context determines whether some code, upon encountering 
problematic situation, should fail or throw. In particular, deterministic predicates
(which always succeed) can only _throw_ to signal a problem, whereas
semi-deterministic or non-deterministic predicates may choose to fail instead.

Suppose we want to use the same code in case a passed argument is found to be
outside its expected domain:

- throw an ISO-standard compliant _domain error_ using predicate
  [`domain_error(+ValidDomain, +Culprit)`](https://eu.swi-prolog.org/pldoc/doc_for?object=domain_error/2) 
  from [`library(error)`](https://eu.swi-prolog.org/pldoc/man?section=error), or
- just fail  
  
This shall be controlled by an option in an _option list_, as expected by
[`library(option)`](`https://eu.swi-prolog.org/pldoc/man?section=option`). The
predicate that extract the option's value from the option list would be
[`option(?Option, +OptionList, +Default)`](https://eu.swi-prolog.org/pldoc/doc_for?object=option/3).

```
optional_domain_error(ValidDomain,Culprit,Options) :-
   option(throw_or_fail(What),Opts,throw),
   ((What==throw) -> existence_error(ObjectType,Culprit) ; fail).   
```

And so:

```
?- optional_domain_error("less than 0",10,[]).
ERROR: Domain error: `less than 0' expected, found `10'
```

```
?- optional_domain_error("less than 0",10,[throw_or_fail(throw)]).
ERROR: Domain error: `less than 0' expected, found `10'
```

```
?- optional_domain_error("less than 0",10,[throw_or_fail(fail)]).
false.
```

## Good idea: Using SWI-Prolog dict in a (non-ISO) exception term

This example code concerning throwing and catching: 

[`catchy.pl`](code/catchy.pl)

demonstrates an exception term of the form `error(Dict,Context)`, where `Dict` carries information 
about the exception according to a (so far informal) convention.

The exception term is still "ISO-like" because it follows the `error(Formal,Context)` convention, 
where `Context` can stay unbound or is instantiated to a backtrace if [`catch_with_backtrace/3`]() 
is used instead of simple [`catch/3`](). 

It is however, non-ISO, in particular because it is based on a non-ISO object, the
SWI-Prolog ['dict'](https://eu.swi-prolog.org/pldoc/man?section=bidicts) (a map structure)).
It will thus not be printed correctly at the toplevel by default (unless an
appropriate handler predicate has been hooked-in ... TODO!). If code unable to deal
with non-ISO exceptions calls code that throws one of these exceptions, there may be
trouble

On the other hand, we gain a great amount of flexibility.

For example, a predicate that throws:

```
funky_2(X,false) :-
   X == 0
   -> throw(error(
         _{class    : "domain_error",
           expected : "Something different from 0",
           got      : "Got 0",
           what     : "Argument 'X'",
           where    : "funky_2/2",
           culprit  : X},
         _Context))
   ; format("funky/2 received ~d\n",[X]).
```

and the corresponding exception handler:

```
goal_of_recovery_2(error(Dict,Context)) :-
   is_dict(Dict),
   !,
   format("Oops, exception of class ~q\n",[Dict.class]),
   (get_dict(expected, Dict, V1)  -> format("Expected : ~q\n",[V1]) ; true),
   (get_dict(got, Dict, V2)       -> format("Got      : ~q\n",[V2]) ; true),
   (get_dict(where, Dict, V3)     -> format("Where    : ~q\n",[V3]) ; true),
   (get_dict(what, Dict, V4)      -> format("What     : ~q\n",[V4]) ; true),
   (get_dict(culprit, Dict, V5)   -> format("Culprit  : ~q\n",[V5]) ; true),
   % contain backtraces if "catch_with_backtrace/3" is used
   (nonvar(Context)    
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
     

