# Prolog Exceptions

_References to the SWI-Prolog manual pages and more are collected at the tail end of this page._

## Throwing exceptions 

Predicate [`throw/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=throw/1) takes a single argument, the _exception term_:

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

Note that the above exceptions terms use a non-standard formal term but retain the structure of the ISO standard exception term,
`error(Formal,Context)`.

## "It actually works"

As the page for [`throw/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=throw/1) says:

> ISO demands that `throw/1` make a copy of `Exception`, walk up the stack to a `catch/3` call, backtrack 
> and try to unify the copy of `Exception` with `Catcher`.

A copy is made to transport the content of a variable visible at the point-of-throw to the point-of-catch 
while backtracking is performed on anything else.

Here is an example:

```
top(X) :-
   Tip=Fin,               % Create an empty "open list" structure rooted at "Tip" to which we will append
   trial(X,Tip,Fin),
   format("Tip at top/1 has been rolled back to ~q\n",[Tip]).
  
trial(X,Tip,Fin) :-
   Fin=[level1|NewFin],   % The open list grows at "Fin" but stays rooted at "Tip"
   catch(inner(X,Tip,NewFin),
         level1(Tip2,Fin2),
         format("Caught level1(~q,~q) while Tip=~q, Fin=~q\n",[Tip2,Fin2,Tip,Fin])).
   
inner(X,Tip,Fin) :-
   Fin=[level2|NewFin],   % The open list grows at "Fin" but stays rooted at "Tip"
   catch(innerer(X,Tip,NewFin),
         level2(Tip2,Fin2),
         format("Caught level2(~q,~q) while Tip=~q, Fin=~q\n",[Tip2,Fin2,Tip,Fin])).

innerer(X,Tip,Fin) :-
   Fin=[level3|NewFin],   % The open list grows at "Fin" but stays rooted at "Tip"
   catch(innerest(X,Tip,NewFin),
         level3(Tip2,Fin2),
         format("Caught level3(~q,~q) while Tip=~q, Fin=~q\n",[Tip2,Fin2,Tip,Fin])).
   
innerest(X,Tip,Fin) :-
   compound_name_arguments(T,X,[Tip,Fin]),
   format("Throwing ~q\n",[T]),  
   throw(T).
```   

Then

```
?- top(level2).
Throwing level2([level1,level2,level3|_10906],_10906)
Caught level2([level1,level2,level3|_112],_112) while Tip=[level1,level2|_108], Fin=[level2|_108]
Tip at top/1 has been rolled back to [level1,level2|_108]
true.

?- top(level1).
Throwing level1([level1,level2,level3|_1210],_1210)
Caught level1([level1,level2,level3|_1068],_1068) while Tip=[level1|_1054], Fin=[level1|_1054]
Tip at top/1 has been rolled back to [level1|_1054]
true.
```

As one can see, the open list rooted at `Tip` is `[level1,level2,level3|_]` at `innerest/3`, and this
structure is wholly communicated to the catch point (although with different unbound variables due to
copying). On the other other, the catch point only sees the rolled-back original `Tip` after the
catch: `[level1,level2|_108]` or `[level1|_1054]` instead of the whole structure reachable from 
`Tip` when the `throw/1` was performed.

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
semi-deterministic or non-deterministic predicates may choose to _fail_ instead.

Suppose we want to use the same code in case a passed argument is found to be
outside its expected domain, but depending on a "option", the code shall:

- throw an ISO-standard compliant _domain error_ using predicate
  [`domain_error(+ValidDomain, +Culprit)`](https://eu.swi-prolog.org/pldoc/doc_for?object=domain_error/2) 
  from [`library(error)`](https://eu.swi-prolog.org/pldoc/man?section=error), or
- just fail  
  
The option shall be found in an _option list_, as expected by
[`library(option)`](https://eu.swi-prolog.org/pldoc/man?section=option). The
predicate that extract the option's value from the option list would be
[`option(?Option, +OptionList, +Default)`](https://eu.swi-prolog.org/pldoc/doc_for?object=option/3).

The predicate:

```
optional_domain_error(ValidDomain,Culprit,Options) :-
   option(throw_or_fail(What),Opts,throw),
   ((What==throw) -> existence_error(ObjectType,Culprit) ; fail).  % The "else fail" is optional
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

### An alternative to Exceptions: Conditions

SWI-Prolog pack [`condition`](https://eu.swi-prolog.org/pack/list?p=condition) provides a _condition_ system as found in 
[ZetaLisp](https://en.wikipedia.org/wiki/Lisp_Machine_Lisp) and later [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp).

More on [this page](../about_conditions).
   
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
     

