# Problems with the ISO standard exception terms

## Non-uniformity

The ISO-Standard exception terms are not "uniform": they are compound terms of arity 3-1 (including non-compound terms, i.e. atoms)

What would be nice (or at least nicer than is the standard now) is to have error terms like these:

```
error(type_error         ,[~list of mandatory args~],[~list of free args~]).
error(instantiation_error,[~list of mandatory args~],[~list of free args~]).
```

Instead we get variability in number of arguments, together with straight-jacketed terms: 

```
error(permission_error(Arg0,Arg1,Arg2), Context). % 3-argument compound term on first position
error(type_error(Arg0,Arg1), Context).            % 2-argument compound term on first position
error(syntax_error(Arg0), Context).               % 1 argument compound term on first position
error(instantiation_error, Context).              % atom (not 0-argument compound term) on first position
```

## Overspecified `Formal`

We can't append any other information to the ISO-Standard `Formal` term, even though one might be interested in
the name of the variables involved in the exception, or want to transmit some informative message for the user. 
Only the `Context` term can be used.

The ISO-Standard stipulates that atoms chosen from a restricted set must appear in certain positions of `Formal`. This
is unnecessarily restrictive as there is no way the ISO-Standard can list all the possible atoms and information may well have have to 
carried in terms more complex than atoms. Additionally the intended meaning of the listed atoms is undescribed and in some cases is obscure.

### Overbearing formalization puts cart before horse

When you write the part of a predicate that verifies whether the predicate's contract is being respected,
i.e. whether the arguments are usable, it may be difficult to even decide between whether you are in presence
of a "type error" or a "domain error" and even trying to do so may force you to perform inelegant code contortions.
And in the end **you don't care**! Because what will the caller (or even the programmer) actually do with that
information? In fact, detailed information is most likely only of interest to predicates "very near" the throw point,
and they can set up their own task-specific convention.

What anyone and anything somewhat interested in catching and handling the exception wants is:

- That part of the predicate's contract was violated on entry
- Maybe some information about the argument or the argument tuple that cause offense ("which arguments and what were their values")
- Maybe an indication on what to do next, which the thrower might have an idea about

And that's it. This leads to an error term like for example:

```
error(contract_violation(_{ what:vector_length_larger_than_1,
                            where:entry(foo/2),
                            args:[Arg1,Arg2,Arg3], 
                            msg:"The three args must be a complex vector of length ~1" }).
```

This is relaxingly informative but even allowed according to the ISO standard. Moreover the formal would have to
be name `contract_nonviolation`. But of course the thrower wants to tell us about a "contract violation", 
not that it wants to see a "contract nonviolation" ... of course it does. Cognitively, that is *also* backwards.

It should be up to the code which catches the exception to decide whether this is a domain error or type error, but it
probably won't even want to. 

And note: What will the catching predicate do with that? Probably trash everything and ask the user for guidance. It always comes
down to "trash everything and then ask around".

## Missing possibilities.

The ISO-Standard is missing entries for a "can't happen",  "illegal state reached" or "assertion violated". This is just to be
expected, no specification can hope to be complete & precise in an open problem domain like a programming language. 

In SWI-Prolog, some non-ISO exceptions may be encountered. As mentioned, the predicate
[assertion/1](https://eu.swi-prolog.org/pldoc/doc_for?object=assertion/1) throws a non ISO-standard exception with
an exception term `error(assertion_error(Reason,Culprit),Context)`. Others exist, take a look at the error 
text generation in file ` boot/messages.pl`.

If you **have** to invent your own exception consider this (in the context of `library(jpl)`:

Jan Wielemaker writes:

> All errors must use `error(Formal, Context)` because that is what the development environment expects.
> `Formal` should be one of the ISO terms if (reasonably) appropriate. If nothing is appropriate you
> may look in `boot/messages.pl` whether SWI-Prolog already defines something more appropriate or you may
> invent your own `Formal`. In this case my vote would got for `jpl_state_error` with enough arguments
> to provide enough context for a good message. So, not `illegal_state_error`. Look at the ISO terms.
> There is no `illegal_type_error`, bad something, etc. It would be double, we are already talking 
> about an error.

Compare with the Java World: While the Prolog standard doesn't even _have_ a standard exception that can be thrown
by failing tests (SWI Prolog invents its own for `assertion/1`, namely `error(assertion_error(Reason,Culprit),Context)`), 
the Java World tries to standardize _which ones_ should be thrown across testing frameworks:

   - https://github.com/ota4j-team/opentest4j - The project
   - https://github.com/ota4j-team/opentest4j/tree/master/src/main/java/org/opentest4j - The `Exception` classes

## The exception term is not properly associated with a module

In Java, the `toString()`  method is associated to the Exception (at least conceptually), so any code that catches the 
exception can easily format and print it correctly. 

In Prolog, the corresponding Module must define a printing predicate whose head uniquely unifies with the term
carried by the exception and which is called from a "generic printer" that uses the multi-module exception printing
predicate as dispatch table. This works (although how do I do it practically? To be done) but it is ... not very nice.

## Ideally...

If non-ISO is an option, you can be inventive (or not):

```
?- catch(throw("This is my own term and I like it"),C,true).
C = "This is my own term and I like it".
```

The exception term might be better as open-ended representation, for example:

```
error(DistinguishableTypeCode,ListOrTaggedListOrAssocOrDictOfExtraValues).
```

And then you can use lists (or even better, dicts) instead of using compound terms of variable arity to transmit data to the catch point.

```
throw_mystyle_existence_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(mystyle([[error,existence],type-Type,term-Term,pred-Pred,msg-ExText]).
```

## Compare with the Java World

We have a nice, user-extensible exception hierarchy:

![Java Exception Hierarchy](pics/Java_Exception_Hierarchy.svg)

