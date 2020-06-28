# SWI-Prolog Exceptions

This page is referenced from the comment on the SWI-Prolog manual page for [`throw/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=throw/1)

## Pages of interest in the SWI-Prolog manual

  - [`library(error)`](https://eu.swi-prolog.org/pldoc/man?section=error) - Has a description of exception meanings & usage
  - [Chapter 4.10: Exception handling](https://eu.swi-prolog.org/pldoc/man?section=exception)
     - [`catch/3`](https://www.swi-prolog.org/pldoc/doc_for?object=catch/3)
     - [`throw/1`](https://www.swi-prolog.org/pldoc/doc_for?object=throw/1)
     - [`catch_with_backtrace/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=catch_with_backtrace/3)
     - [Urgeny of exceptions](https://eu.swi-prolog.org/pldoc/man?section=urgentexceptions)
     - [Debugging and exceptions](https://eu.swi-prolog.org/pldoc/man?section=debugexceptions)
     - [The exception term](https://eu.swi-prolog.org/pldoc/man?section=exceptterm)
     - [Printing messages](https://eu.swi-prolog.org/pldoc/man?section=printmsg) (from exceptions, but can be used more generally)
     - [The exception term](https://www.swi-prolog.org/pldoc/man?section=exceptterm)
  - [B.6 Hooks using the exception predicate](https://eu.swi-prolog.org/pldoc/man?section=exception3)
  - [A.14 library(debug): Print debug messages and test assertions](https://eu.swi-prolog.org/pldoc/man?section=debug)
     - [assertion/1](https://eu.swi-prolog.org/pldoc/doc_for?object=assertion/1)

## Some reading

  - [Coding Guidelines for Prolog](https://arxiv.org/abs/0911.2899) offers a bit of commentary on _when_ to throw, but does not go further.

## Throwing exceptions 

Predicate [`throw/1`](https://www.swi-prolog.org/pldoc/doc_for?object=throw/1) takes a single argument, the _exception term_:

```text
throw(+Exception).
```

User predicates are free to choose the structure of their exception terms (i.e. they can define their own conventions) but _should_
adhere to the ISO-Standard if possible, in particular for libraries.

SWI-Prolog built-in predicates throw exception terms as specified by the ISO-Standard unless the exception does not fit any of
the ISO Standard error term definitions.

In particular [assertion/1](https://eu.swi-prolog.org/pldoc/doc_for?object=assertion/1) throws a non ISO-standard exception with
an exception term `error(assertion_error(Reason,Culprit),Context)`. Note that in this case the exception term reflects the
structure of a ISO-standard exception term.

## Throwing ISO-Standard exceptions 

### The ISO-Standard exception term

The ISO Standard stipulates that the exception term be of the form `error(Formal, Context)`.

- The `Formal` term is the formal description of the error, specifying the error class and error context information. 
  It may be an atom or variously a compound term of arity 1,2 or 3. The ISO Standard lists the admissible `Formal` terms in chapter
  7.12.2 pp. 62-63 ("Error classification").
  
- The `Context` term, if set (i.e. if not a fresh variable), gives additional context information to help the programmer
  in debugging or to allow error-handling routines to decide what to do next. The structure of `Context` is left unspecified
   y the ISO Standard.

The `Context` is generally of the form `context(Name/Arity, Message)`, where `Name/Arity` is the predicate indicator of the 
built-in predicate that raises the error, and `Message` provides an additional description of the error.

Any part of this structure may be a fresh variable if no appropriate information exists.

We thus have the following term hierarchy:

```prolog
thrower(FormalFunctor,FormalArgs,PredInd,Msg) :-
    compound_name_arguments(Formal,FormalFunctor,FormalArgs),  % Formal = FormalFunctor(FormalArgs...) (very variable)
    compound_name_arguments(Context,context,[PredInd,Msg]),    % Context = context(PredInd,Msg)
    compound_name_arguments(Exception,error,[Formal,Context]), % Exception = error(Formal,Context)
    throw(Exception).
```

- The ISO-Standard document "ISO/IEC 13211-1, 1st edition 1995-06-01" lists the error terms in Chapter 7.12, pages 61 ff.
- The following page substantially _is_ the ISO Standard text: https://www.deransart.fr/prolog/exceptions.html 
- Ulrich Neumerkel lists the various error classes [here](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/error_k).
  The context is a discussion leading up to the second corrigendum of the ISO standard.

### Using `library(error)` 

ISO-Standard exceptions can be thrown with [`library(error)`](https://www.swi-prolog.org/pldoc/man?section=error).
It exports predicates which (as terms) look exactly like the ISO-Standard `Formal` part of the corresponding exception term.

The following predicates exist (in order of appearance in the ISO Standard):

  - [instantiation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=instantiation_error/1)
     - `instantiation_error(+FormalSubTerm)` 
     - the `FormalSubTerm` is not currently exploited as the standard demands the `Formal` be the atom `instantiation_error` only
  - [uninstantiation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=uninstantiation_error/1)
     - `uninstantiation_error(+Culprit)`      
     - appears in Corrigendum 2: ISO/IEC 13211-1:1995/Cor.2:2012(en)
  - [type_error/2](https://eu.swi-prolog.org/pldoc/doc_for?object=type_error/2)
     - `type_error(+ValidType,+Culprit)`
  - [domain_error/2](https://eu.swi-prolog.org/pldoc/doc_for?object=domain_error/2)
      - `domain_error(+ValidDomain,+Culprit)`
  - [existence_error/2](https://eu.swi-prolog.org/pldoc/doc_for?object=existence_error/2)
      - `existence_error(+ObjectType,-Culprit)`
  - [existence_error/3](https://eu.swi-prolog.org/pldoc/doc_for?object=existence_error/3) (**not** ISO)
      - `existence_error(+ObjectType,+Culprit,+Set)`
      - the third argument `Set` makes this a non-ISO exception. `Set` goes into the `Formal` like this: `Formal=existence_error(ObjectType,Culprit,Set)`
  - [permission_error/3](https://eu.swi-prolog.org/pldoc/doc_for?object=permission_error/3)
      - `permission_error(+Operation,+PermissionType,+Culprit)`
  - [representation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=representation_error/1)
      - `representation_error(+Flag)`
  - [resource_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=resource_error/1)
      - `resource_error(+Resource)`
  - [syntax_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=syntax_error/1)
      - `syntax_error(+ImplDepAtom)`
      
Note that there is no facility for **catching standard errors**, which is done by
a successful unification of the a thrown term with a "catcher" term passed to
[`catch/3`](https://www.swi-prolog.org/pldoc/doc_for?object=catch/3). 

The generator for user-readable error messages based on the exception term can be found in `boot/messages.pl`, where there is code like this:

```prolog
iso_message(resource_error(Missing)) -->
    [ 'Not enough resources: ~w'-[Missing] ].
iso_message(type_error(evaluable, Actual)) -->
    { callable(Actual) },
    [ 'Arithmetic: `~p'' is not a function'-[Actual] ].
iso_message(type_error(free_of_attvar, Actual)) -->
    [ 'Type error: `~W'' contains attributed variables'-
      [Actual,[portray(true), attributes(portray)]] ].
```

### List of the ISO-Standard exception term

> Most errors defined in this part of ISO/IEC 13211 occur because the arguments of the goal fail to satisfy a particular
> condition; they are thus detected before execution of the goal begins, and no side effect will have taken place.
> The exceptional cases are: Syntax Errors, Resource Errors, and System Errors.

In order of appearance in the ISO-Standard:

#### Instantiation Error

An argument or one of its components is uninstantiated, but an instantiated argument or component is required instead.
In effect, "I need more data".

```
Formal = instantiation_error
```

The `Formal` is just the atom `instantiation_error`. One cannot communicate to the caller
which argument is the one that failed, and one _has_ to use `Context` to do that.
This exception is pointlessly deficient in information carrying capacity and the specification needs improvement!

Thrown with [instantiation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=instantiation_error/1). The argument passed
to `instantiation_error/1` is just for future extensions and remains unused.

#### Uninstantiation error

An argument or one of its components is instantiated, and an uninstantiated argument or argument component is required.  

```
Formal=uninstantiation_error(Culprit)
```

- `Culprit` is the argument or one of its components which caused the error.

Thrown with [uninstantiation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=uninstantiation_error/1).

#### Type Error

An argument or one of its components is instantiated but incorrect.

```
Formal=type_error(ValidType,Culprit)
```

- `ValidType` is an atom chosen from
 `[atom, atomic, byte, callable, charactder, compound, evaluable, in_byte, in_character, integer, list, number, predicate_indicator, variable]`.
- `Culprit` is the argument or one of its components which caused the error.

Thrown with [type_error/2](https://eu.swi-prolog.org/pldoc/doc_for?object=type_error/2)

_Personal Note:_ The "list" is the odd one in the above, because "list" is not a type but a convention on how a term is supposed to look
like non-locally. It would make more sense to generate a "Domain Error" if an argument turns out to not be a list.  
epecially as there is already a "non_empty_list" Domain Error. Oh well!

#### Domain Error

An argument's type is correct but the value is outside the domain for which the procedure is defined. ("Domain Error occurs when the value is not a a member of an implementation defined or implementation-dependent set.")


_Personal Note:_ This exception lacks the possibility to properly express being outside the allowed subdomain
if that subdomain is spanned by several arguments instead of just one.

```
Formal=domain_error(ValidDomain,Culprit)
```

- `ValidDomain` is an atom chosen from
 `[character_code_list, close_option, flag_value, io_mode, non_empty_list, not_less_than_zero, operator_priority, operator_specifier, prolog_flag, read_option, source_sink, stream, stream_option, stream_or_alias, stream_position, stream_property, write_option]`.
- `Culprit` is the argument or one of its components which caused the error.

Thrown with [domain_error/2](https://eu.swi-prolog.org/pldoc/doc_for?object=domain_error/2)

#### Existence Error

An object on which an operation is to be performed does not exist. 

```
Formal=existence_error(ObjectType,Culprit)
```

- `ObjectType` is an atom chosen from `[procedure, source_sink, stream]`.
- `Culprit` is the argument or one of its components which caused the error.

Thrown with [existence_error/2](https://eu.swi-prolog.org/pldoc/doc_for?object=existence_error/2).

SWI-Prolog has a non-ISO-conformant extension with an enlarged `Formal`

```
Formal=existence_error(ObjectType,Culprit,Set)
```

Thrown with [existence_error/3](https://eu.swi-prolog.org/pldoc/doc_for?object=existence_error/3).
 
#### Permission Error

The runtime system (or the thread) is lacking permission to perform a specific operation.

```
Formal=permission_error(Operation,PermissionType,Culprit)
```

- `Operation` is an atom chosen from `[access, create, input, modify, open, output, reposition]`.
- `PermissionType` is an atom chosen from `[binary_stream, flag, operator, past_end_of_stream, private_procedure, static_procedure, source_sink, stream, text_stream]`
- `Culprit` is the argument or one of its components which caused the error.

Thrown with [permission_error/3](https://eu.swi-prolog.org/pldoc/doc_for?object=permission_error/3).

_Personal Note:_ The intended semantics of the atoms listed are not clear at all. Recommendation: Don't stick closely to this over-specification.

#### Representation Error

An implementation-defined limit has been breached. 

```
Formal=representation_error(Flag)
```

- `Flag` is an atom chosen from `[character, character_code, in_character_code, max_arity, max_integer, min_integer]`.

Thrown with [representation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=representation_error/1).

#### Evaluation Error

The operands of an evaluable functor are such that the operation has an exceptional value or event.

```
Formal=evaluation_error(Error)
```

- `Error` is an atom chosen from `[float_overflow, int_overflow, undefined, underflow, zero_divisor]`.

Thrown with [representation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=representation_error/1).

_Personal Note:_ This does not seem to cover all of the IEEE 754 exceptional cases.

#### Resource Error

The runtime system has insufficient resources to complete execution. A resource error
may happen for example when a calculation on unbounded integers has a value which
is too large.

```
Formal=resource_error(Resource)
```

- `Resource` denotes an implementation-dependent atom.

Thrown with [resource_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=resource_error/1).

#### Syntax Error

A sequence of characters which are being input as a read-term do not conform to an acceptable syntax. "Being input as a read-term" means
this error is thrown by predicates like [`read/1`](https://www.swi-prolog.org/pldoc/doc_for?object=read/1) which are about
deserializatoin of terms.

```
Formal=syntax_error(ImplDepAtom)
```

- `ImplDepAtom` denotes an implementation-dependent atom.

Thrown with [syntax_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=syntax_error/1). 

#### System Error

Can happen at any point of computation. The conditions for a System Error and the actions taken by a Prolog runtime 
after occurrence are implementation-dependent. A System Error may happen for example 

- in interactions with the operating system (for example, a disc crash or interrupt), or
- when a goal `throw(T)` has been executed and there is no active goal `catch/3`. 

The `Formal` is a parameterless atom (which means one has the same problems as for `instantiation_error`).

```
Formal=system_error
```

There is no corresponding `library(error)` predicate. "System Error" should not be thrown from user code. The counterpart in the Java world would be [Error](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Error.html).

## Examples

Here we throw an exception using the appropriate exception-throwing predicates from `library(error)` 
Note they are not called `throw_type_error/2` etc. just `type_error/2` etc, which may cause some
confusion on reading. We catch the exception in the catcher term _C_ which unifies with anything.
`C` is then printed by the Prolog toplevel.

```prolog
?- catch(type_error(type,term),C,true).
C = error(type_error(type, term), _2844).
```

```prolog
?- catch(domain_error(type,term),C,true).
C = error(domain_error(type, term), _3974).
```

```prolog
?- catch(existence_error(type,term),C,true).
C = error(existence_error(type, term), _5108).
```

```prolog
?- catch(permission_error(action,type,term),C,true).  % 3 args!
C = error(permission_error(action, type, term), _1314).
```

```prolog
?- catch(instantiation_error(term),C,true). % noargs! The term is not passed along currently
C = error(instantiation_error, _7032).
```

```prolog
?- catch(syntax_error(term),C,true). % 1 args!
C = error(syntax_error(term), _8162).
```

## Some problems with the ISO-Standard exception terms

### Non-uniformity

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

### Overspecified `Formal`

We can't append any other information to the ISO-Standard `Formal` term, even though one might be interested in
the name of the variables involved in the exception, or want to transmit some informative message for the user. 
Only the `Context` term can be used.

The ISO-Standard stipulates that atoms chosen from a restricted set must appear in certain positions of `Formal`. This
is unnecessarily restrictive as there is no way the ISO-Standard can list all the possible atoms and information may well have have to 
carried in terms more complex than atoms. Additionally the intended meaning of the listed atoms is undescribed and in some cases is obscure.

### Missing possibilities.

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

### Ideally...

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

## Good ideas

### Avoiding cleartext errors littering you code

To collect the cleartext error messages outside of the `throw/1` calls, just do this:

```
% Set up some informative atoms, and make them retrievable by code

exception_code(e01,changing_from_start_to_end_but_step_is_zero).
exception_code(e02,going_to_plus_infinity_but_step_is_zero).
exception_code(e03,going_to_plus_infinity_but_step_is_negative).
exception_code(e04,going_to_minus_infinity_but_step_is_zero).
exception_code(e05,going_to_minus_infinity_but_step_is_positive).
exception_code(e06,decreasing_from_start_to_end_but_step_is_positive).
exception_code(e07,increasing_from_start_to_end_but_step_is_negative).
```

Then we can do simple throws like these, advantageously replacing those of `library(error)`:

```
% ===
% Predicates that throw. "Pred" is generally the predicate descriptor of the
% predicate from where the throw originates.
% ===

throw_instantiation_error(Pred,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(instantiation_error,context(Pred,ExText))).

throw_domain_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(domain_error(Type,Term),context(Pred,ExText))).

throw_type_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(type_error(Type,Term),context(Pred,ExText))).

throw_existence_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(existence_error(Type,Term),context(Pred,ExText))).

throw_permission_error(Pred,Action,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(permission_error(Action,Type,Term),context(Pred,ExText))).

% This is not ISO but we need it.
% It replaces throwing a system_error with parameters which is
% also not ISO, and also incorrect: system_error should be used when
% the harddisk crashes, not when the assertion fails.

throw_state_error(Pred,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; throw(ExText = ExCode)),
   throw(error(state_error,context(Pred,ExText))).
```

### Selecting throwing or faling at runtime

We can add wrappers to fail or throw depending on options list contents.
If `Options` is a list containing the atom `throw`, then throw, else fail:

```
throw_existence_error(Pred,Type,Term,ExCode,Options) :
   (nonvar(Options),memberchk(throw,Options))
   ->
   throw_existence_error(Pred,Type,Term,ExCode)
   ;
   fail. % actually unnecessary to write this
```

