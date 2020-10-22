# Throwing ISO standard exceptions 

## The ISO-Standard exception term

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

## Using `library(error)` 

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

If you want to decorate the exceptoin with more context information, take a look
at [Adding context to errors: prolog_exception_hook](https://eu.swi-prolog.org/pldoc/man?section=excepthook), 
which allows to do that before the jump to the approriate `catch/3` is performed.

The generator for user-readable error messages based on the exception term can be found 
in `boot/messages.pl`, where there is code like this:

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

You may be able to change the printing of messages based on exception. See
[4.10.4 Printing messages](https://eu.swi-prolog.org/pldoc/man?section=printmsg) and
in particular [Predicate message_hook/3](https://eu.swi-prolog.org/pldoc/doc_for?object=message_hook/3).

### Examples

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

## List of the ISO-Standard exception term

> Most errors defined in this part of ISO/IEC 13211 occur because the arguments of the goal fail to satisfy a particular
> condition; they are thus detected before execution of the goal begins, and no side effect will have taken place.
> The exceptional cases are: Syntax Errors, Resource Errors, and System Errors.

**Style**

Note that the ISO standard formal term tries ot express what _should be the case_ or _what is the expected correct state_, and 
not what _is the problem_.

For example:

- If a variable is found to be uninstantiated but should be instantiated, you get a
  formal term equal to `instantiation_error`: The problem is not that there is an 
  unwanted instantiation, but that the correct state is the one with an instantiated variable.
- In case a variable is found to be instantiated but should be uninstantiated (because 
  it will be used for output), you get a formal term equal to `uninstantiation_error(Culprit)`: 
  The problem is not that there is lack of instantiation, but that the correct state is the one 
  which `Culprit` (or one of its subterms) is "more uninstantiated" than is the case.
- If you try to disassemble an empty list with compound_name_arguments/3, you get a formal 
  `type_error(compound,[])`. The problem is not that `[]` is (erroneously) a 
  compound term, but that a compound term is expected and `[]` doesn't belong to that class.

In order of appearance in the ISO-Standard:

### Instantiation Error

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

Note that this exception is often used to express something completely different:

```
?- atom_length(X,12).
ERROR: Arguments are not sufficiently instantiated
ERROR: In:
ERROR:   [10] atom_length(_1192,12)
ERROR:    [9] <user>
```

The exception says that "arguments are not sufficiently instantiated", which is true on a low level. But what
it actually should say is that `atom_length/2` _cannot work in the direction +Length->-Atom_, which is the
real high-level message that should be given. But there isn't even an ISO exception to express that.

### Uninstantiation error

An argument or one of its components is instantiated, and an uninstantiated argument or argument component is required.  
In effect, "I need some space for output".

Note that the atom expresses what is _lacking_ (an "uninstantiation" is lacking), not what _is the problem or what is the matter_ 
(that would mean the atom be `required_uninstantiation_error` or something similar). Again, unusual.

```
Formal=uninstantiation_error(Culprit)
```

- `Culprit` is the argument or one of its components which caused the error.

Thrown with [uninstantiation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=uninstantiation_error/1).

### Type Error

An argument or one of its components is instantiated but incorrect.

```
Formal=type_error(ValidType,Culprit)
```

- `ValidType` is an atom chosen from
 `[atom, atomic, byte, callable, character, compound, evaluable, in_byte, in_character, integer, list, number, predicate_indicator, variable]`.
- `Culprit` is the argument or one of its components which caused the error.

Thrown with [type_error/2](https://eu.swi-prolog.org/pldoc/doc_for?object=type_error/2)

_Personal Note:_ The "list" is the odd one in the above, because "list" is not a type but a convention on how a term is supposed to look
like non-locally. It would make more sense to generate a "Domain Error" if an argument turns out to not be a list.  
epecially as there is already a "non_empty_list" Domain Error. Oh well!

### Domain Error

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

### Existence Error

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
 
### Permission Error

The runtime system (or the thread) is lacking permission to perform a specific operation.

```
Formal=permission_error(Operation,PermissionType,Culprit)
```

- `Operation` is an atom chosen from `[access, create, input, modify, open, output, reposition]`.
- `PermissionType` is an atom chosen from `[binary_stream, flag, operator, past_end_of_stream, private_procedure, static_procedure, source_sink, stream, text_stream]`
- `Culprit` is the argument or one of its components which caused the error.

Thrown with [permission_error/3](https://eu.swi-prolog.org/pldoc/doc_for?object=permission_error/3).

_Personal Note:_ The intended semantics of the atoms listed are not clear at all. Recommendation: Don't stick closely to this over-specification.

### Representation Error

An implementation-defined limit has been breached. 

```
Formal=representation_error(Flag)
```

- `Flag` is an atom chosen from `[character, character_code, in_character_code, max_arity, max_integer, min_integer]`.

Thrown with [representation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=representation_error/1).

### Evaluation Error

The operands of an evaluable functor are such that the operation has an exceptional value or event.

```
Formal=evaluation_error(Error)
```

- `Error` is an atom chosen from `[float_overflow, int_overflow, undefined, underflow, zero_divisor]`.

Thrown with [representation_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=representation_error/1).

_Personal Note:_ This does not seem to cover all of the IEEE 754 exceptional cases.

### Resource Error

The runtime system has insufficient resources to complete execution. A resource error
may happen for example when a calculation on unbounded integers has a value which
is too large.

```
Formal=resource_error(Resource)
```

- `Resource` denotes an implementation-dependent atom.

Thrown with [resource_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=resource_error/1).

### Syntax Error

A sequence of characters which are being input as a read-term do not conform to an acceptable syntax. "Being input as a read-term" means
this error is thrown by predicates like [`read/1`](https://www.swi-prolog.org/pldoc/doc_for?object=read/1) which are about
deserializatoin of terms.

```
Formal=syntax_error(ImplDepAtom)
```

- `ImplDepAtom` denotes an implementation-dependent atom.

Thrown with [syntax_error/1](https://eu.swi-prolog.org/pldoc/doc_for?object=syntax_error/1). 

### System Error

Can happen at any point of computation. The conditions for a System Error and the actions taken by a Prolog runtime 
after occurrence are implementation-dependent. A System Error may happen for example 

- in interactions with the operating system (for example, a disc crash or interrupt), or
- when a goal `throw(T)` has been executed and there is no active goal `catch/3`. 

The `Formal` is a parameterless atom (which means one has the same problems as for `instantiation_error`).

```
Formal=system_error
```

There is no corresponding `library(error)` predicate. "System Error" should not be thrown from user code. The counterpart in the Java world would be [Error](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Error.html).


