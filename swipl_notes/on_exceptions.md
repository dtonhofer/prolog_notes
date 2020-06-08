# Throwing errors

## Throwing ISO-standard errors

[`library(error)`](https://www.swi-prolog.org/pldoc/man?section=error) has facilities for **throwing standard errors** (but not for **catching standard errors** you have to write the appropriate catcher term to match the exception term), and also a discussion of the error classes.

  - type_error/2 (ISO)
  - domain_error/2 (ISO)
  - existence_error/2 (ISO)
  - existence_error/3 (not ISO)
  - permission_error/3 (ISO)
  - instantiation_error/1 (ISO)
  - uninstantiation_error/1 (ISO, in corrigendum)
  - representation_error/1 (ISO)
  - syntax_error/1 (ISO)
  - resource_error/1 (ISO)

Try for example this:

Here we throw an exception using the appropriate exception-throwing predicates from library(error) Note they are not called throw_type_error/2 etc. just type_error/2 etc, which may cause some confusion on reading. We then catch the exception in the catcher term _C_ 

```
?- catch(type_error(type,term),C,true).
C = error(type_error(type, term), _2844).

?- catch(domain_error(type,term),C,true).
C = error(domain_error(type, term), _3974).

?- catch(existence_error(type,term),C,true).
C = error(existence_error(type, term), _5108).

?- catch(permission_error(action,type,term),C,true).  % 3 args!
C = error(permission_error(action, type, term), _1314).

?- catch(instantiation_error(term),C,true). % noargs! The term is not passed along currently
C = error(instantiation_error, _7032).

?- catch(syntax_error(term),C,true). % 1 args!
C = error(syntax_error(term), _8162).
```

## Problems with the ISO standard exception terms

Sadly, the ISO error terms are not "uniform": they are compound terms of
arity 3-1 (including non-compound terms, i.e. atoms). Which is kinda hair-raising.

What would be nice (or at least nicer than is the standard now) is to have error terms like these:

```
...
error(type_error,[~list of mandatory args~],[~list of free args~]).
error(instantiation_error,[~list of mandatory args~],[~list of free args~]).
...
```

Instead we get the following variability in straightjacketed terms, which seems pointless. 

The =Extra= freshvar in second position of the term =error/2= is used when the exception is thrown from foreign code.

```
...
error(domain_error(Arg0,Arg1), Extra).
error(type_error(Arg0,Arg1), Extra).
error(syntax_error(Arg0), Extra).
error(instantiation_error, Extra).
...
```

So we can't append any other information to the ISO standard exception inner term, even though one might be interested in the name of the variables involved in the exception, or want to transmit some informative message for the user. 

One _could_ use the =Extra= argument of the outer `error/2` term, but it cannot be set by calling the provided calls from library(error). So if you want to use that, you have to construct the =error/2= term and call throw/1 yourself (library soon).

Also, for =domain_error/2=, the inner term is designed to make a statement about one variable only, instead of a combination of variables. Unfortunately, the latter is by far the more interesting case. 

Finally, there are assumptions about =Arg0= and =Arg1= terms. At least the toplevel expects something like =domain_error(integer,5)=. Otherwise, what it prints is confusing.

The ISO standard does not provide any structure for the "can't happen" error, "illegal state error" or "assertion error". These are not ISO standard "system errors". This is seriously bad. What to do?

## Rolling your own exceptions

The exception term might possibly be better being open-ended like this (but that's my opinion):

```
error(DistinguishableTypeCode,ListOrTaggedListOrAssocOrDictOfExtraValues).
```

And then you can use lists (or even better, dicts) instead of using compound terms of variable arity to transmit data to the catch point.

Of course, nothing prohibits you from throwing in your own style -- **if the caller is fine with handling non-ISO standard exceptions**:

```
?- catch(throw("This is my own term and I like it"),C,true).
C = "This is my own term and I like it".
```

## Rolling my own!!

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

Then we can do simple throws like these, advantageously replacing those of library(error):

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

throw_illegal_state_error(Pred,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; throw(ExText = ExCode)),
   throw(error(illegal_state_error,context(Pred,ExText))).
```

We can also have wrappers to fail or throw depending on options list contents. If "Options" is a list containing the atom =throw=, then throw, else fail:

```
throw_existence_error(Pred,Type,Term,ExCode,Options) :
   (nonvar(Options),memberchk(throw,Options))
   ->
   throw_existence_error(Pred,Type,Term,ExCode)
   ;
   fail. % actually unnecessary to write this
```

Alternatively, if non-ISO is an option, you can choose a better representation. Here with a list having the error class and subclass as a sublist on position 0, followed by an arbitrary set of key-value pairs (in SWI Prolog, you can use dicts instead):

```
throw_mystyle_existence_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(mystyle([[error,existence],-(type,Type),-(term,Term),-(pred,Pred),-(msg,ExText)]).
```

# When to throw which ISO Standard error

  - Ulrich Neumerkel lists the various error classes here in a standardization discussion: http://www.complang.tuwien.ac.at/ulrich/iso-prolog/error_k - the context is a discussion leading up to the second corrigendum of the ISO standard.
  - *Coding Guidelines for Prolog* https://arxiv.org/abs/0911.2899 offers a bit of commentary on _when_ to throw, but does not go further.
  - The following page substantially _is_ the ISO Standard text: https://www.deransart.fr/prolog/exceptions.html 

The ISO standard document ( *ISO/IEC 13211-1, 1st edition 1995-06-01* ) stipulates the following (paraphrased), in Chapter 7.12, pages 61 ff.

The "Error_term" determines the class of the error an can be:

### Instantiation Error

An argument or one of its components is a variable, and an instantiated argument or component is required.

Throw instantiation_error

(In effect, "I need more data").

### Uninstantiation error

"There shall be an Uninstantiation Error when an argument or one of its components is not a variable, and a variable or a component as variable is required. It has the form uninstantiation_error(Culprit) where Culprit is the argument or one of its components which caused the error."

Appears in ISO/IEC 13211-1:1995/Cor.2:2012(en)

See also: See http://www.complang.tuwien.ac.at/ulrich/iso-prolog/error_k 

### Type Error

An argument or one of its components is incorrect, but not a variable ("A Type Error occurs when a value does not belong to one of the types defined in this part of ISO/IEC 13211"). 

Throw type_error(ValidType,Culprit)

  - "ValidType" is one of: "atom, atomic, byte, callable, character, compound, evaluable, in_byte, in_character, integer, list, number, predicate_indicator, variable".
  - "Culprit" is the argument or one of its components which are the reason for the error.

---

*Personal Note:* 

The "list" is the odd one in the above, because "list" is not a type but a convention on how a term is supposed to look like non-locally. It would make more sense to generate a "Domain Error" if an argument turns out to not be a list. Especially as there is already a "non_empty_list" Domain Error. Oh well!

---

### Domain Error

An argument's type is correct but the value is outside the domain for which the procedure is defined. ("Domain Error occurs when the value is not a a member of an implementation defined or implementation-dependent set.")

Throw domain_error(ValidDomain, Culprit)

  - "ValidDomain" is one of "character_code_list, close_option, flag_value, io_mode, non_empty_list, not_less_than_zero, operator_priority, operator_specifier, prolog_flag, read_option, source_sink, stream, stream_option, stream_or_alias, stream_position, stream_property, write_option"
  - "Culprit" is the argument or one of its components which are the reason for the error.

### Existence Error

An object on which an operation is to be performed does not exist. 

Throw existence_error(ObjectType, Culprit)

  - "ObjectType" is one of "procedure, source_sink, stream"
  - "Culprit" is the argument or one of its components which are the reason for the error.
 
### Permission Error

The runtime system (or the thread) is lacking permission to perform a specific operation.

Throw permission_error(Operation, PermissionType, Culprit)
 
  - "Operation" is one of "access, create, input, modify, open, output, reposition"
  - "PermissionType" is one of "binary_stream, flag, operator, past_end_of_stream, private_procedure, static_procedure, source_sink, stream, text_stream"
  - "Culprit" is the argument or one of its components which are the reason for the error.

### Representation Error

An implementation-defined limit has been breached. 

Throw representation_error(Flag)

  - "Flag" is one of "character, character_code, in_character_code, max_arity, max_integer, min_integer"

### Evaluation Error

The operands of an evaluable functor are such that the operation has an exceptional value. 

Throw evaluation_error(Error)

  - "Error" is one of "float_overflow, int_overflow, undefined, underflow, zero_divisor"

### Resource Error

The runtime system has insufficient resources to complete execution. "A Resource Error may happen for example when a calculation on unbounded integers has a value which is too large."

Throw resource_error(Resource)

  - "Resource" is an implementation-dependent atom.

### Syntax Error

A sequence of characters which are being input as a read-term do not conform to the syntax. 

Throw syntax_error(imp_dep_atom)

  - "imp_dep_atom" denotes an implementation-dependent atom.

### System Error

Can happen at any point of computation. The conditions for a System Error and the actions taken by a Prolog runtime after occurrence are implementation-dependent. A System Error may happen for example (a) in interactions with the operating system (for example, a disc crash or interrupt), or (b) when a goal throw(T) has been executed and there is no active goal catch/3. 

Throw =system_error=

Note by editor: As a programmer, you would NOT throw System Error (the more so as it doesn't take any parameter). It sounds like the counterpart of [Java Error](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Error.html).

### Note

"Most errors defined in this part of ISO/IEC 13211 occur because the arguments of the goal fail to satisfy a particular condition; they are thus detected before execution of the goal begins, and no side effect will have taken place. The exceptional cases are: Syntax Errors, Resource Errors, and System Errors."
