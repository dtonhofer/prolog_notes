# Directive, Code, Goal

## In a File

### Directives

- Module declarations, Module imports, Debug configuration
- Changes to runtime behaviour, parser etc. - These can often by replaced by predicate calls
 Initialization, code to be run at once, order to pull in other code (either by consulting or by textual insertion)

### Code

Goes into the Prolog database in whatever module declaration is in effect (if none given, into module user)
                             
### Goals

Not seen in code
                             
## At the toplevel

### Directives

Directives may apply to a module scop or to global scope.

Directives cannot be entered at the toplevel (at the command prompt) with the `:-` syntax. However,
if what comes after `:-` is a valid predicate, they can.

Consulting a file executes its directives (in the context of the `user` module) and inserts the predicates in the `user` module.

### Code

Not seen at the toplevel. You have to enter "code entering mode" by specifying `[user]` to add predicates to the user module.

### Goals

Anything you type at the "?- " prompt up to termination with "." is a "goal".

The environment which prints the "?- " waits for input, processes the input and outputs the result (and performs
the "obtain more answers" looping until the query fails) is called the Prolog Toplevel (The Real-Eval Print Loop, REPL)

This is like working in an SQL database REPL.

The "goal" is anything that can appear on the RHS of program clause and can be arbitrarily complex.

The goal is executed immediately by the Prolog Processor. While it runs it maintains internal state (as a store of terms)
and tracks computation progress (the stack, which maps to the depth-first AND-OR search tree, which maps to SLD resolution, more or less) 

The goal is interpreted in the context of the Prolog Database which contains the clauses (facts and rules) that make up the predicates.

Certain goals side-effect the Prolog Database, eg. assertz/asserta and retract. On return the database has been modified. 
Use those sparingly - state should mainly exist in the context of a running program, inside live terms. 
