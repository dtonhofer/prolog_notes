https://eu.swi-prolog.org/pldoc/man?section=whymodules

## Modules are objects

As noted by Paulo Moura "Prolog Modules" can be seen as "objects", but lack any further subdivision into namespaces, hierarchical or otherwise. The Module name (or namespace name, which is the same thing) _is_ the object name (n.b. _not_ the class name).

Here is a good discussion on Stack Overflow: [Objected Oriented Programming in SWI-Prolog](https://stackoverflow.com/questions/28154041/objected-oriented-programming-in-swi-prolog/28158817#28158817)

## The "xref" tool

Here is the page of [`library(prolog_xref)`](https://eu.swi-prolog.org/pldoc/man?section=prologxref): **Prolog cross-referencer data collection**

And there is a page concerning the [`xref` tool](https://eu.swi-prolog.org/pldoc/man?section=xref)

where we read:

_SWI-Prolog's cross-referencer is split into two parts._ 

   - _The standard Prolog library `library(prolog_xref) is an extensible library for information gathering described in section A.34, and_ 
   - _the XPCE library `library(pce_xref)` provides a graphical front-end for the cross-referencer described here._

_We demonstrate the tool on CHAT80, a natural language question and answer system by Fernando C.N. Pereira and David H.D. Warren._

## Some problems IMHO

### a) No recursive namespaces

One of the Big Things missing is **recursive namespaces** (i.e. recursive Modules).

Modules are are a first step but they just allow to structure the overall namespace by a single level. Sometimes I just want to make clear _inside_ a Module that certain predicates are tightly related. Being able to define a Module inside a Module would help. Compare with Java, which has the hierarchy _package-name (1 level) -> class-name -> nested-class-name_  

This is all made worse by the community's propensity to create large (and I mean _large_) Modules containing all manner of functionality. There is no real reason for this increase in suffering 🙁 Again, recursive Modules might help.  

### b) You cannot find out the name of the current module

Modules are first class and module names may be created at load time. This happens for example when you create a [`pluinit`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) test block (code enclosed between `:-begin_tests(foo)` and `:-end_tests(foo)`. 

Sometimes you need to refer to the module in which your code is located: you may want to pass the qualified name of a predicate defined in the module to a predicate outside the module for calling. But you don't know the name of the current module and you can't just write `$this_module:my_predicate`. You can guess the name of your module and write `guessed_module_name:my_predicate`, hoping that the module generation algorithm won't change in the future. That's awkward.

### c) The module interface declaration is very old-school

You can only define a list of predicate indicators like `foo/4` at the top of the module. This is on the one hand hard to maintain (it would be far better to have on-predicate annotations declaring them as "exported" or "public") and lacks information (deprecation info, version info, type info, a one-liner call example etc.)

### d) Weird import convention

The module has a module name but `:- use_module()` requires the filename of the file with the module source. Compare with Java where you `import com.example.foo.Bar`, not `import library('com/example/foo/Bar.java')`. 
 
