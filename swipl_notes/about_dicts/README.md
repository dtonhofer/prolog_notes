# SWI-Prolog dicts

This is companion information for the SWI-Prolog manual page [Dicts: structures with named arguments](https://eu.swi-prolog.org/pldoc/man?section=bidicts).

## What

_Dicts_ are an SWI-Prolog-specific extension (at least for now, maybe they will find a way into the ISO standard one day).

They are the SWI-Prolog equivalent of "maps", "hashes", "hashmap", "dictionaries" or "association lists" in other programming languages.
The goal is to associate a (possibly complex) value to a key inside a container. The implementation of the containeris, however, fixed,
unlike in languages like Java for example, where one gets to choose the actual implementation to realize an instance of the
[`Map` interface](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Map.html).

Dicts are opaque structures (actually compound terms, but you are not supposed to notice that) composed of
a _tag_ and 0..N _key-value pairs_ where the keys are unique and must be an atom or a "small integer". 

The tag should be an atomic type of a fresh variable ("anonymous dict"). (You actually _can_ use any term whatsover
as tag, even the dict itself, but this is an implementation artifact; do not go overboard here!)

Test code to assemble/analyze a dict: [`dict_assembly_disassembly_testing.pl`](code/dict_assembly_disassembly_testing.pl)

### See also

For an alternative using a library (as opposed to a language built-in), take a look at
[`library(assoc)`: Association lists](https://eu.swi-prolog.org/pldoc/man?section=assoc)
written by Richard A. O'Keefe, L.Damas, V.S.Costa and Markus Triska.

### Helper library

There is an additional helper library: [`library(dicts)`](https://eu.swi-prolog.org/pldoc/man?section=dicts). 
It _defines utilities that operate on lists of dicts, notably to make lists of dicts consistent by adding missing keys,
converting between lists of compounds and lists of dicts, joining and slicing lists of dicts._

(A pretty printer seems to be missing in that library. TODO!).

## Comnparison and unification

When comparing two dicts, you can use:

   - `==`, then the tags must pass `==`, the key sets must be equal and all the key-value pairs must pass `==`
   - `=`,  then the tags must unify, the key sets must be equal and the values must unify for each key
     (at least that's the current approach).
     Unification being well defined, dicts can appear in clause heads in particular.

There should be a way to perform `==` while disregarding the tag. There is not, but you can use this:

```none
dict_equality_sans_tag(D1,D2) :-
   ((var(D1);is_dict(D1)) -> true ; type_error("dict or var",D1)),
   ((var(D2);is_dict(D2)) -> true ; type_error("dict or var",D2)),   
   (nonvar(D1),nonvar(D2)) 
   -> 
   (assertion((is_dict(D1),is_dict(D2))),
    dict_pairs(D1,_Tag1,Pairs1), % Pairs1 will be ordered by natural order of keys
    dict_pairs(D2,_Tag2,Pairs2), % Pairs2 will be ordered by natural order of keys
    Pairs1 == Pairs2).
```

Some [`plunit`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) unit tests:

- [`dict_equality_testing.pl`](code/dict_equality_testing.pl) (uses `dict_equality_sans_tag/2`)
- [`dict_unification_testing.pl`](code/dict_equality_testing.pl)

## dot-calls involving dicts

"Function calls associated to a dict" are detected when the `./2` notation appears (we could thus call those calls "dot calls").  
The dot call is replaced by the method-returned term (which may in particular be a dict or a uninstantiated term).

With the provided dot calls and some additional conventions, one may be able to use them as objects 
for [Prototype-based Object Orientation](https://en.wikipedia.org/wiki/Prototype-based_programming). 

Dicts can also replace the nonlocal and convention-based "list" structure `[a,b,c]` by a 
local/enforced "dict" structure `_{0:a,1:b,2:c}` (but compound terms can do that, too - in fact if you need
large direct-access array structures, use compound terms to represent them!)

### Predefined dot-calls

There is nothing special about those.

The following [predefined ones exist](https://eu.swi-prolog.org/pldoc/man?section=ext-dicts-predefined):

   - `.get(?Key)` - throws if key does not exist
   - `.put(+NewKeyValuePairs)` - evaluates to a new dict with additions/replacements, the method counterpart of `put_dict/3`; `NewKeyValuePairs` can take one of several different forms
   - `.put(+KeyPath, +Value)` - evaluates to a new dict with single addition/replacement, where one can specify a path through nested dicts; the method counterpart of `put_dict/4`

### User-defined dot-calls

These are always associated to dicts with defined tag name. This is also the _name of the module hosting the dot-calls_.

You must define a separate module file to host the method code for the given dict tag. 
The name of the module file does not need to match the name of the module though (so you can name it "module_point.pl" for example)

Example as used in the manual text: Module "point" matches dict tag "point". 
Note that the `module/2` directive declares that no predicate is being exported!

```
:- module(point, [point_length/2]).

M.multiply(F)                           % Method ".multiply/1" on dict M
   := point{x:X, y:Y}                   % Returns a "point" dict
   :- X is M.x*F, Y is M.y*F.           % Body where dict M is accessible 

M.len()                                 % Method ".len/0" on dict M
   := Len                               % Returns a "Len" value (a number, actually)
   :- Len is sqrt(M.x**2 + M.y**2),     % Body where dict M is accessible
      debug("pointlength_method","~q.len() = ~q",[M,Len]).
      
point_length(point{x:X,y:Y},Len) :-    
   Len is sqrt(X**2 + Y**2),
   debug("pointlength_predicate","point{x:~q,y:~q} = ~q",[X,Y,Len]).
```

After loading the above:

```
?- X = point{x:12,y:13}.multiply(12).
X = point{x:144, y:156}.
```

We are still not in "functional programming" territory. 
You _must enter a goal_ (above, a goal involving unification). Just entering the method call won't do:

```
?- point{x:12,y:13}.multiply(12).
ERROR: Unknown procedure: ('.')/2
ERROR:     However, there are definitions for:
ERROR:         ('.')/3
false.
```

Let's backtrack over a dot call:

```
?- debug("pointlength_method").
true.

?- between(1,5,Y),L=point{x:1,y:Y}.len(),fail.
% point{x:1,y:1}.len() = 1.4142135623730951
% point{x:1,y:2}.len() = 2.23606797749979
% point{x:1,y:3}.len() = 3.1622776601683795
% point{x:1,y:4}.len() = 4.123105625617661
% point{x:1,y:5}.len() = 5.0990195135927845
false.
```

## Predicate calls involving dicts

   - [`is_dict/1`](https://eu.swi-prolog.org/pldoc/man?predicate=is_dict/1), 
     [`is_dict/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=is_dict/2) : Check for dicty-ness, possibly unifying tag, too.
   - [`get_dict/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=get_dict/3) : Same as `.get/1`  dot-call but fails if key does not exist.
   - [`get_dict/5`](https://eu.swi-prolog.org/pldoc/doc_for?object=get_dict/5) : Combined get/put, should really be called **fork_dict/5**. 
   - [`dict_create/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_create/3) : Assemble a dict from a tag and "several possible forms of list".
   - [`dict_pairs/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_pairs/3) : Assemble/Disassemble a dict from/to a tag and a key-value list.
   - put_dict/3 : Update a Dict to a new Dict from "several possible forms of list"  ; the predicate counterpart of .put/1
   - put_dict/4 : Update a Dict to a new Dict with single addition/replacement, where one can specify a path through nested dicts; the predicate counterpart of .put/2
   - del_dict/4: Update a Dict to a new Dict with single deletion
   - :</2: "Selection operator" (I prefer "Projection operator"): Project the right-hand dict onto the left-hand dict, unifying the values of common keys (thus either failing or continuing with possibly both dicts involved in the operation further refined). Fails if the left-hand dict has a key that is not in the right-hand dict.
   - select_dict/3: Similar to the "Selection operator", but also retains the "Rest": those Key-Value pairs only present in the right-hand dict.
   - >:</2: "Symmetric selection operator" or "partial unification" (vague) (I prefer "Symmetric projection operator"): Unify the values of common keys, ignoring any other keys, thus either failing or continuing with possibly both dicts involved in the operation further refined.

## Destructive assignment in dicts

[Better use those only rarely](https://eu.swi-prolog.org/pldoc/man?section=ext-dict-assignment)

