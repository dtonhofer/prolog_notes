# SWI-Prolog dicts

This is companion information for the SWI-Prolog manual page [Dicts: structures with named arguments](https://eu.swi-prolog.org/pldoc/man?section=bidicts).

Some time ago (Oct 2020) I prepared a revised version of [Dicts: structures with named arguments](https://eu.swi-prolog.org/pldoc/man?section=bidicts) that I hope is easier to digest but it was ultimately not accepted. It's still here:

   - [PDF](manual_review/dict_manual_review.pdf)
   - [LaTeX and generated HTML](manual_review/)

## What

_Dicts_ are an SWI-Prolog-specific extension 

They are the SWI-Prolog equivalent of "maps", "hashes", "hashmap", "dictionaries" or "association lists" in other programming languages.
The idea is to associate a (possibly complex or recursive) _value_ to a _key_ inside a _container_. 
Unlike in languages like Java for example, where one gets to choose the actual implementation to realize an instance of the
[`Map` interface](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Map.html), the 
implementation of the dict is fixed.

Dicts are opaque structures (they are actually 
<a href="https://eu.swi-prolog.org/pldoc/man?section=bidicts#sec:ext-dicts-implementation">compound terms</a>, 
but you are not supposed to make use of that detail) composed of
a _tag_ and 0..N _key-value pairs_ where the keys are unique and must be an atom or a "small integer" (in practice, a
very large integer).

The tag should be an atomic type (generally an atom) or an unbound variable ("anonymous dict"). 
You actually _can_ use any term whatsover as tag, even the dict itself, but this is an implementation artifact.

The dict is essentially an immutable data structure. You use dicts in the following way in Prolog:

- Logic programming style: Start with a dict with a fixed number of keys, where the values are uninstantiated
  or just partially instantiated. The values are refined as computation progresses, eventually becoming ground.
- Functional programming style/accumulator style: Construct (mint?) new dicts from existing dicts whenever needed by adding, removing or
  changing dict entries. The ever-regenerated dict is weaved in and out of predicate calls.

Dicts can be easily used as (immutable) random-access arrays (possibly with missing keys) if you use integer keys.

Example of using a dict as array:

```
?- 
Dict = data{0:a, 1:b, 2:c, 3:d, 4:e}, forall(between(0,4,Key),(Value=Dict.get(Key),format("~q",[Value]))).
abcde
Dict = data{0:a,1:b,2:c,3:d,4:e}.

?- 
Dict = data{0:a, 1:b, 2:c, 3:d, 4:e}, forall(get_dict(Key,Dict,Value),format("~q",[Value])).
abcde
Dict = data{0:a,1:b,2:c,3:d,4:e}.
```

## Large dicts? Why not!

Dicts can be LARGE and lookup performance is very good. They can thus be used as arrays, with the keys the numeric index.

Here is some performance-exercising code:

[`dict_perf_test.pl`](/swipl_notes/about_dicts/code/perf/dict_perf_test.pl)

Building a dict of **50'000 entries** and performing **500'000 lookups** on it is fast.

We are getting 1'000'000 lookups/s on a Linux machine for which:

   - `cat /proc/cpuinfo` reports: `Intel(R) Xeon(R) CPU W3520  @ 2.67GHz` and
   - `free -h` reports 23GiB free

```
?- using_dict(50_000, 500_000, dict, builtin, quiet).

Filling a list of length 50000 with random atoms of length 10
% 10,991,240 inferences, 2.277 CPU in 2.287 seconds (100% CPU, 4827997 Lips)
Creating pairs from 50000 keys, where the values are strings of length 3
% 4,957,654 inferences, 1.123 CPU in 1.128 seconds (100% CPU, 4413060 Lips)
Creating a dict of size 50000, with tag 'p' using built-in predicate dict_create/3
% 1 inferences, 0.012 CPU in 0.012 seconds (100% CPU, 83 Lips)
Creating a random lookup sequence of size 500000 based on 50000 keys (using dicts)
% 3,500,002 inferences, 0.897 CPU in 0.900 seconds (100% CPU, 3901866 Lips)
Looking up 500000 entries in a dict of size 50000 (storing the result)
ResultLength is 500000
% 2,000,005 inferences, 0.242 CPU in 0.243 seconds (100% CPU, 8264940 Lips)
Performed 2057894 lookup/s (delta = 0.242967 s)
true.
```

## Comparison and unification

When comparing two dicts, you can use:

   - `==`, then the tags must pass `==`, the key sets must be equal and all the key-value pairs must pass `==`
   - `=`,  then the tags must unify, the key sets must be equal and the values must unify for each key
     (at least that's the current approach). Unification of dicts being thus well-defined, dicts can appear
     in clause heads.

There should be a way to perform `==` while disregarding the tag. There is not, but you can use the following:

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

Some [`plunit`](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) unit tests to exercise functionality:

- [`dict_getting_testing.pl`](code/dict_getting_testing.pl)
- [`dict_unification_testing.pl`](code/dict_unification_testing.pl)
- [`dict_assembly_disassembly_testing.pl`](code/dict_assembly_disassembly_testing.pl)
- [`dict_put_dotcall_testing.pl`](code/dict_put_dotcall_testing.pl)
- [`dict_equality_testing.pl`](code/dict_equality_testing.pl)
- [`dict_selection_testing.pl`](code/dict_selection_testing.pl)

## dot-calls involving dicts

"Function calls associated to a dict" are detected when the `./2` notation appears 
(we could thus call those calls **dot calls** and given them a special notation e.g. `.get/1` but so far the only one who does so is me).  
The dot call is replaced by the method-returned term (which may in particular be a dict or a uninstantiated term).

With the provided dot calls and some additional conventions, one may be able to use them as objects 
for [Prototype-based Object Orientation](https://en.wikipedia.org/wiki/Prototype-based_programming). 

Dicts can also replace the nonlocal and convention-based "list" structure `[a,b,c]` by a 
local/enforced "dict" structure `_{0:a,1:b,2:c}` (but compound terms can do that, too - in fact if you need
large direct-access array structures, use compound terms to represent them!)

### Predefined dot-calls

There is nothing special about those.

The following [predefined ones exist](https://eu.swi-prolog.org/pldoc/man?section=ext-dicts-predefined):

   - [`.get(?Key)`](https://eu.swi-prolog.org/pldoc/man?section=ext-dicts-predefined#m-get-1) - throws if key does not exist
   - [`.put(+NewKeyValuePairs)`](https://eu.swi-prolog.org/pldoc/man?section=ext-dicts-predefined#m-put-1) - evaluates to a new dict with additions/replacements, the method counterpart of `put_dict/3`; `NewKeyValuePairs` can take one of several different forms
   - [`.put(+KeyPath, +Value)`](https://eu.swi-prolog.org/pldoc/man?section=ext-dicts-predefined#m-put-2) - evaluates to a new dict with single addition/replacement, where one can specify a path through nested dicts; the method counterpart of `put_dict/4`

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

- Type testing
   - [`is_dict/1`](https://eu.swi-prolog.org/pldoc/man?predicate=is_dict/1), 
     [`is_dict/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=is_dict/2) : Check for dicty-ness, possibly unifying tag, too.
- Getting values by key (or enumerating them on backtracking)     
   - [`get_dict/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=get_dict/3) : Same as `.get/1`  dot-call but fails if key does not exist.
   - [`get_dict/5`](https://eu.swi-prolog.org/pldoc/doc_for?object=get_dict/5) : Combined get/put, should really be called **fork_dict/5**. 
- Extracting keys into a list (from `library(dicts)`):
   - [`dict_keys/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_keys/2) : True when Keys is an ordered set of the keys appearing in Dict.
- Assembling/disassembling dicts   
   - [`dict_create/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_create/3) : Assemble a dict from a tag and "several possible forms of list".
   - [`dict_pairs/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_pairs/3) : Assemble/disassemble a dict from/to a tag and a key-value list.
- Minting new dicts from existing ones   
   - [`put_dict/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=put_dict/3) : Mint a dict from "several possible forms of list"  ; the predicate counterpart of `.put/1`
   - [`put_dict/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=put_dict/4) : Mint a dict from an existing dict with single addition/replacement, where one can specify a path through nested dicts; the predicate counterpart of `.put/2`
   - [`del_dict/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=del_dict/4): Mint a dict from an existing dict with single deletion.
- Projecting one dict onto another   
   - [`:</2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3A%3C)/2): "Selection operator" (I would really prefer
     "projection operator"): Project the right-hand dict onto the left-hand dict, unifying the values of common keys (thus either failing
     or continuing with possibly both dicts involved in the operation further refined). Fails if the left-hand dict has a key that is not in
     the right-hand dict.
   - [`select_dict/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=select_dict/3): Similar to the "selection operator", but also retains the
     `Rest`: those `Key-Value` pairs only present in the right-hand dict.
   - [`>:</2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3E%3A%3C)/2): "Symmetric selection operator" or "partial unification" (vague)
     (I prefer "Symmetric projection operator"): Unify the values of common keys, ignoring any other keys, thus either failing or
     continuing with possibly both dicts involved in the operation further refined.

There is also [`library(dicts)`](https://www.swi-prolog.org/pldoc/man?section=dicts) which 

> defines utilities that operate on lists of dicts, notably to make lists of dicts consistent by adding missing keys, 
> converting between lists of compounds and lists of dicts, joining and slicing lists of dicts

- [`dicts_same_tag/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=dicts_same_tag/2)
- [`dict_keys/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_keys/2)
- [`dicts_same_keys/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=dicts_same_keys/2)
- [`dicts_to_same_keys/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dicts_to_same_keys/3)
- [`dict_fill/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=dict_fill/4)
- [`dicts_join/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dicts_join/3)
- [`dicts_join/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=dicts_join/4)
- [`dicts_slice/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=dicts_slice/3)
- [`dicts_to_compounds/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=dicts_to_compounds/4)

See also [Dict integration](https://www.swi-prolog.org/pldoc/man?section=ext-integration)

## Destructive assignment in dicts

[Better use those only rarely](https://eu.swi-prolog.org/pldoc/man?section=ext-dict-assignment)

