# Random notes taken while coding

None of these may be based in reality or be good ideas.

## Local naming contexts with constants

Would so cool

Instead of this, which becomes extremely annoying when source gets large:

```
:- begin_tests(generate_slashy_typedesc).

test("generate slashy typedesc 01", true(Out == int)) :-
   generate(int,new_slashy,Out).

test("generate slashy typedesc 02", true(Out == void)) :-
   generate(void,new_slashy,Out).

test("generate slashy typedesc 03", true(Out == double)) :-
   generate(double,new_slashy,Out).
   
:- end_tests(generate_slashy_typedesc).
```

How about

```
$txt  = "generate slashy typedesc".
$what = new_slashy.

:- begin_tests(generate_slashy_typedesc).

test("$txt 01", true(Out == int)) :-
   generate(int,$what,Out).

test("$txt 02", true(Out == void)) :-
   generate(void,$what,Out).

test("$txt 03", true(Out == double)) :-
   generate(double,$what,Out).
   
:- end_tests(generate_slashy_typedesc).
```

## An example of symmetry breakdown in a DCG 

If Prolog were more adapt at managing constraints & relationship, this mess could be avoided and one would 
just need a single rule to transfrom from list of codes to atoms and vice-versa:

```
% ---
% For direct handling of an identifier, we suffer symmetry breakdown.
% ---

jpl_java_id_raw(A) --> { atom(A),! },  % guard
                       { atom_codes(A,[C|Cs]),
                         jpl_java_id_start_char(C) },
                       [C],
                       jpl_java_id_part_chars(Cs). 

% building X from the character code list

jpl_java_id_raw(X) --> { var(X),! },  % guard
                       [C],
                       { jpl_java_id_start_char(C) },
                       jpl_java_id_part_chars(Cs),
                       { atom_codes(X,[C|Cs]) }.
                       
jpl_java_id_part_chars([C|Cs]) --> [C], { jpl_java_id_part_char(C) } ,!, jpl_java_id_part_chars(Cs).
jpl_java_id_part_chars([])     --> [].
```

## Printing terms in full while debugging

https://swi-prolog.discourse.group/t/is-there-a-way-to-make-the-stacktrace-print-terms-in-full/2720

```
:- create_prolog_flag(backtrace,            true, [type(boolean), keep(true)]).
:- create_prolog_flag(backtrace_depth,      20,   [type(integer), keep(true)]).
:- create_prolog_flag(backtrace_goal_depth, 3,    [type(integer), keep(true)]).
:- create_prolog_flag(backtrace_show_lines, true, [type(boolean), keep(true)]).
```

How you do it:

```
set_prolog_flag(backtrace_depth,100).         % prints more tacktrace
set_prolog_flag(backtrace_goal_depth,10).     % prints more of the terms (e.g. depth of lists in arguments)
```

Also of interest:

```
set_prolog_flag(answer_write_options,[max_depth(0)]).
set_prolog_flag(debugger_write_options,[max_depth(0)]).
```

See also:

https://eu.swi-prolog.org/pldoc/man?section=flags

## Naming is a problem

For a language dealing with predicates which relate "thing A" to "thing B", it's concerning that there is no specical character or convention
for forming the predicate name "thingA_related_to_thingB" like "thingA⊗thingB" (and for some reasons of typing, ASCII is still prevalent)

## Something like module friend declarations would be nice

Modules export predicates not because client code needs them but because they are
called by test code. This is a usual problem but how to solve it?

## What is this escaping syntax

```
?- X=($).
X =  ($).

?- atom(($)).
true.

?- ($) == '$'.
true.

?- atom($).
true.

?- X=$.
|    (waits)
```

`$` is special.


## Write your test cases

Without test cases, you are toast!

Compared to imperative programs, and even functional programs, the fact 
that a predicate can be run in "several different ways" (unless you stick
to functional style, which may be good practice in certain cases) demands
that you code test cases for these "several cases".

Plus you have no typing, and a variable can fresh or not and unfication
makes data flow in both ways in one operation. A lot of things are going on
and you may actually miss a good part of the edge cases, or wrapping results
in a list, or not think about the case of the freshvar at argument position Z.

Without test cases, you are toast!

https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)

## What's the Poodle's Kernel of a clause?

Maybe it's setting up network of connections between the variables, through which data flows upon a call.

But it's clumsily done as the connections are not apparent, how can one make this more visible. 

Quite possibly linear text is absolutely not the right representation for logic programs. 

I also notice how in logic one discusses logic system but apart from mentioning unification, there is no discussion about inter-variable dataflow.

In Prolog, everything is about inter-variable dataflow and the logic is trivial, and force-fit into whatever not-quite
logic operatîon we are doing currently. Maybe Prolog should be 
designated a language of "Dataflow Programming" rather than of "Logic Programming". It actually would make a lot of sense.

## Local naming contexts

Wouldn't it be cool if one had this:

```
cumulative_2([K-V|Pairs,TotalStars,[K-CumulV|Cumul]) :-
   % Keep the Chars inside!
   Chars^{
     atom_chars(V,Chars),
     phrase(stars(StarsCount),Chars),
   }
   TotalStars #= StarsCount + TotalStarsLower,
   cumulative_2(Pairs,TotalStarsLower,Cumul).
```

```
cumulative_2([K-V|Pairs,TotalStars,[K-CumulV|Cumul]) :-
   % Make StarsCount and V "connect to their context"
   StarsCount&V&{
     atom_chars(V,Chars),
     phrase(stars(StarsCount),Chars),
   }
   TotalStars #= StarsCount + TotalStarsLower,
   cumulative_2(Pairs,TotalStarsLower,Cumul).
```

It's another way of writing a separate predicate of course..

## Local naming contexts (would that be hoierachical modules?)

One should be able to create a naming context for helper predicates:
They should only be visible from the "master predicate". Constricting
scope is always win. In fact, Prolog should generally make it easier
to add helper predicates. However, this demands specific support from
the editor.

## Are cyclic terms taken seriously

If they are, there should be a built-in to "break a cycle" into a linear
cycle-less graph so that a graphy with cycles can be safely examined.

## Prolog is like taking a hike along the crest of a mountain

You have to follow the small path of truth (computational success and domain/type adequacy) 
while left and right vats abysses of falseness open up (computational failure and domain/type inadequacy)
Still, do not check every single step in detail!

## Don't try to throw for this and that unless you need precise exception messages

It is generally easier to write a predicate which fails radically and in carefree fashion
when it gets "stuff it cannot digest". You can then wrap that predicate into a predicte
which throws if the wrapped predicaste fails. You will not get detailed exceptions, but
at least the code is readable and low-maintenance.

## Guard Handling

Should fresh variables in the guard be able to "bleed out" into the rest of the clause?
It may be efficient but style-wise this does not seem like a good idea.

```
openlist_last_nonempty(Olist,Last) :-
   nonvar(Olist),
   Olist=[Stuff|More], % it would be cool to specify that More cannot be var here, in the unification
   var(More),
   !,
   Last=Stuff. % could be done "in the guard" but that doesn't sound right
```   

In fact, the guard above is best moved out into a proper guard predicate:

```
is_final_listbox(Olist) :-
   nonvar(Olist),
   Olist=[_|More],
   var(More).
   
openlist_last_nonempty(Olist,Last) :-
   is_final_listbox(Olist), % guard
   !,
   Olist=[Last|_]. 
```
   
## Asymmetric Unification (Pattern Matching)

There might be a need for an asymmetric unify operation: Something like `A <<= B`.
Nothing of B is "further instantiated", instead only A is further instantiated .. .would that make sense?
What happens to variables? It would be a lot more like pattern matching.

## Specify that a variable cannot/must unify to a hole _inside of unification_

Instead of:

```
is_final_listbox(Olist) :-
   nonvar(Olist),
   Olist=[Stuff|More], 
   var(More).
```

Why not have somethign like:

```
is_final_listbox(Olist) :-
   nonvar(Olist),
   Olist=[Stuff|More#]. % the # means unificatoin only works if More is unified with a freshvar
```

## Where is my declarativity

Prolog is touted as declarative, but it actually works only because it throws
the declarativeness of FOL overboard and explicitly performs an exhaustive search for
"witnesses" in defined order instead of formula manipulation and deduction.

## Problems with Module visibility

Calling predicates defined inside a plunit using metacalls demans that on
indicate the "plunit module" (one needs to guess a bit because that is a 
constructed name.

Metacalls made from Module X should really be able to see predicates of Module X auotmatically.

## Styleguide items

- Do not be afraid to add task-specific mini-predicates! But Prolog should support attaching these to the predicates where they are used.
- Helper predicates: add a _2 etc.

## Truth value reification

When is it appropriate to not succeed/fail a predicate but to deterministcially succed and return the truth value as an atom `true`, `false`
(and possibly `unknown`, although that cannot be called unlike the others, for de-reification). In this case "predicate success" means
"computation success", not query success.

Maybe wehn you don't need to backtrack or don't want to have the control construct `->`? 

## Meta-predicates

Using meta-predicates is quite helpful as it de-litters the code (especially getting rid of the ugly and unreadable `->`).
Prolog is on the same level of possibilites as LISP/Clojure for that (in fact, whole goals can be moved around as structures,
but there is problem as there is no problem Lambda abstraction, but we have yall, but does it always work?). Also good for
expressing intentions.

The "programming communnity" does not seem to use metapredicates often though.

## Compiler warnings

- The compiler should probably warn if predicate names differ in capitalization only
- Compiler does not warn here: `MiDict.get_dict(Id)` even if `MiDict` is fresh. It should.

## Add must_be, but in assertions

Of course you can always compile-out the must-be's .. but why not have them in assertions?

## Assertions should have a hierarchical topic structure, like debugs

So that you can switch them off by sector.

Or, as this is logic, one may even use a more general goal to decide whether an assertion is on-topic or not.

## Accumulator generation naming

Name them AccIn, AccOut in the argument list

Name them Acc0, Acc1, Acc2 etc in a clause. This is easily recognizable by eye scan.

## The Prolog stlye guid by Covington should be in an annotable Wiki

Currently the Prolog style guide is a dead document ... buried in a PDF like a scientific paper. What a tragedy!

## Modules should be small 

Currently modules are overly fat. I don't know why. Weird tradition.

## Hierarchical Module system

There needs to be a study for Module Best Practice in e.g. the Java world (inlcuding OSGi), CIAO Prolog etc.

Should Modules be like objects or classes?

## Add helper predicates to make intention clear

Even if they are just a name indicating intention and calling another predicate with a less clear name.

Again, editor support for such operaions would be a great help.

## Make Call arguments directly available

Arguments appearing in the head should be accessible by special variables like `$ARG1`, `$ARG2` etc. 
With this, Then one can write clauses which can both have structure in the head (for unification) and which can 
access that structure in the body w/o  the need to reconstruct it by hand: `foo([X,Y|Z],A,B) :- g($ARG1).` instead of `foo([X,Y|Z],A,B) :- g([X,Y|Z]).`

## Add HERE documents

[HERE documents](https://en.wikipedia.org/wiki/Here_document) to embed text blocks to output elegantly in code. 
(but you would also need string interpolation to make this really useful)

## Extract Module info

A way to automatically extract the signature of a module (including comments etc.)

## This smells: False vs. Exceptions

Failing a predicate and exiting from a predicate via an exception are wto sides of the same coin. An exception can be seen as a "super-fail"
which backtracks not to the preceding activation on-stack but to the catch marker on stack, which can be rather "far away". This way of
backtracking can also transmit information to the catch point, essentially saving something from the pocket univers that is being completely
obliterated as state is being rolled back, something that a "standard fail" cannot do.

Here the two aspects of Prolog, which are modeling on one hand and computing on the other hand rub against each other.

- A "fail" signals the failure of a query against some modeling problem (as in "is joshua the father of abraham?", you want to fail,
  not to "super-fail"). I is practically certain that a "fail" results from a failure of unification, including a lack of
  appropriately matching head or else an explicit call to fail (which includes `\+`)
- A "super fail" signals the failure of a computation (e.g. because an expectation on types was violated). If you have a "super fail"
  while doing a quewry against your modeling problem, something is wrong with your implementation. Note that practically the only response
  to "super fail/exception" is to cut off a whole subsystem (e.g. shut down the I/O activity, closing files etc), reinitialize and
  hope that a reattempt will succeed (in fact, I have the suspicion that "exceptions" are actually a "punt the problem to the coder"
  approach to avoid having to identify (and have proper support for) module boundaries that can be declaratively labeled as "corrupted"
  and thrown away on "exceptional conditions".

Prolog however, often uses "fail" to signal problems in computation, not allowing good info to reach the caller or debugger.

On the other hand, "super fails" are awkward to use.

In any case, something is not right about having these two ways of breaking off computation. Maybe there should just be
a "labeled false" carrying some information about what could not be solved in modeling or what went wrong in computing.
And no exceptins at all.

As an aside, Wikipedia states in [Exception Handling: Criticism](https://en.wikipedia.org/wiki/Exception_handling#Criticism)

> Exception handling is often not handled correctly in software, especially when there are multiple sources of exceptions;
> data flow analysis of 5 million lines of Java code found over 1300 exception handling defects.[15] Citing multiple
> prior studies by others (1999–2004) and their own results, Weimer and Necula wrote that a significant problem with
> exceptions is that they "create hidden control-flow paths that are difficult for programmers to reason about".

I can attest to the failure of proper exception handling in Java; it would be interesting to make a study of exception handling
in Prolog (or proper control flow handling per se).
~                                                                     

## Deprecation markers

That would be nice. 

## IPC in Prolog

How is it done? If there are multiple threads or engines running? 

The native model would be to have a channel onto which one writes terms which cause a predicate to be run on arrival.

Do "waiting threads" make sense even? Maybe not. Need to study this model.

Haha solution: Send message to a Java ActiveMqeue via JPL. 

## Shellcheck

Prolog needs a "shellcheck". That linter is worth gold.

## Functo the word

According to the ISO standard, the "predicate" is the same as "functor": predicate symbol + arity (not _quite_ the predicate signature).

I do think functor should no longer be used.

## Modes

https://stackoverflow.com/questions/19268558/meaning-of-instantiation-mode-indicators-in-arguments-of-prolog-predicates

