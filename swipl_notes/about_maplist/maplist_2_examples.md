# Examples for the Prolog predicate `maplist/2`

- On this page: `maplist/2` (1 goal, 1 list to verify or test)
- For examples about `maplist/3` (1 goal, 2 lists to relate) see [this page](maplist_3_examples.md)
- For examples about `maplist/4` (1 goal, 3 lists to relate) see [this page](maplist_4_examples.md)

**Table of Contents**

- About<a name="about"></a>
- Intro<a name="intro"></a>
- See also<a name="see_also"></a>
- How to pass parameters to _goal_<a name="how_to_pass_parameters_to_goal"></a>
   - Sidenote: What about testing 'exists' instead of 'forall'?<a name="what_about_testing_exists_instead_of_forall"></a>
   - λ expressions would be nice<a name="lambda_expression_would_be_nice"></a>
   - Lambda expression in Prolog<a name="lambda_expressions_in_prolog"></a>
   - Library `yall` can help you all<a name="library_yall_can_help_you_all"></a>
- Backtracking in _maplist/N_<a name="backtracking_in_maplist"></a>
- Going multilevel: Using _maplist/N_ inside of _maplist/N_<a name="maplist_inside_of_maplist"></a>
- Some applications of _maplist/2_<a name="some_applications_of_maplist_2"></a>
   - Writing list elements (maybe non-ground) out to a data sink<a name="writing_to_a_data_sink"></a>
   - Reading list elements from a data source<a name="reading_list_elements_from_a_data_source"></a>
   - Generating a list of random numbers<a name="generating_a_list_of_random_numbers"></a>
   - Generating a list of random pairs<a name="generating_a_list_of_random_pairs"></a>
   - Verifying the contract of predicates<a name="verifying_the_contract_of_predicates"></a>
   - Generating binary patterns<a name="generating_binary_patterns"></a>
   - Computing a value for each element of a list<a name="computing_a_value_for_each_list"></name>
   - Imposing constraints among variables<a name="imposing_constraints_among_variables"></a>
   - Addendum: Crazy idea: Using _maplist/2_ to copy a list the hard way<a name="crazy_idea"></a>

## About<a name="about"></a>

Here we list a few examples for the predicate [`maplist/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/2) 
from [library(apply)](https://eu.swi-prolog.org/pldoc/man?section=apply) as run with SWI-Prolog.

> `library(apply)`: This module defines meta-predicates that apply a predicate on all members of a list.

We use [SWI-Prolog](https://www.swi-prolog.org/) throughout. However, `maplist/N`, while not in the [ISO standard](https://en.wikipedia.org/wiki/Prolog#ISO_Prolog), at least [not yet](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/prologue), is a common predicate ([GNU Prolog](http://gprolog.org/manual/gprolog.html#sec223), [SICStus Prolog](https://sicstus.sics.se/sicstus/docs/4.3.0/html/sicstus/lib_002dlists.html), [ECLiPSe](https://eclipseclp.org/doc/bips/lib/lists/index.html)). Other Prologs _should_ work the same. 

We also use the [`library(yall)`](https://www.swi-prolog.org/pldoc/man?section=yall) lambda notation imported from [Logtalk](https://logtalk.org/). This _is_ specific to SWI-Prolog.

In SWI-Prolog, in order for structures not to be elided at with ellipses ("`|...`"), but instead printed in full, you may have to first call:

````none
?- 
set_prolog_flag(answer_write_options,[max_depth(0)]).
````

## Intro<a name="intro"></a>

The description for [`maplist/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=maplist/2) says:

> `maplist(:Goal, ?List)`
>
> _True if Goal can successfully be applied on all elements of List. Arguments are reordered to 
> gain performance as well as to make the predicate deterministic under normal circumstances._

Thus you have a single list, _List_, and `maplist/2` will call the _Goal_ (which must be a predicate) on each
list element. `maplist/2` will fail at the first failed call of _Goal_.

Easy to understand. This will allow us to avoid writing failure-driven loops or explicit recursion, making code cleaner.

If you are using a functional programming language, you know all about the "map" function, which takes another
function (a closure) and applies it to a sequence of items, however defined. If you are using Java, this
corresponds to 
`java.util.stream.Streams.map`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#map-java.util.function.Function-) 
(See also this DZone article: [Java 8 Map, Filter, and Collect Examples](https://dzone.com/articles/how-to-use-map-filter-collect-of-stream-in-java-8)). 

Prolog's `maplist/N` additionally bring backtracking over possible solutions of the _Goal_ predicate into the mix.

On Wikipedia:

- [map: higher-order function](https://en.wikipedia.org/wiki/Map_(higher-order_function))
- [map: parallel pattern](https://en.wikipedia.org/wiki/Map_(parallel_pattern)) - Somewhat related in the context of parallel architectures or concurrent processing.

It's all about calling a predicate (or a more complex goal) for each element of a list. The list should be
of **known length**, as there is no way to have the called predicate or goal tell `maplist/2` that it should stop going
through the list: returning `false` from the called predicate will cause `maplist/2` to fail, and variable
bindings built up during its run will be rolled back. For `maplist/3`, `maplist/4`, `maplist/5`, the lists 
must all be of the same length. If the length of a list is unspecified (i.e. if the corresponding argument is
an unbound variable or an open list that list will grow to the required length.

Here is `maplist/2` with a list of unconstrained length. It will backtrack over possible lengths, never stopping:

```none
just_one(1).

?- 
maplist(just_one,E).

E = [] ;
E = [1] ;
E = [1,1] ;
E = [1,1,1] ;
E = [1,1,1,1] ;
....
```

For the other maplists:

```none
just_one_two_three(1,2,3).

?- 
maplist(just_one_two_three,[L1a,L1b,L1c],L2,L3).

L1a = L1b, L1b = L1c, L1c = 1,
L2 = [2,2,2],
L3 = [3,3,3].

?-
maplist(just_one_two_three,[L1a,L2b,L3c|LMore],L2,L3).

L1a = L2b, L2b = L3c, L3c = 1, LMore = [], L2 = [2,2,2], L3 = [3,3,3] ;
L1a = L2b, L2b = L3c, L3c = 1, LMore = [1], L2 = [2,2,2,2], L3 = [3,3,3,3] ;
L1a = L2b, L2b = L3c, L3c = 1, LMore = [1,1], L2 = [2,2,2,2,2], L3 = [3,3,3,3,3] ;
...
```

(...what happens if you `maplist/2` a [lazy list](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lazy_lists.pl)?)

## See also<a name="see_also"></a>

Markus Triska has a page on [metapredicates](https://www.metalevel.at/prolog/metapredicates), which includes `maplist/N`.

## How to pass parameters to _goal_<a name="how_to_pass_parameters_to_goal"></a>

Consider this predicate, which tests whether a list element is less than 5.

In standard Prolog notation, indicate the name of the predicate as an atom (`verify`):

````none
verify(Element) :- format("verify(~q)\n", [I]), 5 > Element.
````

````none
?- 
maplist(verify, [1,2,3,4]).

verify(1)
verify(2)
verify(3)
verify(4)
true.
````

Alternatively, you may indicate the name of the predicate as a compound term of arity 0 (`verify()`). This is SWI-Prolog specific:

````none
?- 
maplist(verify(), [1,2,3,4]).

verify(1)
verify(2)
verify(3)
verify(4)
true.
````

`maplist/2` is passed the name of the predicate and will build a new term from it by appending
the current list element (giving `verify(1)` for example), then call that.

The `verify` predicate may depend on a second value. Above, we hardcoded the upper 
limit to be 5. Why not pass it as a parameter:

```none
verify(Limit,Element) :- format("verify(~q > ~q)\n", [Limit,Element]), Limit>Element.
```

Then:

```none
?- 
maplist(verify(5), [1,2,3,4,5]).

verify(5 > 1)
verify(5 > 2)
verify(5 > 3)
verify(5 > 4)
verify(5 > 5)
false.
```

The same as earlier happens. You pass a term `verify(5)` to `maplist/2`. This term may be regarded
as a half-parameterized call to `verify/2`, with the last argument still missing.
`maplist/2` will append the current list element as the missing argument to form a complete 2-argument
goal-with-one-literal (atomic goal), giving `verify(5,1)` for example, then call that.

Note that the goal given to `maplist/2` cannot be complex:

```none
?- 
maplist((verify(6),verify(2)),[1,2,3,4,5,6]).

ERROR: Unknown procedure: (',')/3
```

### Sidenote: What about testing 'exists?' instead of 'forall'?<a name="what_about_testing_exists_instead_of_forall"></a>

`maplist/2` performs "forall" processing over the list.

If you want to check whether there 'exists' an element in the list that fullfills a predicate, here are two ways of doing it.

- Using [`include/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=include/3), which is the "filter" predicate.
- Using [`foldl/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=foldl/4), which is not as direct because one needs a helper predicate.
- If the criterium reduces to unification, you can just use [`member/2` ](https://eu.swi-prolog.org/pldoc/doc_for?object=member/2)
  or [`memberchk/2`](https://eu.swi-prolog.org/pldoc/man?predicate=memberchk/2).

This time, let's use the SWI-Prolog [unit testing framework](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27))
to write code. We assume that we are not looking for variables!

```none
verify(Element) :- format("verify(~q)\n", [Element]), 5 > Element.
```

Using `include/3`

```none
:- begin_tests(exists_with_include).

test(find,         true(Founds == [2,3])) :- include(verify, [7,2,9,3,10], Founds).
test(notfind,      true(Founds == []))    :- include(verify, [7,8,9,5,10], Founds).
test(notfindempty, true(Founds == []))    :- include(verify, [], Founds).
   
:- end_tests(exists_with_include).
```

Using `foldl/4`

```
% Pass accumulator "Acc" on if "Acc" is bound, which is supposed to mean "I found an element and it's in Acc"

foldy_verify(_,Acc,Acc) :-
   nonvar(Acc),!.

% Otherwise verify the current element, and pass it on if it passes the test.

foldy_verify(Element,Acc,AccNext) :- 
   var(Acc),
   !,
   (verify(Element) -> AccNext = Element ; AccNext = Acc).   

:- begin_tests(exists_with_foldl).

test(find,         true(Found == 2)) :- foldl(foldy_verify, [7,2,9,3,10], _, Found),nonvar(Found).
test(notfind,      true)             :- foldl(foldy_verify, [7,8,9,5,10], _, Found),var(Found).
test(notfindempty, true)             :- foldl(foldy_verify, [], _, Found),var(Found).
   
:- end_tests(exists_with_foldl).
```

After loading the above, run the unit test blocks with [`run_tests/0`](https://eu.swi-prolog.org/pldoc/doc_for?object=run_tests/0).

### λ expressions would be nice<a name="lambda_expression_would_be_nice"></a>

A much smoother syntax would be given by a [_lambda expression_](https://en.wikipedia.org/wiki/Lambda_calculus#Lambda_terms)
explicitly showing the "argument attach point", as in:

```none
?- 
maplist(λElement.verify(3,Element), [1,2,3,4,5]).
```

It is immediately recognizable that the inner predicate takes one argument and will use it on second position of `verify/2`.

You could even do:

```none
?- 
maplist(λLimit.verify(Limit,3), [1,2,3,4,5]).
```

The inner predicate takes one argument, `Limit`, and will use it on _first_ position of `verify/2`. 

The above cannot be done with standard Prolog notation - you have to use a helper predicate:

```none
my_verify(Element,Limit) :- verify(Limit,Element). % swap the arguments
```

```
?- 
maplist(my_verify(3), [5,4,3,2,1]).

verify(5 > 3)
verify(4 > 3)
verify(3 > 3)
false.
```

### Lambda expression in Prolog<a name="lambda_expressions_in_prolog"></a>

Concretely, something quite similar to λ-adorned notation can be had by using the following:

- [`library(yall)`](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/yall.pl) which comes from the `Logtalk`
  language (see the [description in the Logtalk manual](https://logtalk.org/manuals/userman/predicates.html#lambda-expressions)).

or alternatively:

- [pack `lambda`](https://www.swi-prolog.org/pack/file_details/lambda/prolog/lambda.pl)

We will be using `library(yall)` here.

### Library `yall` can help you all<a name="library_yall_can_help_you_all"></a>

With `library(yall)` notation we can write a one-liner, creating an anonymous predicate of one 
variable `I`, which calls `verify/2` with the position of the argument evident and exchangeable:

```none
?- 
maplist([Element]>>verify(3,Element), [1,2,3,4,5]).

verify(3 > 1)
verify(3 > 2)
verify(3 > 3)
false.

?- maplist([Element]>>verify(Element,3), [5,4,3,2,1]).

verify(5 > 3)
verify(4 > 3)
verify(3 > 3)
false.
```

So we use `[X]>>` instead of `λX.`, staying in ASCII-land.

This notation "shims" or "wraps" the predicate `verify/2`. The documentation also says this is a "closure" (though it's not 
_really_ a [closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)) ... in fact in Prolog there are 
no non-local variables to "lexically close over" at all. It's just a goal.)

As an non-inconsiderable bonus, this notation is much more readable than the implied argument passing of standard Prolog.

This lambda notation allows us to invoke complex goals inline. Don't go too far though, as code quickly becomes difficult to read:

```none
?- 
maplist([Element]>>(verify(Element,6),verify(Element,2)),[1,2,3,4,5,6]).

verify(1 < 6)
verify(1 < 2)
verify(2 < 6)
verify(2 < 2)
false.
```

Note that the lambda shim, doing unification, allows modification on call and on return.
This is no use in the case of `maplist/2` but here is an application of `maplist/4` which selects the minimum of two lists:

```none
?- 
L1=[1,3,5,2,1], L2=[3,3,1,0,1], maplist([E1,E2,max(E1,E2)=O]>>(O is max(E1,E2)),L1,L2,L3).

L1 = [1, 3, 5, 2, 1],
L2 = [3, 3, 1, 0, 1],
L3 = [max(1, 3)=3, max(3, 3)=3, max(5, 1)=5, max(2, 0)=2, max(1, 1)=1].
```

## Backtracking in _maplist/N_<a name="backtracking_in_maplist"></a>

Using [`atom/1`](https://www.swi-prolog.org/pldoc/doc_for?object=atom/1), we can test whether elements or a list are atoms:

```none
?- 
maplist(atom,[a,c,d]).
true.

?- 
maplist(atom,[a,[],d]).
false.
````

As seen aleady, test whether all elements, assumed to be numeric, are less than 5. 
We are also performing side-effects by printing to `stdout`:

```none
verify(X) :- format("verify(~d)\n", [X]), X < 5.
```

```none
?- maplist(verify, [1,2,3,4]).
verify(1)
verify(2)
verify(3)
verify(4)
true.

?- maplist(verify, [1,2,3,4,5,6]).
verify(1)
verify(2)
verify(3)
verify(4)
verify(5) % breaks off here!
false.
````

The above looks like a short-circuting test loop like this (Perl):

```perl
my $allok = 1;
for my $element (@list) {
   $allok = verify($element);
   last if (!$allok)
}
if ($allok) { ... }
```

But that's **not really what happens**. What actually happens is:

```none
?- 
verify(1),verify(2),verify(3),verify(4).
```

This is a conjunction of predicate calls. 

**If any of the predicates leaves choicepoint, we can backtrack over them.**

A "short-circuiting test loop" is given by Prolog's [`forall/2`](https://www.swi-prolog.org/pldoc/doc_for?object=forall/2), 
which works differently:

> `forall(:Cond, :Action)`: For all alternative bindings of `Cond`, `Action` can be proven.

The syntax to use for `forall/2` is markedly different from the one for `maplist/2`. 
You have to use a "generator predicate" as `Cond`, and an actual 
_call_ using the local variable (here, `Element`) instead of the predicate's _name_ as for `maplist/2`:

```none
?- 
List=[a,b,d],forall(member(Element,List),atom(Element)).
true.

?- 
List=[a,[],d],forall(member(Element,List),atom(Element)).
false.
```

*However*, forall just calls the predicate once for each list element (because it just wants one solution: it is all about verification):

```none
?- 
List=[1-a,2-b,2-c,3-d],
forall(                               % Is it true that
   member(Element,List),              % for every member Element of List
   ( Element=N-C,
     member(N-X,List),                % there is (at least one, and one suffices) element in List with the same N?
     format("~w ",[N-C-X]) )).
          
1-a-a 2-b-b 2-c-b 3-d-d 
```

Whereas `maplist/N` chains the calls into a conjunction and leaves the choicepoints open. Note the same at all:

With some output reformatting:

```none
?- 
List=[1-a,2-b,2-c,3-d],
maplist( 
   [N-C]>>(member(N-X,List),format("~w ",[N-C-X])),
   List).

1-a-a 2-b-b 2-c-b 3-d-d  List = [1-a, 2-b, 2-c, 3-d] ;
            2-c-c 3-d-d  List = [1-a, 2-b, 2-c, 3-d] ;
      2-b-c 2-c-b 3-d-d  List = [1-a, 2-b, 2-c, 3-d] ;
            2-c-c 3-d-d  List = [1-a, 2-b, 2-c, 3-d] ;
false.
```

## Going multilevel: Using _maplist/N_ inside of _maplist/N_<a name="maplist_inside_of_maplist"></a>

If you want to test a list of lists, with the condition that every sublist has the same length, you can use `maplist/N` inside of `maplist/N`:

```none
% Are the elements atoms? Test using atoms/1.

?- 
List=[[a,b],[c,d]], maplist(maplist(atom),List).

List = [[a, b], [c, d]].

% Print the elements using write/1.

?-
List=[[a,b],[c,d]], maplist(maplist(write),List).

abcd
List = [[a, b], [c, d]].
```

The lambda syntax of `library(yall)` shows what is happening in a more transparent way. The sublist appears as `S`
in the inner call to `maplist/2`, in which elements appear as `Element`: 

```none
?- 
List=[[a,b],[c,d]], maplist([S]>>maplist([Element]>>atom(Element),S),List).

List = [[a, b], [c, d]].
```

## Some applications of _maplist/2_<a name="some_applications_of_maplist_2"></a>

### Writing list elements (maybe non-ground) out to a data sink<a name="writing_to_a_data_sink"></a>

Writing terms, the last of which happens to be an unbound variable.
Side-effects are lacking a logical interpretation. That doesn't stop us:

```none
?- 
maplist(write,["a->","b->","c->",X]).

a->b->c->_5626
true.
```

### Reading list elements from a data source<a name="reading_list_elements_from_a_data_source"></a>

Getting new information from a data source. 
Pulling in oracular data from a source is also lacking a logical interpretation (See [Interactive Computation](https://en.wikipedia.org/wiki/Interactive_computation). That doesn't stop us either:


```none
?- 
maplist(read,[X,f(Y,Z),c]).
|: "Hello, World".
|: f(1000,2201).
|: c.

X = "Hello, World",
Y = 1000,
Z = 2201.
```

### Generating lists<a name="generating_lists"></a>

For example, to unify `List` with successively larger lists of 1s:

```none
?- 
maplist([Element]>>(Element=1),List).
List = [] ;
List = [1] ;
List = [1, 1] ;
List = [1, 1, 1] ;
List = [1, 1, 1, 1] ;
List = [1, 1, 1, 1, 1] ;
...
```

Or just with a list of specific length, even without `library(yall)` lambda notation:

```none
length(List,10), maplist(=(1), List).
```

### Generating a list of random numbers<a name="generating_a_list_of_random_numbers"></a>

With standard notation, the helper predicate name given to `maplist/2` will be transformed into term `ur(Element)` and then called:

```none
ur(Element) :- Element is random(100).
```

```none
?- 
length(List,6),maplist(ur,List).

List = [66, 19, 7, 30, 42, 75].
```

`library(yall)` lambda notation explicitly shows that the current element of a list appears as `Element` in the called predicate:

```none
?- 
length(List,6),                                    % create a list of 6 fresh (and unbound) variables
maplist([Element]>>(Element is random(100)),List). % each unbound variable is unified with the result of random(100)
   
L = [61, 15, 82, 74, 83, 31].
```

To unify all the elements of a list of known length with a random float, use the following.
The `random_float/0` is a 0-arity function (used on the RHS of `is/2`). It unsurprisingly 
evaluates to a random float: [`random_float/0`](https://www.swi-prolog.org/pldoc/doc_for?object=f(random_float/0)).

```none
?- 
length(List,5),
maplist([Element]>>(Element is random_float),List).

List = [0.8203682301866675, 0.86789174167603, 0.9560836782052566, 0.2545485344026232, 0.7363884829219359].
```

### Generating a list of random pairs<a name="generating_a_list_of_random_pairs"></a>

Here we generate a list of `Character-Number` [pairs](https://eu.swi-prolog.org/pldoc/man?section=pairs) with
[`random_between/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=random_between/3):

```none
list_of_random_pairs(List,Length) :-
   length(List,Length),
   maplist(
      [Char-Num]>>(                    % this predicate with single arg "Char-Num" is properly deterministic 
         random_between(0,9,Num),      % random number between 0 and 9 inclusive
         random_between(97,122,Code),  % random 16-bit Unicode codepoint of a character in the range a-z
         atom_codes(Char,[Code])       % transform code into "character" aka. "char", i.e. atom of length 1
      ),
      List).

?- 
set_prolog_flag(answer_write_options,[max_depth(100)]).
true.

?- 
list_of_random_pairs(L,10).
L = [q-6,d-4,d-9,s-3,g-0,e-6,w-5,u-7,v-8,a-8].
```                        

### Verifying the contract of predicates<a name="verifying_the_contract_of_predicates"></a>

Suppose your predicate has as [contract](https://en.wikipedia.org/wiki/Design_by_contract) that it only accepts list of 
integers >= 0.

(This is fraught with annoying edge cases... there really should be a declarative minilanguage for this; something like
[Hamcrest for Erlang](https://github.com/hyperthunk/hamcrest-erlang))

```none
% This will fail if List is not a list of integers, but succeed with `List=[]` 
% if `List` is an unbound variable.
  
my_predicate(List) :-
   maplist([Element]>>(integer(Element),Element>=0),List).
   do_work(List).

% you also allow List to be an unbound variable itself

my_predicate(List) :-
   (var(List) -> true ; maplist([Element]>>(integer(Element),Element>=0),List)).
   do_work(List).
   
% you also allow List to contain unbound variables

my_predicate(List) :-
   (var(List) -> true ; maplist( [Element]>> (var(Element) -> true; integer(Element),Element>=0 ) ,List)).
   do_work(List).
```

If you would rather throw instead of failing (When to throw, when to fail? Prolog guidelines 
[don't say anything](https://arxiv.org/abs/0911.2899), standard Prolog predicates are unsure/inconsistent,
and IMHO, it is up to the caller to demand a throw or a failure in any case using flags, 
but that's for another time entirely). See also
[Exception Handling](https://www.swi-prolog.org/pldoc/man?section=exception):

```none
ayyylmao(Msg,Args) :-
   % Msg is a "format/2" format string, Args a list of arguments.
   % List of arguments must *exactly* match the placeholders or you will get an Exception!
   % In a prod system, one would shield against further errors by catching any error that happens here!
   format(string(Buf),Msg,Args)),
   throw(Buf).

my_predicate(List) :-
   ((var(List) -> true ; maplist( [Element]>> (var(Element) -> true; integer(Element),Element>=0 ) ,List)) 
   -> true
   ; ayyylmao("You dun goofed, this list has problems: ~q",[List])),
   do_work(List).
```

Then:

```Logtalk
?- my_predicate([a,b,c]).
ERROR: Unhandled exception: "You dun goofed, this list has problems: [a,b,c]"
```

Okay, this is getting a bit off the mark. Also take a look at the existing 
predicate: [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2).

### Generating binary patterns<a name="generating_binary_patterns"></a>

A simple way to generate all binary patterns of length 3 for example:

Using `library(yall)` notation, which explicitly shows that the current element of a list appears as `Element` in the called predicate:

```none
?- 
length(List,3),maplist( [Element]>>(member(Element,[0,1])) , List).
List = [0,0,0] ;
List = [0,0,1] ;
List = [0,1,0] ;
List = [0,1,1] ;
List = [1,0,0] ;
List = [1,0,1] ;
List = [1,1,0] ;
List = [1,1,1].
```

The above is equivalent to the query:

```none
?- 
L=[I0,I1,I2], member(I0,[0,1]), member(I1,[0,1]), member(I2,[0,1]).
```

The predicate calls inside `maplist/2` are chained into a conjunction, **not simply called in turn**. And the 
choicepoints are left open. As long as all the calls succeed, we can **backtrack through possible solutions.**

In other words, `length(List,3),maplist( [Element]>>(member(Element,[0,1])) , List).` is a compressed way of writing

```none
L=[L0,L1,L2],Vs=[0,1],member(L0,Vs),member(L1,Vs),member(L2,Vs).
```

Similarly, this goal will at first generate a list of only 1s but backtracking will
enumerate all 4^5 possibilites of 5-element lists with values from `[1,2,3,4]`:

```none
length(List,5),maplist( [Element]>>(member(Element,[1,2,3,4])) , List).
List = [1,1,1,1,1] ;
List = [1,1,1,1,2] ;
List = [1,1,1,1,3] ;
List = [1,1,1,1,4] ;
List = [1,1,1,2,1] ;
List = [1,1,1,2,2] ;
List = [1,1,1,2,3] ;
List = [1,1,1,2,4] 
...
```

You could alternatively "unroll" maplist and write:

```none
L=[L0,L1,L2,L3,L4],Vs=[1,2,3,4],member(L0,Vs),member(L1,Vs),member(L2,Vs),member(L3,Vs),member(L4,Vs).
```

### Computing a value for each element of a list<a name="computing_a_value_for_each_list"></a>

Although it is better to use [`maplist/3`](https://www.swi-prolog.org/search?for=maplist%2F3)
for this (see also [`maplist/3` examples](maplist_3_examples.md)), `maplist/2` _can_ be used 
for computing values from list elements.

You cannot do aggregation operations like `max`, `min` or summation over the list elements because
the goal passed to `maplist/2` does not keep internal state (unless it has hidden
internal state, e.g by using `assert/2` and `retract/1` -- don't do that!) Value computation
stays "individual". Use [`foldl/N`](https://www.swi-prolog.org/pldoc/doc_for?object=foldl/4),
[`scanl/N`](https://www.swi-prolog.org/pldoc/doc_for?object=scanl/4) or homegrown predicates for this. 

The helper predicate name `compute_sqrt` given to `maplist/2` will be 
transformed to a term of arity 1 with a list element as argument, then called:

```none
compute_sqrt([In,Out]) :- Out is sqrt(In).
```

```none
?- 
maplist(compute_sqrt,[[1,S0],[2,S1],[3,S2],[4,S3]]).
S0 = 1.0,
S1 = 1.4142135623730951,
S2 = 1.7320508075688772,
S3 = 2.0.
```

Using `library(yall)`, we can write a one-liner, properly showing the variables in use:

```logtalk
maplist([[In,Out]]>>(Out is sqrt(In)),[[1,S0],[2,S1],[3,S2],[4,S3]]).
S0 = 1.0,
S1 = 1.4142135623730951,
S2 = 1.7320508075688772,
S3 = 2.0.
```

Again, you are better off with `maplist/3` than `maplist/2` for this kind of application.

### Imposing constraints among variables<a name="imposing_constraints_among_variables"></a>

This comes from Markus Triska's video presentation on [Map Coloring using Prolog](https://www.youtube.com/watch?v=6XD7vBbywMc).

Let us use Constraint Logic Programming over Finite Domains (in SWI-Prolog, provided
by [`library(clpfd)`](https://eu.swi-prolog.org/pldoc/man?section=clpfd)). The map coloring
problem demands that a solution obey the constraint that adjacent colors be different. The 
operator `#\=` connects terms or variables and imposes an inequality constraint on any solution.

Here is the map to color. Colors are represented by variables `A,B,C,D,E,F`:

```
 +-----------+---+---+
 |     A     | E |   |
 +---+---+---+   |   |
 |   |   |   |   |   |
 |   | C |   +---+   |
 |   +-+-+   |       |
 |  B  |  D  |   F   |
 +-----+-----+       |
 |                   |
 +-------------------+
```

The following CLP(FD) specification corresponds to the map:

```none
:- use_module(library(clpfd)).

colouring(Cs) :-
  Cs = [A,B,C,D,E,F],                   % 6 regions and their assigned colors
  A #\= B, A #\= C, A #\= D, A #\= E,   % inequalities involving A 
  B #\= C, B #\= D, B #\= F,            % more inequalities involving B
  C #\= D,                              % more inequalities involving C
  D #\= E, D #\= F,                     % more inequalities involving D
  E #\= F.                              % more inequalities involving E
```

Now Prolog can go to work finding a solution that fulfills all listed inequalities.

This still feels not at all that much different from a generate-and-test method. Now, in order to improve
readability, you can write the following, using `maplist/2`. Here, I add some `debug/3` statements from
[`library(debug)`](https://eu.swi-prolog.org/pldoc/man?section=debug) to show that no
backtracking is going on:

```none
:- use_module(library(debug)).
:- use_module(library(clpfd)).

colouring(Cs) :-
  Cs = [A,B,C,D,E,F],          debug(cs,"one",[]),
  maplist(#\=(A), [B,C,D,E]),  debug(cs,"two",[]),
  maplist(#\=(B), [C,D,F]),    debug(cs,"three",[]),
  C #\= D,                     debug(cs,"four",[]),
  maplist(#\=(D), [E,F]),      debug(cs,"five",[]),
  E #\= F,                     debug(cs,"six",[]).
```

We can now ask things like:

```
?- 
debug(cs),colouring(Cs),Cs ins 1..3,label(Cs).
% one
% two
% three
% four
% five
% six
false.
```

So we set up the inequalities using `maplist/2`, then ask for a solution. No backtracking occurs, at least
not on-stage. This is very different from a generate-and-test approach. "Setting up inequalities" must mean
creating some datastructure behind-the-scenes. Where do these inequalities "live" you ask? They
are [attributes attached to variables](https://eu.swi-prolog.org/pldoc/man?section=attvar).

### Addendum: Crazy idea: Using _maplist/2_ to copy a list the hard way<a name="crazy_idea"></a>

This code regenerates the list passed to `maplist/2` by re-constructing said list in the 
unbound variable given by the first argument to the predicate passed to `maplist/2`. 
This variable gets updated step-by-step.

```none
% "Append" Elem to the "List with dangling tail" "Conses" by going down the chain
% until the dangling tail is found, then setting it to another consbox containing
% Elem in head position

consify(Conses,Elem) :- 
   var(Conses),
   Conses = [Elem|_RestConses].
   
consify(Conses,Elem) :- 
   nonvar(Conses),
   Conses = [_|RestConses],
   consify(RestConses,Elem).

% Make a list of "Conses" by constraining/patching its dangling "rightmost tail", 
% still a variable, with '[]'

listify(Conses) :-
   var(Conses),
   format("Conses is var and will now be constrained to be []. The end!\n"),
   Conses = [].
   
listify(Conses) :-
   nonvar(Conses),
   Conses = [_|BackOfList], 
   format("Conses is not var but ~w\n",Conses),
   listify(BackOfList).
```

Then: 

```none
?- 
maplist(consify(Conses),[1,2,3,4,X,Y,Z]), 
listify(Conses).
   
Conses is not var but [1,2,3,4,_26266,_26272,_26278|_26954]
Conses is not var but [2,3,4,_26266,_26272,_26278|_26954]
Conses is not var but [3,4,_26266,_26272,_26278|_26954]
Conses is not var but [4,_26266,_26272,_26278|_26954]
Conses is not var but [_26266,_26272,_26278|_26954]
Conses is not var but [_26272,_26278|_26954]
Conses is not var but [_26278|_26954]
Conses is var and will now be constrained to be []. The end!
Conses = [1, 2, 3, 4, X, Y, Z] ;
```

- The above looks like the "self-modifying code of logic programming", the state of the 
  computation has direct influence on the predicate values; there should probably be
  some special syntax to highlight this.
- It is somewhat similar to what one would do with a "difference list on an open list", 
  just expressed in a more complex way.
  
