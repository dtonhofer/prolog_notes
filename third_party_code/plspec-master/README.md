# plspec

Inspired by [clojure.spec](https://clojure.org/about/spec), *plspec* aims to provide similar support for describing valid data.

## Get started

First, use `plspec`.
Then, decide what you want to check.

### Setup 
```
% 1.
:- use_module(plspec).  % get spec_pre, spec_invariant, spec_post and all kinds of defspec

% 2.
:- enable_spec_check(foo/2). % flag foo/2 for runtime checks
% or
:- enable_spec_check([foo/2, bar/3]). % flag all predicates in the list for runtime checks
% or
:- enable_all_spec_checks. % flag all spec'd predicates for runtime checks
```


## Semantics

Specs always have to be written somewhere above the predicate (because of term expansion magic).
You can annotate your predicate like this:

```
:- spec_pre(member/2,[any,any]). % mandatory
:- spec_invariant(member/2,[any,list(any)]).
:- spec_post(member/2,[any,var],[any,list(any)]).
member(E,[E|_]).
member(E,[_|T]) :-
    member(E,T).
```
    
But what does all of that mean? Let's look at the semantics in more detail:


### spec_pre/2

At least one `spec_pre/2` is mandatory right now.
In order to define a valid call to your predicate, the arguments have to match at least one of the specifiations in `spec_pre/2`.

In the example above, we have `spec_pre(member/2,[any,any]).`
This means that we annotate a predicate named `member` of arity 2.
Both arguments are specified to be of `any` type, i.e. every single call is a valid call.

If a call is not valid, you'll get an exception. Basically, this describes a precondition.


### spec_invariant/2

You may optionally call `spec_invariant/2`.
`spec_invariant/2` checks *your* code.
If variables are bound to values, they still have to be able to unify with a value of the given type.

In our example, we call `spec_invariant(member/2,[any,list(any)]).`
We do not care about the first argument and specify `any` for it.
For the second argument, however, we say that *if* the value is bound, it has to become a list (of any type).
Let's consider some values:

* `_`, the anonymous variable, is always a valid value.
* `[_|_]` is a valid value for any list of any type.
* `[1|_]` is okay for a list of any type or a list of integers. If we specified a list of atoms, we would throw an exception.
* `[_|a]` is not okay for a list. An exception will be thrown.
* `[]`, the empty list, always is a valid list of any type.

If you choose not to bind the variable in your predicate, an exception will be thrown even when the caller decides to bind the variable to a disallowed type later on. Neat, huh?

`spec_invariant/2`additionally allows naming of arguments. Another valid spec which might be more useful is:
`spec_invariant(member/2, [element_in_list:any, listy_mc_listface:list(any)]).`, where names are given via the functor `:/2`.
Note that either all or none of the specs must contain names.

### spec_post/3

Let's take a look at `spec_post/3`.
You can have as many as you want of these.
Aside from the predicate it references, this one actually takes two specs.
The first spec acts somewhat as a guard.
If the first spec was true when the predicate was called, the second spec has to hold when the predicate succeeds (*if* it succeeds, that is).

In the example, we can see `spec_post(member/2,[any,var],[any,list(any)]).`.
The promise you make here is that if the second argument was a variable and `member/2` succeeds, the second variable will become a list. Otherwise, you guessed it, you get an exception.


### built-ins specs

We have implemented a few specs:

* `any` allows any value
* `atomic` allows atomic values
* `atom` allows any atom
* `integer` (or `int` for short) allows integer values
* `float` allows float values
* `number` allows any kind of numbers
* `var` allows variables
* `ground` allows ground values (be careful if you use it in an invariant! There, it is equivalent to `any`.)
* `nonvar` allows values which are nonvar (be careful if you use it in an invariant! There, it is equivalent to `any`.)

There are building blocks to construct more complicated specs:

* `compound(X)` allows terms with a given functor and arity, as well as given specs for its arguments. For example, `compound(int_wrapper(int))` will allow `int_wrapper(2)`, but not `int_wrapper(pi)` or `foo(2)`.
* `list(X)` or `[X]` allows homogeneous lists whose members are of type `X`, e.g. `list(int)` only allows integers as members.
* `tuple(X)` allows heterogeneous lists of a fixed length. An example is `tuple([int, atomic])` which will accept `[2, foo]`, but neither `[foo, 2]` or `[2, foo, bar]`.
* `and(X)` takes a list `X` of other specs. Valid values have to conform to each of the specs. For example, `and([ground, list(any)])` only allows lists that are ground. 
* `one_of(X)` also takes a list `X` of other specs. Valid values have to conform to at least one of the specs. For example, `one_of([int, atomic])` will accept `3` and `foo`, but will not allow `[1]`.


### Unbound specs

Sometime you want to be more specific than `any`, but not as specific as `float`. For these cases you can use unbound specs as in the following example:

You want to reverse a list and you want to make sure that the order really changed. Look at the following implementation of `reverse/2`:

```
reverse_order([H|T],S) :-
    reverse_order(T,[H],S).

reverse_order([],Res,Res) :- !.
reverse_order([H|T],Buffer,S) :-
    reverse_order(T,[H|Buffer],S).
```
Now you want to make sure if the spec `tuple([X,Y])` holds for the first argument, `tuple([Y,X])`should be true for the second argument after the predicated succeded. This information is more specific than `tuple([any, any])` and conveniently machtes a lot of cases which you therefore don't have to add manually like `tuple([integer, atomic])`.
Attention: Some values are excluded from unifying with unbound specs. These are `ground`, `var`, `nonvar` and `any`, for the obvious reason that our specs would be always true if we allow them.

The unbound spec is unified with the most general spec. Look at the following predicate:
```
:- spec_pre(my_member/2,[X,list(X)]).
my_member(E,[E|_]).
my_member(E,[_|T]) :-
    my_member(E,T).
```
The call `my_member(a, [1,2,3])` would not cause plspec to throw an type error, because with `X = atomic` the spec succeedes.


### define your own specs

We had extensibility in mind when we wrote *plspec*. Of course, you can write your own specs and I will tell you how:


#### defspec/2

`defspec/2` is what you probably want in most of the cases. It defines an alias and is kinda mighty on its own already. Let's say you want to define your own tree spec:

```
:- defspec(tree(X), one_of([compound(node(tree(X), X, tree(X))),
                            atom(empty)])).
```

And we're done already. A tree either is empty, or a term of arity 3 where the left and right arguments are trees themselves and the center argument is a value of the given type.

##### try a spec!

You don't believe me? We can ask `valid/2` if you want me to. Yeah, I'm gonna do that. Hey, `valid/2`, get over here!
```
?- valid(tree(int), empty).
true.
```

Okay, the empty tree works. How about more complex ones?

```
?- valid(tree(int), node(empty, 1, empty)).
true.
?- valid(tree(int), node(node(empty, 1, empty),
|                        2,
|                        node(empty, 3, empty))).
true.
```

And, most importantly, what does NOT work?

```
?- valid(tree(int), node(node(empty, 1, empty),
|                        a, % not an int!
|                        node(empty, 3, empty))).
false.
?- valid(tree(int), node(node(empty, 1, emppty), % oop, a typo
|                        1,
|                        node(empty, 3, empty))).
false.
```

You can also ask `valid/2` for specs that match the given value:
```
?- valid(X, 1).
X = integer;
X = atomic;
X = number;
?- valid(tree(X), node(empty, 1, empty)).
X = integer;
X = atomic;
X = number;
```
To be not to general, `var`, `ground`, `nonvar` and `any`are excluded for possible values of `X`.


#### defspec_pred/2

Sometimes you want to check a value yourself. I get that.
That's where you use `defspec_pred/2`.

```
int(even, X) :- integer(X), 0 is X mod 2.
int(odd, X) :- integer(X), 1 is X mod 2.
:- defspec_pred(int(X), int(X)).
```

Then, you can define whether your integers are even or odd!
However, you should ensure that these predicates *NEVER* bind any variables. 
Also wnsure that your predicates always run correctly. The guard `integer(X)` is needed,
because otherwise it could cause plspec to crash, if your spec_pred is checked again an unbound variable.

```
?- valid(int(even), 0). 
true.
?- valid(int(odd), 0).
false.
?- valid(int(even), 1).
false.
?- valid(int(odd), 1).
true.
```

What happens is that we will call your predicate and just append the value as last argument to the call.


#### defspec_pred_recursive/4

Okay, this is the part where it gets crazy. You will only ever want this if you're knee deep in your own Prolog code and require extra fancy specs.

You would call it like `defspec_pred_recursive(Spec, ValidPred, MergePred, MergePredInvariant)`.
I will talk about `ValidPred` first.

In order to verify your Spec, we will call `ValidPred`. The first arguments are those you wire directly in the `defspec`.
Again, you should ensure that this predicate *NEVER* binds any variables. 
We will append three arguments. The first one is the value we are looking it. The second one is a variable where you shall return us a list of specs you want us to check later on. The last one is a list of variables that should match these specs. These lists, thus, must have the same length.

The idea is that you will only check a small part there. We implemented, for example, `compound` this way. `compound` will only check that the functor matches and delays the validation of its arguments until later. We will do that for you, somewhere in our code. Don't worry about that. The contract is that you will not receive variables during invariant checks. If you spec does not consume any part of the data, like `one_of` or `and`, continue reading but use the predicate below.

`MergePred` shall merge fully instantiated values. Arguments appended are these lists you returned, first the specs, second the values and lastly a variable that should be bound to either `true` or `false(_)`. `MergePred` shall never fail. I have implemented `and` for, e.g., compounds, as well as `or` for `one_of`.

Analogously, `MergePredInvariant` deals with values which may not be fully instantiated. In a `compound`, the arguments may be variables. You return us these variables, we will do the callback when they get bound. Of course, this is co-routine based. This is the fun part.
As for `MergePred`, I implemented `and_invariant` as well as `or_invariant`.
You want anything else, you deal with it yourself. *plspec* allows you to do so. If you want exactly *n* out of *m* specs to be fulfilled, be my guest. But you implement it.


#### defspec_connective/4

If you want to define some kind of logical connective between multiple specs, this is the predicate for you. It works exactly as `defspec_pred_recursive/4`. It is a separate predicate because `ValidPred` will do different things here, i.e. it will not consume any part of the data.

### modules

It is possible to store specs in a separate file. The idea is, every `spec_X(Predicate/Arity, [Specs])` gets expanded to `spec_X(Module:Predicate/Arity, [Specs])`.
Thus, your annotation without explicitly stating a module is used for a predicate in the same module.

If you want to add a spec to a predicate in another module, just add the module name. An example is `spec_pre(MyOtherFancyModule:foo_predicate/1, [...])`.


### register custom error handler

You can register your own error handler by calling `set_error_handler/1`. The argument shall be a predicate which can be called with a single argument, the error message. Note that the format of the error message might be changed in the future. The handler should not rely on a specific format and simply, e.g., print or store the error messages.


## bugs and feedback

We probably have tons of bugs and lots of room for improvement. If you use *plspec* and are having a bad time, please let us know. Feel free to open an issue for anything that seems wrong or could be done better. Be it documentation, examples, testcases or bugs. We can only improve if you tell us how.


## future work

It would be useful to be able to generate values out of specs, be it for testing or simply getting a feel how valid data should look.
