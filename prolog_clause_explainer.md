# Prolog clause explainer:

You are presented with this line in Prolog:

````
h(X,Z) :- p(X,Y) , q(Y,Z,w) .
````

How to read this?

First off:

- the `X`, `Y`, `Z` are _variables_ because they are symbols starting with an uppercase letter.
- the `h`, `p`, `q` are _predicate identifiers_ used in `predicate calls`:
   - they start with a lowercase letter (indicating they are not variables)
   - and depend on arguments `q(.,.)`
   - they appear at the line's "top level", not inside parenthesis of other expressions: `p(X,Y)`, not `q(p(X,Y),Y)`.
- finally the `w`, not being a variable, is a constant.

Unlike in other programming languages, nothing in that line "resolves" to anything, nothing is replaced by values, computed or looked-up. The constant `w` is just the constant `w`. 

In fact, you will often see things like this:

````
h(X,k) :- p(f(X),g(a,b)),q(X).
````

The `f(X)` and `g(a,b)` look like function calls, and `f` and `g` are even called _function symbols_ but there is no 
intent to express an evaluation. `f(X)` and `g(a,b)` are just syntax to represent trees of stuff. Trees of stuff are also 
called _terms_. If a term is a leaf of the tree, it is generally an _atom_ like `a`, also called a _constant_, or a variable like `X`. Otherwise it is a special atom appearing as a "functor", with subterms listed inside parenthesis. If the functor appears at the top level, it is a "predicate symbol", otherwise it is a _fucntion symbol_.

Here, 

- the variables are in deep blue;
- the functors which appear as predicate symbols are in yellow;
- the functors which appear as function symbols are in orange;
- the constants are in purple;
- "operators", which are just specially designated functors written in infix fashion, are in grey.

![Prolog tree of stuff](pics/prolog_clause_explainer/Prolog%20Tree%20of%20Stuff.png)

Back to:

````
h(X,Z) :- p(X,Y) , q(Y,Z,w) .
````

That line can be read in isolation of anything else in the logic program. None of the elements
are determined by context. In other languages this is not the case: the meaning of `p(x)` depends on what `x` has
been set to somewhere else in the code. Not so here.

The line can be separated into the following parts:

![Prolog clause parts](pics/prolog_clause_explainer/Prolog%20Clause.png)

The line can be read "logically":

![Prolog clause logical reading](pics/prolog_clause_explainer/Prolog%20Clause%20Logical%20Reading.png)

Note that the "material implication" premiss is on the left, and the consequent is on the right, as is the custom.

- All the variables appearing in the premiss are universally quantified over the whole clause.
- All the variables appearing in the conclusion only are existentially quantified over the conclusion.

This reading can be of use when you try to map a problem into Prolog. 

It is taken to be a "true sentence", i.e. a constraint over an universe of "floating things". 
It states that whenever you encounter three floating things `X`, `Y`, `Z` where there is a relation `p/2` between
`X` and `Y` and a relation `q/3` between `Y`, `Z` and a known floating thing `w`, you can conclude there is also
a relation `h` between floating things `X` and `Z`. 

![Prolog network of floating things](pics/prolog_clause_explainer/Prolog%20Clause%20Network.png)

Such a clause actually a very compressed way to express that a certain graph has a certain structure. In fact, a Prolog program in its entirety is a compressed description of a certain set of graphs that we want to examine.

An important restriction is that we only talk about the *existence* of relations. Prolog (or more correctly, Horn-clause
logic) does not allow us to talk about the *non-existence* of relations because there is just no "strict negation" in 
the language.  

The inference above has worked from premiss to conclusion. This is "modus ponens".

Prolog works the other way round. It tries to find out whether the conclusion holds by checking whether the premiss holds. Thus backwards-chaining or search. This is waek sauce because the conclusion may hold irrespective of whether the premiss holds - there is no "equivalence" sign in the constraint just an "implication" sign. But that's ok - we agree to work in a system in which the conclusion is assumed to be false unless it can be proven through one of our rules (it is "minimal" in that sense). It turns out that this is quite sufficient.

On the other hand the coder's reading is more traditional.

Roughly, consider `h(X,Y)` as a call to a function that:

- Takes two pointers to "logical variables" `X` and `Y`, which are actually blackboards onto which are inscribed "things we know" about `X` and `Y` (for example, the blackboard for `X` may contain the information that `X` is an integer between 10 and 100, and the blackboard for `Y` may contain the information that `Y` is `2*X`)
- Calls the functions of its right-hand side in order from left-to-right. Each of these refines the information regarding `X` and `Y` (never relaxing the information, but always making it more precise).   
- Returns `true` if there is an approach at refinining `X` and `Y` (and the `Y`) such that all of the functions of the right-hand side return true. Finding this approach is where "search" and "backtracking" enter the picture. In that case, the blackboards for `X` and `Y` contain the consistent additional information about `X` and `Y` (for example, the blackboard for `X` may now contain the information that `X` is 55, and the blackboard for `Y` may contain the information that `Y` is 110)
- Returns "false" (gives up) if such an approach cannot be found. In that case, the blackboards for `X` and `Y` are reset to the state prior to the call.
- If `true` is returned, the caller can consult the refined state for `X` and `Y` in the blackboards for these variables, which he has access to, and possibly even created as "fresh" variables. Note that if the topmost level in the call hierarchy created the variable, the information created way down in the hierarchy is "instantly" communicated to the topmost level, because the blackboards are always shared, never local (though they may go out of scope, as happens for the `Y` blackboard on return in this case).

![Prolog Calling](pics/prolog_clause_explainer/Prolog%20Calling.png)

By rearranging the picture, the well-known structure of an AND tree appears:

![Prolog AND tree](pics/prolog_clause_explainer/Prolog%20Calling%20AND%20Tree.png)

Together with backtracking, the whole of the Prolog program appears as an AND-OR tree, and execution of a Prolog program is all about exploring that tree (images TBD).

It is often the case, that at call time, one of the variables is completely known, and one is completely unknown. This happens in particular whenever one maps function evaluation onto predicate calls. Then one gets the picture of an information flow weaving through the clause:

![Prolog information flow](pics/prolog_clause_explainer/Prolog%20Infoflow.png)
