# What Prolog needs as additional features

... Just some crazy ideas I'm writing down as I code.

A lot of things could be done by having a code preprocessor & editor support. There would be additional syntax, but no changes to the implementation (WAM or otherwise).

- Predicate arguments appearing in the head should be accessible by special variables like `$ARG1`, `$ARG2` etc. This is an application of the DRY principle.
  With this, Then one can write clauses which can both have structure in the head (for matching) and which can access that structure in the body w/o
  the need to reconstruct it by hand: `foo([X,Y|Z],A,B) :- g($ARG1).` instead of `foo([X,Y|Z],A,B) :- g([X,Y|Z]).`    
- [HERE documents](https://en.wikipedia.org/wiki/Here_document) to embed text blocks to output elegantly in code. (but you would also need string 
  interpolation to make this really useful)
- Local naming contexts so that helper predicates are only visible to and can only be called from their "master predicates" 
  (and so that helper predicates can be identified as helper predicates). This can be achived by some naming convention for
  predicate names with some additional enforcement, e.g. hierarchical names for the helper predicates: `master$helper(..) :- ....`
  Or maybe modules need to be made hierarchical.
- A way to automatically extract the signature of a module (including comments etc.)

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
