# Example code to verify predicate arguments

## Preliminary notes

> **Note on naming:**
>
> - **Parameters** are the **formal parameters**: Source code lists the _parameters_: `foo(Param0,Param1) :- bar(Param0,Param1).`
> - **Arguments** are the **actual parameters**: `foo/2` is called with two atom _arguments_: `foo(a0,a1)` 
> 
> In Prolog, the entries of a compound term are _also_ called arguments: `p(Arg0,Arg1)`
> 
> A _parameter_ takes on an actual value, the _argument_ at that position.
>
> **verifying**, **checking** and **testing** of arguments carries more or less the same meaning.
> We shall use _verifying_.

We show an example of a predicate that extensively verfies the arguments it has been passed.

This is a defensive approach. If a predicate _fails_ instead of _throwing_ for calls where one or the other 
parameter has a value situtated out of its valid domain, someone may start to rely on this beahviour.
If the domain of the predicate is extended later to cover more cases, unexpected success may lead to
hard-to-diagnose problems.

Sometimes just failing instead of throwing makes some sense:

```
?- atom_length(a,-1).
false.
```

However, this looks very much like an artifact of extremely weak typing: a length value should never be negative, really.

Another point of view is that whether a predicate should fail or throw depending on bad arguments
depends on the context: It should be the caller that decides. One solution would be to pass a "processing option"
telling the predicate to fail, not throw. ANother would be to add a "lenient" wrapping predicate, which catches
all exceptions thrown by the wrapped predicate, dumps them and just calls `fail` instead. See below.

In fact, I content that a predicate should NOT jsut fail on out-of-domain arguments, but **always** throw.
One **does** want to be informed when a predicate is called with garbage. It could be an important information.
If the caller really wants to just have the predicate fail on bad input, doing so should be made explicit in the code.
Thus there should be an `atom_length_ng/2` and  `atom_length_ng/3`:

```
% Not exported from the module in which this is defined.

atom_length_strict(Atom,Length) :-
   ((nonvar(Atom),\+atom(Atom))        -> type_error(atom,Atom) ; true),
   ((nonvar(Length),\+integer(Length)) -> type_error(integer,Length) ; true),
   ((nonvar(Length),Length<0)          -> domain_error(positive_integer,Length) ; true),
   atom_length(Atom,Length).

opts_contains_lenient(Opts) :-
   is_dict(Opts),               % fails if not dict
   get_dict(lenient,Opts,true). % fails if key does not exist

% Exported from the module in which this is defined.

% Decision whether to be strict or lenient is pulled from an 
% "options" SWI-Prolog dict (one way of doing it, it seems the most palatable)

% Default behaviour is **strict**:

atom_length_ng(Atom,Length) :- 
   atom_length_ng(Atom,Length,_).
   
% Special way of eliciting **lenient** behaviour:

atom_length_ng(Atom,Length,Opts) :-
   opts_contains_lenient(Opts) 
   -> catch(atom_length_strict(Atom,Length),_Catcher,fail) 
   ;  atom_length_strict(Atom,Length).

```

And thus:

```
?- atom_length_ng(atom,-1).
ERROR: Domain error: `positive_integer' expected, found `-1'

?- atom_length_ng(atom,-1,_).
ERROR: Domain error: `positive_integer' expected, found `-1'

?- atom_length_ng(atom,-1,_{lenient:true}).
false.
```

That's more like it. 

## Code 

The whole code: [case_study.pl](code/case_study.pl)

### What does the example predicate describe?

In the present example, we have a 2-D _numbers_ domain, associated to a _zone names_ domain:

```
     Y
     ^     
     |
   2 +-----------+-------------+
     |           |             |
     | Z = X*Y*Y |  undefined  |
     | "zone C"  |             |
     |           |             | 
   1 +-----------+-------------+
     |           |             |
     | Z = X*Y   |  Z=X*X*Y    |
     | Z = X+Y   |  "zone B"   |
     | "zone A"  |             | 
     +-----------+-------------+---> X
   (0,0)         1             2
```

- Any pair of numbers _(X,Y)_ that is not inside one of "zone A", "zone B" or "zone C" is considered "out-of-domain".
- For "zone A", there are two values computed from _(X,Y)_ (there is non-determinism), the other two zones just yield a single value.

### Code structure

The "core predicate" `core_foo/4` assumes that the type and the domain of the arguments seen is correct (it 
is thus relatively fragile).

It consists of three clauses, each protected by a _guard_ that verifies in which zone the pair _(X,Y)_ 
lies, as that cannot be verified through head unification alone. The guard performs no variable
bindings and does not throw exceptions. It just succeeds or fails. (You can make double sure that
variable bindings are not performed by preceding the call to the guard with the double negation, `\+ \+`,
but that usage is rare).

The guard goals have been moved out to their own predicates for nicer code structure.

Guard conditions could be made laxer (i.e. some conditions could be left out) as we do assume that arguments are
inside the domain, but let's keep it relatively strict for clarity. Code clarity is always worth more than
a few cycles saved:

```
guard_zone_a(X,Y) :- 0=<X, X<1, 0=<Y, Y<1.
guard_zone_c(X,Y) :- 0=<X, X<1, 1=<Y, Y<2.
guard_zone_b(X,Y) :- 1=<X, X<2, 0=<Y, Y<1.
```

We arrange the code so that we commit to the selected clause after each guard (these are green cuts,
not changing the semantics, as only a single guard in the set of guards can succeed). So:

```
core_foo(X,Y,Z,zone_c) :- guard_zone_a(X,Y), !, (Z is X+Y;Z is X*Y). 
core_foo(X,Y,Z,zone_a) :- guard_zone_a(X,Y), !, Z is X*Y*Y.
core_foo(X,Y,Z,zone_d) :- guard_zone_d(X,Y), !, Z is X*X*Y.
```

Or maybe clearer using [`->`](https://eu.swi-prolog.org/pldoc/doc_for?object=(-%3E)/2):

```
core_foo(X,Y,Z,zone_a) :- guard_zone_a(X,Y) -> (Z is X+Y;Z is X*Y). 
core_foo(X,Y,Z,zone_c) :- guard_zone_c(X,Y) -> Z is X*Y*Y.
core_foo(X,Y,Z,zone_b) :- guard_zone_b(X,Y) -> Z is X*X*Y.
```

Then, just using the above with no further verifications:

```
?- core_foo(0.1,0.1,Z,Name).
Z = 0.2,
Name = zone_a ;
Z = 0.010000000000000002,
Name = zone_a ;
false.
```

For `core_foo/4`, anything outside of the allowed domain of the "three allowed zone" _fails_, but we want it to _throw_ instead:

```
?- core_foo(300,300,Z,Name).
false.
```

If the caller calls `core_foo/4` with a bad zone identifer, the call also fails, which is what we want:

```
?- core_foo(0.1,0.1,Z,zone_b).
false.
```

The "core part" of the predicate shall now be preceded by predicate `before_foo/4` that performs all
argument verifications and throws on problems. 

It behaves like a "throwing guard" and is deterministic: It just throws or succeeds.

It does not change variable bindings. Again, you can make double sure that variable bindings are not
performed by preceding the call to `before_foo/4` with the double negation, `\+ \+`. That construct lets
through exceptions.

In strongly and statically typed languages, many of these tests are made superfluous because
the compiler will refuse to even compile the code if a verification premises might become _false_
The verification code won't even exist in the generated code. 

Here, the burden of verification rests with us, the verification code will be live at runtime and 
running it repeatedly might well become costly. 

A special dynamic predicate by which the verification code can be switched on and off might be of use.

Some of these verifications can be replaced by the nicer and more declarative
[`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2), `must_be(+Type, @Term)`

In some cases (not in this example) one might want to check whether a variable `Out` is able to "take on a type/domain" 
at predicate success. For example if `Out` is list of unbound variables, can it be refined to a list of integers?
(the answer is yes). A predicate `can_be(+Type, @Term)` might be of use, but there is no such thing in the library.

The verifications are all written as:

> desired_condition -> true ; throw_some_error

In order to perform fine-grained announcements, the same variable may occur in several verifications.

```
% ---
% Define a list of allowed values for "zone names" once, centrally:
% ---

const(zone_names,[zone_a,zone_b,zone_c]).

% ---
% A dynamic predicate through which verification code can be disabled on demand:
% ---

:- dynamic verify/2.

verify(foo/4).   % It's on for foo/4!

% ---
% The predicate to verify all the arguments that will be passed to core_foo/4.
% It either succeeds (deterministically) or throws. It looks a bit ugly.
% ---

before_foo(X,Y,Z,ZoneName) :-
   const(zone_names,ZoneNames),                        % Retrieve the allowed values for Zone names
   (nonvar(X)  -> true ; instantiation_error('X')),    % X needs to be instantiated
   (nonvar(Y)  -> true ; instantiation_error('Y')),    % Y needs to be instantiated
   (var(Z)     -> true ; uninstantiation_error('Z')),  % Z needs to be uninstantiated (to take up numeric value)
   (number(X)  -> true ; type_error(number,X)),        % X,Y must be a number: that's a condition on the type
   (number(Y)  -> true ; type_error(number,Y)),
   ((0=<X,X<2) -> true ; domain_error("[0,2[",X)),     % X,Y must be within a known 1-D range: that's a condition on the domain
   ((0=<Y,Y<2) -> true ; domain_error("[0,2[",Y)),     % ISO standard says nothing about how to *express* the domain :-(
   (\+ (1=<X,X<2,1=<Y,Y=<2)                            % X,Y must be within a known 2-D range: that's a condition on the domain
       -> true 
       ;  domain_error("valid zone",[X,Y])),           % Again, how do we *express* the domain in the exception term??
   ((var(ZoneName);atom(ZoneName))                     % ZoneName must be uninstantiated or an atom (to compare against)
       -> true  
       ;  type_error(atom,ZoneName)),                  % ... that's a condition on the type
   (nonvar(ZoneName)                                   % If Zone has been given
      -> (memberchk(ZoneName,ZoneNames)                % ... it must be one of the allowed zone values ("enum")
         -> true
         ;  domain_error(ZoneNames,ZoneName))          % Otherwise it's a domain error. 
      ; true).                                         % Again, expressing "what's wrong" and "what would be right" is nearly impossible
```

The actual predicate `foo/4` is then simply as follows.

```
foo(X,Y,Z,ZoneName) :-
   (verify(foo/4) -> \+ \+ before_foo(X,Y,Z,ZoneName) ; true),
   core_foo(X,Y,Z,ZoneName).
```   

The `verify(foo/4) -> ` switch could also be performed in `before_foo/4` directly, or as a separate `before_foo/4` clause. 

Note that clause selection for `foo/4` is "weak" in the sense that the only criterium for success is whether
head unification succeeeds (unless the caller uses terms with unbound variables that have a 
[`dif/2` constraint](https://eu.swi-prolog.org/pldoc/doc_for?object=dif/2) or are otherwise 
[attributed](https://eu.swi-prolog.org/pldoc/man?section=attvar), so that coroutine predicates may veto
head unification behind-the-scenes). Prolog might benefit from being extended to
[Many-sorted logic](https://en.wikipedia.org/wiki/Many-sorted_logic) so that head unification fails for
mismatched "sorts" (which are types, more or less). But then, one might not want it to fail but to
throw.

### Failing instead of throwing

If you want to make `foo/4` lenient, in the sense that instead of thrown exceptions, the predicate just fails
(in which case predicate failure has the scrambled meaning of "core predicate fails in its domain" or "bad arguments passsed"), 
the best approach seems to be to catch-and-fail:

```
foo_lenient(X,Y,Z,ZoneName) :- 
   catch(foo(X,Y,Z,ZoneName),_Catcher,fail).
```

In some extreme cases it might be interesting to to code a second `before_foo_lenient/4` which never throws, just fails, and
is invoked instead of `before_foo/4` depending on on a processing option passed to now `foo/5`, formerly `foo/4`. But that
duplicates code and is nasty. Just do that if the cost of many no-operation throw-catch sequences can actually be measured.

The problem with the above is that it also catches exceptions generated "deeper down the stack", by predicates called by foo/4. 
These you might not want to catch, they could have something to do with I/O etc.

And so:

```
?- core_foo(300,300,Z,Name).
false.

?- foo(300,300,Z,Name).
ERROR: Domain error: `[0,2[' expected, found `300'

?- foo_lenient(300,300,Z,Name).
false.
```

