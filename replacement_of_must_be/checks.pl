:- module(onepointfour_basics_checks,
          [
           check_that/2        % check_that(X,Conditions)
          ,check_that/3        % check_that(X,Conditions,Throw)
          ,check_that_named/3  % check_that_named(X,Conditions,Name)
          ,check_that_named/4  % check_that_named(X,Conditions,Name,Throw)
          ]).

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

/*  MIT License Follows (https://opensource.org/licenses/MIT)

    Copyright 2021 David Tonhofer <ronerycoder@gluino.name>

    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files
    (the "Software"), to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge,
    publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*

 * **********************************
 * A replacement for the must_be/2 predicate (which is used to
 * check preconditions on predicate entry) of Prolog, in particular
 * SWI-Prolog.
 *
 * See: https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2
 * **********************************

Note the structure given to check_that(X,Cs), with X being the term to check.

Cs is a list of *conditions*, which are compound terms.
The list of conditions behaves like a "conditional and" (aka. "short-circuiting and")
which can also "break out to succees" or throw at a condition.

  true           : Succeed unconditionally, do not verify conditions "further to the right" in the list of conditions.
  false,fail     : Fail unconditionally.
  break(Check)   : Succeed (and break off further tests) if Check succeeds, and do not verify conditions "further to the right" in the list of conditions.
  fail(Check)    : Fail if Check succeeds.
  lenient(Check) : If Check succeeds, proceed with conditions "further to the right", otherwise fail if "not throw" or throw a Check-relevant exception if "throw" ("throw" is switched on if Throw is one of =|true|= or =|throw|=).
  strict(Check)  : If Check succeeds, proceed with conditions "further to the right", otherwise throw a Check-relevant exception, irrespective of the value of Throw.
  fullylenient(Check) : Fails even if the value is underspecified to make a proper decision (i.e. if X is uninstantiated).
                        Generally, one would throw in that case.
                        The Prolog predicates actually behave like that, i.e. "atom(_)" fails instead of throwing, actually performing a second-order check about the state of computation.
                        In this context, one would eschew "fullylenient" and perform a releavant "fail" check to the left to get the same effect

Shorther aliases of the above:

  tr
  fa
  br(Check)
  fa(Check)
  le(Check)
  st(Check)
  fule(Check)

Synopsis:

  Fail if X is not a string

     check_that(X,[lenient(string)])

  Throw if X is not a string

    check_that(X,[strict(string)])

  Throw if X is not an integer and then fail if X is not a positive integer

    check_that(X,[strict(int),lenient(posint)])

  Or you can switch "lenient" to behave "strictly":

    check_that(X,[strict(int),lenient(posint)],throw)

  Fail if foo is not a member of the given list with a supplementary argument:

    check_that(foo,[lenient(member([alpha,bravo,charlie]))]).

  Throw if foo is not a member of the given list (it would really cool if
  one could inject the source code line into the message; that can be done
  with term_expansion I think. Maybe later)

    check_that(foo,[lenient(member([alpha,bravo,charlie]))],throw).
    ERROR: check failed : domain error: "the culprit is outside the required domain"
    ERROR:    message   : "the value should fulfill 'list-member-ship-ness'"
    ERROR:    culprit   : foo

Usage scenarios:

  - Use check_that/2 to verify predicate entry preconditions
    - Expecting to switch them off at "production time" (assertions) to gain performance
    - They are also goo "live documentation" saying exactly what parts of the input space are covered by throws and which ones by fails
      One can rely on "implicitly building" that space via the behaviour of the predicates called in turn, but may become unclear
      what that space is. (This may be ok depending on coding style)
    - Using check_that/2 "normally" as a guard to make the predicate
      - throw on extremely bad arguments (i.e. badly typed ones)
      - fail on bad arguments (i.e. out-of-domain ones)
     - Logic/Search parts of the program will preferentially fail (or only enter the situation where a fail makes sense)
     - Functional programming parts of the program will preferentially throw (or only enter the situation where a throw makes sense)
  - Use check_that/2 to verify invariants inside of predicates (generally not needed as this is done by checking pre/postconditions in Prolog)
    - Expecting to switch them off at "production time" (assertions) to gain performance
    - TODO: Such cases must be marked as "this is not expected to be violated in running code" (throwing a really nasty exception)
      And then the test must be switch-offable using a specific hierarchical key, just like you switch logging on or off that way.
  - Use check_that/2 normally in code, as just a particular form of a guard, i.e. it is not expected that they will be switched off
  - (There is no easy way to perform postconditions-on-success in Prolog except by wrapping a predicate with another predicate. Annoying.)

Assertions

  - Assertions are basically the subset of conditions that one does not expect to fail or throw at runtime.
    The idea is to remove those tests because they "never fail" and the insurance will pick up the slack if they do.
    Prolog is special in that "failing" is an integral part of its approach, so switching off checks wholesale is not an option
    (Unless one wants to really have separate instructions for "necessary checks" and "assertion checks")
    To "remove unnecessary checks" it must both be possible to:
    1) identify them. This can be done my giving them a special name, e.g. lenient_assert instead of just lenient
    2) be able to "remove them" cheaply; this can be done during compilation phase: the marked conditions can be written out
    3) select those which should be removed based on program structure: eg. all those in module XY should be removed
       this also seems a case for the compilation phase

Desgin question especially clear in case of complex checking of lists:

  - The structure to test may have multiple layers of testing. For example, for a "proper list of char":
    - X is a var             -> fail or exception
    - X is not a proper list -> fail or exception
    - X contains vars        -> fail or exception
    - X contains non-chars   -> fails or exception
    - and finally success
    A caller could demand throwing down to any level of the above (and possibly accept var)
    The **proper way to have this flexibility** is exactly to use check_that/2 with the detailed
    more-and-more-precise check sequence, going from strictness to leniency depending on taste, for example
      check_that(X,[break(var),strict(list),strict(list(nonvar)),lenient(list(char))])
    instead of a single monolitic
      check_that(X,[break(var),strict(chars)])
    However, in that case the "rightmost checks" may perform wasteful checks against that we already
    know will succeed. So there is a need for doing bare-bones checks (maybe?). Probably not worth it.
    Note that what exception to throw is generally made in this order:
    X uninstantiated -> throw instantiation error
    X instantiated but not of the correct type (e.g. expecting integer but it's a float) -> throw type error
    X of the correct type but not in the correct domain (e.g. expecting positive integer but it's negative) -> throw domain error

Note that eval generally fails (or throws, if so demanded) if the question is "about" an uninstantiated variable.
However, passing an uninstantiated variable is actually a different level in that the question posed by the
predicate is "ill-formed" (e.g. is "_" an atom? cannot be answered (there is not enough information) unless one
considers this is a second-order question, in that "unbound variables" become themselves the object of discourse;
in which case "second-order: is an unbound variable an atom" can be answered with 'false'). Anyway, should ill-formed
questions _really_ be answered with failure/standard-throw or with an unconditional throw of a dedicated exception?
I am unsure about this. For, it's "as usual".

The Check is either an atom or a compound term from the following list of atoms and compound terms (aliases are indicated with "/")
TODO: The unbound variable is mostly a special case and should maybe merit special handling (an unconditional exception)

  var nonvar
  nonground ground
  atom/symbol
  atomic/constant
  compound
  boolean                            (either the atom 'true' or the atom 'false')
  pair                               (a compound term with functor name '-' and arity 2)
  string                             (an SWI-Prolog string)
  stringy                            (either an atom or an SWI-Prolog string)
  nonempty_stringy                   (nonempty stringy: a stringy that is different from '' and "")
  char                               (an atom of length 1, this is the traditional Prolog 'char' type)
  char_list,chars                    (a proper list of 0 or more chars; unbound elements are not allowed)
  code                               (any integer between 0 and 0x10FFFF meant to represent an Unicode code point)
  code_list,codes                    (a proper list of 0 or more codes; unbound elements are not allowed)
  chary                              (a char or a code)
  chary_list,charys                  (a proper list of 0 or more chars or codes (but consistently only one of those); unbound elements are not allowed)
  stringy_typeid                     (one of the atoms 'string' or 'atom'; compare to 'boolean')
  chary_typeid                       (one of the atoms 'char' or 'code'; compare to 'boolean')
  number                             (any number)
  float                              (any float, including +1.0Inf, -1.0Inf, NaN, -0.0)
  float_not_nan                      (any float, excluding NaN)
  float_not_inf                      (any float, excluding +1.0Inf, -1.0Inf)
  float_not_neginf                   (any float, excluding -1.0Inf)
  float_not_posinf                   (any float, excluding +1.0Inf)
  int/integer                        (an integer)
  rational                           (a rational, incldues integers)
  nonint_rational/proper_rational    (a rational that is not an integer)
  negnum/negnumber                   (strictly negative number)
  posnum/posnumber                   (strictly positive number)
  neg0num/neg0number                 (negative-or-zero number)
  pos0num/pos0number                 (positive-or-zero number)
  non0num/non0number                 (non-zero number)
  negint/negative_integer            (strictly negative integer)
  posint/positive_integer            (strictly positive integer)
  neg0int                            (negative-or-zero integer)
  pos0int/nonneg                     (positive-or-zero integer)
  negfloat posfloat                  (strictly negative/positive float)
  neg0float pos0float                (negative-or-zero/positive-or-zero float)
  inty                               (an integer or a float that represents an integer, e.g 1 or 1.0)
  neginty posinty                    (strictly negative/positive inty)
  neg0inty pos0inty                  (negative-or-zero/positive-or-zero inty)
  list/proper_list                   (a proper list, including the empty list)
  nonempty_list                      (a proper list that is not empty)
  dict                               (an SWI-Prolog dict)
  cyclic                             (a term that has a cyclic structure)
  acyclic                            (a term that has no cyclic structure for now, but have acquire it later unless it is also ground)
  acyclic_forever                    (a term that is both ground and acyclic)
  stream                             (a term that is a stream name (atom) or a valid stream handle (blob))
  member(ListOfValues)               (member of a list of values; test is unification)
  random(Probability)                (randomly fails with probability 0 =< Probability =< 1)
  forall(ListOfChecks)               (recursive: the term to check must pass all checks in ListOfChecks)
  forany(ListOfChecks)               (recursive: the term to check must pass at least one check in ListOfChecks)
  fornone(ListOfChecks)              (recursive: the term to check must pass no check in ListOfChecks), also useful as negation
  passall(Check)                     (all the terms in the list of terms that is the input term must pass the Check)
  passany(Check)                     (at least one term in the list of terms that is the input term must pass the Check)
  passnone(Check)                    (none of the terms in the list of terms that is the input term must pass the Check)

Compare with: Checks from must_be/2. Those marked "Ok" have been implemented in "check_that/N"

 n/a any                     : any term, including an unbound variable (this reduces to true)
  Ok atom,symbol             : passes atom/1
  Ok atomic,constant         : passes atomic/1
  Ok oneof(L)                : ground term that is member of L; if there is an unbound variable in L, everything passes!
  Ok compound                : passes compound/1, the complement of atomic/1 (includes dicts, but don't use that fact as that dict implementation through compounds may change!)
  Ok pair                    : a key-value pair or rather a compound term -/2
  Ok boolean                 : one of the two atoms 'true' or 'false'
  Ok integer                 : passes integer/1
  Ok positive_integer        : integer > 0
  Ok negative_integer        : integer < 0
  Ok nonneg                  : nonneg *integer*; this should really be called "nonneg_integer"
  Ok                           ...corresponding tests >=0, >0 etc for *general* numbers are missing
  Ok float                   : passes float/1 (in SWI-Prolog, an IEEE 64-bit float)
  Ok rational                : a non-integer rational or an integer
  Ok number                  : passes number/1
     between(FloatL,FloatU)  : if FloatL is float, all other values may be float or integer (FIXME?); the limits are both INCLUSIVE; limits may be equal but NOT reversed
     between(IntL,IntU)      : if IntL is integer, all other values must be integer; the limits are both INCLUSIVE; limits may be equal but NOT reversed
                            FIXME: there should be specific between_int/2 and between_float/2 if one goes that way.
  Ok acyclic                 : passes acyclic_term/1, i.e. is an acyclic term; includes unbound var (the term is a tree if one disregards "internal sharing")
  Ok cyclic                  : passes cyclic_term/1, i.e. the term contains cycles (the term is a "rational tree"). Does not accept an unbound variable.
  Ok char                    : atom of length 1
  Oj chars                   : list of 1-character atoms; includes the empty list
  Ok code                    : unicode code point (any integer between 0 and 0x10FFFF)
  Ok codes                   : list of integers >= 0; includes the empty list
  Ok string                  : passes string/1, an SWI-Prolog string
     text                    : atom or string or chars or codes (but not numbers even though some predicates "textify" those)
  Ok var                     : passes var/1; must be an unbound variable
  Ok nonvar                  : passes nonvar/1; anything except an unbound variable
  Ok list,proper_list        : passes is_list/1 and is a proper/closed list; empty list is allowed
     list(Type)              : proper list with elements of type Type (must_be/2(Type,_) is called on elements); empty list is allowed;
                               on error the index is not indicated (why~~~). A type like "list(list(integer))" is ok!
     list_or_partial_list    : A partial list (one ending in a variable: [x|_]). This includes an unbound variable.
     callable                : passes callable/1. Relatively usesless, as "callable" is ill-defined. Basically (atom(X);compound(X))
  Ok dict                    : passes is_dict/1, an SWI-prolog dict (which is just a compound term of a particular form)
     encoding                : valid name for a character encoding; see current_encoding/1, e.g. utf8 (but not "utf8" or 'utf-8'; also fails for 'iso_8859_1')
  Ok stream                  : passes is_stream/1, is a stream name or valid stream handle
     type                    : Meta: Term is a valid type specification for must_be/2. This is done by looking up whether a clause `has_type(Type,_) :- ....` exists.
                               Example: must_be(type,list(integer)). However, "must_be(type,list(grofx))": OK, but "must_be(type,grofx)": NOT OK.
Possible extension:

  predicate_indicator     : A Name/Arity predicate indicator
  nonempty_list           : A list that is also nonempty
  list_length_X           : Tests for length of lists (larger, smaller, equal)
  subsumes
  would_unify
  does_not_unify
  dif

## More

   @license [MIT License](https://opensource.org/licenses/MIT)
   @author David Tonhofer (ronerycoder@gluino.name)

*/

% ---
% 1) 'lenient' checks will fail instead of throw (unless there the check cannot be executed)
% 2) No specific name for the value is given, so messages in exceptions will be relatively generic.
% If Conditions is not a list, it is transformed into a list first.
% ---

check_that(X,Conditions) :-
   check_that(X,Conditions,false).

% ---
% 1) You can select whether 'lenient' checks should fail or throw. If throwing is preferred,
%    set Throw to one of the atoms 'throw' or 'true'. Anything else means failing is preferred.
% 2) No specific name for the value is given, so messages in exceptions will be relatively generic.
% If Conditions is not a list, it is transformed into a list first.
% ---

check_that(X,Conditions,Throw) :-
   is_proper_list(Conditions)
   ->
   check_that_1(Conditions,X,"",Throw)
   ;
   check_that_1([Conditions],X,"",Throw).

% ---
% As above, but additionally pass a name to be be used in messages used in exceptions
% ---

check_that_named(X,Conditions,Name) :-
   check_that_named(X,Conditions,Name,false).

% ---
% As above, but additionally pass a name to be be used in messages used in exceptions
% ---

check_that_named(X,Conditions,Name,Throw) :-
   is_proper_list(Conditions)
   ->
   check_that_1(Conditions,X,Name,Throw)
   ;
   check_that_1([Conditions],X,Name,Throw).


                     /*********************************************
                      * What lies beneath the exported predicates *
                      *********************************************/

% ---
% Two-step process:
% 1) Verify the syntactic correctness of all conditions.
%    Throw if there is a problems with the syntax.
%    TODO: That should be done during compilation, i.e. using term expansion, because
%    checking the syntax once is enough!
% 2) Evaluate the conditions and their embedded checks.
%
% For the syntax checks, the code tries to tell the caller _why_ there is a problem,
% instead of just "failing" with zero information left, so it's a bit unidiomatic,
% Prolog needs something to handle that elegantly.
% ---

check_that_1(Conditions,X,Name,Throw) :-
   % syntax check
   no_var_in_list_or_throw(Conditions),
   wellformed_conds_or_throw(Conditions,X),
   % evaluate the conditions
   check_that_2(Conditions,X,Name,Throw).

no_var_in_list_or_throw(Conditions) :-
   no_var_in_list(Conditions)
   ->
   true
   ;
   throw_various(syntax,"unbound variable in conditions list",Conditions).

no_var_in_list([C|More]) :-
   nonvar(C),
   no_var_in_list(More).
no_var_in_list([]).

wellformed_conds_or_throw(Conditions,X) :-
   wellformed_conds(Conditions,X) % may also throw
   ->
   true
   ;
   throw_various(syntax,"conditions do not pass syntax check",Conditions).

% ---
% wellformed_conds(Conditions,X)
%
% Verify the conds of Conditions for syntactic correctness. This is done
% outside of actual check-evaluation eval/4, to have clean code and to be able to
% disable the verification for well-formedness by (currently) commenting out
% the call to wellformed/2.
% ---

wellformed_conds([Condition|More],X) :-
   exists_cond_or_throw(Condition),
   wellformed_cond_or_throw(Condition,X),
   wellformed_conds(More,X).
wellformed_conds([],_).

exists_cond_or_throw(Condition) :-
   exists_cond(Condition)
   ->
   true
   ;
   throw_various(unknown_condition,"unknown condition found during syntax check",Condition).

exists_cond(true).
exists_cond(tr).
exists_cond(false).
exists_cond(fail).
exists_cond(fa).
exists_cond(break(_Check)).
exists_cond(br(_Check)).
exists_cond(fail(_Check)).
exists_cond(fa(_Check)).
exists_cond(lenient(_Check)).
exists_cond(le(_Check)).
exists_cond(strict(_Check)).
exists_cond(st(_Check)).
exists_cond(fullylenient(_Check)).
exists_cond(fule(_Check)).

wellformed_cond_or_throw(Condition,X) :-
   atom(Condition)
   ->
   true
   ;
   (Condition =.. [_,Check], wellformed_check_or_throw(Check,X)).

wellformed_check_or_throw(Check,X) :-
   wellformed_check_2(Check,X)
   ->
   true
   ;
   throw_various(unknown_or_problematic_check,"unknown or problematic check found during syntax check",Check).

wellformed_check_2(Check,_)                 :- atom(Check),!,atomoform_checks(AFCs),memberchk(Check,AFCs).
wellformed_check_2(member(ListOfValues),_)  :- is_proper_list(ListOfValues).
wellformed_check_2(type(ListOfTypes),_)     :- is_proper_list(ListOfTypes),atomoform_checks(AFCs),maplist({AFCs}/[T]>>memberchk(T,AFCs),ListOfTypes).
wellformed_check_2(random(Probability),_)   :- number(Probability),0=<Probability,Probability=<1.
wellformed_check_2(forall(ListOfChecks),X)  :- wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(forany(ListOfChecks),X)  :- wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(fornone(ListOfChecks),X) :- wellformed_list_of_checks(ListOfChecks,X).
wellformed_check_2(passall(Check),ListOfX)  :- wellformed_check_over_list(Check,ListOfX).
wellformed_check_2(passany(Check),ListOfX)  :- wellformed_check_over_list(Check,ListOfX).
wellformed_check_2(passnone(Check),ListOfX) :- wellformed_check_over_list(Check,ListOfX).

wellformed_list_of_checks(ListOfChecks,X) :-
   is_proper_list(ListOfChecks),
   forall(
      member(Check,ListOfChecks),
      wellformed_check_or_throw(Check,X)). % ** recursive **

wellformed_check_over_list(Check,ListOfX) :-
   is_proper_list_or_throw(Check,ListOfX),
   forall(
      member(M,ListOfX),
      wellformed_check_or_throw(Check,M)). % ** recursive **

is_proper_list_or_throw(Check,ListOfX) :-
   is_proper_list(ListOfX)
   ->
   true
   ;
   throw_various(type,"check needs a list as argument",[check(Check),arg(ListOfX)]).

% ---
% All the atoms for "elementary checks", i.e. those that
% are not a compound term, are listed here.
% ---

atomoform_checks(
   [
   var,nonvar,
   nonground,ground,
   atom,symbol,
   atomic,constant,
   compound,
   boolean,
   pair,
   string,stringy,
   char,code,chary,
   char_list,chars,
   code_list,codes,
   chary_list,charys,
   nonempty_stringy,
   stringy_typeid,
   chary_typeid,
   number,float,integer,int,rational,nonint_rational,proper_rational,
   negnum,negnumber,
   posnum,posnumber,
   neg0num,neg0number,
   pos0num,pos0number,
   non0num,non0number,
   float_not_nan,
   float_not_inf,
   float_not_neginf,
   float_not_posinf,
   negint,negative_integer,
   posint,positive_integer,
   neg0int,pos0int,nonneg,
   negfloat,posfloat,
   neg0float,pos0float,
   inty,
   neginty,posinty,
   neg0inty,pos0inty,
   list,proper_list,
   nonempty_list,
   dict,
   cyclic,acyclic_now,acyclic_forever,
   stream
   ]
).

% ---
% check_that_2(Conditions,TermToCheck,NameOfTerm,ThrowFlag)
%
% One we have passed the basic syntactic/well-formedness checks of
% check_that_1/4, we can proceed to evaluate the checks in-order.
% Generally this means unpacking the compound term of the check and
% calling eval/4.
%
% In case we find an unknown check, we throw ISO type error in the
% penultimate clause. If check_that_1/4 indeed performed
% well-formedness checks, this actually can't happen.
% ---

check_that_2([Condition|More],X,Name,Throw) :-
   exists_cond_or_throw(Condition), % always succeeds if the syntax check was passed
   check_that_3(Condition,X,Name,Throw,Outcome), % needs no internal cut
   !, % no need to go back on success
   outcome_branching(Outcome,Condition,More,X,Name,Throw). % recurses
check_that_2([],_,_,_).

outcome_branching(break,_,_,_,_,_)           :- !.
outcome_branching(done,_,_,_,_,_)            :- !.
outcome_branching(fail,_,_,_,_,_)            :- !,fail.
outcome_branching(next,_,More,X,Name,Throw)  :- !,check_that_2(More,X,Name,Throw).
outcome_branching(Outcome,Condition,_,_,_,_) :- throw_various(unknown_outcome,"bug: condition yields unknown outcome",[Condition,Outcome]).

check_that_3(true  ,_,_,_,break).
check_that_3(tr    ,_,_,_,break).

check_that_3(fail  ,_,_,_,fail).
check_that_3(false ,_,_,_,fail).
check_that_3(fa    ,_,_,_,fail).

check_that_3(break(Check),X,Name,_,Outcome) :-
   eval(Check,X,Name,false,true)
   ->
   Outcome = break
   ;
   Outcome = next.
check_that_3(br(Check),X,Name,Throw,Outcome) :-
   check_that_3(break(Check),X,Name,Throw,Outcome).

check_that_3(fail(Check),X,Name,_,Outcome) :-
   eval(Check,X,Name,false,true)
   ->
   Outcome = fail
   ;
   Outcome = next.
check_that_3(fa(Check),X,Name,Throw,Outcome) :-
   check_that_3(fail(Check),X,Name,Throw,Outcome).

check_that_3(lenient(Check),X,Name,Throw,Outcome) :-
   eval(Check,X,Name,Throw,true)
   ->
   Outcome = next
   ;
   Outcome = fail.
check_that_3(le(Check),X,Name,Throw,Outcome) :-
   check_that_3(lenient(Check),X,Name,Throw,Outcome).

check_that_3(strict(Check),X,Name,_,Outcome) :-
   eval(Check,X,Name,throw,true)
   ->
   Outcome = next
   ;
   throw_various(strict_check_fails,"check should throw instead of fail",Check).
check_that_3(st(Check),X,Name,_,Outcome) :-
   check_that_3(strict(Check),X,Name,_,Outcome).

check_that_3(fullylenient(Check),X,Name,Throw,Outcome) :-
   eval(Check,X,Name,Throw,Throw) % pass Throw also on last position
   ->
   Outcome = next
   ;
   Outcome = fail.
check_that_3(fule(Check),X,Name,Throw,Outcome) :-
   check_that_3(fullylenient(Check),X,Name,Throw,Outcome).

check_that_3([],_,_,_,done).

% ---
% Checking the "Throw" flag for being set
% ---

throw_is_set(Throw) :- Throw==true.
throw_is_set(Throw) :- Throw==throw.

% ---
% Special precondition checks: we want to unconditionally throw if X does not have enough
% information for a meaningful answer, i.e. if we encounter an "instantiation error"
% ---

/*
precondition_X_must_be_ground(X,Name,Ness,Throw) :-
   ground(X)
   ->
   true
   ;
   (
      throw_is_set(Throw), % if this fails, the call fails (which is what we want)
      select_name(Name,Name2),
      format(string(Msg),"~s should be ground. Can't check for '~s-ness'",[Name2,Ness]),
      throw_2(too_little_instantiation,Msg,X)
   ).
*/

precondition_X_must_be_instantiated(X,Name,Ness,Throw) :-
   nonvar(X)
   ->
   true
   ;
   (
      throw_is_set(Throw), % if this fails, the call fails (which is what we want)
      select_name(Name,Name2),
      format(string(Msg),"~s should be instantiated. Can't check for '~s-ness'",[Name2,Ness]),
      throw_2(too_little_instantiation,Msg,X)
   ).

precondition_X_must_be_list(X,Name,Ness,Throw) :-
   is_proper_list(X)
   ->
   true
   ;
   (
      throw_is_set(Throw), % if this fails, the call fails (which is what we want)
      select_name(Name,Name2),
      format(string(Msg),"~s should be a proper list. Can't check for '~s-ness'",[Name2,Ness]),
      throw_2(type,Msg,X)
   ).

% ---
% Predicates which check whether the Throw flag is set, and if so,
% construct an exception message and then throw via throw_2/3.
% Otherwise they fail.
%
% "Ness" is a a free string that expresses what was expected byut not seen
% For example, if "Ness" is "integer", the message will be about missing
% "integer-ness".
% ---

throw_or_fail_for_case_random(Throw) :-
   throw_is_set(Throw), % if this fails, the call fails (which is what we want)
   throw_2(random,"random failure after calling maybe/1").

throw_or_fail(Error,X,Name,Throw,Ness) :-
   throw_is_set(Throw), % if this fails, the call fails (which is what we want)
   select_name(Name,Name2),
   format(string(Msg),"~s should fulfill '~s-ness'",[Name2,Ness]),
   throw_2(Error,Msg,X).

% ---
% Basement throwing predicate constructing the exception term itself.
% ---

throw_2(domain(Expected),Msg,Culprit)             :- throw(error(check(domain                   ,Expected,Msg,Culprit),_)).
throw_2(type(Expected),Msg,Culprit)               :- throw(error(check(type                     ,Expected,Msg,Culprit),_)).
throw_2(domain,Msg,Culprit)                       :- throw(error(check(domain                   ,_       ,Msg,Culprit),_)).
throw_2(type,Msg,Culprit)                         :- throw(error(check(type                     ,_       ,Msg,Culprit),_)).
throw_2(too_much_instantiation,Msg,Culprit)       :- throw(error(check(too_much_instantiation   ,_       ,Msg,Culprit),_)). % ISO's "uninstantiation error"
throw_2(too_little_instantiation,Msg,Culprit)     :- throw(error(check(too_little_instantiation ,_       ,Msg,Culprit),_)). % ISO's "instantiation error"
throw_2(passall,Msg,Culprit)                      :- throw(error(check(passall                  ,_       ,Msg,Culprit),_)).
throw_2(passany,Msg,Culprit)                      :- throw(error(check(passany                  ,_       ,Msg,Culprit),_)).
throw_2(passnone,Msg,Culprit)                     :- throw(error(check(passnone                 ,_       ,Msg,Culprit),_)).
throw_2(forall,Msg,Culprit)                       :- throw(error(check(forall                   ,_       ,Msg,Culprit),_)).
throw_2(forany,Msg,Culprit)                       :- throw(error(check(forany                   ,_       ,Msg,Culprit),_)).
throw_2(fornone,Msg,Culprit)                      :- throw(error(check(fornone                  ,_       ,Msg,Culprit),_)).
throw_2(_,_,_)                                    :- throw("Bug! You forgot a throw_2/3"). % Having this saves the day when debugging.

throw_2(random,Msg)                               :- throw(error(check(random                   ,_       ,Msg,_      ),_)).

throw_various(Type,Msg,Culprit)                   :- throw(error(check(Type,_,Msg,Culprit),_)).
throw_on_bug(strict_check_just_fails,Msg,Culprit) :- throw(error(check(strict_check_just_fails,_,Msg,Culprit),_)).

% ---
% Helpers for throwing
% ---

select_name(Name,"the value") :-
   (var(Name);Name=='';Name==""),
   !.
select_name(NameIn,NameOut) :-
   format(string(NameOut),"~q",[NameIn]).

% ---
% Properly printing the error(check(_,_,_,_),_) exception term
% by adding rules to the prolog::error_message//1 multifile DCG rule.
% ---

:- multifile prolog:error_message//1.  % 1-st argument of error term

prolog:error_message(check(Type,Expected,Msg,Culprit)) -->
    { build_main_text_pair(Type,MainTextPair) },
    [ MainTextPair, nl ],
    lineify_expected(Expected),
    lineify_msg(Msg),
    lineify_culprit(Culprit).

% ---
% Helpers for prolog:error_message(Formal)
% ---

extended_msg(domain,                   "the culprit is outside the required domain").
extended_msg(type,                     "the culprit is not of the required type").
extended_msg(too_much_instantiation,   "the culprit is already (fully) instantiated").
extended_msg(too_little_instantiation, "the culprit is not instantiated (enough)").
extended_msg(not_ground,               "the culprit should be ground").
extended_msg(random,                   "this is a random error due to the outcome of maybe/1").

make_sure_it_is_string(X,X) :-
   string(X),
   !.
make_sure_it_is_string(X,Str) :-
   atom(X),
   !,
   atom_string(X,Str).
make_sure_it_is_string(X,Str) :-
   format(string(Str),"~q",[X]).

build_main_text_pair(Type,MainTextPair) :-
   extended_msg(Type,ExMsg),
   !,
   make_sure_it_is_string(ExMsg,ExMsgStr),
   MainTextPair = 'check failed : ~q error (~s)'-[Type,ExMsgStr].
build_main_text_pair(Type,MainTextPair) :-
   MainTextPair = 'check failed : ~q error'-[Type].

lineify_expected(Expected) -->
   { nonvar(Expected), make_sure_it_is_string(Expected,ExpectedStr) },
   !,
   [ '   expected  : ~s'-[ExpectedStr], nl ].
lineify_expected(_) --> [].

lineify_msg(Msg) -->
   { nonvar(Msg), make_sure_it_is_string(Msg,MsgStr) },
   !,
   [ '   message   : ~s'-[MsgStr], nl ].
lineify_msg(_) --> [].

lineify_culprit(Culprit) -->
   { nonvar(Culprit), make_sure_it_is_string(Culprit,CulpritStr) },
   !,
   [ '   culprit   : ~s'-[CulpritStr], nl ].
lineify_culprit(_) --> [].

% ----
% The "type test" for inty terms: accepts an integer
% or a float that represents an integer. Anything else
% causes failure to type error.
% ---

just_an_inty(X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"inty",TP),
   ((integer(X);inty_float(X))
    ->
    true
    ;
    throw_or_fail(type(int_or_float),X,Name,Throw,"inty")).

% ---
% Accept a float that represents an integer
% round(X)=:=X fails for NaN because NaN =/= NaN, so there is no
% need to test for NaN separately.
% ---

inty_float(X) :-
   float(X),
   X =\= -1.0Inf,
   X =\= +1.0Inf,
   round(X)=:=X.

% ---
% This one just exists to make code clearer
% ---

is_proper_list(L) :-
   is_list(L).

% ---
% eval(Check,TermToCheck,NameOfTerm,ThrowOrFail,ThrowOrFailForPrecondition)
% This predicate needs no internal cuts as a unique decision is taken on arg 1
% ---

eval(var,X,Name,Throw,_TP) :-
   var(X)
   ->
   true
   ;
   throw_or_fail(too_much_instantiation,X,Name,Throw,"var").

eval(nonvar,X,Name,Throw,_TP) :-
   nonvar(X)
   ->
   true
   ;
   throw_or_fail(too_little_instantiation,X,Name,Throw,"nonvar").

eval(ground,X,Name,Throw,_TP) :-
   ground(X)
   ->
   true
   ;
   throw_or_fail(domain,X,Name,Throw,"ground").

eval(nonground,X,Name,Throw,_TP) :-
   \+ground(X)
   ->
   true
   ;
   throw_or_fail(domain,X,Name,Throw,"nonground").

eval(atom,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"atom",TP),
   (atom(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"atom")).

eval(symbol,X,Name,Throw,TP) :-
   eval(atom,X,Name,Throw,TP).

eval(atomic,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"atomic",TP),
   (atomic(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"atomic")).

eval(constant,X,Name,Throw,TP) :-
   eval(atomic,X,Name,Throw,TP).

eval(compound,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"compound",TP),
   (compound(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"compound")).

eval(boolean,X,Name,Throw,TP) :-
   eval(atom,X,Name,Throw,TP),
   ((X==true;X==false)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"boolean")).

eval(pair,X,Name,Throw,TP) :-
   eval(compound,X,Name,Throw,TP),
   (X = -(_,_)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"pair")).

eval(string,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"string",TP),
   (string(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"string")).

eval(stringy,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"stringy",TP),
   ((atom(X);string(X))
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"stringy")).

eval(nonempty_stringy,X,Name,Throw,TP) :-
   eval(stringy,X,Name,Throw,TP),
   ((X\=='',X\== "")
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"nonempty stringy")).

eval(char,X,Name,Throw,TP) :-
   eval(atom,X,Name,Throw,TP),
   % Note that we need to test atom/1 first because atom_length/2 transforms-to-atom!
   % atom_length/2 may be too wasteful to test for a precise length (?)
   (atom_length(X,1)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"char")).

eval(code,X,Name,Throw,TP) :-
   eval(integer,X,Name,Throw,TP),
   ((0=<X,X=<0x10FFFF)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"code")).

eval(chary,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"chary",TP),
   (integer(X)
    ->
       ((0=<X,X=<0x10FFFF)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Throw,"code"))
    ;
    atom(X)
    ->
       (atom_length(X,1)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Throw,"char"))
    ;
    throw_or_fail(type,X,Name,Throw,"chary")).

eval(number,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"number",TP),
   (number(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"number")).

eval(float,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"float",TP),
   (float(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float")).

eval(int,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"integer",TP),
   (integer(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"integer")).

eval(integer,X,Name,Throw,TP) :-
   eval(int,X,Name,Throw,TP).

eval(rational,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"rational",TP),
   (rational(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"rational")).

eval(nonint_rational,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"nonint_rational",TP),
   (rational(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"nonint_rational")),
   (integer(X)
    ->
    throw_or_fail(domain,X,Name,Throw,"nonint_rational")
    ;
    true).

eval(proper_rational,X,Name,Throw,TP) :-
   eval(nonint_rational,X,Name,Throw,TP).

eval(negnum,X,Name,Throw,TP) :-
   eval(number,X,Name,Throw,TP),
   ((X < 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly negative number")).

eval(negnumber,X,Name,Throw,TP) :-
   eval(negnum,X,Name,Throw,TP).

eval(posnum,X,Name,Throw,TP) :-
   eval(number,X,Name,Throw,TP),
   ((X > 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly positive number")).

eval(posnumber,X,Name,Throw,TP) :-
   eval(posnum,X,Name,Throw,TP).

eval(neg0num,X,Name,Throw,TP) :-
   eval(number,X,Name,Throw,TP),
   ((X =< 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"number that is =< 0")).

eval(neg0number,X,Name,Throw,TP) :-
   eval(neg0num,X,Name,Throw,TP).

eval(pos0num,X,Name,Throw,TP) :-
   eval(number,X,Name,Throw,TP),
   ((X >= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"number that is >= 0")).

eval(pos0number,X,Name,Throw,TP) :-
   eval(pos0num,X,Name,Throw,TP).

eval(non0num,X,Name,Throw,TP) :-
   eval(number,X,Name,Throw,TP),
   ((X =\= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"number that is not 0")).

eval(non0number,X,Name,Throw,TP) :-
   eval(non0num,X,Name,Throw,TP).

eval(float_not_nan,X,Name,Throw,TP) :-
   eval(float,X,Name,Throw,TP),
   ((NaN is nan,X \== NaN) % arithmetic comparison would fail
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"float that is not NaN")).

eval(float_not_inf,X,Name,Throw,TP) :-
   eval(float,X,Name,Throw,TP),
   ((X =\= -1.0Inf,X =\= +1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"float that is not positive or negative infinity")).

eval(float_not_neginf,X,Name,Throw,TP) :-
   eval(float,X,Name,Throw,TP),
   ((X =\= -1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"float that is not negative infinity")).

eval(float_not_posinf,X,Name,Throw,TP) :-
   eval(float,X,Name,Throw,TP),
   ((X =\= +1.0Inf)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"float that is not positive infinity")).

eval(negint,X,Name,Throw,TP) :-
   eval(int,X,Name,Throw,TP),
   ((X<0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly negative integer")).

eval(negative_integer,X,Name,Throw,TP) :-
   eval(negint,X,Name,Throw,TP).

eval(posint,X,Name,Throw,TP) :-
   eval(int,X,Name,Throw,TP),
   ((X>0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly positive integer")).

eval(positive_integer,X,Name,Throw,TP) :-
   eval(posint,X,Name,Throw,TP).

eval(neg0int,X,Name,Throw,TP) :-
   eval(int,X,Name,Throw,TP),
   ((X =< 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"integer that is =< 0")).

eval(pos0int,X,Name,Throw,TP) :-
   eval(int,X,Name,Throw,TP),
   ((X >= 0)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"integer that is >= 0")).

eval(nonneg,X,Name,Throw,TP) :-
   eval(pos0int,X,Name,Throw,TP).

eval(inty,X,Name,Throw,TP) :-
   just_an_inty(X,Name,Throw,TP).

eval(neginty,X,Name,Throw,TP) :-
   eval(inty,X,Name,Throw,TP),
   (((integer(X),X<0);(float(X),X<0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly negative inty")).

eval(posinty,X,Name,Throw,TP) :-
   eval(inty,X,Name,Throw,TP),
   (((integer(X),X>0);(float(X),X>0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly positive inty")).

eval(neg0inty,X,Name,Throw,TP) :-
   eval(inty,X,Name,Throw,TP),
   (((integer(X),X=<0);(float(X),X=<0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"inty that is =< 0")).

eval(pos0inty,X,Name,Throw,TP) :-
   eval(inty,X,Name,Throw,TP),
   (((integer(X),X>=0);(float(X),X>=0.0))
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"inty that is >= 0")).

eval(negfloat,X,Name,Throw,TP) :-
   eval(float_not_nan,X,Name,Throw,TP),
   (X<0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly negative float")).

eval(posfloat,X,Name,Throw,TP) :-
   eval(float_not_nan,X,Name,Throw,TP),
   (X>0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly positive float")).

eval(neg0float,X,Name,Throw,TP) :-
   eval(float_not_nan,X,Name,Throw,TP),
   (X=<0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"float that is =< 0")).

eval(pos0float,X,Name,Throw,TP) :-
   eval(float_not_nan,X,Name,Throw,TP),
   (X>=0.0
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"float that is >= 0")).

eval(list,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"list",TP),
   (is_proper_list(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"proper list")).

eval(proper_list,X,Name,Throw,TP) :-
   eval(list,X,Name,Throw,TP).

eval(nonempty_list,X,Name,Throw,TP) :-
   eval(list,X,Name,Throw,TP),
   (X \== []
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"proper nonempty list")).

eval(dict,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"dict",TP),
   (is_dict(X)
    ->
    true
    ;
    throw_or_fail(type,X,Name,Throw,"dict")).

eval(stringy_typeid,X,Name,Throw,TP) :-
   eval(atom,X,Name,Throw,TP),
   ((X==atom;X==string)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"stringy_typeid")).

eval(chary_typeid,X,Name,Throw,TP) :-
   eval(atom,X,Name,Throw,TP),
   ((X==char;X==code)
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"chary_typeid")).

eval(char_list,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"char_list",TP),
   precondition_X_must_be_list(X,Name,"char_list",Throw), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   forall(member(MX,X),eval(char,MX,Name,Throw,TP)). % TODO: Open up to get at the index

eval(chars,X,Name,Throw,TP) :-
   eval(char_list,X,Name,Throw,TP).

eval(code_list,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"code_list",TP),
   precondition_X_must_be_list(X,Name,"code_list",Throw), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   forall(member(MX,X),eval(code,MX,Name,Throw,TP)). % TODO: Open up to get at the index

eval(codes,X,Name,Throw,TP) :-
   eval(code_list,X,Name,Throw,TP).

eval(chary_list,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"chary_list",TP),
   precondition_X_must_be_list(X,Name,"chary_list",Throw), % Dual traversal, but one is in C, so this may be faster than "unifying" to a single traversal.
   eval(forany([chars,codes]),X,Name,Throw,TP).  % TODO: Open up to get at the index and get better error messages
   % Simple, but the error is confusing for "check_that([a,2],strict(chary_list))" for example and does not give the index

eval(charys,X,Name,Throw,TP) :-
   eval(chary_list,X,Name,Throw,TP).



eval(member(ListOfValues),X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"list_membership",TP), % must be ground or must be instantiated ? difficult ...
   precondition_X_must_be_list(ListOfValues,Name,"list_membership",TP), % actually, it is not X but ListOfValues which must be a list!
   ((\+ \+ member(X,ListOfValues)) % Use \+ \+ to roll back any accidental bindings
    ->
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"list_membership")).

eval(random(Probability),_X,_Name,Throw,_TP) :-
   maybe(Probability)  % throws type error on value not in [0.0,1.0]
   ->
   true
   ;
   throw_or_fail_for_case_random(Throw).

% These REALLY are "second order" (i.e. about how the terms are represented)
% Indeed, asking whether an unbound variable is cyclic or not makes some.
% These questoins are actually too rough.
% One might have:
% 1) Assuredly cyclic
% 2) Not cyclic for now but can become cyclic later (case of any nonground
%    compound, including the unbound variable)
% 3) Not cyclic now and cannot become cyclic later (case of atomic or
%    ground compound)

eval(cyclic,X,Name,Throw,_TP) :-
   cyclic_term(X) % fails on X unbound, which is ok
   ->
   true
   ;
   throw_or_fail(type,X,Name,Throw,"cyclic").

eval(acyclic_now,X,Name,Throw,_TP) :-
   \+ cyclic_term(X)
   ->
   true
   ;
   throw_or_fail(type,X,Name,Throw,"tentatively acyclic").

eval(acyclic_forever,X,Name,Throw,_TP) :-
   (ground(X),\+ cyclic_term(X))
   ->
   true
   ;
   throw_or_fail(type,X,Name,Throw,"acyclic now and cannot become cyclic later").

eval(stream,X,Name,Throw,TP) :-
   precondition_X_must_be_instantiated(X,Name,"stream",TP),
   (atom(X)
    ->
       (is_stream(X)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Throw,"atom-naming-a-stream"))
    ;
    atomic(X)
    ->
       (is_stream(X)
        ->
        true
        ;
        throw_or_fail(domain,X,Name,Throw,"atomic-designating-a-stream"))
    ;
    throw_or_fail(type,X,Name,Throw,"atom-or-atomic")).

% ---
% eval with forall/forany/fornone
% ---

eval(forall(ListOfChecks),X,Name,Throw,TP) :-
   forall_forall_loop(ListOfChecks,X,Name,Throw,TP) % Throw is passed
   ->
   true
   ;
   throw_or_fail(forall,checks(ListOfChecks)-item(X),Name,Throw,"all of the checks succeed for the item").

eval(forany(ListOfChecks),X,Name,Throw,TP) :-
   forany_forall_loop(ListOfChecks,X,Name,TP) % Throw is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(forany,checks(ListOfChecks)-item(X),Name,Throw,"at least one of the checks succeeds for the item").

eval(fornone(ListOfChecks),X,Name,Throw,TP) :-
   fornone_forall_loop(ListOfChecks,X,Name,TP) % Throw is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(fornone,checks(ListOfChecks)-item(X),Name,Throw,"none of the checks succeeds for the item").

% ---
% eval with passall/passany/passnone
% TODO: - We give not precise indicates which X failed the check how (i..e no index)
%       - Also, passany and passnone do not give any information about what went wrong at
%         all as they fail the individual checks and then throw a generic exception
%       Is fixing both of these worth the complexity?
% ---

eval(passall(Check),ListOfX,Name,Throw,TP) :-
   passall_forall_loop(Check,ListOfX,Name,Throw,TP) % Throw is passed; thrown exception informs about problem
   ->
   true
   ;
   throw_or_fail(passall,check(Check)-items(ListOfX),Name,Throw,"all of the items pass the check").

eval(passany(Check),ListOfX,Name,Throw,TP) :-
   passany_forall_loop(Check,ListOfX,Name,TP) % Throw is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(passany,check(Check)-items(ListOfX),Name,Throw,"at least one of the items passes the check").

eval(passnone(Check),ListOfX,Name,Throw,TP) :-
   passnone_forall_loop(Check,ListOfX,Name,TP) % Throw is not upheld, can only fail
   ->
   true
   ;
   throw_or_fail(passnone,check(Check)-items(ListOfX),Name,Throw,"none of the items passes the check").

% ---
% More helpers
% ---

forall_forall_loop(ListOfChecks,X,Name,Throw,TP) :-
  forall(                           % success of ListOfChecks is empty
      member(Check,ListOfChecks),
      eval(Check,X,Name,Throw,TP)).

forany_forall_loop(ListOfChecks,X,Name,TP) :-
   \+forall(                        % failure if ListOfChecks is empty
      member(Check,ListOfChecks),
      \+eval(Check,X,Name,false,TP)).  % disable throwing

fornone_forall_loop(ListOfChecks,X,Name,TP) :-
   (ListOfChecks == [])             % force failure if ListOfChecks is empty
   ->
   false
   ;
   forall(
      member(Check,ListOfChecks),
      \+eval(Check,X,Name,false,TP)). % disable throwing

passall_forall_loop(Check,ListOfX,Name,Throw,TP) :-
  forall(                           % success if ListOfX is empty
      member(X,ListOfX),
      eval(Check,X,Name,Throw,TP)).

passany_forall_loop(Check,ListOfX,Name,TP) :-
   \+forall(                        % failure if ListOfX is empty
      member(X,ListOfX),
      \+eval(Check,X,Name,false,TP)).  % disable throwing

passnone_forall_loop(Check,ListOfX,Name,TP) :-
   (ListOfX == [])                  % force failure if ListOfX is empty
   ->
   false
   ;
   forall(
      member(X,ListOfX),
      \+eval(Check,X,Name,false,TP)). % disable throwing

