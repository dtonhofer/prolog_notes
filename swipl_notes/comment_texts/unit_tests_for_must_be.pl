% ===================================
% The unit tests below illustrate the approaches (provided by the system or not) 
% to type testing. They are used in discussion illustration rather than actual
% unit testing.
%
% See also "Type tests in Prolog" by Markus Triska
% https://youtu.be/ZIv0G4b1xBQ?t=123
% ===================================

% ===
% PART 1
% Using atom/1 from the standard. This is not a well-behaved predicate!
% ==

% ---
% Observe this unsettling behaviour: 
%
% ?- atom(X).
% false.
%
% which is like "Are there any atoms?" - "NO!"
%
% Its behaviour:
%
% It's a freshvar     : Should be "I don't know", instead (silently) fails
% It's not a freshvar : It's an atom     : Success.
%                       It's not an atom : Failure.
%
% library(error) also provides is_of_type/2, which behaves the same
% way, but takes the name of a type as argument, which is more
% flexible when coding.
% ---

:- begin_tests(is_atom).

test("Default atom/1 with atom")              :- atom(foo).
test("Default atom/1 with not-an-atom", fail) :- atom(1/137).
test("Default atom/1 with freshvar", fail)    :- atom(_). % BAD

test("is_of_type/2 atom with atom" )             :- is_of_type(atom,foo).
test("is_of_type/2 atom with not-an-atom", fail) :- is_of_type(atom,1/137).
test("is_of_type/2 atom with freshvar", fail)    :- is_of_type(atom,_). % STILL BAD

:- end_tests(is_atom).

% ===
% PART 2: atom_si/1
% "Sufficiently Instantiated" type checks as described in Markus Triska's lecture.
% ===

% ---
% The type checking predicate throws an error if it cannot decide. In a sense,
% the exception is used as a third truth value, which is not a bad approach.
% (Should Prolog have a third truth value? Yes. No. Which one? It's complicated!)
%
% The corresponding predicates do not exist in the SWI-Prolog system (why!)
% We add atom_si/1 for illustration purpose only.
% 
% atom_si/1 where "si" stands for "sufficiently instantiated"
%
% It's a freshvar     : Throws an instantiation error ("I don't know", 
%                       closest to a third logical value that you can get)
% It's not a freshvar : It's an atom     : Success.
%                       It's not an atom : Failure.
% ---

:- begin_tests(sufficiently instantiated).

atom_si(X) :- nonvar(X),!,(atom(X) -> true; false).
atom_si(X) :- var(X),instantiation_error(X).

test("atom_si/1 with atom")                                 :- atom_si(foo).
test("atom_si/1 with not-an-atom",fail)                     :- atom_si(1/137).
test("atom_si/1 with freshvar", error(instantiation_error)) :- atom_si(_).

:- end_tests(sufficiently_instantiated).

% ===
% PART 3: must_be/2 from library(error)
% ===

% ---
% Asserting that a type relation holds for arguments that must be 
% "sufficiently instantiated" (what this means depends on the type) 
% to proceed. This predicate behaves like an assertion/1 - if it 
% doesn't like what it sees, it throws!
%
% This used in checking parameters at library entry points, or
% among the helper predicates (during development).
%
% Jan Wielemaker writes: 
% 
% must_be/2 is not optimized in any way. It is no more and no less than a
% cheap and explicit way to add type checks intended primarily for public
% APIs of libraries. Some people tend to stick it in everywhere. That may
% seriously slow down the code. I almost never use it in cases where 
% built-in predicates do a reasonable job anyway.
%
%
% It's a freshvar     : Throws an instantiation error ("I don't know")
% It's not a freshvar : It's an atom     : Success.
%                       It's not an atom : Throws a type error ("NOPE!")
% ---

:- begin_tests(must_be).

test("must_be/2 atom with atom")                                   :- must_be(atom,foo).
test("must_be/2 atom with not-an-atom", error(type_error(_,_),_))  :- must_be(atom,1/137).
test("must_be/2 atom with freshvar", error(instantiation_error))   :- must_be(atom,_).

:- end_tests(must_be).

% ===
% PART 4: can_be/2
% "Is it possible to be" type checks as described in Markus Triska's lecture.
% ===

% ---
% Asserting that an argument can be instantiated to be of some type.
% This can be used to check parameters to be instantiated later
% in a clause. In effect, it's an early test for whether an 
% unification might fail later.
%
% The corresponding predicates do not exist. We add an implmentation
% for "atom" for illustration.
% ---

:- begin_tests(can_be).

can_be(atom, X) :- var(X),!.
can_be(atom, X) :- nonvar(X),!,(atom(X) -> true; type_error(atom,X)).

test("can_be/2 atom with atom")                                  :- can_be(atom,foo).
test("can_be/2 atom with not-an-atom", error(type_error(_,_),_)) :- can_be(atom,1/137).
test("can_be/2 atom with freshvar")                              :- can_be(atom,_).

:- end_tests(can_be).

