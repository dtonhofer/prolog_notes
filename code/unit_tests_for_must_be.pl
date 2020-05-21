% ===
% The unit tests below illustrate the approaches (provided by the system or not) to type testing
%
% See also "Type tests in Prolog": https://youtu.be/ZIv0G4b1xBQ?t=123 by Markus Triska
%
% Run the unit tests with: 
% ?- rt(_).
% ===

:- begin_tests(type_testing_for_atom).

% ===
% Default Practice in Prolog
% ===

% Using atom/1 from the standard. Silently fails for wrong types.
% Observe: 
% ?- atom(X). "Are there atoms" - "NO!"
%
% It's a freshvar     : Should be "I don't know", but (silently) fails
% It's not a freshvar : It's an atom     : Success.
%                       It's not an atom : Failure.

test(default_isatom)          :- atom(foo).
test(default_isnotatom, fail) :- atom(1/137).
test(default_freshvar, fail)  :- atom(_).

% ---
% library(error) also provides is_of_type/2, which behaves in the
% standard way, but takes the name of a type as argument, which is more
% flexible.
% ---

test(isoftype_isatom)          :- is_of_type(atom,foo).
test(isoftype_isnotatom, fail) :- is_of_type(atom,1/137).
test(isoftype_freshvar, fail)  :- is_of_type(atom,_).

% ===
% "Sufficiently Instantiated" type checks.
% The type checking predicate throws an error if it cannot decide.
%
% The corresponding predicates do not exist. We add atom_si/1
% for illustration.
% ===

% atom_si/1 where "si" stands for "sufficiently instantiated"
%
% It's a freshvar     : Throws an instantiation error ("I don't know", 
%                       closest to a third logical value that you can get)
% It's not a freshvar : It's an atom     : Success.
%                       It's not an atom : Failure.

atom_si(X) :- nonvar(X),!,(atom(X) -> true; false).
atom_si(X) :- var(X),instantiation_error(X).

test(si_atom_isatom)                               :- atom_si(foo).
test(si_atom_isnotatom,fail)                       :- atom_si(1/137).
test(si_atom_freshvar, error(instantiation_error)) :- atom_si(_).

% ===
% Asserting that a type relation holds for arguments that must be 
% "sufficiently instantiated" (what this means depends on the type) 
% to proceed.
%
% This is used in checking parameters at library entry points, or
% among the helper predicates (during development).
%
% must_be/2 from library(error)
% ===

% It's a freshvar     : Throws an instantiation error ("I don't know")
% It's not a freshvar : It's an atom     : Success.
%                       It's not an atom : Throws a type error ("NOPE!")

test(mustbe_atom_isatom)                               :- must_be(atom,foo).
test(mustbe_atom_isnotatom, error(type_error(_,_),_))  :- must_be(atom,1/137).
test(mustbe_atom_freshvar, error(instantiation_error)) :- must_be(atom,_).

% ===
% Asserting that an argument can be instantiated to be of some type.
% This can be used to check parameters to be constrained/bound later
% in the predicate. In effect, it's an early test for whether an 
% unification will fail later.
%
% can_be/2 
%
% The corresponding predicates do not exist. We add an implmentation
% for "atom" for illustration.
% ===

can_be(atom, X) :- var(X),!.
can_be(atom, X) :- nonvar(X),!,(atom(X) -> true; type_error(atom,X)).

test(canbe_atom_isatom)                              :- can_be(atom,foo).
test(canbe_atom_isnotatom, error(type_error(_,_),_)) :- can_be(atom,1/137).
test(canbe_atom_freshvar)                            :- can_be(atom,_).

:- end_tests(type_testing_for_atom).

rt(type_testing_for_atom) :- run_tests(type_testing_for_atom).
 
