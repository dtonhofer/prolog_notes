% Tests from https://www.complang.tuwien.ac.at/ulrich/iso-prolog/length

% ===
% atom_length/2
% ===

:- begin_tests(test_atom_length_against_iso_prolog_wg17).

% This is according to standard
test("atom_length(A,N)",error(instantiation_error,_)) :-   
   atom_length(_A,_N). 

% This is according to standard
test("atom_length(a,a)",error(type_error(integer,a),_)) :- 
   atom_length(a,a).

% This is according to standard
test("atom_length(a,1.1)",error(type_error(integer,1.1),_)) :- 
   atom_length(a,1.1).

% Standard expects:
/*
test("atom_length(a,-1)",error(domain_error(not_less_than_zero,-1))) :- 
   atom_length(a,-1).
*/

test("atom_length(a,-1)",fail) :-
   atom_length(a,-1).

% Standard expects:
/*
test("atom_length(1,N)",error(type_error(atom,1))) :-
  atom_length(1,_N).
*/

test("atom_length(1,N)",true(N == 1)) :-
  atom_length(1,N).

:- end_tests(test_atom_length_against_iso_prolog_wg17).

% ===
% length/2
% ===

:- begin_tests(test_length_against_iso_prolog_wg17).

% This is according to standard
test("length(L,N)",true(Bag = [0-[],1-[_],2-[_,_]])) :- 
   bagof(N-L,limit(3,length(L,N)),Bag).

% This is according to standard
test("length(L,0)",true(L == [])) :- 
   length(L,0).

% This is according to standard
test("length([_|L],0)",fail) :- 
   length([_|_L],0).

% Standard wants this to just fail
test("length(2,0)",error(type_error(list,2),_)) :- 
   length(2,0).

% Standard wants this to just fail
test("length([_|2],0)",error(type_error(list,[_|2]),_)) :-
   length([_|2],0).

% Standard wants this to just fail
test("length([_|2],N)",error(type_error(list,[_|2]),_)) :-
   length([_|2],_N).

% Standard wants this to just fail
test("length([_|2],2)",error(type_error(list,[_|2]),_)) :-
   length([_|2],2).

% This is according to standard
% !! Different behaviour from atom_length/2, which just fails !!
test("length(L,-1)",error(domain_error(not_less_than_zero,-1))) :-
   length(_L,-1).

% This is according to standard
test("length([],-1)",error(domain_error(not_less_than_zero,-1))) :-
   length([],-1).

% This is according to standard
test("length(a,-1)",error(domain_error(not_less_than_zero,-1))) :-
   length(a,-1).

% This is according to standard
test("length([],-0.1)",error(type_error(integer,-0.1))) :-
   length([],-0.1).

% This is according to standard
test("length(L,-0.1)",error(type_error(integer,-0.1))) :-
   length(_L,-0.1).

% This is according to standard
test("length([a],1.0)",error(type_error(integer,1.0))) :-
   length([a],1.0).
   
% This is according to standard
test("length(L,1.0)",error(type_error(integer,1.0))) :-
   length(_L,1.0).

% This is according to standard
test("length(L,1.1)",error(type_error(integer,1.1))) :-
   length(_L,1.1).

% This is according to standard
test("length(L,1.0e99)",error(type_error(integer,1.0e99))) :-
   length(_L,1.0e99).

% This is according to standard
test("N is 2^64, length([], N)",fail) :-
   N is 2^64, length([], N).

% This is according to standard
test("length([],0+0)",error(type_error(integer,0+0))) :-
   length([],0+0).

% This is according to standard
test("length([],-_)",error(type_error(integer,-_))) :-
   length([],-_).

% This is according to standard
test("length([a],-_)",error(type_error(integer,-_))) :-
   length([a],-_).

% Standard is unclear
test("length([a,b|X],X)",fail) :-
   length([a,b|X],X).

% Standard is unclear
test("length(L,L)",fail) :-
   length(L,L).

% This is according to standard
test("L = [_|_], length(L,L)",error(type_error(integer,[_|_]))) :-
   L = [_|_], length(L,L).

% This is according to standard
test("L = [_], length(L,L)",error(type_error(integer,[_]))) :-
   L = [_], length(L,L).

% This is according to standard
test("L = [1], length(L,L)",error(type_error(integer,[1]))) :-
   L = [1], length(L,L).

% Undefined in standard
test("L = [a|L], length(L,N)",error(type_error(list,_))) :-
   L = [a|L], length(L,_N).

% Undefined in standard
test("L = [a|L], length(L,0)",error(type_error(list,_))) :-
   L = [a|L], length(L,0).

% Undefined in standard
test("L = [a|L], length(L,7)",error(type_error(list,_))) :-
   L = [a|L], length(L,7).

:- end_tests(test_length_against_iso_prolog_wg17).


