:- module(heatwarp_conversion,[convert_to_string/2]).

% ===
% Perform certain type conversion in an easy-on-the-programmer's mind matter
% ===

/*
% clause 1: if the firsst argument X is nonvar/instantiated, commit with a cut & 
%           leave all further checks to atom_string/2, which may throw if the argument 
%           is bad or else suceed or fail; atom_string/2 does magic conversions on X, too
% clause 2: the path taken if first argument X is is fresh/uninstantiated, which
%           we don't accept, so we throw

convert_to_string(X,Str) :- nonvar(X),!,atom_string(X,Str). 
convert_to_string(X,_)   :- var(X),!,instantiation_error(X).
   
convert_to_atom(X,Atom)  :- nonvar(X),!,atom_string(Atom,X).
convert_to_atom(X,_ )    :- var(X),!,instantiation_error(X).
*/

% Exactly the above, but using must_be/2

convert_to_string(X,Str) :- must_be(nonvar,X),atom_string(X,Str). 
convert_to_atom(X,Atom)  :- must_be(nonvar,X),atom_string(Atom,X).

