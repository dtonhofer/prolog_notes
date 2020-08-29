:- module(heavycarbon_strings_conversion,
          [convert_to_string/2,
           convert_to_atom/2,
           leveling_string/2]).

% ==============================================================================
% convert_to_string(++X,?Str)
% convert_to_atom(++X,?Atom)
%
% Perform certain "type conversions", or rather "type assurances"
% in a way that's easy on the mind of the programmer.
%
% These are basically applications of atom_string/2 "working one-way only"; they
% make the data flow apparent.
% ==============================================================================

/*
% clause 1: if the firsst argument X is nonvar/instantiated, commit with a cut &
%           leave all further checks to atom_string/2, which may throw if the
%           argument is bad or else suceed or fail; atom_string/2 does magic
%           conversions on X, too
% clause 2: the path taken if first argument X is is fresh/uninstantiated, which
%           we don't accept, so we throw

convert_to_string(X,Str) :- nonvar(X),!,atom_string(X,Str).
convert_to_string(X,_)   :- var(X),!,instantiation_error(X).

convert_to_atom(X,Atom)  :- nonvar(X),!,atom_string(Atom,X).
convert_to_atom(X,_ )    :- var(X),!,instantiation_error(X).
*/

%
% Exactly the above, but using must_be/2
%

convert_to_string(X,Str) :-
   must_be(nonvar,X),atom_string(X,Str).

convert_to_atom(X,Atom) :-
   must_be(nonvar,X),atom_string(Atom,X).

% ==============================================================================
% leveling_string(++KnownStr,?PossibleStr)
%
% Suppose you generate a string "S" and have to "return it" through some
% argument, ArgN. In "extended notation":
%
% my_call(X,Y,Z,ArgN) :- build_string(X,Y,Z,S), ArgN = S.
%
% ArgN may be fresh/uninstantiated, so unifiying it with S is what
% you want. It may, however, also have been instantiated at call time to a
% string, or something that might be transformed into a string (in particular,
% an atom) or something that cannot be interpreted as a string no matter what.
% In that case, atom_string/2 comes in handy; it takes ArgN, transforms it into
% a string in flexible fashion (even stringying an integer etc. if needed),
% and unifies it with S, thus verifying equality, if ArgN was instantiated.
% Conversely, it ArgN was fresh, ArgN is instantiated to S.
%
% Let's call this operation a "leveling" operation (unitl I think of something
% better).
% ==============================================================================

leveling_string(KnownStr,PossibleStr) :-
   must_be(nonvar,KnownStr), % no need to specify "string" here, we can convert...
   atom_string(KnownStr,PossibleStr).


