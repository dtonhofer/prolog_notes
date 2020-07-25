:- module(heavycarbon_strings_leveling,
          [leveling_string/2]).

% ===
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
% Let's call this operation a "leveling" operation (unitl I think of something
% better). 
% ===

leveling_string(KnownStr,PossibleStr) :- 
   must_be(nonvar,KnownStr), % no need to specify "string" here, we can convert...
   atom_string(KnownStr,PossibleStr).

