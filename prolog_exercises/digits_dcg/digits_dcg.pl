:- module(digits_dcg,
   [
   nonempty_atom_of_digits//1
   ]).

% ---
% Grab the longest nonempty sequence of digits.
% See also: https://eu.swi-prolog.org/pldoc/man?section=basics
%
% Sadly, the code for reading codes from a list and getting an atom
% and the code for checking an atom is not the same! 
%
% There is no inherent reason why this should be so, except that 
% atom_codes/2 does not establihs a "constraint" among variables but 
% is basically two functions pressed into one predicate.
% ---

% If D is instantiated to an atom, 
% break it into character codes and check that they are digits.

nonempty_atom_of_digits(D) --> 
   { atom(D),! }, % guard
   { atom_codes(D,Ns) }, 
   nonempty_digits(Ns).

% If D is uninstantiated, 
% grab character codes representing digits from the (implicit) input list
% and merge them into an atom when done.

nonempty_atom_of_digits(D) --> 
   { var(D),! },  % guard   
   nonempty_digits(Ns),
   { atom_codes(D,Ns) }.

% ---
% Grabbing the digits from the implicit list (working as input)
% Putting the digits into the implicit list (working as output) 
% ---

% The "greedy processing" of "digits + more" comes first

nonempty_digits([N|Ns]) --> 
   single_digit(N), 
   nonempty_digits(Ns), 
   !. % cut to avoid backtracking to shorter strings

% The single digit comes last

nonempty_digits([N]) -->
   single_digit(N).

% Does the compiler make sure `0123456789` is just broken up once?
% I don't know!

single_digit(N) --> 
   [N], { memberchk(N,`0123456789`) }.


