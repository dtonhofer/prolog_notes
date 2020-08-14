:- module(digits_dcg,
   [
   nonempty_atom_of_digits//1
   ]).

% ---
% Grab the longest nonempty sequence of digits.
% See also: https://eu.swi-prolog.org/pldoc/man?section=basics
% Sadly, the code for reading codes from a list and getting an atom
% and the code for checking an atom is not the same! 
% There is inherent reason why this is so excpet that atom_codes/2
% is not a "constraint" among variables but two functions pressed
% into one.
% ---

% If D is instantiated to an atom, 
% break it into character codes and check that they are digits.

nonempty_atom_of_digits(D) --> 
   { atom(D),! }, % guard
   { atom_codes(D,Ns) }, 
   jpl_nonempty_digits(Ns).

% If D is uninstantiated, 
% grab character codes representing digits from the (implicit) input list
% and merge them into an atom when done.

nonempty_atom_of_digits(D) --> 
   { var(D),! },  % guard   
   jpl_nonempty_digits(Ns),
   { atom_codes(D,Ns) }.

% ---
% Grabbing the digits from the implicit list (working as input)
% Putting the digits into the implicit list (working as output) 
% ---

% The "greedy processing" of "digits + more" comes first

jpl_nonempty_digits([N|Ns]) --> 
   jpl_single_digit(N), 
   jpl_nonempty_digits(Ns), 
   !. % cut to avoid backtracking to shorter strings

% The single digit comes last

jpl_nonempty_digits([N]) -->
   jpl_single_digit(N).

% Does the compiler make sure `0123456789` is just broken up once?
% I don't know!

jpl_single_digit(N) --> [N], { memberchk(N,`0123456789`) }.

