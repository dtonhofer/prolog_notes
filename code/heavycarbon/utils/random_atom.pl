:- module(heavycarbon_random_atom,
          [
             random_atom/1   % random_atom(-Atom)
          ]).

% ===
% the_list/2 always returns the same values computed at runtime.
% Table it so that it is not needlessly executed over and over.
% ===

:- table the_list.

the_list(List,Max) :- 
   atom_chars('abcdefghijklmnopqrstuvwxyz',List), 
   length(List,Length), 
   succ(Max,Length).

% ===
% Generate a random list of characters picked randomly from the list of
% characters set up by the_list/2.
% The length of the list is found by throwing a coin after each character
% selection. The coin's probability of saying "one more character" decreases 
% according to a sigmoid that is a function of the number of characters 
% already in the collection (it decreases slowly at first, then has an 
% exponential tail going to 0). 
% That gives nicer results than decreasing the probability according to a
% exponential in the lnegth of already-collected characters, which would be
% the case by throwing the same biased each time.
% ===

random_char_list([X|More],AlreadyGot) :-
   the_list(List,Max), 
   random_between(0,Max,N),
   nth0(N,List,X),
   CutValue is 1-(1/(1+exp(2-(AlreadyGot/2)))), % nice sigmoid expressing decreasing probability of continuing, plot it at https://www.desmos.com/calculator
   ((random(F),F<CutValue) 
    -> (succ(AlreadyGot,AlreadyGotNow),random_char_list(More,AlreadyGotNow)) 
    ;  More=[]).

% ===
% Generate a nonempty random atom with characters a-z having random length
% ===

random_atom(Atom) :-
   random_char_list(Chars,0),
   atom_chars(Atom,Chars).

