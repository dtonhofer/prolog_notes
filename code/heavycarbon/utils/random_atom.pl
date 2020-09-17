:- module(heavycarbon_random_atom,
          [
              random_atom_1/1   % random_atom_1(-Atom), creates no empties
             ,random_atom_2/1   % random_atom_2(-Atom), also creates empties
             ,random_char/1     % A random character a-z
          ]).

% ===
% "the_list/2" always returns the same values computed at runtime.
% Table it so that it is not needlessly executed over and over.
% ===

:- table the_list.

the_list(List,Max) :- 
   atom_chars('abcdefghijklmnopqrstuvwxyz',List), 
   length(List,Length), 
   succ(Max,Length).

% ===
% Select a uniformly random character from the list given by the_list/2
% ===

random_char(X) :-
   the_list(List,Max), 
   random_between(0,Max,N),
   nth0(N,List,X).

% ===
% Nice sigmoid expressing decreasing probability of continuing with one
% more character given there already are L characters.
% Plot it at https://www.desmos.com/calculator
% ===

sigmoid_value(L,P) :- P is 1-(1/(1+exp(2-(L/2)))).

% ===
% Generate a *nonempty* random atom with characters a-z having random length
% ===

random_atom_1(Atom) :-
   random_char_list_1(Chars,0),
   atom_chars(Atom,Chars).

% ===
% Generate a random list of characters picked randomly from the list of
% characters set up by the_list/2.
%
% The length of the list is found by throwing a coin after each character
% selection. The coin's probability of saying "one more character" decreases 
% according to a sigmoid that is a function of the number of characters 
% already in the collection (it decreases slowly at first, then has an 
% exponential tail going to 0). 
%
% That gives nicer results than decreasing the probability according to a
% exponential in the lnegth of already-collected characters, which would be
% the case by throwing the same biased coin each time.
% ===

random_char_list_1([X|More],AlreadyGot) :-
   random_char(X),
   sigmoid_value(AlreadyGot,P),
   random(Toss),                % Between 0.0 =< Toss < 1.0
   ((P<Toss)                    % Stop if Toss is above P
    -> More = []
    ; (succ(AlreadyGot,AlreadyGotNow),random_char_list_1(More,AlreadyGotNow))).

% ===
% Another approach. This one also generates atoms that may be empty
% ===

random_atom_2(Atom) :-
   random_char_list_2(Chars,0),
   atom_chars(Atom,Chars).

random_char_list_2([],AlreadyGot) :-
   sigmoid_value(AlreadyGot,P), % P becomes smaller and smaller as AlreadyGot becomes larger (but even for 0, it's not 1.0)
   random(Toss),                % Between 0.0 =< Toss < 1.0
   P<Toss,                      % Stop if Toss is above P
   !.

random_char_list_2([X|More],AlreadyGot) :-
   random_char(X),
   succ(AlreadyGot,AlreadyGotNow),
   random_char_list_2(More,AlreadyGotNow).


