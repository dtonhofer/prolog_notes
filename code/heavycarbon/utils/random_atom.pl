:- module(heavycarbon_random_atom,
   [
    random_char/1       % random_char(X)
   ,random_text/3       % random_text(Text,Len,Options)
   ,random_atom/3       % random_atom(Text,Len,Options)
   ,random_string/3     % random_string(Text,Len,Options)
   ,random_char_list/2  % random_char_list(List,Options)
   ]).

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).
:- use_module(library('heavycarbon/utils/in_prefix.pl')).

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
% Select a uniformly random character from the list given by "the_list/2"
% ===

random_char(X) :-                    % EXPORT
   the_list(List,Max), 
   random_between(0,Max,N),
   nth0(N,List,X).

% ===
% Sigmoid expressing decreasing probability "P" of continuing with "one
% more character" given there already are "L" characters.
% Plot it at https://www.desmos.com/calculator
% ===

sigmoid_value(L,P) :- 
   P is 1-(1/(1+exp(2-(L/2)))).

% ---
% Generate text, with the length of the result either imposed (L bound) or
% communicated by (L unbound). Options includes the atom 'atom', 'string', 
% 'nonempty'.
% ---

random_text(Text,Len,Options) :-     % EXPORT
   nonvar(Len),
   !,
   length(Chars,Len),                % throws if Len < 0 or not an integer
   random_char_list(Chars,Options),  % fill the list of known length
   transform(Chars,Text,Options).

random_text(Text,Len,Options) :-     % EXPORT
   var(Len),
   !,
   random_char_list(Chars,Options),  % fill a list of random length
   length(Chars,Len),
   transform(Chars,Text,Options).

transform(Chars,Text,Options) :- 
   switch(
      (option_string(Options),\+option_atom(Options)), string_chars(Text,Chars),
      (option_atom(Options),\+option_string(Options)), atom_chars(Text,Chars),
      atom_chars(Text,Chars)   % default if no option or oth options
   ).
 
random_atom(Text,Len,Options) :-    % EXPORT
   random_text(Text,Len,[atom|Options]).

random_string(Text,Len,Options) :-  % EXPORT
   random_text(Text,Len,[string|Options]).

% ---
% Option checkers.
% ---

option_atom(Options)     :- in_prefix_with_stoplist(atom,[atom,string],Options).
option_string(Options)   :- in_prefix_with_stoplist(string,[atom,string],Options).
option_nonempty(Options) :- in_prefix_with_stoplist(nonempty,[],Options).

% ---
% "List" can be unbound, then the length is chosen randomly (according to a
% markov process), or else a proper list of unbound variables, thus giving 
% the length. 
% "Option" may be a list. If it contains the atom "nonempty", then the 
% "List" will contain at least 1 character.
% ---

random_char_list(List,Options) :-   % EXPORT
   var(List),
   !,
   if_then_else(
      option_nonempty(Options),
      (random_char_list_inner(L2,1),random_char(X),List=[X|L2]), % correctly start at "1"
      (random_char_list_inner(List,0))).
 
random_char_list(List,Options) :-
   nonvar(List),
   !,
   must_be(list(var),List),
   if_then(
     option_nonempty(Options),
     (length(List,LL),must_be(positive_integer,LL))),
   maplist(random_char,List).

% ---
% random_char_list(-Result,+CharsGeneratedAlready)
%
% The length of the list is found by throwing a coin before each character
% selection. The coin's probability of saying "one more character" decreases 
% according to a sigmoid that is a function of the number of characters 
% already in the list (it decreases slowly at first, then has an 
% exponential tail going to 0). That gives nicer results than decreasing the
% probability according to a exponential in the length of already-collected
% characters, which would be the case by throwing the same biased coin each time.
% ---

% *Stop criterium*

random_char_list_inner([],AlreadyGot) :-
   sigmoid_value(AlreadyGot,P), % P becomes smaller and smaller as AlreadyGot becomes larger (but even for 0, it's not 1.0)
   random(Toss),                % Between 0.0 =< Toss < 1.0
   P<Toss,                      % Stop if Toss is above P
   !.

% *More chars!*

random_char_list_inner([X|More],AlreadyGot) :-
   random_char(X),
   succ(AlreadyGot,AlreadyGotNow),
   random_char_list_inner(More,AlreadyGotNow).

