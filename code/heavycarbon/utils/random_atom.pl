% =============================================================================
% Utilities to generate random atoms or strings (generally, for tests)
% =============================================================================
% Running the tests: There should be a file "random_atom.plt" nearby.
% Then, if the root directory for "code" is on the library path:
%
% ?- use_module(library('heavycarbon/utils/random_atom.pl')).
% ?- load_test_files([]).
% ?- run_tests.
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Changes:
% 2020-09-XX Version 1
% 2020-12-27 Version 2, using library(option) and otherwise rewritten
% =============================================================================

:- module(heavycarbon_random_atom,
          [
          random_char/1       % random_char(X) - generate char
         ,random_text/2       % random_text(Text,Len) - generate *atom* of length Len (if given)
         ,random_text/3       % random_text(Text,Len,Options) - generate *atom* or *string* 
                                 % with options
                                 % empty(yes|no)      (default: no) 
                                 % what(string|atom)  (default: atom)
         ,random_char_list/2  % random_char_list(Chars,Options) - generate list of chars
                                 % with options
                                 % empty(yes|no)      (default: no)
         ,random_char_list/3  % random_char_list(Chars,Options,Tosses) - generate list of chars with details on process
          ]).

:- use_module(library(option)).

% ===
% "char_list/2" always returns the same values computed at runtime, namely:
% Chars: the list of chars ['a','b','c',....,'z'].
% Max:   the maximum allowed 0-based index into the list Chars
% The predicate is tabled so that it is not needlessly executed over and over.
% char_list(-Chars,-Max).
% ===

:- table char_list.

char_list(Chars,Max) :-
   atom_chars('abcdefghijklmnopqrstuvwxyz',Chars),  % explode atom into list of chars
   length(Chars,Length),                            % get its length
   succ(Max,Length).                                % adjust to 0-indexed

% ===
% Generate text, with the length of the result either specified (if L is bound) or
% determined (if L is unbound). Option processing is done through library(option).
% See https://eu.swi-prolog.org/pldoc/man?section=option
% Options is a list which may or may not contain:
% - the option what(W), where W is one of the atoms 'atom', 'string' (default 'atom')
% - the option empty(YN), where YN is one of 'yes', 'no' (default 'no')
% Corresponding "direct-to-atom" and "direct-to-string" predicates also exist.
% The just look at the option empty(YN)
% ===

% EXPORTED
random_text(Text,Len) :-
   random_text(Text,Len,[]).         % only default options

% EXPORTED
random_text(Text,Len,Options) :-
   nonvar(Len),
   !,
   length(Chars,Len),                % throws if Len < 0 or not an integer
   random_char_list(Chars,Options),  % fill the list of known length
   transform(Chars,Text,Options).    % generate appropriately-typed text

% EXPORTED
random_text(Text,Len,Options) :-
   var(Len),
   !,
   random_char_list(Chars,Options),  % fill a list of random length (the length results from a markov process)
   length(Chars,Len),
   transform(Chars,Text,Options).

% ===
% Depending on whether Options indicate to generate a 'string' or an 'atom',
% generate text of the requested type. The default is 'atom'.
% ===

transform(Chars,Text,Options) :-
   option(what(W),Options,atom),
   if_then_else(
      (W==string),                  % non-standard case which must be specified
      string_chars(Text,Chars),
      atom_chars(Text,Chars)).      % else/default case: W==atom or something else or W is var because Options is funky

% ===
% Actually generate the list of characters.
% "Chars" can be an unbound variable.
%    Then:
%    - The length of the resulting list that is eventually unified with "Chars" is chosen
%      according to a Markov process.
%    - The option "empty(YN)" from the "Options" list is considered.
%      YN is one of 'yes', 'no' (default 'no'). The resulting list may by this means be
%      allowed to be empty.
% "Chars" can be a proper list of unbound variables, thus specifying the length.
%    Then:
%    - The option "empty(YN)" from the "Options" list is considered; if the
%      specified length is 0 and "empty(no)" is given, the predicate fails, thus
%      staying consistent with the case of "Chars" an unbound variable.
%
% Interesting thought: These are actually "compressed" clauses for the actual predicate
% random_char_list(Chars,Length,Options)
% where Chars is always unbound, an Length may or may not be bound. If Length is
% bound, the Chars are selected from a smaller subspace of all possible Chars.
% If one could give a set of Length, how would one choose a Chars from the union of
% smaller subspaces with correct relative probabilities?
%
% There is a 3-arg version, which can only be used if "Chars" is an unbound variable.
% It also builds a list holding the changing probability for deciding "more characters"
% and the coin tosses made to make that decision.
% ---

% EXPORT
random_char_list(Chars,Options,Tosses) :-
   var(Chars),
   !,
   if_then_else(
      option(empty(no),Options,no),              % empty(no) first in Options or empty/1 missing from Options
      (random_char_list_inner(L2,1,Tosses2),
       random_char(X),Chars=[X|L2],              % correctly start at length "1"
       Tosses = [1|Tosses2]),
      (random_char_list_inner(Chars,0,Tosses))). % might generate nothing

% EXPORT
random_char_list(Chars,Options) :-
   var(Chars),
   !,
   random_char_list(Chars,Options,_). % call the 3-arg predicate, then dump the "tosses"

% EXPORT
random_char_list(Chars,Options) :-
   nonvar(Chars),
   !,
   must_be(list(var),Chars),               % "an abundance of caution"; enforce a meaningful call!
   if_then(
      (option(empty(no),Options,no),       % empty(no) first in Options or empty/1 missing from Options
       length(Chars,0)),                   % and user requested an empty list
      fail),                               % fail for consitency reasons
   maplist(random_char,Chars).             % fill list

% ===
% random_char_list(-Result,+CharsGeneratedAlready)
%
% The length of the list is found by throwing a biased coin before each character
% selection. The coin's probability of saying "one more character" decreases
% according to a sigmoid that is a function of the number of characters
% already in the list (it decreases slowly at first, then has an
% exponential tail going to 0). That gives nicer results than decreasing the
% probability according to a exponential in the length of already-collected
% characters, which would be the case by throwing the same biased coin each time.
%
% The style below is very imperative but easier to understand than if you
% separate it into two clauses, onw for "stop criterium reached + cut" and
% one for "one more character and recursive call". Note that there are two
% open lists in action to which we append, but written in differing style:
% "Chars" and "[Toss|Tosses]" (the latter collects the sigmoid floats, just for
% fun)
% ===

random_char_list_inner(Chars,CharsGot,[Toss|Tosses]) :-
   sigmoid_value(CharsGot,P),    % P becomes smaller and smaller as CharsGot becomes larger (but even for 0, it's not 1.0)
   random(Coin),                 % Coin is a value [0.0..1.0)
   if_then_else(
      (P<Coin),                  % Biased decision on whether to stop, become more probably with length
      (Chars=[],                 % Decision to stop
       Toss=(P<Coin),
       Tosses=[]),
      (random_char(X),           % Otherwise another random character selection
       Chars=[X|More],           % Construction of result list (note this is exactly the procedure of "appending to an open list"
       succ(CharsGot,CharsGot2), % We have 1 more character now
       Toss=(P>=Coin),
       random_char_list_inner(More,CharsGot2,Tosses))).

% ===
% Select a "uniformly random" char from the list of chars given by "char_list/2"
% This is used from inside maplist/1 and the random_char_lits_inner/2 generator.
% One can call it with instantiated X, but it's probably going to fail...
% ===

% EXPORTED
random_char(X) :-
   char_list(Chars,Max),
   random_between(0,Max,Selected),
   nth0(Selected,Chars,X).

% ===
% Sigmoid expressing decreasing probability "P" of continuing with "one
% more character" given there already are "L" characters.
% Plot it at https://www.desmos.com/calculator
% ===

sigmoid_value(L,P) :-
   P is 1-(1/(1+exp(2-(L/2)))).

% ===
% Helpers to beautify conditions. TODO: Move these to a library.
% ===

if_then(Condition,Then) :-
   call(Condition) -> call(Then) ; true.

if_then_else(Condition,Then,Else) :-
   call(Condition) -> call(Then) ; call(Else).

