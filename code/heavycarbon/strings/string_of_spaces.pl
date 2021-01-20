% =============================================================================
% Generate strings/atoms made entirely of the character 0x20 ("SPACE")
%
% string_of_spaces(?N,?Spaces)
%   N      : integer >= 0
%   Spaces : a string on output (accepts the same stuff atom_string/2
%            accepts on input)
%
% Examples
% --------
%
% ?- 
% use_module(library('heavycarbon/strings/string_of_spaces.pl')).
% true.
%
% ?- 
% string_of_spaces(10,Spaces).
% Spaces = "          ".
%
% ?- 
% string_of_spaces(N,"    ").
% N = 4.
%
%
% ?- s
% string_of_spaces(N," hey  ").
% false.
%
% -----------------------------------------------------------------------------
%
% Another way of "generating a string" is: 
%
%    length(Codes, N),
%    maplist(=(0'\s), Codes),
%    string_codes(Codes, String).
%
% Or even like this:
%
%    format(string(Spaces), '~t~*|', [2]).  % two spaces
%
% Another way of checking whether a string contains spaces only is:
%
%    string_codes(Codes, String).
%    maplist(=(0'\s), Codes),
%
% Or even like this:
%
%    split_string(Spaces, "", " ", [""]).
%
% See also:
% 
%    https://swi-prolog.discourse.group/t/once-this-predicate-is-tabled-it-loops-forever/2848
% 
% And this code concerning "exponentiation" of an associative operation:
%
%    https://swi-prolog.discourse.group/t/power-implementation/1937
%
% But why spend brainpower on producing cute ways of generating/accepting
% strings-of-spaces where one doesn't even see later that that's what the
% code actually does (in other words "who uses such puzzle-solving tricks in
% in actual code")
% ==============================================================================
% TODO: There should be something similar for atoms (or rather, the predicates
%       need to take an option 'string' or 'atom' (by default, string) 
%       (but should it then be called "atomic_of_spaces"?
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under: 
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Latest review: Tue 19 January 2021
% =============================================================================

:- module(heavycarbon_strings_spaces,
   [
      string_of_spaces/2  % string_of_spaces(N,Spaces)
   ]).

:- use_module(library('heavycarbon/strings/stringy.pl')).
:- use_module(library('heavycarbon/support/meta_helpers.pl')). 


string_of_spaces(N,Spaces) :-
   nonvar(Spaces),                     % case: "check length or determine length of 'Spaces'"
   !,
   stringy_ensure(Spaces,Str,string),  % make sure it's a string for "==" later; may throw
   string_length(Str,N),               % length is now known
   string_of_spaces(N,StrNew),         % regenerate spacey string for comparison
   Str == StrNew.                      % must be the same (i.e. fail if Spaces is not "spacey")

string_of_spaces(N,Spaces) :-
   var(Spaces),nonvar(N),              % case: "generate a string"
   !,
   gen_string_of_spaces(N,Spaces).

string_of_spaces(N,Spaces) :-
   var(Spaces),var(N),                 % case: "generate pairs"
   !,
   between(0,inf,N),
   if_then_else(gen_string_of_spaces(N,Spaces),true,throw("Problem in gen_string_of_spaces/2")). % no ISO-compatible throw

% ---
% Actually generate (possibly large) strings rapidly using string_concat/3.
% Add specific cases besides length 0 and 1 for fast generation for
% small strings.
% ---

gen_string_of_spaces( 0,"")           :- !.
gen_string_of_spaces( 1," ")          :- !.
gen_string_of_spaces( 2,"  ")         :- !.
gen_string_of_spaces( 3,"   ")        :- !.
gen_string_of_spaces( 4,"    ")       :- !.
gen_string_of_spaces( 5,"     ")      :- !.
gen_string_of_spaces( 6,"      ")     :- !.
gen_string_of_spaces( 7,"       ")    :- !.
gen_string_of_spaces( 8,"        ")   :- !.
gen_string_of_spaces( 9,"         ")  :- !.
gen_string_of_spaces(10,"          ") :- !.

gen_string_of_spaces(N,Spaces) :-
   N>10, !,
   divmod(N,2,Times,Remainder),
   gen_string_of_spaces(Times,S1),
   string_concat(S1,S1,S2),
   (Remainder>0
    -> (string_of_spaces(Remainder,SR), string_concat(S2,SR,Spaces))
    ;  Spaces = S2).

