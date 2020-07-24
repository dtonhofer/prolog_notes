% ===
% Generating/Recognizing/Verifying strings of spaces
% ===

:- consult([conversion]).

% ---
% Actually possibly larege strings generate using string_concat/3
% Add specific cases besides length 0 and 1 for fats generation for
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

% ===
% Call this
% ===

string_of_spaces(N,Spaces) :-
   nonvar(Spaces),                     % case: check or determine length
   !,
   convert_to_string(Spaces,Str),      % make sure it's a string for "==" later
   string_length(Str,N),               % length is now known
   string_of_spaces(N,StrNew),         % regenerate spacey string
   Str == StrNew.                      % must be the same (i.e. fail if Spaces is not "spacey")
   
string_of_spaces(N,Spaces) :-          
   var(Spaces),nonvar(N),              % case: generate a string
   !,
   gen_string_of_spaces(N,Spaces).

string_of_spaces(N,Spaces) :-
   var(Spaces),var(N),                 % case: generate pairs
   !,
   between(0,inf,N),
   gen_string_of_spaces(N,Spaces).

