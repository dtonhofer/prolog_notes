% ===
% Generating/Recognizing/Verifying strings of spaces
% ===

string_of_spaces(0,"")     :- !.
string_of_spaces(1," ")    :- !.
string_of_spaces(2,"  ")   :- !.
string_of_spaces(3,"   ")  :- !.
string_of_spaces(4,"    ") :- !.

% ===
% string_of_spaces(?N,?Spaces).
%
% N      is supposed to be an integer
% Spaces is supposed to be a String consisting only of " "
%        Actually type testing is left for lower predicates!
%  
% ===

convert_to_string(X,Str) :- nonvar(X),atom_string(X,Str).

string_of_spaces(N,Spaces) :-
   var(N),nonvar(Spaces),              % case: "determining string length"
   !,
   convert_to_string(Spaces,Str),

   string_length(Spaces,Len),          % also accepts atom, number
   string_of_spaces(SpacesN,Len),      % regenerate spacey string
   

string_of_spaces(N,Spaces) :-
   nonvar(N),nonvar(Spaces),
   string_of_spaces(N,XSpaces),
   XSpaces == Spaces.

string_of_spaces(N,Spaces) :-
   nonvar(N),var(Spaces),
   !,
   spaces(N,Spaces).

string_of_spaces(N,Spaces) :-
   var(N),var(Spaces),
   !
   generate_string_of_spaces(N,Spaces).


% generate_string_of_spaces(0,"").
% generate_string_of_spaces(N,Spaces) :- 
%    generate_string_of_spaces(Nsm,SpacesSm),
%    succ(Nsm,N), 
%    string_concat(" ",SpacesSm,Spaces).

generate_string_of_spaces(N,Spaces) :-
   between(0,inf,N),
   spaces(N,Spaces).

  
 




string_of_spaces(N,Spaces) :-
   N>4, !,
   divmod(N,2,Times,Remainder),
   string_of_spaces(Times,S1),
   string_concat(S1,S1,S2),
   (Remainder>0
    -> (string_of_spaces(Remainder,SR), string_concat(S2,SR,Spaces))
    ;  Spaces = S2).

% ===
% Unit tests
% ===

:- begin_tests(spaces).

test(multiple) :-
   forall(between(0,2000,Len),
          (spaces(Len,Spaces),
           string_length(Spaces,Len),
           debug(test_spaces,"Ok for length ~d\n",[Len]))).

:- end_tests(spaces).



