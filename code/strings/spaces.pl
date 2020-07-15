% ===
% Generating strings of spaces
% ===

spaces(0,"")     :- !.
spaces(1," ")    :- !.
spaces(2,"  ")   :- !.
spaces(3,"   ")  :- !.
spaces(4,"    ") :- !.

spaces(N,Spaces) :- 
   N>4, !,
   divmod(N,2,Times,Remainder),
   spaces(Times,S1),
   string_concat(S1,S1,S2),
   (Remainder>0 
    -> (spaces(Remainder,SR), string_concat(S2,SR,Spaces)) 
    ;  Spaces = S2). 

% ===
% Testing
% ===

:- begin_tests(spaces).
   
   test(multiple) :-
      forall(between(0,2000,Len),
             (spaces(Len,Spaces),
              string_length(Spaces,Len),
              debug(test_spaces,"Ok for length ~d\n",[Len]))).
   
:- end_tests(spaces).

rt(spaces) :- run_tests(spaces).


