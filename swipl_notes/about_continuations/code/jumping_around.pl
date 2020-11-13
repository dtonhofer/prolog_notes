% Trying SWI-Prolog delimited continuations

% trial/3 takes the names of three "reset points", one of "level_1", "level_2", "level_3"
% The innermost/3 predicate will shift/1 to the given reset points in turn. The code
% following the reset points performs another reset/3 on the received continuation, unless
% it is zero. This way, we can actually jump around in the call stack.
%
% For example:
%
% ?- trial(level_1,level_2,level_3).
% Shifting level_1
% Exiting reset at level_1, with continuation a compound term
% Back at innerest, shifting level_2
% Exiting reset at level_2, with continuation a compound term
% Back at innerest, shifting level_3
% Exiting reset at level_3, with continuation a compound term
% Exiting reset at level_3, with continuation == 0
% Exiting reset at level_2, with continuation == 0
% Exiting reset at level_1, with continuation == 0

trial(X,Y,Z) :-
   reset(inner(X,Y,Z),level_1,Cont),
   print(level_1,Cont),
   ((Cont == 0) 
    -> true
    ; (reset(Cont,level_1,Cont2),print(level_1,Cont2))),
   !.
   
inner(X,Y,Z) :-
   reset(innerer(X,Y,Z),level_2,Cont),
   print(level_2,Cont),
   ((Cont == 0) 
    -> true  
    ; (reset(Cont,level_2,Cont2),print(level_2,Cont2))).
   
innerer(X,Y,Z) :-
   reset(innermost(X,Y,Z),level_3,Cont),
   print(level_3,Cont),
   ((Cont == 0) 
    -> true
    ; (reset(Cont,level_3,Cont2),print(level_3,Cont2))).
   
innermost(X,Y,Z) :-
   format("Shifting ~q\n",[X]),
   shift(X),
   format("Back at innerest, shifting ~q\n",[Y]),
   shift(Y),
   format("Back at innerest, shifting ~q\n",[Z]),
   shift(Z). % Although this is the last instruction, the continuation on reset-exit will NOT be 0   
   
print(Level,0) :-
   format("Exiting reset at ~q, with continuation == 0\n",[Level]).

print(Level,Cont) :-
   Cont \== 0,
   format("Exiting reset at ~q, with continuation a compound term\n",[Level]).
