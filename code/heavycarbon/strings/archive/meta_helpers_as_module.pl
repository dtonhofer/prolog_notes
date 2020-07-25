:- module(heavycarbon_strings_other_helpers,
          [switch/7, 
           if_then_else/3,
           cannot_happen_error/1]).

:- meta_predicate switch(0,0,0,0,0,0,0).
:- meta_predicate if_then_else(0,0,0).

% ---
% Helper: A better "switch" than an unreadable sequence of ->/2
% ---

switch(If1,Then1,If2,Then2,If3,Then3,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(Else).

% ---
% Helper: An implementation of ->/2. Pass three goals.
% ---

if_then_else(Condition,Then,Else) :- 
   call(Condition) -> call(Then) ; call(Else).

% ---
% Helper: Throw a non-ISO standard exception which is used when a switch
% covers all the cases but you are paranoid enough to fill something into
% the final "else" anyway. This is not an ISO standard exception because
% the "formal" term "cannot happen" is not in the list of allowed terms.
% ---

cannot_happen_error(Msg) :-
   throw(error(cannot_happen,context(_,Msg))).

