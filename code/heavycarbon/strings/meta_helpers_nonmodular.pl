% ==============================================================================
% Metapredicates that mostly help in making code readable.
% ==============================================================================
%
% This is not a module! Often, the goals called contain module-private
% predicates, and then the call "from another module" fails. (This could 
% be fixed in the Prolog runtime at some considerable cost of complexity.)
%
% We will just "consult" this file from any module that needs the predicates,
% thus creating "local copies" of said predicates in each module.
%
% (What does the compiler do with unused in-module code?)

% ---
% A better "switch" than an unreadable sequence of ->/2
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
% An implementation of ->/2. Pass three goals.
% ---

if_then_else(Condition,Then,Else) :- 
   call(Condition) -> call(Then) ; call(Else).

% ---
% Throw a non-ISO standard exception which is used when a switch
% covers all the cases but you are paranoid enough to fill something into
% the final "else" anyway. This is not an ISO standard exception because
% the "formal" term "cannot happen" is not in the list of allowed terms.
% ---

cannot_happen_error(Msg) :-
   throw(error(cannot_happen,context(_,Msg))).

