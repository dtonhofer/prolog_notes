% =============================================================================
% Metapredicates that are supposed to help in programming:
%
%   switch       (replace an unreadable sequence of "-> ;")
%   switch/else  (ditto)
%   if-then-else (drop that nasty "-> ;")
%   if_then      (ditto, no need for the "; true" at the end)
%   unless       (à la Perl)
% 
% and also statements to reify success/failure:
%
%   reify
%   reify_outcome
%
% Examples:
% ---------
%
% :- use_module(library('heavycarbon/support/meta_helpers.pl')).
%
% val_string(X,S) :-
%    switch(
%       between(0,10,X),
%       S="between 0 and 10",
%       between(11,20,X),
%       S="between 11 and 20",
%       between(21,30,X),
%       S="between 21 and 30",
%       between(31,40,X),
%       S="between 31 and 40",
%       S="above 40").
%      
% ?- val_string(4,S).
% S = "between 0 and 10".
%
% ?- reify_outcome((random(X),X>0.5),["That went well",X],["That didn't work",X],Out).
% X = 0.7658474306692046,
% Out = ["That went well",0.7658474306692046].
%
% ?- reify_outcome((random(X),X>0.5),["That went well",X],["That didn't work",X],Out).
% Out = ["That didn't work",X].
%
% =============================================================================
% Note that using this instead of directly-inlined "->" may slows down a
% program markedly. 30% slowdown if there are lots of these calls is not 
% impossible. 
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under: 
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Latest review: Tue 19 January 2021
% =============================================================================


:- module(heavycarbon_meta_helpers,
   [
       switch/4          % switch(If1,Then1,If2,Then2)  (throws if 'else' condition is hit)
      ,switch/5          % switch(If1,Then1,If2,Then2,Else)  
      ,switch/6          % switch(If1,Then1,If2,Then2,If3,Then3)  (throws if 'else' condition is hit) 
      ,switch/7          % switch(If1,Then1,If2,Then2,If3,Then3,Else) 
      ,switch/8          % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4)  (throws if 'else' condition is hit)
      ,switch/9          % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,Else) 
      ,switch/10         % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5)  (throws if 'else' condition is hit)
      ,switch/11         % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,Else)
      ,switch/12         % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6)  (throws if 'else' condition is hit)
      ,switch/13         % switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6,Else)
 
      ,if_then_else/3    % if_then_else(Condition,Then,Else)
      ,reify_outcome/4   % reify_outcome(Condition,SuccessThing,FailureThing,Out) (unifies "Out" with either "SuccessThing" or "FailureThing")
      ,reify/2           % reify(Goal,Outcome) (unifies "Outcome" with either 'true' or 'false') (should properly be 'true' or 'fail')
      ,if_then/2         % if_then(Condition,Then) (nothing happens if the 'else' condition is hit as "Condition" fails)
      ,unless/2          % unless(Condition,Else)  (nothing happens if the 'then' condition is hit as "Condition" succeeds)
   ]).

:- meta_predicate           % https://eu.swi-prolog.org/pldoc/doc_for?object=(meta_predicate)/1
       switch(0,0,0,0)
      ,switch(0,0,0,0,0)
      ,switch(0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0,0,0)
      ,switch(0,0,0,0,0,0,0,0,0,0,0,0,0)
      ,if_then_else(0,0,0)
      ,reify_outcome(0,?,?,?)
      ,reify(0,?)
      ,if_then(0,0)
      ,unless(0,0).

% ---
% A better "switch" than an unreadable sequence of ->/2
% ---

% switch/4

switch(If1,Then1,If2,Then2) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   unhandled_else_error.

% switch/5

switch(If1,Then1,If2,Then2,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(Else).

% switch/6

switch(If1,Then1,If2,Then2,If3,Then3) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   unhandled_else_error.

% switch/7

switch(If1,Then1,If2,Then2,If3,Then3,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(Else).

% switch/8

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   unhandled_else_error.

% switch/9

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(Else).

% switch/10

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   unhandled_else_error.

% switch/11

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   call(Else).

% switch/12

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   call(If6)
   ->  call(Then6)
   ;   unhandled_else_error.

% switch/13

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,If5,Then5,If6,Then6,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(If5)
   ->  call(Then5)
   ;   call(If6)
   ->  call(Then6)
   ;   call(Else).

% ---
% An implementation of ->/2. Pass three goals.
% ---

if_then_else(Condition,Then,Else) :- 
   call(Condition) -> call(Then) ; call(Else).

% ---
% Reification of an outcome. Pass a "Goal" and the "SuccessThing" to be unified with "Out"
% if the "Goal" succeeds and the "FailureThing" to be unified with "Out" if the Goal fails
% ---

reify_outcome(Condition,SuccessThing,FailureThing,Out) :-
   call(Condition) -> (Out = SuccessThing) ; (Out = FailureThing).

% ---
% Simpler reification to just the truth values 'true' or 'false'
% ---

reify(Goal,Outcome) :-
   if_then_else(call(Goal),(Outcome=true),(Outcome=false)).

% ---
% An implementation of ->/2 with an "else" that does nothing. Pass two goals.
% ---

if_then(Condition,Then) :- 
   call(Condition) -> call(Then) ; true.

% ---
% An implementation of ->/2 with an "then" that does nothing. Pass two goals.
% ---

unless(Condition,Else) :- 
   call(Condition) -> true ; call(Else).

% ---
% Throw a non-ISO standard exception which is used when a switch
% hit an "else" and there is not Goal that can be called for that.
% This is not an ISO standard exception because the "formal" term
% 'programming_error' is not in the list of allowed terms.
% ---

unhandled_else_error :-
   throw(error(programming_error,context(_,"hit the unhandled 'else' case of a 'switch'"))).

