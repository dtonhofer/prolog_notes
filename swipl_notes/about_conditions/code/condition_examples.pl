:- use_module(library(condition)).

% ---
% Setting a "handler" (which is a predicate of arity 2, and is also called a
% "Restarter" but should have really have been called an "Advisor") when 
% going "deeper" using setup_call_cleanup/3.
% ---
%
% The call to the handler predicate is set up in the thread-local database of
% the "condition" module as a clause with head "handler(Condition,Advice)"
% before any existing clause of handler/2. This is done by the library code:
%
% add_handler(Advisor,Ref) :-
%     condition:asserta((handler(C,A) :- call(Advisor,C,A)), Ref).
%
% When the "business code" meets a problem or impasse, it should not throw but
% should call "signal(Condition,Advice)" to receive (backtrackable) "Advice"
% about "Condition", with the Advice from the most recently registered handler
% coming first. It can then decide according to some algorithm what to 
% actually do.
%
% If there is no matching handler clause, an exception with exception term
% "Condition" is raised (signal/3 accepts default "Advice" which is
% returned instead).
%
% Adding and removing handler predicates via the setup_call_cleanup/3 call
% ensures that the handler is always removed when the "business predicate"
% (here levelN/2) is "exhausted" (including an exception being thrown)

level0(X) :-
   setup_call_cleanup(
      add_handler(baz_advisor,Ref),
      level1(X),
      rm_handler(Ref)).

level1(X) :-
   setup_call_cleanup(
      add_handler(bar_advisor,Ref),
      level2(X),
      rm_handler(Ref)).

level2(X) :-
   setup_call_cleanup(
      add_handler(foo_advisor,Ref),
      level3(X),
      rm_handler(Ref)).

level3(X) :-
   % Suppose we want advice now on one or the other problem
   % The preceding calls have setup a context which provide answers
   (X == 1)
   -> (findall(Advice,signal(problem(1),Advice),AL1), format("Advice on problem(1): ~q\n",[AL1])),
   ;
   (X == 2) 
   -> (findall(Advice,signal(problem(2),Advice),AL2), format("Advice on problem(2): ~q\n",[AL2])),
   ;
   (X == 3)
   -> (findall(Advice,signal(problem(3),Advice),AL3), format("Advice on problem(3): ~q\n",[AL3])),
   ;
   (X == 4)
   -> (findall(Advice,signal(problem(4),Advice),AL4), format("Advice on problem(4): ~q\n",[AL4])),
   ;
   format("Everything is going extremely well\n").

% ===
% In this implementation, the "handlers/advisors" all return advice that
% is a compound term "may/2" or "must/2", the action on position 1 and
% the name of the originating advisor on position 2.
% ===

% ---
% foo_advisor(Condition,RestartAdvice) is a "handler": a predicate
% that can give advice (possibly computed or congruent with side effects
% like cleaning the disk if the problem is 'not enough space') on what 
% to do given a problem description.
% Here, the RestartAdvice is static and coded directly in a fact.
% ---

foo_advisor(problem(1),may(ignore,foo)).
foo_advisor(problem(2),may(ask_user,foo)).
foo_advisor(problem(3),must(throw(error(system_error)),foo)).

% ---
% bar_advisor(Condition,RestartAdvice) is another "handler" installed
% at shallower call depth than foo_advisor/2
% ---

bar_advisor(problem(1),may(ignore,bar)).
bar_advisor(problem(1),may(ignore,bar)).
bar_advisor(problem(4),must(use_default_value,bar)).

% ---
% baz_advisor(Condition,RestartAdvice) is another "handler" installed
% at shallower call depth than baz_advisor/2
% ---

baz_advisor(problem(1),may(ignore,baz)).
baz_advisor(problem(2),may(throw(error(type_error(_,_),_)),baz)).

