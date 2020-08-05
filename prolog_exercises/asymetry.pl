% ---
% Explicit loading to avoid autoloading phenomena with yall
% (actually not needed in this case, nothing changes if you do it or not)
% ---

:- use_module(library(apply)).
:- use_module(library(yall)).

% ---
% Prolog "database" form
% ---

foo(a(K)) :- 
   debug(foo,"Database form: a(K) with K = ~q",[K]), 
   (var(K) -> K= 666 ; true).

foo(b(K)) :- 
   debug(foo,"Database form: b(K) with K = ~q",[K]), 
   (var(K) -> K= 999 ; true).

% ---
% Prolog "goal" form. Note that the goalform is a conjunction.
% There is no implication (and definitely no '->'  control construct)
% Proving X :- Y is the same as running X,Y 
% ---

goalform(X) :-
   G = 
      ((X = a(K), 
        debug(foo,"Goal form: a(K) with K = ~q",[K]), 
        (var(K) -> K=666 ; true));
       (X = b(K), 
        debug(foo,"Goal form: b(K) with K = ~q",[K]), 
        (var(K) -> K=999 ; true))),
   call(G).

% ---
% Using yall on goal form.
% For the inner variable, use XX for clarity, but it works as well
% if that variable is called X
% ---

goalform_yall(X) :-
   G = 
      ({}/[XX]>>
         ((XX = a(K), 
           debug(foo,"Goal form: a(K) with K = ~q",[K]), 
           (var(K) -> K=666 ; true));
          (XX = b(K), 
           debug(foo,"Goal form: b(K) with K = ~q",[K]), 
           (var(K) -> K=999 ; true)))),
   call(G,X).

% ---
% Using yall on goal form, where you infect the Goal with a K.
% This form of yall is not working (it should, the outer K
% should not be visible inside the >> goal. Note the {}
% braces saying "no variables in the >> are imported from
% outside".
% ---

goalform_yall_bad(X) :-
   K = as, % the andromeda strain
   G = 
      ({}/[X]>>
         ((X = a(K), 
           debug(foo,"Goal form: a(K) with K = ~q",[K]), 
           (var(K) -> K=666 ; true));
          (X = b(K), 
           debug(foo,"Goal form: b(K) with K = ~q",[K]), 
           (var(K) -> K=999 ; true)))),
   call(G,X).

% this doesn't work either, which is worse:

goalform_yall_very_bad(X) :-
   K = as, % the andromeda strain
   G = 
      ({}/[X,K]>>
         ((X = a(K), 
           debug(foo,"Goal form: a(K) with K = ~q",[K]), 
           (var(K) -> K=666 ; true));
          (X = b(K), 
           debug(foo,"Goal form: b(K) with K = ~q",[K]), 
           (var(K) -> K=999 ; true)))),
   call(G,X,_).

% ---
% Debugging on
% ---

:- debug(foo).

% ---
% Testing
% ---

:- begin_tests(database_form).

test("db form: collect them all", true(Bag = [a(666),b(999)])) :- bagof(X,foo(X),Bag).
test("db form: a(_)"            , true(Bag == [a(666)]))       :- bagof(a(U),foo(a(U)),Bag).
test("db form: b(_)"            , true(Bag == [b(999)]))       :- bagof(b(U),foo(b(U)),Bag).
test("db form: a(quux)"         , true)                        :- foo(a(quux)).
test("db form: b(quux)"         , true)                        :- foo(b(quux)).
test("db form: ruhroh(_)"       , fail)                        :- foo(ruhroh(_)).

:- end_tests(database_form).

:- begin_tests(goal_form).

test("goal form: collect them all", true(Bag = [a(666),b(999)])) :- bagof(X,goalform(X),Bag).
test("goal form: a(_)"            , true(Bag == [a(666)]))       :- bagof(a(U),goalform(a(U)),Bag).
test("goal form: b(_)"            , true(Bag == [b(999)]))       :- bagof(b(U),goalform(b(U)),Bag).
test("goal form: a(quux)"         , true(Bag = [yes]))           :- bagof(yes,goalform(a(quux)),Bag). % nondet
test("goal form: b(quux)"         , true)                        :- goalform(b(quux)).
test("goal form: ruhroh(_)"       , fail)                        :- goalform(ruhroh(_)).

:- end_tests(goal_form).

:- begin_tests(goal_form_yall).

test("goal form, yall: collect them all", true(Bag = [a(666),b(999)])) :- bagof(X,goalform_yall(X),Bag).
test("goal form, yall: a(_)"            , true(Bag == [a(666)]))       :- bagof(a(U),goalform_yall(a(U)),Bag).
test("goal form, yall: b(_)"            , true(Bag == [b(999)]))       :- bagof(b(U),goalform_yall(b(U)),Bag).
test("goal form, yall: a(quux)"         , true(Bag = [yes]))           :- bagof(yes,goalform_yall(a(quux)),Bag). % nondet
test("goal form, yall: b(quux)"         , true)                        :- goalform_yall(b(quux)).
test("goal form, yall: ruhroh(_)"       , fail)                        :- goalform_yall(ruhroh(_)).

:- end_tests(goal_form_yall).

:- begin_tests(goal_form_yall_bad).

test("goal form, yall, bad: collect them all", true(Bag = [a(as),b(as)])) :- bagof(X,goalform_yall_bad(X),Bag).
test("goal form, yall, bad: a(_)"            , true(Bag == [a(as)]))      :- bagof(a(U),goalform_yall_bad(a(U)),Bag).
test("goal form, yall, bad: b(_)"            , true(Bag == [b(as)]))      :- bagof(b(U),goalform_yall_bad(b(U)),Bag).
test("goal form, yall, bad: a(quux)"         , fail)                      :- bagof(yes,goalform_yall_bad(a(quux)),_Bag).
test("goal form, yall, bad: b(quux)"         , fail)                      :- goalform_yall_bad(b(quux)).
test("goal form, yall, bad: ruhroh(_)"       , fail)                      :- goalform_yall_bad(ruhroh(_)).

:- end_tests(goal_form_yall_bad).

:- begin_tests(goal_form_yall_very_bad).

test("goal form, yall, very bad: collect them all", true(Bag = [a(as),b(as)])) :- bagof(X,goalform_yall_very_bad(X),Bag).
test("goal form, yall, very bad: a(_)"            , true(Bag == [a(as)]))      :- bagof(a(U),goalform_yall_very_bad(a(U)),Bag).
test("goal form, yall, very bad: b(_)"            , true(Bag == [b(as)]))      :- bagof(b(U),goalform_yall_very_bad(b(U)),Bag).
test("goal form, yall, very bad: a(quux)"         , fail)                      :- bagof(yes,goalform_yall_very_bad(a(quux)),_Bag).
test("goal form, yall, very bad: b(quux)"         , fail)                      :- goalform_yall_very_bad(b(quux)).
test("goal form, yall, very bad: ruhroh(_)"       , fail)                      :- goalform_yall_very_bad(ruhroh(_)).

:- end_tests(goal_form_yall_very_bad).

