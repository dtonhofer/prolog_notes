% ===
% Call this manually from the toplevel
%
% catchy(+X,+ISO,+Backtrace)
%
% X is an integer =/= 0   : The predicate succeeds
% X is 0                  : The predicate throws a "business exception"
% X is anthing else       : An ISO-standard "type error" is thrown via must_be/2, and is rethrown from the exception handler
%
% ISO is the atom 'true'  : The business exception (i.e. the exception term) is an ISO-standard "domain error" exception term
% ISO is the atom 'false' : The business exception (i.e. the exception term) is a homegrown exception term carrying a SWI-Prolog dict
% ISO is anything else    : An ISO-standard "type error" is thrown via must_be/2, and is rethrown from the exception handler
%
% Backtrace is the atom 'true'  : A backtrace for the business exception is captured using catch_with_backtrace/3
% Backtrace is the atom 'false' : No backtrace for the business exception is captured as catch/3 is used
% Backtrace is anything else    : An ISO-standard "type error" is thrown via must_be/2, and is rethrown from the exception handler
%

/*
catchy(0,true,true).
catchy(0,true,false).
catchy(0,false,true).
catchy(0,false,false).
catchy(1,true,true).
catchy(1,true,false).
catchy(1,false,true).
catchy(1,false,false).
*/

% ===

% ~~~
% catchy/3 entry.
% ~~~

catchy(X,ISO,Backtrace) :-
   must_be(boolean,Backtrace),
   % must_be(integer,X),      % not done here, but in the funky/2 "business predicate"
   % must_be(boolean,ISO),    % not done here, but in the funky/2 "business predicate"
   catchy_2(Backtrace,X,ISO). % Backtrace moved to parameter position 0 for indexing purposes

catchy_2(true,X,ISO) :-
   catch_with_backtrace(
      funky(X,ISO),                      % can succeed, fail or throw
      ExceptionTerm,                     % unconstrained unification, catch anything
      goal_of_recovery(ExceptionTerm)).  % can succeed, fail or throw itself

catchy_2(false,X,ISO) :-
   catch(
      funky(X,ISO),                      % can succeed, fail or throw
      ExceptionTerm,                     % unconstrained unification, catch anything
      goal_of_recovery(ExceptionTerm)).  % can succeed, fail or throw itself

% ===
% Called in case funky/2 throws
% ===

% ~~~
% goal_of_recovery/2 entry.
% The outcomes:
% A matching exception term is found and recovery handling succeeds
%    --> success.
% A matching exception term is found but recovery handling throws for some reason
%    --> the new expection up the stack.
% No matching exception term is found
%    --> failure, rethrow the original exception
% A matching exception term is found but recovery handling fails for some reason
%    --> indistinguishable from "no matching exception term is found"
%        without further(awkward) work; assume it doesn't happen.
% ~~~

goal_of_recovery(ExceptionTerm) :-
   goal_of_recovery_2(ExceptionTerm)
   -> true
   ; throw(ExceptionTerm).

% ~~~
% Clause-branch by handled exception
% ~~~

goal_of_recovery_2(error(domain_error(ValidDomain,Culprit),Context)) :-
   !,
   format("Oops, ISO-standard 'domain error' exception\n",[]),
   format("Valid domain : ~q\n",[ValidDomain]),
   format("Culprit      : ~q\n",[Culprit]),
   % contain backtraces if "catch_with_backtrace/3" is used
   (nonvar(Context) -> format("Context      : ~q\n",[Context]) ; true).

goal_of_recovery_2(error(Dict,Context)) :-
   is_dict(Dict),
   !,
   format("Oops, exception of class ~q\n",[Dict.class]),
   (get_dict(expected, Dict, V1)  -> format("Expected : ~q\n",[V1]) ; true),
   (get_dict(got, Dict, V2)       -> format("Got      : ~q\n",[V2]) ; true),
   (get_dict(where, Dict, V3)     -> format("Where    : ~q\n",[V3]) ; true),
   (get_dict(what, Dict, V4)      -> format("What     : ~q\n",[V4]) ; true),
   (get_dict(culprit, Dict, V5)   -> format("Culprit  : ~q\n",[V5]) ; true),
   % contain backtraces if "catch_with_backtrace/3" is used
   (nonvar(Context)               -> format("Context  : ~q\n",[Context]) ; true).

% ---
% funky(+X,+ISO)
% If X == 0, throw an exception, otherwise succeed
% If ISO == true:
%    throw an ISO-standard "domain_error" exception using "library(error)"
% If ISO == false
%    throw an non-ISO-standard exception
% If ISO is not any of the atoms {true, false}
%    must_be/2 throws an ISO-standard "type_error" which is, however
%    "not expected" by the exception handler
% ---

% ~~~
% funky/2 entry checks
% ~~~

funky(X,ISO) :-
   must_be(integer,X),   % throws if not
   must_be(boolean,ISO), % throws if not
   once(funky_2(X,ISO)). % after the preliminary checks, we can go in; once/1 for determinacy

% ~~~
% funky/2 business end, throws the "business exception"
% ~~~

% Actually throws the term "error(domain_error(_,_),_)" via a call to predicate "domain_error(_,_)"

funky_2(X,true) :-
   X == 0
   -> domain_error("X must be non-zero in funky_2/2",X)
   ; format("funky/2 received ~d\n",[X]).

% Actually throws the term "error(Dict,_)"; the ISO-standard structure "error(_,_)" is
% retained so that a call_with_backtrace/3 is able to fill in the second argument in
% "error(Dict,_)" with the backtrace

funky_2(X,false) :-
   X == 0
   -> throw(error(
         _{class    : "domain_error",
           expected : "Something different from 0",
           got      : "Got 0",
           what     : "Argument 'X'",
           where    : "funky_2/2",
           culprit  : X},
         _Context))
   ; format("funky/2 received ~d\n",[X]).
