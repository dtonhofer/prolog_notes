% =============================================================================
% A between/3 which accepts unbound variables as Arg1 and Arg2 and 
% generates possible intervals [Arg1,Args] in which Arg3 lies on backtracking:
%
% ?- between_x(X,Y,4).
% X = Y, Y = 4 ;
% X = 3, Y = 4 ;
% X = 4, Y = 5 ;
% X = 2, Y = 4 ;
% X = 3, Y = 5 ;
% X = 4, Y = 6 ;
% X = 1, Y = 4 ;
% ...
%
% NOTE: The exception throwing is not ISO Standard, pretty experimental,
% and too unstructured.
% =============================================================================
% Running the tests: There should be a file "between_x.plt" nearby.
% Then, if the root directory for "code" is on the library path:
%
% ?- use_module(library('heavycarbon/utils/between_x.pl')).
% ?- load_test_files([]).
% ?- run_tests.
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Last review: 2020-12-14
% =============================================================================

:- module(between_x,
          [
          between_x/3  % between_x(Low,High,Value)
          ]).

between_x(Low,High,Value) :-
   what_is_x(Low,Arg1),
   what_is_x(High,Arg2),
   what_is_x(Value,Arg3),
   % let generators take over!
   % pass Low,High,Value separately to avoid having to extract them for error handling
   decide(Arg1,Arg2,Arg3,Low,High,Value),
   assertion(integer(Value)),              % remove if you are sure it works
   assertion(between(Low,High,Value)).     % remove if you are sure it works

% Transform argument into a nicer representation

what_is_x(X,var(X))    :- var(X),!.
what_is_x(X,int(X))    :- integer(X),!.
what_is_x(X,infinity)  :- X == inf,!.    % "infinity" is easier to distinguish in source than "inf"
what_is_x(X,other(X)).

% Generators

generate_interval_around_fixed_a3(A1,A2,A3) :-
   between(1,inf,IntervalSize),
   OffsetStart is -(IntervalSize-1),
   between(OffsetStart,0,Offset),
   A1 is A3+Offset,
   A2 is A1+IntervalSize-1.

generate_interval_below_fixed_a2(A1,A2,A3) :-
   between(0,inf,D1),
   A1 is A2-D1,
   between(A1,A2,A3).

generate_interval_above_fixed_a1(A1,A2,A3) :-
   between(0,inf,D1),
   A2 is A1+D1,
   between(A1,A2,A3).

generate_interval_with_variable_a2(A1,A2,A3) :-
   ((A1 =< A3) -> true ; non_iso_domain_error(expecting(smaller_or_equal(arg1(A1),arg3(A3))),A1,A2,A3)),
   between(A3,inf,A2).

generate_interval_with_variable_a1(A1,A2,A3) :-
   ((A3 =< A2) -> true ; non_iso_domain_error(expecting(smaller_or_equal(arg3(A3),arg2(A2))),A1,A2,A3)),
   between(0,inf,D),
   A1 is A3-D.

generate_interval_below_fixed_a3(A1,A3) :-
   between(0,inf,D),
   A1 is A3-D.

% Three var: too loose!

decide(var(_),var(_),var(_),Low,High,Value) :- non_iso_instantiation_error(expecting(not(all_args_unbound)),Low,High,Value).

% Two var, one int

decide(var(A1),var(A2),int(A3),_,_,_) :- !,generate_interval_around_fixed_a3(A1,A2,A3).
decide(var(A1),int(A2),var(A3),_,_,_) :- !,generate_interval_below_fixed_a2(A1,A2,A3).
decide(int(A1),var(A2),var(A3),_,_,_) :- !,generate_interval_above_fixed_a1(A1,A2,A3).

% One var, two int

decide(int(A1),int(A2),var(A3),_,_,_) :- !,between(A1,A2,A3). % generate in a standard way
decide(int(A1),var(A2),int(A3),_,_,_) :- !,generate_interval_with_variable_a2(A1,A2,A3).
decide(var(A1),int(A2),int(A3),_,_,_) :- !,generate_interval_with_variable_a1(A1,A2,A3).

% Three int: punt to between/3 to accept/reject in a standard way

decide(int(A1),int(A2),int(A3),_,_,_) :- !,between(A1,A2,A3).

% infinity as argument A2

decide(var(_),infinity,var(_),Low,High,Value) :- non_iso_domain_error(expecting(not(infinity(upper_interval_bound))),Low,High,Value).
decide(var(A1),infinity,int(A3),_,_,_) :- generate_interval_below_fixed_a3(A1,A3).
decide(int(A1),infinity,var(A3),_,_,_) :- between(A1,inf,A3). % generate in a standard way
decide(int(A1),infinity,int(A3),_,_,_) :- between(A1,inf,A3). % accept in a standard way

% Other "infinities"

decide(_,_,infinity,Low,High,Value) :- non_iso_domain_error(expecting(not(infinity(value))),Low,High,Value).
decide(infinity,_,_,Low,High,Value) :- non_iso_domain_error(expecting(not(infinity(lower_interval_bound))),Low,High,Value).

% Various others

decide(other(_),_,_,Low,High,Value) :- non_iso_type_error(expecting(one_of([var,integer,atom:inf],arg1(Low))),Low,High,Value).
decide(_,other(_),_,Low,High,Value) :- non_iso_type_error(expecting(one_of([var,integer,atom:inf],arg2(High))),Low,High,Value).
decide(_,_,other(_),Low,High,Value) :- non_iso_type_error(expecting(one_of([var,integer,atom:inf],arg3(Value))),Low,High,Value).

% Throwing non ISO but at least meaningful exceptions

non_iso_domain_error(Desc,A1,A2,A3) :-
    throw(my_error(domain_error,Desc,info{low:A1, high:A2, value:A3})).

non_iso_type_error(Desc,A1,A2,A3) :-
    throw(my_error(type_error,Desc,info{low:A1, high:A2, value:A3})).

non_iso_instantiation_error(Desc,A1,A2,A3) :-
    throw(my_error(instantiation_error,Desc,info{low:A1, high:A2, value:A3})).

