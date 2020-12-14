:- module(between_x,
          [
              between_x/3
          ]).   

% ============================================================================
% 2020-12-14
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ============================================================================

% A between/3 which accepts unbound variables as Arg1 and Arg2.

between_x(Low,High,Value) :-
   what_is_x(Low,Arg1),
   what_is_x(High,Arg2),
   what_is_x(Value,Arg3),
   % let generators will take over!
   % pass Low,High,Value separately to avoid having to extract them for error handling
   decide(Arg1,Arg2,Arg3,Low,High,Value),
   assertion(integer(Value)),
   assertion(between(Low,High,Value)).
   
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
   ((A1 =< A3) -> true ; non_iso_domain_error(required_arg1_smaller_or_equal_arg3,A1,A2,A3,[A1,'=<',A3],[A1,'>',A3])),
   between(A3,inf,A2).

generate_interval_with_variable_a1(A1,A2,A3) :-
   ((A3 =< A2) -> true ; non_iso_domain_error(required_arg3_smaller_or_equal_arg2,A1,A2,A3,[A3,'=<',A2],[A3,'>',A2])),
   between(0,inf,D),
   A1 is A3-D.

generate_interval_below_fixed_a3(A1,A3) :-
   between(0,inf,D),
   A1 is A3-D.
       
% Three var: too loose!

decide(var(_),var(_),var(_),Low,High,Value) :- non_iso_instantiation_error(all_variables_unbound,Low,High,Value).

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

decide(var(_),infinity,var(_),Low,High,Value) :- non_iso_domain_error(upper_interval_bound_is_infinity,Low,High,Value).
decide(var(A1),infinity,int(A3),_,_,_) :- generate_interval_below_fixed_a3(A1,A3).
decide(int(A1),infinity,var(A3),_,_,_) :- between(A1,inf,A3). % generate in a standard way
decide(int(A1),infinity,int(A3),_,_,_) :- between(A1,inf,A3). % accept in a standard way

% Other "infinities"

decide(_,_,infinity,Low,High,Value) :- non_iso_domain_error(value_is_infinity,Low,High,Value).
decide(infinity,_,_,Low,High,Value) :- non_iso_domain_error(lower_interval_bound_is_infinity,Low,High,Value).

% Various others

decide(other(_),_,_,Low,High,Value) :- non_iso_domain_error(arg1_is_infinity,Low,High,Value).
decide(_,other(_),_,Low,High,Value) :- non_iso_domain_error(arg2_is_infinity,Low,High,Value).
decide(_,_,other(_),Low,High,Value) :- non_iso_domain_error(arg3_is_infinity,Low,High,Value).

% Throwing non ISO but at least meaningful exceptions

non_iso_domain_error(Desc,A1,A2,A3) :-
    throw(my_error(domain_error,Desc,info{low:A1, high:A2, value:A3})).

non_iso_domain_error(Desc,A1,A2,A3,Required,Found) :-
    throw(my_error(domain_error,Desc,info{low:A1, high:A2, value:A3, required: Required, found: Found})).
    
non_iso_instantiation_error(Desc,A1,A2,A3) :-
    throw(my_error(instantiation_error,Desc,info{low:A1, high:A2, value:A3})).
    
