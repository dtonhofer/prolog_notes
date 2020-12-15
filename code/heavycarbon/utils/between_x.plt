% ===
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ===

:- use_module(library('heavycarbon/utils/between_x.pl')).

reify(Call,Value) :- (Call -> Value=true ; Value=false).

:- begin_tests(between_x).

% === free lower bound only === 

test("free lower bound only: value is ABOVE upper bound",[throws(my_error(domain_error,expecting(smaller_or_equal(_,_)),_))]) :-
   between_x(_,100,1000).

test("free lower bound only: value AT upper bound",Bag == [100,99,98,97,96,95,94,93,92,91]) :-
   bagof(LowerLimit,limit(10,between_x(LowerLimit,100,100)),Bag).
   
test("free lower bound only: value BELOW upper bound",Bag == [10,9,8,7,6,5,4,3,2,1]) :-
   bagof(LowerLimit,limit(10,between_x(LowerLimit,100,10)),Bag).

test("free lower bound only: upper bound is infinity",Bag == [10,9,8,7,6,5,4,3,2,1]) :-
   bagof(LowerLimit,limit(10,between_x(LowerLimit,inf,10)),Bag).
   
% === free upper bound only === 

test("free upper bound only: value BELOW lower bound",[throws(my_error(domain_error,expecting(smaller_or_equal(_,_)),_))]) :-
   between_x(-100,_,-1000).

test("free upper bound only: value AT lower bound",Bag == [-100,-99,-98,-97,-96,-95,-94,-93,-92,-91]) :- 
   bagof(UpperLimit,limit(10,between_x(-100,UpperLimit,-100)),Bag).

test("free upper bound only: value ABOVE lower bound",Bag = [-50,-49,-48,-47,-46,-45,-44,-43,-42,-41]) :-
   bagof(UpperLimit,limit(10,between_x(-100,UpperLimit,-50)),Bag).
   
test("free upper bound only: lower bound is -infinity",[throws(my_error(type_error,expecting(one_of([var,integer,atom:inf],arg1(-inf))),_))]) :-
   between_x(-inf,_,10).

% === only one of lower,upper or value given === 
   
test("given lower bound only",true(Bag = [[100,100,100],[100,100,101],[100,101,101],[100,100,102],[100,101,102],[100,102,102],[100,100,103],[100,101,103],[100,102,103],[100,103,103]])) :-
   bagof([100,Value,Upper],limit(10,between_x(100,Upper,Value)),Bag).
   
test("given upper bound only",true(Bag = [[100,100,100],[99,99,100],[99,100,100],[98,98,100],[98,99,100],[98,100,100],[97,97,100],[97,98,100],[97,99,100],[97,100,100]])) :-
   bagof([Lower,Value,100],limit(10,between_x(Lower,100,Value)),Bag).

test("given value only",true(Bag = [[100,100,100],[99,99,100],[99,100,100],[98,98,100],[98,99,100],[98,100,100],[97,97,100],[97,98,100],[97,99,100],[97,100,100]])) :-
   bagof([Lower,Upper,100],limit(10,between_x(Lower,100,Upper)),Bag).

% === all free ===

test("all arguments unbound",[throws(my_error(instantiation_error,expecting(not(all_args_unbound)),info{high:_12284,low:_12276,value:_12280}))]) :-
   between_x(_,_,_).

:- end_tests(between_x).
