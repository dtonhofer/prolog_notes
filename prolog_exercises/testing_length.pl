:- begin_tests(test_length).

% Create lists of fresh vars if list is unspecified 
% and length is >= 0

test(positive_length_gives_list_with_fresh_vars,true) :-
   length(L,10),
   nonvar(L),
   maplist(var,L),
   length(L,10).

test(zero_length_gives_empty_list,true) :-
   length(L,0),
   L==[].

% Compute length of proper lists of length 0 and 10

test(proper_list_0,true(Len=0)) :-
   length([],Len).

test(proper_list_10,true(Len=10)) :-
   length([0,1,2,3,4,5,6,7,8,9],Len).

% Complement a partial/open list once with an unspecified length.
% This "closes" it by unifying the open tail with []. 
% TODO: How to test what is generated on backtracking rounds 1 and 2?
% There is a "once" but there is no "twice" or "thrice".
   
test(partial_list,true([L,T,Len]=[[0,1,2,3],[],4])) :- 
   once((L=[0,1,2,3|T],length(L,Len))).
   
% Complement a partial/open list once a unspecified length.

test(partial_list_1,true) :-
   length([0,1,2,3|T],4),
   length(T,0).

test(partial_list_2,true) :-
   length([0,1,2,3|T],5),
   maplist(var,T),
   length(T,1).

test(partial_list_3,true) :-    
   length([0,1,2,3|T],6),
   maplist(var,T),
   length(T,2).

test(partial_list_4,true) :-    
   length([0,1,2,3|T],7),
   maplist(var,T),
   length(T,3).
   
% Edge case. "Tail of List is equivalent to Int"

test(weird_stuff_1,fail) :- length([1,2,3|L],L).
test(weird_stuff_2,fail) :- length(L,L).

% Failure on bad input

test(negative_length,error(domain_error(not_less_than_zero, BadLength),context(length/2,_))) :-
   BadLength = -1,
   length(_,BadLength).

test(non_integer_length,error(type_error(integer, BadLength),context(length/2,_))) :-
   BadLength = foo,
   length(_,BadLength).

:- end_tests(test_length).

rt :- run_tests(test_length).
