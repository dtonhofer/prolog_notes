/*
dict_sum(Dict,Total) :-
   (Dict = _{}) -> (Total = 0); aggregate(sum(Val), Key^get_dict(Key,Dict,Val), Total).
*/

dict_sum(_{} ,0)     :- !.
dict_sum(Dict,Total) :- aggregate(sum(Val), Key^get_dict(Key,Dict,Val), Total).

:- begin_tests(sum).

test(sum1, [ true(T) ]) :- Dict = quux{},        dict_sum(Dict,Total), T = ( Total == 0 ).
test(sum2, [ true(T) ]) :- Dict = quux{x:1,y:2}, dict_sum(Dict,Total), T = ( Total == 3 ).

:- end_tests(sum).

rt(_) :- run_tests(sum).
