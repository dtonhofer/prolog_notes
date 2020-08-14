:- use_module(['ab_dcg.pl']).

% load with ?- [test_ab_dcg].
% run with  ?- run_tests.

:- begin_tests(ab_dcg).

test(generate,true(Bag == ['abababababab']))  :- 
   bagof(T,phrase_acceptable(T,6),Bag).
   
test(recognize,true(Bag == [3])) :- 
   bagof(N,phrase_acceptable("ababab",N),Bag).
   
test(enumerate5,true(Bag == [['',0],['ab',1],['abab',2],['ababab',3],['abababab',4]])) :- 
   bagof([T,N],limit(5,phrase_acceptable(T,N)),Bag). 
   
:- end_tests(ab_dcg).

