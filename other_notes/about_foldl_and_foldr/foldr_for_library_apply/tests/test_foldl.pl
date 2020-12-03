:- begin_tests(foldl).

test("foldl empty, starter is atom", true(V == foo)) :-
   foldl(false, [], foo, V).

test("foldl empty, starter is unbound", true(VA == VB)) :-
   foldl(false, [], VA, VB).

test("foldl construction to verify order of arguments, 1 list", true(V == startabcd)) :-
   foldl([E,FL,TR]>>atom_concat(FL,E,TR), 
      [a,b,c,d],
      start, V).

test("foldl construction to verify order of arguments, 2 lists", true(V == 'start(a1)(b2)(c3)(d4)')) :-
   foldl([E1,E2,FL,TR]>>atomic_list_concat([FL,'(',E1,E2,')'],TR),
      [a,b,c,d],
      [1,2,3,4],
      start, V).

test("foldl construction to verify order of arguments, 3 lists", true(V == 'start(a1x)(b2y)(c3z)(d4k)')) :-
   foldl([E1,E2,E3,FL,TR]>>atomic_list_concat([FL,'(',E1,E2,E3,')'],TR),
      [a,b,c,d],
      [1,2,3,4],
      [x,y,z,k],
      start, V).

test("foldl construction to verify order of arguments, 4 lists", true(V == 'start(a1xq)(b2yw)(c3ze)(d4kr)')) :-
   foldl([E1,E2,E3,E4,FL,TR]>>atomic_list_concat([FL,'(',E1,E2,E3,E4,')'],TR),
      [a,b,c,d],
      [1,2,3,4],
      [x,y,z,k],
      [q,w,e,r],
      start, V).

test("foldl construction, unifying unbound variables", true([L,V] == [[k,k,k,k,k,k],k])) :-
   length(L,6),
   foldl([X,X,X]>>true, L, K, V),
   K = k.
   
test("foldl building sequence of monotonically increasing ints", true([L,Final] == [[0, 1, 2, 3, 4, 5],6])) :-
   length(L,6),
   foldl([E,FL,TR]>>(succ(FL,TR),FL=E), L, 0, Final).

test("foldl building sequence of monotonically increasing ints, just verify") :-
   foldl([E,FL,TR]>>(succ(FL,TR),FL=E), [0,1,2,3,4,5], 0, 6).
   
test("foldl breakoff with failure", fail) :-
   foldl([E,_,_]>>call(E), [true,false,type_error(_,_)], _, _).

test("foldl breakoff with exception", error(type_error(_,_))) :-
   foldl([E,_,_]>>call(E), [true,type_error(_,_)], _, _).
   
test("foldl on single open list", Bag = [[[], 0], [[0], 1], [[0, 1], 2], [[0, 1, 2], 3]]) :-
   findall(
      [L,Final],
      limit(4,
         foldl([E,FL,TR]>>(succ(FL,TR),FL=E), L, 0, Final)
      ), 
      Bag).

test("foldl on multiple open lists, one proper list, count number of vars in each column", true(Final == 'start,1,1,2,3,3,3')) :-
   foldl([E1,E2,E3,E4,FL,TR]>>(how_many_vars(E1,E2,E3,E4,S),atomic_list_concat([FL,',',S],TR)),
         [1,2|_],
         [1,2,3|_],
         _,
         [1,2,3,4,5,6],
         start, Final).

test("foldl on all open lists, count number of vars in each column", true(Bag == ['start,0,1,2,3', 'start,0,1,2,3,4', 'start,0,1,2,3,4,4', 'start,0,1,2,3,4,4,4', 'start,0,1,2,3,4,4,4,4'])) :-
   findall(
      Final,
      limit(5,
         foldl([E1,E2,E3,E4,FL,TR]>>(how_many_vars(E1,E2,E3,E4,S),atomic_list_concat([FL,',',S],TR)),
               [1,2|_],
               [1,2,3|_],
               [1|_],
               [1,2,3,4|_],
               start, Final)),
         Bag).

how_many_vars(V1,V2,V3,V4,S) :-
   include(var,[V1,V2,V3,V4],Vars),
   length(Vars,S).

:- end_tests(foldl).
