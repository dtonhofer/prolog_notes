:- begin_tests(foldr).

test("foldr empty, starter is atom", true(V == foo)) :-
   foldr(false, [], foo, V).

test("foldr empty, starter is unbound", true(VA == VB)) :-
   foldr(false, [], VA, VB).

test("foldr construction to verify order of arguments, 1 list", true(V == startdcba)) :-
   foldr([E,FR,TL]>>atom_concat(FR,E,TL), 
      [a,b,c,d], 
      start, V).

test("foldr construction to verify order of arguments, 2 lists", true(V == 'start(d4)(c3)(b2)(a1)')) :-
   foldr([E1,E2,FR,TL]>>atomic_list_concat([FR,'(',E1,E2,')'],TL),
      [a,b,c,d],
      [1,2,3,4],
      start, V).

test("foldr construction to verify order of arguments, 3 lists", true(V = 'start(d4k)(c3z)(b2y)(a1x)')) :-
   foldr([E1,E2,E3,FR,TL]>>atomic_list_concat([FR,'(',E1,E2,E3,')'],TL),
      [a,b,c,d],
      [1,2,3,4],
      [x,y,z,k],
      start, V).

test("foldr construction to verify order of arguments, 4 lists", true(V == 'start(d4kr)(c3ze)(b2yw)(a1xq)')) :-
   foldr([E1,E2,E3,E4,FR,TL]>>atomic_list_concat([FR,'(',E1,E2,E3,E4,')'],TL),
      [a,b,c,d],
      [1,2,3,4],
      [x,y,z,k],
      [q,w,e,r],
      start, V).

test("foldr construction, unifying unbound variables", true([L,V] == [[k,k,k,k,k,k],k])) :-
   length(L,6),
   foldr([X,X,X]>>true, L, K, V),
   K = k.
   
test("foldr building sequence of monotonically increasing ints", true([L,Final] == [[5, 4, 3, 2, 1, 0],6])) :-
   length(L,6),
   foldr([E,FR,TL]>>(succ(FR,TL),FR=E), L, 0, Final).

test("foldr building sequence of monotonically increasing ints, just verify") :-
   foldr([E,FR,TL]>>(succ(FR,TL),FR=E), [5,4,3,2,1,0], 0, 6).
   
test("foldr breakoff with failure", fail) :-
   foldr([E,_,_]>>call(E), [type_error(_,_),false,true], _, _).

test("foldr breakoff with exception", error(type_error(_,_))) :-
   foldr([E,_,_]>>call(E), [type_error(_,_),true], _, _).
   
test("foldr on single open list", Bag = [[[], 0], [[0], 1], [[1, 0], 2], [[2, 1, 0], 3]]) :-
   findall(
      [L,Final],
      limit(4,
         foldr([E,FR,TL]>>(succ(FR,TL),FR=E), L, 0, Final)
      ), 
      Bag).

test("foldr on multiple open lists, one proper list, count number of vars in each column", true(Final == 'start,3,3,3,2,1,1')) :-
   foldr([E1,E2,E3,E4,FR,TL]>>(plunit_foldr:how_many_vars(E1,E2,E3,E4,S),atomic_list_concat([FR,',',S],TL)),
         [1,2|_],
         [1,2,3|_],
         _,
         [1,2,3,4,5,6],
         start, Final).

test("foldl on all open lists, count number of vars in each column", true(Bag == ['start,3,2,1,0','start,4,3,2,1,0','start,4,4,3,2,1,0','start,4,4,4,3,2,1,0','start,4,4,4,4,3,2,1,0'])) :-
   findall(
      Final,
      limit(5,
         foldr([E1,E2,E3,E4,FL,TR]>>(plunit_foldr:how_many_vars(E1,E2,E3,E4,S),atomic_list_concat([FL,',',S],TR)),
               [1,2|_],
               [1,2,3|_],
               [1|_],
               [1,2,3,4|_],
               start, Final)),
         Bag).

test("foldr(reverse(list)) equals foldl(list)") :- 
   fold_datum_left_and_fold_reversed_datum_right([],X0,X0),
   fold_datum_left_and_fold_reversed_datum_right([a],X1,X1),
   fold_datum_left_and_fold_reversed_datum_right([a,b],X2,X2),
   fold_datum_left_and_fold_reversed_datum_right([a,b,c],X3,X3).

% ---

how_many_vars(V1,V2,V3,V4,S) :-
   include(var,[V1,V2,V3,V4],Vars),
   length(Vars,S).

fold_datum_left_and_fold_reversed_datum_right(Datum,Vfoldl,Vfoldr) :-
   foldl([E,FL,TR]>>atom_concat(FL,E,TR),Datum,start,Vfoldl),
   reverse(Datum,DatumR),
   foldr([E,FR,TL]>>atom_concat(FR,E,TL),DatumR,start,Vfoldr).

:- end_tests(foldr).

