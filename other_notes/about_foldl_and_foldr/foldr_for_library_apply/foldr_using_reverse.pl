reversel(Xs, Ys, L) :-
  reversel(Xs, [], Ys, Ys, 0, L).
  
reversel([], Ys, Ys, [], L, L).

reversel([X|Xs], Rs, Ys, [_|Bound], Lc, Lf) :-
  succ(Lc,Ln),
  (nonvar(Lf) -> Ln=<Lf ; true),
  reversel(Xs, [X|Rs], Ys, Bound, Ln, Lf).
  
  

 % they commit after reverse so that a failure by foldl
 % doesn't lead to an infinite failure-driven loop with
 % a reverse/2 applied on an open list
 
foldyrev(A,B,Length) :-
   (
      nonvar(Length) 
      ->
      (((integer(Length),Length>=0) -> true ; false),CheckLength=true)
      ;
      CheckLength=false
   ),   
   foldyrev2(A,[],B,0,Length,CheckLength).
   
foldyrev2(A,BGrow,BFinal,LGrow,LFinal,CheckLength) :-
   var(A)
   ->     
   ((BGrow = BFinal, LGrow = LFinal) ;
   
      (LGrowNext is LGrow+1, 
        ((CheckLength==true) -> (LGrowNext>LFinal) ; true),
        foldyrev2(_,[_|BGrow],BFinal,LGrowNext,LFinal,CheckLength)))

   
   
   ;
   (A == [])
   -> 
   (BGrow = BFinal, LGrow = LFinal)
   ;
   (A = [X|Xs])
   ->
   (LGrowNext is LGrow+1, 
   ((CheckLength==true) -> 
      ((LGrowNext>LFinal) -> domain_error("Bad length",LFinal) ; true)
      ; true),
   foldyrev2(Xs,[X|BGrow],BFinal,LGrowNext,LFinal,CheckLength)).
 
:- begin_tests(fl).

test(1,true([B,Length]==[[d,c,b,a],4])) :-
   foldyrev([a,b,c,d],B,Length).
   
test(2,true([B,Length]==[[],0])) :- 
   foldyrev([],B,Length).
   
test(3) :- 
   foldyrev([a,b,c,d],[d,c,b,a],4).
   
test(4,fail) :- 
   foldyrev([a,b,c,d],[x,x,b,a],4).

test(5,fail) :- 
   foldyrev([a,b,c,d],[d,c,b,a],10).

test(6,error(domain_error(_,_))) :- 
   foldyrev([a,b,c,d],[d,c,b,a],2).
   
:- end_tests(fl).

/*
   
 
 
foldr(Goal, List, V0, V) :-
    reverse(List,ListR),
    (true;
    foldl(Goal, ListR, V0, V).

foldr(Goal, List1, List2, V0, V) :-
    reverse(List1,List1R),
    reverse(List2,List2R),
    !,
    foldl(Goal, List1R, List2R, V0, V).

foldr(Goal, List1, List2, List3, V0, V) :-
    reverse(List1,List1R),
    reverse(List2,List2R),
    reverse(List3,List3R),
    !,
    foldl(Goal, List1R, List2R, List3R, V0, V).

foldr(Goal, List1, List2, List3, List4, V0, V) :-
    reverse(List1,List1R),
    reverse(List2,List2R),
    reverse(List3,List3R),
    reverse(List4,List4R),
    !,
    foldl(Goal, List1R, List2R, List3R, List4R, V0, V).
    */
