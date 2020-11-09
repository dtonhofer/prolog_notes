% ===
% Trying weird reset sequence. Experimental.
% ===

handler_1(Goal,Count) :-
   ((Count == 0) 
    -> reset(handler_2(_,0),postbox_1(X),Cont)
    ;  reset(Goal,postbox_1(X),Cont)),
   format("handler_1 at count = ~q received ~q\n",[Count,X]),
   ((Cont == 0) 
    -> true 
    ; (
       compound_name_arity(Cont,Name,Arity),
       format("The continuation has name '~q' and ~d arguments\n",[Name,Arity]),
       succ(Count,NewCount),
       handler_1(Cont,NewCount))).

handler_2(Goal,Count) :-
   ((Count == 0) 
    -> reset(handler_3(_,0),postbox_2(X),Cont)
    ;  reset(Goal,postbox_2(X),Cont)),
   format("handler_2 at count = ~q received ~q\n",[Count,X]),
   ((Cont == 0) 
    -> true 
    ; (succ(Count,NewCount),handler_2(Cont,NewCount))).

handler_3(Goal,Count) :-
   ((Count == 0) 
    -> reset(worker(10),postbox_3(X),Cont)
    ;  reset(Goal,postbox_3(X),Cont)),
   format("handler_3 at count = ~q received ~q\n",[Count,X]),
   ((Cont == 0) 
    -> true 
    ; (succ(Count,NewCount),handler_3(Cont,NewCount))).

worker(Count) :-
   (Count < 0) 
   -> true
   ;  (random(X),
       random_member(P,[postbox_1(X),postbox_2(X),postbox_3(X)]), 
       format("worker at count ~q will shift ~q\n",[Count,P]),
       shift(P),
       format("worker back from shift\n"),
       NewCount is Count-1,
       worker(NewCount)).

run :-
   handler_1(_,0).
 


