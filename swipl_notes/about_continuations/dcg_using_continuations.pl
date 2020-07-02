

c(E) :- shift(c(E)).

phrase(Goal,Lin,Lout) :-
   reset(Goal,Cont,Term),
   ( Cont == 0 -> 
      Lin = Lout
   ;  Term = c(E) -> 
      Lin = [E|Lmid],
      phrase(Cont,Lmid,Lout)
   ).
   
