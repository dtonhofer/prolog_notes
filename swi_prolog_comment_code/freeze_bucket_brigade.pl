bb :-
   debug(ccr),
   Ws=[W0,W1,W2,W3],
   debug(ccr,"freezing bucket_brigade(alpha)",[]),
   freeze(W0,bucket_brigade(alpha,V0,V1,Ws)),
   debug(ccr,"freezing bucket_brigade(bravo)",[]),
   freeze(W1,bucket_brigade(bravo,V1,V2,Ws)),
   debug(ccr,"freezing bucket_brigade(charlie)",[]),
   freeze(W2,bucket_brigade(charlie,V2,V3,Ws)),
   debug(ccr,"freezing bucket_brigade(delta)",[]),
   freeze(W3,bucket_brigade(delta,V3,VV,Ws)),
   % wire up the bucket brigade
   W0=V0,W1=V1,W2=V2,W3=V3,
   what_is_frozen(bb,Ws),
   debug(ccr,"wait for it...",[]),
   sleep(1),
   debug(ccr,"thawing bucket_brigade(alpha)",[]),
   W0=[],
   debug(ccr,"bucket arrived: ~q",[VV]).

what_is_frozen(_,[]) :- !.
what_is_frozen(N,[W|Ws]) :-
   frozen(W,G),
   (G==true
    -> debug(ccr,"~q : Nothing is frozen on: ~q",[N,W])
    ;  debug(ccr,"~q : Goal ~q is frozen on: ~q",[N,G,W])),
   what_is_frozen(N,Ws).


bucket_brigade(N,Vin,Vout,Ws) :-
   debug(ccr,"bucket_brigade ~q: Woke up with V = ~q!",[N,Vin]),
   what_is_frozen(N,Ws),
   Vout=[N|Vin].

