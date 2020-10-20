% ---
% I coded this for
% https://stackoverflow.com/questions/64404558/first-n-fibonacci-numbers-in-prolog
% ---

% ---
% The basic idea is from 
% http://rosettacode.org/wiki/Fibonacci_sequence#Prolog
% ---

% You get an open list with at least 2 elements computed-out.
% Wire up the rest of the list with a coroutine.
% "More" may be unbound variable (open list "fin") or
% '[]' (proper/closed list "fin") or a list.

fib_list([0,1|More]) :-
    wire_up_more(0,1,More).

% If FIN is actually '[]' we are done.
% Note that the head avoids unifying with '[]', then we test with '=='.

wire_up_more(_,_,FIN) :-
   FIN==[],
   !,
   debug(fib,"End of closed list reached").

% If FIN is actually an unbound var, we hit the end of an open list.

wire_up_more(Fa,Fb,FIN) :-
   var(FIN),
   !,
   debug(fib,"End of open list reached with Fa=~q, Fb=~q",[Fa,Fb]),
   freeze(FIN,fib_coroutine(Fa,Fb,FIN)).

% Otherwise the list continues, and we just compute next number.

wire_up_more(Fa,Fb,[Fc|More]) :-
   debug(fib,"The list continues with Fa=~q, Fb=~q, Fc=~q",[Fa,Fb,Fc]),
   fib(Fa,Fb,Fc),
   wire_up_more(Fb,Fc,More).

% This predicate will be called the first time the "fin" of the
% open list participates in a unification, which happens when
% the next as-yet-unrealized element of the list is demanded.
% The goal FIN=[Fc|NewFIN], appends Fc and new fin that is a
% fresh variable NewFIN to the open list, in-place. No need to
% tell the caller anything about NewFIN, which is why it doesn't
% appear in the head.

fib_coroutine(Fa,Fb,FIN) :-
   debug(fib,"Coroutine tiggered with Fa=~q, Fb=~q",[Fa,Fb]),
   fib(Fa,Fb,Fc),
   FIN=[Fc|NewFIN],
   wire_up_fin(Fb,Fc,NewFIN).

% The formula, moved out to follow the don't-repeat-yourself
% principle

fib(V1,V2,VN) :- VN is V1+V2.
