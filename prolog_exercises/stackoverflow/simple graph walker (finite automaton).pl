% https://stackoverflow.com/questions/64807478/implementing-an-automata-in-prolog

:- use_module(library(clpfd)).

accepts(Ts,L) :-                 % Accept the list of transitions Ts of length L if
   start(S),                     % ...starting from a start state S
   accepts_from(S,Ts,L).         % ...you can accept the Ts of length L.                       

accepts_from(S,[T|Ts],L) :-      % Accept the transitions [T|Ts] when at S if
   (nonvar(L) 
    -> L >= 1
    ;  true),                    % L (if it is bound) is at least 1 (this can be replaced by L #> 0)
   delta(S,T,SN),                % ...and there is a transition S->SN via T
   Lm #= L-1,                    % ...and the new length is **constrained to be** 1 less than the previous length
   accepts_from(SN,Ts,Lm).       % ...and you can accept the remaining Ts of length Lm from SN.

accepts_from(S,[],0) :-          % If there is no transition left, length L must be 0 and we accept if
   end(S).                       % ...we are a final state.

delta(1,d,2).
delta(2,a,2).
delta(2,b,2).
delta(2,d,4).
delta(2,e,5).
delta(2,c,3).
delta(3,d,6).
delta(6,c,5).
start(1).
end(4).
end(5).

generate :- 
   between(0,7,L),
   findall(Ts,accepts(Ts,L),Bag),
   length(Bag,BagLength),
   format("Found ~d paths of length ~d through the graph\n",[BagLength,L]),
   maplist({L}/[Ts]>>format("~d : ~q\n",[L,Ts]),Bag).
