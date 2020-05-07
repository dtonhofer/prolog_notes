% Predicate to generate lists of three color atoms.
% You pass it the "current" and it defines the "next" one.
% From https://stackoverflow.com/questions/61526198/getting-all-the-combinations-of-colors-in-a-list

:- use_module(library(clpfd)).

rgbnumber(red,0).
rgbnumber(blue,1).
rgbnumber(green,2).

rgbnumberlist([L0,L1,L2],N) :- 
   rgbnumber(L0,N0), N0 in 0..2,
   rgbnumber(L1,N1), N1 in 0..2,
   rgbnumber(L2,N2), N2 in 0..2,
   N #= N0+N1*3+N2*3*3.

rgbnext(Current,Next,Wrap) :- 
   rgbnumberlist(Current,N),
   succ(N,Nx),
   Nxm is (Nx mod (3*3*3)), % Beware precedence: mod is as strong as *
   (Nx > Nxm -> Wrap = true ; Wrap = false),
   rgbnumberlist(Next,Nxm).
