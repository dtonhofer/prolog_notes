% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-22
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% This is free and unencumbered software released into the public domain.
%
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
%
% For more information, please refer to <http://unlicense.org/>
% ============================================================================

% ===
% "foldx" is able to simulate "foldr" as "foldl" (at least syntactically)
% for arithmetic computation over integers
% It uses CLP(FD) to set up a constraint network for the final result as
% the (tail-call-optimizable) recursion progresses. Once the end of the
% list has been reached, the starter value is fed into the network and
% all values, including the result, can be evaluated by the Prolog Processor.
% ===

:- use_module(library(clpfd)). % We are using #= instead of the raw "is".

% ===
% Standard foldl
% ===

foo_foldl(Foldy,[Item|Items],AccDown,Result) :-
   !,                                          
   call(Foldy,Item,AccDown,AccDownNext),
   foo_foldl(Foldy,Items,AccDownNext,Result).

foo_foldl(_,[],AccDown,Result) :-              
   AccDown=Result.                             

% ===
% Standard foldr
% ===

foo_foldr(Foldy,[Item|Items],Starter,AccUp) :-
   !,                                         
   foo_foldr(Foldy,Items,Starter,AccUpPrev),
   call(Foldy,Item,AccUpPrev,AccUp).

foo_foldr(_,[],Starter,AccUp) :-              
   AccUp=Starter.                             

% ===
% Mutant foldx. Really only works for (arithmetic) constraints.
% ===

foo_foldx(Foldy,[Item|Items],AccDown,AccUp)  :-
   !,                                          
   call(Foldy,Item,AccDown,AccDownNext,AccUpPrev,AccUp),
   foo_foldx(Foldy,Items,AccDownNext,AccUpPrev).

foo_foldx(_,[],AccDown,AccUp) :-
   AccDown=AccUp.                
   
% ===
% Functions to be applied during folding
% ===

% ---
% Standard "squadd" (square and add) to be applied by standard "foldl" and "foldr"
% ---

squadd(Item,In,Out) :- 
   Out #= Item+(In^2).  

% ---
% Special "squadd" replacement to be used by "foldlx" 
% ---

% When emulating "foldl with squadd"

squaddxl(Item,AccDown,AccDownNext,AccUpPrev,AccUp) :- 
   AccDownNext #= Item+AccDown^2, AccUpPrev = AccUp.

% When emulating "foldr with squadd"

squaddxr(Item,AccDown,AccDownNext,AccUpPrev,AccUp) :- 
   AccDownNext = AccDown, AccUp #= Item+AccUpPrev^2.

% ===
% Tests
% ===
      
:- begin_tests(foldx).

% ---
% The usual foldl,foldr
% ---

% "foldl" with "squadd", nothing special

test(foldl_squadd) :- foo_foldl(squadd , [1,2,3,4,5] , 0 , R), R=21909.         

% "foldr" with "squadd", nothing special

test(foldr_squadd) :- foo_foldr(squadd , [1,2,3,4,5] , 0 , R), R=507425426245.  

% ---
% Using "foldx"
% ---

% "foldx" simulating "foldl with squadd" using "squaddxl"
% -> Straightforward evaluation inside "squaddxl", nothing special.

test(foldx_squadd) :- foo_foldx(squaddxl , [1,2,3,4,5] , 0 , R), R=21909. 

% "foldx" simulating "foldr with squadd" using "squaddxr"
% -> It look like "squaddxr" pulls in "values from the future"
%    In fact, it sets up the network to perform evaluation once all the
%    necessary data is there, which happens at the end of the recursion.

test(foldx_squadd) :- foo_foldx(squaddxr , [1,2,3,4,5] , 0 , R), R=507425426245.  

:- end_tests(foldx).

rt :- run_tests(foldx).



