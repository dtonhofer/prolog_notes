% Run in SWI Prolog 8.1

% The github page for this code is at http://bit.ly/38wQAmS_prolog

% Docs:
% Predicate "current_prolog_flag": https://eu.swi-prolog.org/pldoc/doc_for?object=current_prolog_flag/2
% Library "yall":                  https://eu.swi-prolog.org/pldoc/man?section=yall
% Arithmetic functions & preds:    https://eu.swi-prolog.org/pldoc/man?section=arith

% ---
% Call this
% ---

test :-   

  % Check how rounding works in this particular system 
  % "integer_rounding_function": ISO Prolog flag describing rounding by "//" and "rem"
  % arithmetic functions. Value depends on the C compiler used.

  current_prolog_flag(integer_rounding_function,Irf),
  format("Current rounding mode: ~w\n", Irf),   

  % Create a vector of integer between -30 and 30 

  bagof(X,between(-15,15,X),InputVector),
  
  % For every element of the vector, apply work/2 (here we use the syntax
  % of the yall Lambda library to put a shim around work/2 so that it is 
  % correctly called.
  
  maplist([X]>>work_mod_div(X,3),InputVector),
  maplist([X]>>work_intdiv_rem(X,3),InputVector).

% ---
% Compute results for "mod" (modulo) and "div" (floored division)
% ---

work_mod_div(X,Step) :-
   M  is X mod Step,
   FD is X div Step,
   Reconst is Step*FD + M,
   ok_txt(Reconst,X,Step,FD,M,OkTxt),
   flush_right(X,Xstr,3),
   flush_right(FD,FDstr,2),
   flush_right(M,Mstr,2),
   format("(~s mod ~d) = ~s, (~s div ~d) = ~s, ~s\n", [Xstr,Step,Mstr,Xstr,Step,FDstr,OkTxt]).

% ---
% Compute results for "rem" (remainder) and "//" (integer division)
% ---

work_intdiv_rem(X,Step) :-
   R  is X rem Step,
   ID is X // Step,
   Reconst is Step*ID + R,
   ok_txt(Reconst,X,Step,ID,R,OkTxt),
   flush_right(X,Xstr,3),
   flush_right(ID,IDstr,2),
   flush_right(R,Rstr,2),
   format("(~s rem ~d) = ~s, (~s // ~d) = ~s, ~s\n", [Xstr,Step,Rstr,Xstr,Step,IDstr,OkTxt]).
   
% ---
% Make text
% ---

ok_txt(Reconst, X, Step, Div, Rest, Txt) :-
   ((Reconst =:= X) -> format(string(Txt),"OK : ~d*~d+~d=~d",[Step,Div,Rest,Reconst]) ;
                       format(string(Txt),"NOK: ~d*~d+~d=~d instead of ~d",[Step,Div,Rest,Reconst,X])).

% ---
% Why do I have to write a procedure for flushing right? I dunno! format/x should that that!
% ---

flush_right(X,Str,N) :- format(string(Tmp),"     ~d",X),sub_string(Tmp,_,N,0,Str).


/* 

?- test.
Current rounding mode: toward_zero
(-15 mod 3) =  0, (-15 div 3) = -5, OK : 3*-5+0=-15
(-14 mod 3) =  1, (-14 div 3) = -5, OK : 3*-5+1=-14
(-13 mod 3) =  2, (-13 div 3) = -5, OK : 3*-5+2=-13
(-12 mod 3) =  0, (-12 div 3) = -4, OK : 3*-4+0=-12
(-11 mod 3) =  1, (-11 div 3) = -4, OK : 3*-4+1=-11
(-10 mod 3) =  2, (-10 div 3) = -4, OK : 3*-4+2=-10
( -9 mod 3) =  0, ( -9 div 3) = -3, OK : 3*-3+0=-9
( -8 mod 3) =  1, ( -8 div 3) = -3, OK : 3*-3+1=-8
( -7 mod 3) =  2, ( -7 div 3) = -3, OK : 3*-3+2=-7
( -6 mod 3) =  0, ( -6 div 3) = -2, OK : 3*-2+0=-6
( -5 mod 3) =  1, ( -5 div 3) = -2, OK : 3*-2+1=-5
( -4 mod 3) =  2, ( -4 div 3) = -2, OK : 3*-2+2=-4
( -3 mod 3) =  0, ( -3 div 3) = -1, OK : 3*-1+0=-3
( -2 mod 3) =  1, ( -2 div 3) = -1, OK : 3*-1+1=-2
( -1 mod 3) =  2, ( -1 div 3) = -1, OK : 3*-1+2=-1
(  0 mod 3) =  0, (  0 div 3) =  0, OK : 3*0+0=0
(  1 mod 3) =  1, (  1 div 3) =  0, OK : 3*0+1=1
(  2 mod 3) =  2, (  2 div 3) =  0, OK : 3*0+2=2
(  3 mod 3) =  0, (  3 div 3) =  1, OK : 3*1+0=3
(  4 mod 3) =  1, (  4 div 3) =  1, OK : 3*1+1=4
(  5 mod 3) =  2, (  5 div 3) =  1, OK : 3*1+2=5
(  6 mod 3) =  0, (  6 div 3) =  2, OK : 3*2+0=6
(  7 mod 3) =  1, (  7 div 3) =  2, OK : 3*2+1=7
(  8 mod 3) =  2, (  8 div 3) =  2, OK : 3*2+2=8
(  9 mod 3) =  0, (  9 div 3) =  3, OK : 3*3+0=9
( 10 mod 3) =  1, ( 10 div 3) =  3, OK : 3*3+1=10
( 11 mod 3) =  2, ( 11 div 3) =  3, OK : 3*3+2=11
( 12 mod 3) =  0, ( 12 div 3) =  4, OK : 3*4+0=12
( 13 mod 3) =  1, ( 13 div 3) =  4, OK : 3*4+1=13
( 14 mod 3) =  2, ( 14 div 3) =  4, OK : 3*4+2=14
( 15 mod 3) =  0, ( 15 div 3) =  5, OK : 3*5+0=15
(-15 rem 3) =  0, (-15 // 3) = -5, OK : 3*-5+0=-15
(-14 rem 3) = -2, (-14 // 3) = -4, OK : 3*-4+-2=-14
(-13 rem 3) = -1, (-13 // 3) = -4, OK : 3*-4+-1=-13
(-12 rem 3) =  0, (-12 // 3) = -4, OK : 3*-4+0=-12
(-11 rem 3) = -2, (-11 // 3) = -3, OK : 3*-3+-2=-11
(-10 rem 3) = -1, (-10 // 3) = -3, OK : 3*-3+-1=-10
( -9 rem 3) =  0, ( -9 // 3) = -3, OK : 3*-3+0=-9
( -8 rem 3) = -2, ( -8 // 3) = -2, OK : 3*-2+-2=-8
( -7 rem 3) = -1, ( -7 // 3) = -2, OK : 3*-2+-1=-7
( -6 rem 3) =  0, ( -6 // 3) = -2, OK : 3*-2+0=-6
( -5 rem 3) = -2, ( -5 // 3) = -1, OK : 3*-1+-2=-5
( -4 rem 3) = -1, ( -4 // 3) = -1, OK : 3*-1+-1=-4
( -3 rem 3) =  0, ( -3 // 3) = -1, OK : 3*-1+0=-3
( -2 rem 3) = -2, ( -2 // 3) =  0, OK : 3*0+-2=-2
( -1 rem 3) = -1, ( -1 // 3) =  0, OK : 3*0+-1=-1
(  0 rem 3) =  0, (  0 // 3) =  0, OK : 3*0+0=0
(  1 rem 3) =  1, (  1 // 3) =  0, OK : 3*0+1=1
(  2 rem 3) =  2, (  2 // 3) =  0, OK : 3*0+2=2
(  3 rem 3) =  0, (  3 // 3) =  1, OK : 3*1+0=3
(  4 rem 3) =  1, (  4 // 3) =  1, OK : 3*1+1=4
(  5 rem 3) =  2, (  5 // 3) =  1, OK : 3*1+2=5
(  6 rem 3) =  0, (  6 // 3) =  2, OK : 3*2+0=6
(  7 rem 3) =  1, (  7 // 3) =  2, OK : 3*2+1=7
(  8 rem 3) =  2, (  8 // 3) =  2, OK : 3*2+2=8
(  9 rem 3) =  0, (  9 // 3) =  3, OK : 3*3+0=9
( 10 rem 3) =  1, ( 10 // 3) =  3, OK : 3*3+1=10
( 11 rem 3) =  2, ( 11 // 3) =  3, OK : 3*3+2=11
( 12 rem 3) =  0, ( 12 // 3) =  4, OK : 3*4+0=12
( 13 rem 3) =  1, ( 13 // 3) =  4, OK : 3*4+1=13
( 14 rem 3) =  2, ( 14 // 3) =  4, OK : 3*4+2=14
( 15 rem 3) =  0, ( 15 // 3) =  5, OK : 3*5+0=15
true.

*/
