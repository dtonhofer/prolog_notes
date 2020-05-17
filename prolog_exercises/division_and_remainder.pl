% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-XX, 2020-05-XX
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
% Runs on: SWI Prolog 8.1
% with library(yall) for the Lambda Expression in line
% maplist([X]>>div_with_mod(X,3),InputList),
% ============================================================================
% Try out the arithmetic function pairs:
%
% "div" and "mod"
%
% - https://eu.swi-prolog.org/pldoc/doc_for?object=f((mod)/2)
% - https://eu.swi-prolog.org/pldoc/doc_for?object=f((div)/2)
% - https://eu.swi-prolog.org/pldoc/doc_for?object=divmod/4
%
% "//" and "rem"
%
% - https://eu.swi-prolog.org/pldoc/doc_for?object=f((//)/2)
% - https://eu.swi-prolog.org/pldoc/doc_for?object=f((rem)/2)
%
% See also
% --------
%
% Predicate "current_prolog_flag":
%  https://eu.swi-prolog.org/pldoc/doc_for?object=current_prolog_flag/2
%
% Library "yall":
%  https://eu.swi-prolog.org/pldoc/man?section=yall
% ============================================================================

/* Generated output

?- test.
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

( -6 mod -3) =  0, ( -6 div -3) =  2, OK : -3*2+0=-6
( -5 mod -3) = -2, ( -5 div -3) =  1, OK : -3*1+-2=-5
( -4 mod -3) = -1, ( -4 div -3) =  1, OK : -3*1+-1=-4
( -3 mod -3) =  0, ( -3 div -3) =  1, OK : -3*1+0=-3
( -2 mod -3) = -2, ( -2 div -3) =  0, OK : -3*0+-2=-2
( -1 mod -3) = -1, ( -1 div -3) =  0, OK : -3*0+-1=-1
(  0 mod -3) =  0, (  0 div -3) =  0, OK : -3*0+0=0
(  1 mod -3) = -2, (  1 div -3) = -1, OK : -3*-1+-2=1
(  2 mod -3) = -1, (  2 div -3) = -1, OK : -3*-1+-1=2
(  3 mod -3) =  0, (  3 div -3) = -1, OK : -3*-1+0=3
(  4 mod -3) = -2, (  4 div -3) = -2, OK : -3*-2+-2=4
(  5 mod -3) = -1, (  5 div -3) = -2, OK : -3*-2+-1=5
(  6 mod -3) =  0, (  6 div -3) = -2, OK : -3*-2+0=6

Below with current_prolog_flag(integer_rounding_function,toward_zero)

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

( -6 rem -3) =  0, ( -6 // -3) =  2, OK : -3*2+0=-6
( -5 rem -3) = -2, ( -5 // -3) =  1, OK : -3*1+-2=-5
( -4 rem -3) = -1, ( -4 // -3) =  1, OK : -3*1+-1=-4
( -3 rem -3) =  0, ( -3 // -3) =  1, OK : -3*1+0=-3
( -2 rem -3) = -2, ( -2 // -3) =  0, OK : -3*0+-2=-2
( -1 rem -3) = -1, ( -1 // -3) =  0, OK : -3*0+-1=-1
(  0 rem -3) =  0, (  0 // -3) =  0, OK : -3*0+0=0
(  1 rem -3) =  1, (  1 // -3) =  0, OK : -3*0+1=1
(  2 rem -3) =  2, (  2 // -3) =  0, OK : -3*0+2=2
(  3 rem -3) =  0, (  3 // -3) = -1, OK : -3*-1+0=3
(  4 rem -3) =  1, (  4 // -3) = -1, OK : -3*-1+1=4
(  5 rem -3) =  2, (  5 // -3) = -1, OK : -3*-1+2=5
(  6 rem -3) =  0, (  6 // -3) = -2, OK : -3*-2+0=6
true.

*/

% ---
% Main
% ---

test :-

  % Check how rounding works in this particular system
  % "integer_rounding_function": ISO Prolog flag describing rounding by "//" and "rem"
  % arithmetic functions. Value depends on the C compiler used.

  current_prolog_flag(integer_rounding_function,Irf),

  % Create a list of integers

  bagof(X,between(-6,6,X),InputList),

  % For every element of the list, call goals (here we use the syntax
  % of the excellent "yall" Lambda library : "[Args]>>Goal")

  maplist([X]>>div_with_mod(X,3),InputList),
  nl,
  maplist([X]>>div_with_mod(X,-3),InputList),
  nl,
  format("Below with current_prolog_flag(integer_rounding_function,~w)\n", Irf),
  nl,
  maplist([X]>>intdiv_with_rem(X,3),InputList),
  nl,
  maplist([X]>>intdiv_with_rem(X,-3),InputList).

% ---
% Compute results for
%    "div" (floored division)
%    "mod" (modulo)
% ---

div_with_mod(X,Step) :-
   M  is X mod Step,
   FD is X div Step,
   Reconst is Step*FD + M,
   ok_txt(Reconst,X,Step,FD,M,OkTxt),
   flush_right(X,Xstr,3),
   flush_right(FD,FDstr,2),
   flush_right(M,Mstr,2),
   format("(~s mod ~d) = ~s, (~s div ~d) = ~s, ~s\n", [Xstr,Step,Mstr,Xstr,Step,FDstr,OkTxt]).

% ---
% Compute results for
%   "//" (integer division)
%   "rem" (remainder)
% ---

intdiv_with_rem(X,Step) :-
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
   ((Reconst =:= X)
     ->
       format(string(Txt),"OK : ~d*~d+~d=~d",[Step,Div,Rest,Reconst])
     ;
       format(string(Txt),"NOK: ~d*~d+~d=~d instead of ~d",[Step,Div,Rest,Reconst,X])
   ).

% ---
% Why do I have to write a procedure for flushing right?
% I dunno! format/x should do that!
% ---

flush_right(X,Str,N) :-
   format(string(Tmp),"     ~d",X),sub_string(Tmp,_,N,0,Str).

