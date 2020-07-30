:- module(rotate_list,
          [
             rotate_list/3
          ]).

% ============================================================================
% 2020-04-XX
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
% Inspired by a discussion on
%   http://computer-programming-forum.com/55-prolog/358ecf5f07f2de46.htm
% although the solution below, which uses two difference lists and
% a single scan of the original list (an no append/3), was not proposed.
%
% Furthermore, from
%   https://stackoverflow.com/questions/10255703/how-to-rotate-lists-in-prolog
% When you just want to rotate by 1 position:
%   rotatelist([H|T], R) :- append(T, [H], R).
% Works both ways:
%   ?- rotatelist([a,b,c],R).
%   R = [b, c, a].
%   ?- once(rotatelist(R,[a,b,c])).
%   R = [c, a, b].
% ============================================================================
% rotate_list(+List,+N,?Rotated).
%
% Rotates List by N positions. 
% N=0 : no rotation
% N>0 : Rotate "leftwards": move a prefix of (N mod Length) list items to 
%       the back of the list to form the result.
% N<0 : Rotate "rightwards": move a suffix of (abs(N) mod Length) list items
%       to the front of the list to form the result (however, this is done
%       completely the same way as for the case N>0) 
% ============================================================================

rotate_list([],_,[]) :- !.

rotate_list(List,N,Rotated) :-
     length(List,Len),
     Len>0,
     !,     
     Kback is N mod Len,
     H1-T1=X-X,
     H2-T2=Y-Y,
     copy_list(List,Kback,H1,T1,H2,T2),
     Rotated=H2.

copy_list([L|Ls],C,H1,T1,H2,T2) :- C>0,!,succ(Cnext,C),T1=[L|Tnew],copy_list(Ls,Cnext,H1,Tnew,H2,T2). 
copy_list([L|Ls],0,H1,T1,H2,T2) :- !,T2=[L|Tnew],copy_list(Ls,0,H1,T1,H2,Tnew). 
copy_list([],0,H1,T1,_H2,T2)    :- T2=H1,T1=[].

