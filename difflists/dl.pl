% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-05-XX
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

% TODO: Adapt to lessons learned when writing "difflist_length.pl"
% TODO: Moved the list-printing code to a separate file

% ===
% Debugging ouptut part
% ===

% ---
% Printing a difflist via debug/3.
% We use ∅ to express a "fresh term".
% ---

express_dl(Depth,Msg,Tip-Fin) :- 
    integer(Depth)
   ,express2_dl(Tip,Fin,Str)
   ,debug(dl,"~d: ~s: ~s",[Depth,Msg,Str])
   .

% ---
% Express the various cases of Tip-Fin combinations
% ---

express2_dl(Tip,Fin,Str) :-
    Tip == Fin   % "equivalent"
   ,var(Fin)     % and this must also be true
   ,!           
   ,Str="Empty difflist: ∅"
   .
   
express2_dl(Tip,Fin,Str) :-
    Tip \== Fin  % "not equivalent"
   ,var(Fin)     % and this must also be true
   ,!
   ,express_dl_items(Tip,Fin,MoreStr)
   ,with_output_to(string(Str),format("Nonempty difflist: [~s]",[MoreStr]))
   .
   
express2_dl(Tip,Fin,Str) :-
    is_list(Tip)
   ,is_list(Fin)
   ,append(_Prefix,Fin,Tip)
   ,!
   ,with_output_to(string(Str),format("Closed difflist: ~q-~q",[Tip,Fin]))
   .

express2_dl(Tip,Fin,Str) :-   
    with_output_to(string(Str)
   ,format("Unknown stuff: ~q-~q",[Tip,Fin]))
   .
   
% ---
% Express the content of a difflist
% ---

express_dl_items([X|Xs],Fin,Str) :-
    Xs \== Fin
   ,!
   ,express_dl_items(Xs,Fin,MoreStr)
   ,with_output_to(string(Str),format("~q,~s",[X,MoreStr]))
   .
   
express_dl_items([X|Xs],Fin,Str) :-
    Xs == Fin
   ,!
   ,with_output_to(string(Str),format("~q|∅",[X]))
   .

% ===
% Main entry point for the exercise "copy data from one list to another list
% using a difflist". Note that the parameter "Dp" (Depth), used 
% only in debugging is on last place in dl_append_all/3. If it is
% on first place, the compiler makes the program semideterministic.
% ===

dl_do(Data,Result) :- 
    DL = I-I                  % Construct an initial difflist based on a single "fresh term".
                              % The I on the left of "-" is the "open Tip", the other I the "open Fin".
   ,dl_append_all(Data,DL,0)  % When this returns, "I" has been constrained as follows: all "Data" 
                              % has been appended and the difflist has been "closed". I is a real List.
                              % On the other hand, DL is no longer a difflist at all! The quality
   ,Result = I                % of "difflisty-ness" is quite fleeting! 
   .

% ---
% Recurse over input list, appending to difflist
% ---

dl_append_all([X|Xs],DL,Dp) :- 
    express_dl(Dp,"DL before dl_append", DL)
   ,dl_append(X,DL,DLlonger)
   ,express_dl(Dp,"DL after dl_append", DL)
   ,succ(Dp,Dpp)
   ,dl_append_all(Xs,DLlonger,Dpp)
   ,express_dl(Dp,"DL after dl_append_all", DL)
   .
   
dl_append_all([],DL,Dp) :-
    express_dl(Dp,"DL before closing", DL)
   ,dl_close(DL)
   ,express_dl(Dp,"DL after closing", DL)
   .

% ---
% "Close" the difflist and create a real list. This is simply done by
% Disassembling DL into "open Tip" and "open Fin", then constraining 
% the "open Fin" to be []. Can be simplified to: "dl_close(Tip-[])."
% ---

dl_close(DL) :- 
    DL = _Tip-Fin
   ,Fin=[]
   .         

% ---
% Append a single item.
% ---

dl_append(X,DL,DLlonger) :-
    DL       = Tip-Fin           % Disassemble DL into "open Tip" and "open Fin"
   ,Fin      = [X|NewFin]        % Constrain the "open Fin" to be the item with a new "open Fin"
   ,DLlonger = Tip-NewFin        % Longer difflist has same "open Tip" and the new "open Fin"
   .

% ===
% Unit testing part
% ===

:- begin_tests(difflist).

test(one, true(R=[1,2,3])) :- dl_do([1,2,3],R).
test(two, true(R=[]))      :- dl_do([],R).
   
:- end_tests(difflist).

rt :- debug(dl),run_tests(difflist).
