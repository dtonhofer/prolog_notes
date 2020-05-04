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
% BUGS: 
% Cyclic list handling is not supported. Infinity beckons!
% ===
% EVIL IDEA abusing Exception handling:
%
% An evil idea on how to find the length simpler:
% The length_dl/2 calls a predciate length_dl_nasty/2 that:
%  1) Closes the difflist, making it a standard list
%  2) Gets the length using standard length/2 (this misses some
%     cases where the difflist is malformed though)
%  3) Throws an exception with the result!
%     (throw is basically like failing a predicate, just with
%      transportable info and arbitrary rollback up the AND-OR
%      call tree)
% length_dl/2 catches the Exception and extracts the length value.
% The difflist is now magically back in its previous state (open) (I think). 
% dl_length can thus set the length. Done! Would it work? I have to try this.
% ============================================================================
% The full difflists explainer: https://bit.ly/2WmphYy_prolog
% ============================================================================

% ===
% Length of a difference list written as "Tip-Fin"
% ===

length_dl(DL,Length) :-
    DL=Tip-Fin
   ,!
   ,length2_dl(Tip,Fin,Length).
   
length_dl(DL,_) :- 
    DL\=_Tip-_Fin
   ,!
   ,throw(domain_error(difference_list(root_compound_term),DL)).
   
% ---
% Express the various cases of Tip-Fin combinations
% ---

% The empty difference list

length2_dl(Tip,Fin,0) :-
    var(Fin)     % Tip and Fin must be fresh variables
   ,var(Tip)     % Tip and Fin must be fresh variables
   ,Tip == Fin   % and be "equivalent" (i.e. be sharing variables)
   ,!
   .

% A nonempty difference list

length2_dl(Tip,Fin,Len) :-
    nonvar(Tip)  % Tip is known
   ,var(Fin)     % Fin must be fresh variable
   ,Tip \== Fin  % Both must not be "equivalent" (sharing variables are equivalent!)
   ,!
   % walk through open list, this is also verifies that the backbone is good; pass Tip-Fin for throwing
   ,length_dl_walk(Tip,Fin,0,Len,Tip-Fin) 
   .

% An already-closed difference list (i.e. a real list)

length2_dl(Tip,Fin,Len) :-
    is_list(Tip)
   ,is_list(Fin)
   ,append(_Prefix,Fin,Tip)
   ,!
   ,length(Tip,Len)
   .

% Neither a closed list nor a difference list. Could be anything!

length2_dl(Tip,Fin,_) :-
   throw(domain_error(difference_list(overall_structure),Tip-Fin)).
   
% ---
% Iterate over open list. We know that var(Fin)!
% ---

length_dl_walk([_|Xs],Fin,Len,LenFinal,DL) :-
    nonvar(Xs)   % the backbone continues!
   ,!
   ,succ(Len,LenPlus)
   ,length_dl_walk(Xs,Fin,LenPlus,LenFinal,DL)
   .
   
length_dl_walk([_|Xs],Fin,Len,LenFinal,_DL) :-
    var(Xs)    % the end of the backbone of the open list!
   ,Xs == Fin  % double-check the structure (and not using unification!)
   ,!
   ,succ(Len,LenFinal)
   .

length_dl_walk([_|Xs],Fin,_Len,_LenFinal,DL) :-
    var(Xs)    % the end of the backbone of the open list!
   ,Xs \== Fin % if they are not equivalent, we are in bad shape
   ,!
   ,throw(domain_error(difference_list(backbone_fin_mismatch),DL))
   .
   
% ===
% Unit testing part
% ===

:- begin_tests(difflist_length).

test(whatever, throws(domain_error(difference_list(_),_Culprit))) :- 
   length_dl(foo,_).

test(empty_closed_list, true(R=0)) :- 
   DL=F-F,F=[],length_dl(DL,R).
   
test(nonempty_closed_list, true(R=3)) :- 
   DL=[1,2,3|F]-F,F=[],length_dl(DL,R).
   
test(empty_difflist, true(R=0)) :- 
   DL=F-F,length_dl(DL,R).
   
test(nonempty_difflist_1, true(R=3)) :- 
   DL=[1,2,3|F]-F,length_dl(DL,R).
   
test(nonempty_difflist_2, true(R=4)) :- 
   % append 4 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|F]-F,DL=Tip-Fin,Fin=[4|FinNew],DLnew=Tip-FinNew,length_dl(DLnew,R).

test(nonempty_difflist_3, true(R=6)) :- 
   % append 4,5,6 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|F]-F,DL=Tip-Fin,Fin=[4,5,6|FinNew],DLnew=Tip-FinNew,length_dl(DLnew,R).

test(not_a_difflist_0, throws(domain_error(difference_list(_),_Culprit))) :-
   length_dl(_X-_Y,_).
   
test(not_a_difflist_1, throws(domain_error(difference_list(_),_Culprit))) :-
   % append 4 to difflist [1,2,3|_], test the ex-difflist instead of the new difflist
   DL=[1,2,3|F]-F,DL=Tip-Fin,Fin=[4|FinNew],_DLnew=Tip-FinNew,length_dl(DL,_).

test(not_a_difflist_2, throws(domain_error(difference_list(_),_Culprit))) :-
   % append 4 to difflist [1,2,3|_], but wrongly, using some X as new Fin
   DL=[1,2,3|F]-F,DL=Tip-Fin,Fin=[4|_FinNew],DLnew=Tip-_X,length_dl(DLnew,_).
   
:- end_tests(difflist_length).

rt :- run_tests(difflist_length).
