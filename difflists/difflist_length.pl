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
% VERSION:   Tue  5 May 11:49:33 CEST 2020
% BUGS:      Cyclic list handling is not supported. Infinity beckons!
% RUNS ON:   SWI Prolog 8.1.24
% PREDICATE: length_dl/3
% UNIT TEST: Run predicate "rt/0"
% DOCS:      The full difflist explainer: https://bit.ly/2WmphYy_prolog
% ============================================================================
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

% ===
% Relate a difference list (written as the compound term "Tip-Fin") and its
% "Length". The predicate throws if "DL" is not a difflist on call, expressed
% as a compound term "Tip-Fin". The predicate can thus be used to check
% difflist structure, too. "DL" may be an already-closed difflist (i.e. "Tip"
% is a closed/proper list and "Fin" is []). "What" indicates whether the
% difflist has been found to be still "open" or already been "closed".
%
% length_dl(+DL,?Length,?What)
% ===

length_dl(DL,Length,What) :-
   contract_length_dl(DL),
   DL=Tip-Fin,
   length2_dl(Tip,Fin,Length,What).

% Fallback clause throwing an exception if the compound term is not as expected.
% This includes DL being a fresh variable.

contract_length_dl(DL) :-
   (nonvar(DL),DL=_Tip-_Fin)
   -> true
   ;  throw(domain_error(difference_list(root_compound_term),DL)).

% ---
% Express the various cases of Tip-Fin combinations
% ---

% The empty difference list

length2_dl(Tip,Fin,0,open) :-
   var(Fin),     % Tip must be fresh variable
   var(Tip),     % Fin must be fresh variable
   Tip == Fin,   % and be "equivalent" (i.e. be sharing variables)
   !.

% A nonempty difference list

length2_dl(Tip,Fin,Len,open) :-
   nonvar(Tip),  % Tip is nonfresh
   var(Fin),     % Fin must be fresh variable
   Tip \== Fin,  % Both must not be "equivalent" (sharing variables are equivalent!!)
   !,
   % Now walk through open list with an accumulator value starting at 1. This is also
   % verifies that the backbone of the difflist is valid.
   % Pass Tip-Fin compound to be passed unchanged down the call chain in case one needs
   % to throw with Tip-Fin as argument.
   length_dl_walk(Tip,Fin,1,Len,Tip-Fin).

% An already-closed difference list (i.e. a proper list)

length2_dl(Tip,Fin,Len,closed) :-
   is_list(Tip),
   is_list(Fin),
   append(_Prefix,Fin,Tip),
   !,
   length(Tip,Len).

% Neither a closed list nor a difference list. Could be anything!

length2_dl(Tip,Fin,_,_) :-
   throw(domain_error(difference_list(overall_structure),Tip-Fin)).

% ---
% Iterate over open list, accumulating length.
% Note that in call cases, we know that var(Fin)!
% ---

length_dl_walk([_|Xs],Fin,Len,LenFinal,DL) :-
   nonvar(Xs),   % the backbone continues!
   !,
   succ(Len,LenPlus),
   length_dl_walk(Xs,Fin,LenPlus,LenFinal,DL).

length_dl_walk([_|Xs],Fin,LenFinal,LenFinal,_) :-
   var(Xs),     % the end of the backbone of the open list!
   Xs == Fin,   % double-check the structure using "equivalence"
   !.

length_dl_walk([_|Xs],Fin,_,_,DL) :-
   var(Xs),    % the end of the backbone of the open list!
   Xs \== Fin, % but if that's not the Fin, the difflist is in bad shape
   !,
   throw(domain_error(difference_list(backbone_fin_mismatch),DL)).

% ===
% Unit testing part
% ===

:- begin_tests(length_dl).

test(not_difflist, throws(domain_error(difference_list(_),_Culprit))) :-
   length_dl(some_atom,_,_).

test(empty_closed_list, true([R,W]==[0,closed])) :-
   DL=F-F,
   F=[],
   length_dl(DL,R,W).

test(nonempty_closed_list, true([R,W]==[3,closed])) :-
   DL=[1,2,3|F]-F,
   F=[],
   length_dl(DL,R,W).

test(empty_difflist, true([R,W]==[0,open])) :-
   DL=F-F,
   length_dl(DL,R,W).

test(nonempty_difflist_1, true([R,W]==[3,open])) :-
   DL=[1,2,3|F]-F,
   length_dl(DL,R,W).

test(nonempty_difflist_2, true([R,W]=[4,open])) :-
   % append 4 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   DLnew=Tip-FinNew,
   length_dl(DLnew,R,W).

test(nonempty_difflist_3, true([R,W]=[6,open])) :-
   % append 4,5,6 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4,5,6|FinNew],
   DLnew=Tip-FinNew,
   length_dl(DLnew,R,W).

test(not_a_difflist_0, throws(domain_error(difference_list(_),_Culprit))) :-
   length_dl(_X-_Y,_,_).

test(not_a_difflist_1, throws(domain_error(difference_list(_),_Culprit))) :-
   % append 4 to difflist [1,2,3|_], test the ex-difflist instead of the new difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   _DLnew=Tip-FinNew,
   length_dl(DL,_,_).

test(not_a_difflist_2, throws(domain_error(difference_list(_),_Culprit))) :-
   % append 4 to difflist [1,2,3|_], but wrongly, using some X as new Fin
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|_FinNew],
   DLnew=Tip-_X,
   length_dl(DLnew,_,_).

:- end_tests(length_dl).

rt :- run_tests(length_dl).
