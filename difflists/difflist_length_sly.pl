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
% VERSION:   Tue  5 May 12:14:07 CEST 2020
% BUGS:      Cyclic list handling is not supported. Infinity beckons!
% RUNS ON:   SWI Prolog 8.1.24
% PREDICATE: length_dl_sly/2
% UNIT TEST: Run predicate "rt_sly/0"
% DOCS:      The full difflist explainer: https://bit.ly/2WmphYy_prolog
% ============================================================================
% Evil/Sly idea to find the length of a difflist by using/abusing exception
% handling (really, use the non-sly version instead, especially as this method
% does not catch some problems with the difflist):
%
% The length_dl_sly/2:
%  1) Closes the difflist, making it a standard list
%  2) Gets the length using standard length/2 (this misses some
%     cases where the difflist is malformed though)
%  3) Throws an exception with the length value in the exception term!
%     ("throw" is basically like "failing a predicate", just with
%      transportable info and arbitrary rollback up the AND-OR
%      call tree)
% length_dl_sly/2 then catches the Exception and extracts the length value.
% The difflist is now magically back in its previous state (open).
% ============================================================================

% ===
% Relate a difference list (written as the compound term "Tip-Fin") and its "Length"
% ===

length_dl_sly(DL,Length) :-
   contract_length_dl_sly(DL),
   DL=Tip-Fin,
   catch(
      length2_dl_sly(Tip,Fin),        % Call length2_dl_sly, which will throw
      retval(length2_dl_sly,Length),  % With the length as arg of the exception descriptor
      true).                          % Recover: Just "true"

% Fallback clause throwing an exception if the compound term is not as expected.
% This includes DL being a fresh variable.

contract_length_dl_sly(DL) :-
   (nonvar(DL),DL=_Tip-_Fin)
   -> true
   ;  throw(domain_error(difference_list(root_compound_term),DL)).

% ---
% How do we actually do it
% ---

length2_dl_sly(Tip,Fin) :-
   Fin=[],                                % close the diff list
   length(Tip,Length),                    % get its length normally
   throw(retval(length2_dl_sly,Length)).  % throw with the Length in the exception descriptor

% ===
% Unit testing part
% ===

:- begin_tests(length_dl_sly).

test(not_difflist, throws(domain_error(difference_list(_),_Culprit))) :-
   length_dl_sly(some_atom,_).

test(empty_closed_list, true([R,W]==[0,closed])) :-
   DL=F-F,
   F=[],
   length_dl_sly(DL,R),
   length_dl(DL,R,W).

test(nonempty_closed_list, true([R,W]==[3,closed])) :-
   DL=[1,2,3|F]-F,
   F=[],
   length_dl_sly(DL,R),
   length_dl(DL,R,W).

test(empty_difflist, true([R,W]==[0,open])) :-
   DL=F-F,
   length_dl_sly(DL,R),
   length_dl(DL,R,W).

test(nonempty_difflist_1, true([R,W]==[3,open])) :-
   DL=[1,2,3|F]-F,
   length_dl_sly(DL,R),
   length_dl(DL,R,W).

test(nonempty_difflist_2, true([R,W]==[4,open])) :-
   % append 4 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   DLnew=Tip-FinNew,
   length_dl_sly(DLnew,R),
   length_dl(DLnew,R,W).

test(nonempty_difflist_3, true([R,W]==[6,open])) :-
   % append 4,5,6 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4,5,6|FinNew],
   DLnew=Tip-FinNew,
   length_dl_sly(DLnew,R),
   length_dl(DLnew,R,W).

test(not_a_difflist_0, fixme(does_not_detect_bad_difflist)) :- % throws(domain_error(difference_list(_),_Culprit))) :-
   length_dl_sly(_X-_Y,_).

test(not_a_difflist_1, fixme(does_not_detect_bad_difflist)) :- % throws(domain_error(difference_list(_),_Culprit))) :-
   % append 4 to difflist [1,2,3|_], test the ex-difflist instead of the new difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   _DLnew=Tip-FinNew,
   length_dl_sly(DL,_,_).

test(not_a_difflist_2, fixme(does_not_detect_bad_difflist)) :- % throws(domain_error(difference_list(_),_Culprit))) :-
   % append 4 to difflist [1,2,3|_], but wrongly, using some X as new Fin
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|_FinNew],
   DLnew=Tip-_X,
   length_dl_sly(DLnew,_).

:- end_tests(length_dl_sly).

rt_sly :- run_tests(length_dl_sly).

