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
% Unit tests for the predicate relating a difference list (aka list
% difference, difflist) to its length, length_dl/5.
%
% VERSION:   Sun 10 May 20:19:44 CEST 2020
% RUNS ON:   SWI Prolog 8.1.24
% DOCS:      The full difflist explainer:
%            https://bit.ly/2STX769_prolog
%            https://bit.ly/2WmphYy_prolog
% ============================================================================

:- include(length_dl).

:- begin_tests(length_dl).

% ---
% Passing bad parameters
% ---

test(not_difflist, throws(length_dl(contract_error,difflist(_),_))) :-
   length_dl(some_atom,_,_,_).

test(not_length, throws(length_dl(contract_error,length(_),_))) :-
   length_dl(X-X,not_length,_,_).

test(not_listtype, throws(length_dl(contract_error,listtype(_),_))) :-
   length_dl(X-X,5,not_listtype,_).

test(not_intention, throws(length_dl(contract_error,intention(_),_))) :-
   length_dl(X-X,5,closed,foo).

% ---
% Trying "already closed" difflists (i.e. plain lists)
% ---

test(empty_closed_list, true([Length,Listtype]==[0,closed])) :-
   DL=X-X,
   X=[],
   length_dl(DL,Length,Listtype,_).

test(nonempty_closed_list, true([Length,Listtype]==[3,closed])) :-
   DL=[1,2,3|F]-F,
   F=[],
   length_dl(DL,Length,Listtype,_).

test(nonempty_closed_list_2, true([Length,Listtype]==[6,closed])) :-
   DL=[1,2,3|F]-F,
   F=[a,b,c],
   length_dl(DL,Length,Listtype,_).

test(nonempty_closed_list_3, true([Length,Listtype]==[3,closed])) :-
   length_dl([1,2,3]-[3],Length,Listtype,_).

% ---
% Intention: "verify" vs "determine" vs "verify_determine"
% ---

test(intention_verify, true(Intention==verify)) :-
   length_dl([1,2,3]-[],3,closed,Intention).

test(intention_verify_determine, true(Intention==verify_determine)) :-
   length_dl([1,2,3]-[],3,_,Intention).

test(intention_verify_determine, true(Intention==verify_determine)) :-
   length_dl([1,2,3]-[],_,closed,Intention).

test(intention_determine, true(Intention==determine)) :-
   length_dl([1,2,3]-[],_,_,Intention).

% ---
% Bad guess at intention. In fact, intention should always be fresh.
% ---

test(intention_bad_guess_1, throws(length_dl(consistency_error,_,_))) :-
   length_dl([1,2,3]-[],3,closed,determine).

test(intention_bad_guess_2, throws(length_dl(consistency_error,_,_))) :-
   length_dl([1,2,3]-[],3,_,verify).

test(intention_bad_guess_3, throws(length_dl(consistency_error,_,_))) :-
   length_dl([1,2,3]-[],_,closed,verify).

test(intention_bad_guess_4, throws(length_dl(consistency_error,_,_))) :-
   length_dl([1,2,3]-[],_,_,verify).

% ---
% Templating: the difflist is fresh and is set to a difflist either of a
% given length, or of increasing length at backtracking
% ---

test(templatize_difflist_of_length_0) :-
   length_dl(DL,0,open,templatize),
   DL=X-X,fresh(X).

test(templatize_difflist_of_length_0, nondet) :-
   between(0,9,L),                   % generate increasing L on backtracking
   assertion(cured(L)),              % evidently
   length_dl(DL,L,open,templatize),  % generate difflist template of length L
   assertion(cured(DL)),             % evidently
   length_dl(DL,L,open,verify).      % verify difflist template of length L

% ---
% Analyzing: (Verifying/Determining) open difflists: the difflist is nonfresh/cured
% and its length and its type are determined
% ---

test(empty_open_difflist, true([L,LT]==[0,open])) :-
   length_dl(X-X,L,LT,_).

test(nonempty_open_difflist_1, true([L,LT]==[3,open])) :-
   length_dl([1,2,3|Fin]-Fin,L,LT,_).

test(nonempty_open_difflist_2, true([L,LT]==[4,open])) :-
   % append 4 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|X]-X,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   DLnew=Tip-FinNew,
   length_dl(DLnew,L,LT,_).

test(nonempty_open_difflist_3, true([L,LT]==[6,open])) :-
   % append 4,5,6 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|X]-X,
   DL=Tip-Fin,
   Fin=[4,5,6|FinNew],
   DLnew=Tip-FinNew,
   length_dl(DLnew,L,LT,_).

% ---
% Bad constructions which should throw structure_error
% ---

test(not_a_difflist_0, throws(length_dl(structure_error,_,_))) :-
   length_dl(_X-_Y,_,_,_).

test(not_a_difflist_1, throws(length_dl(structure_error,_,_))) :-
   % append 4 to difflist [1,2,3|_], test the ex-difflist instead of the new difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   _DLnew=Tip-FinNew,
   length_dl(DL,_,_,_).

test(not_a_difflist_2, throws(length_dl(structure_error,_,_))) :-
   % append 4 to difflist [1,2,3|_], but wrongly, using some X as new Fin
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|_FinNew],
   DLnew=Tip-_X,
   length_dl(DLnew,_,_,_).

test(not_a_difflist_3, throws(length_dl(structure_error,_,_))) :-
   length_dl([1,2,3]-_X,_,_,_).

test(not_a_difflist_4, throws(length_dl(structure_error,_,_))) :-
   length_dl([1,2,3|_X]-_Y,_,_,_).

test(not_a_difflist_5, throws(length_dl(structure_error,_,_))) :-
   length_dl([1,2,3|_X]-[],_,_,_).

test(not_a_difflist_6, throws(length_dl(structure_error,_,_))) :-
   length_dl([1,2,3]-[1,2],_,_,_).

test(not_a_difflist_7, throws(length_dl(structure_error,_,_))) :-
   length_dl([1,2]-[1,2,3],_,_,_).

test(not_a_difflist_8, throws(length_dl(structure_error,_,_))) :-
   length_dl([1,2|X]-[1,2,3|X],_,_,_).

test(not_a_difflist_9, throws(length_dl(structure_error,_,_))) :-
   length_dl([1,2,3|X]-[1,2|X],_,_,_).

:- end_tests(length_dl).

rt :- run_tests(length_dl).

