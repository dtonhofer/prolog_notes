:- use_module(library('heavycarbon/utils/difflist_length.pl')).

:- use_module(library('heavycarbon/support/utils.pl')).

:- begin_tests(difflist_length).

% ---
% Passing bad parameters
% ---

test("not difflist", error(contract(difflist,_))) :-
   difflist_length(some_atom,_).

test("not length", error(contract(length,_))) :-
   difflist_length(X-X,not_length).

test("not listtype", error(contract(listtype,_))) :-
   difflist_length(X-X,5,not_listtype,_).

test("not intention", error(contract(intention,_))) :-
   difflist_length(X-X,5,closed,foo).

test("bad list leads to error",error(analysis(not_a_listbox(foo)),_)) :-
   difflist_length([a,b|foo]-_,_).

% ---
% Trying "already closed" difflists (i.e. proper lists)
% ---

test("empty closed list", true([Length,Listtype]==[0,closed])) :-
   DL=X-X,
   X=[],
   difflist_length(DL,Length,Listtype,_).

test("nonempty closed list", true([Length,Listtype]==[3,closed])) :-
   DL=[1,2,3|F]-F,
   F=[],
   difflist_length(DL,Length,Listtype,_).

test("nonempty closed list 2", true([Length,Listtype]==[6,closed])) :-
   DL=[1,2,3|F]-F,
   F=[a,b,c],
   difflist_length(DL,Length,Listtype,_).

test("nonempty closed list 3", true([Length,Listtype]==[3,closed])) :-
   difflist_length([1,2,3]-[3],Length,Listtype,_).

% ---
% Intention: "verify" vs "determine" vs "verify_determine"
% ---

test("intention verify", true(Intention==verify)) :-
   difflist_length([1,2,3]-[],3,closed,Intention).

test("intention verify_determine", true(Intention==verify_determine)) :-
   difflist_length([1,2,3]-[],3,_,Intention).

test("intention verify_determine", true(Intention==verify_determine)) :-
   difflist_length([1,2,3]-[],_,closed,Intention).

test("intention determine", true(Intention==determine)) :-
   difflist_length([1,2,3]-[],_,_,Intention).

% ---
% Bad guess at intention. In fact, intention should always be fresh.
% ---

test("intention bad guess 1", error(consistency(difflist_nonfresh,type(closed),length(3),intention(determine)),_)) :-
   difflist_length([1,2,3]-[],3,closed,determine).

test("intention bad guess 2", error(consistency(difflist_nonfresh,type(_),length(3),intention(verify)),_)) :-
   difflist_length([1,2,3]-[],3,_,verify).

test("intention bad guess 3", error(consistency(difflist_nonfresh,type(closed),length(_),intention(verify)),_)) :-
   difflist_length([1,2,3]-[],_,closed,verify).

test("intention bad guess 4", error(consistency(difflist_nonfresh,type(_),length(_),intention(verify)),_)) :-
   difflist_length([1,2,3]-[],_,_,verify).

% ---
% Templating: the difflist is fresh and is set to a difflist either of a
% given length, or of increasing length at backtracking
% ---

test("templatize difflist of length 0, 4 args") :-
   difflist_length(DL,0,open,templatize),
   DL=X-X,fresh(X).

test("templatize difflist of length 0, 2 args") :-
   difflist_length(DL,0),
   DL=X-X,fresh(X).

test("templatize various difflists, 4 args", true(T)) :-
   bagof(DL,L^(
      between(0,4,L),
      difflist_length(DL,L,open,templatize), % build
      difflist_length(DL,L,open,verify)  % verify
   ), Bag),
   T = (Bag = [X0-X0,
               [_A0|X1]-X1,
               [_B0,_B1|X2]-X2,
               [_C0,_C1,_C2|X3]-X3,
               [_D0,_D1,_D2,_D3|X4]-X4]).

test("templatize various difflists, 2 args", true(T)) :-
   bagof(DL,L^(
      between(0,4,L),
      difflist_length(DL,L), % build
      difflist_length(DL,L)  % verify
   ), Bag),
   T = (Bag = [X0-X0,
               [_A0|X1]-X1,
               [_B0,_B1|X2]-X2,
               [_C0,_C1,_C2|X3]-X3,
               [_D0,_D1,_D2,_D3|X4]-X4]).

% ---
% Analyzing: (Verifying/Determining) open difflists: the difflist is nonfresh
% and its length and its type are determined
% ---

test("empty open difflist", true([L,LT]==[0,open])) :-
   difflist_length(X-X,L,LT,_).

test("nonempty open difflist 1", true([L,LT]==[3,open])) :-
   difflist_length([1,2,3|Fin]-Fin,L,LT,_).

test("nonempty open difflist 2", true([L,LT]==[4,open])) :-
   % append 4 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|X]-X,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   DLnew=Tip-FinNew,
   difflist_length(DLnew,L,LT,_).

test("nonempty open difflist 3", true([L,LT]==[6,open])) :-
   % append 4,5,6 to difflist [1,2,3|_], test difflist
   DL=[1,2,3|X]-X,
   DL=Tip-Fin,
   Fin=[4,5,6|FinNew],
   DLnew=Tip-FinNew,
   difflist_length(DLnew,L,LT,_).

% ---
% Bad constructions which should throw structure_error
% ---

test("not a difflist 0", error(analysis(difflist_structure),_)) :-
   difflist_length(_X-_Y,_).

test("not a difflist 1", error(analysis(difflist_structure),_)) :-
   % append 4 to difflist [1,2,3|_], test the ex-difflist instead of the new difflist
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|FinNew],
   _DLnew=Tip-FinNew,
   difflist_length(DL,_).

test("not a difflist 2", error(analysis(difflist_structure(nonmatching_fin)),_)) :-
   % append 4 to difflist [1,2,3|_], but wrongly, using some X as new Fin
   DL=[1,2,3|F]-F,
   DL=Tip-Fin,
   Fin=[4|_FinNew],
   DLnew=Tip-_X,
   difflist_length(DLnew,_).

test("not a difflist 3", error(analysis(difflist_structure(closed_list)),_)) :-
   difflist_length([1,2,3]-_X,_).

test("not a difflist 4", error(analysis(difflist_structure(nonmatching_fin)),_)) :-
   difflist_length([1,2,3|_X]-_Y,_).

test("not a difflist 5", error(analysis(difflist_structure),_)) :-
   difflist_length([1,2,3|_X]-[],_).

test("not a difflist 6", error(analysis(difflist_structure),_)) :-
   difflist_length([1,2,3]-[1,2],_).

test("not a difflist 7", error(analysis(difflist_structure),_)) :-
   difflist_length([1,2]-[1,2,3],_).

test("not a difflist 8", error(analysis(difflist_structure),_)) :-
   difflist_length([1,2|X]-[1,2,3|X],_).

test("not a difflist 9", error(analysis(difflist_structure),_)) :-
   difflist_length([1,2,3|X]-[1,2|X],_).

:- end_tests(difflist_length).

% ===
% Tests just with the "tip" list
% ===

:- begin_tests(openlist_length).

test("open list of length 0",true(L == 0)) :-
   openlist_length(_,L).

test("open list of length 1",true(L == 1)) :-
   openlist_length([a|_],L).

test("open list of length 2",true(L == 2)) :-
   openlist_length([a,b|_],L).

test("closed list leads to error",error(analysis(closed_list),_)) :-
   openlist_length([a,b],_).

test("bad list leads to error",error(analysis(not_a_listbox(foo)),_)) :-
   openlist_length([a,b|foo],_).

test("nonlist leads to error",error(analysis(not_a_listbox(foo)),_)) :-
   openlist_length(foo,_).

test("generate a few lists",true(T)) :-
   bagof(OL,L^(
      between(0,4,L),
      openlist_length(OL,L), % build
      openlist_length(OL,L)  % verify
   ), Bag),
   T = (Bag = [_X0,
               [_A0|_X1],
               [_B0,_B1|_X2],
               [_C0,_C1,_C2|_X3],
               [_D0,_D1,_D2,_D3|_X4]]).

:- end_tests(openlist_length).


