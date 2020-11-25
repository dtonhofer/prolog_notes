% 2345678901234567890123456789012345678901234567890123456789012345678901234567
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
% Runs on: SWI Prolog 8.1 
% with library(yall) for the Lambda Expression in line
% maplist([X]>>div_with_mod(X,3),InputList),
% ============================================================================

% This code uses two plunit unit test blocks.
% Run the blocks with
%
% ?- run_tests.
% ?- run_tests(key_value_sorting).
% ?- run_tests(special_sorting).

% "Sorting is stable" (which applies only to the ordering of pairs Key-Value by their
% Key) means: elements having the same Key keep the ordering they have in the
% subsequence of elements with that Key.

% ===
% sort_dispatch(+ListIn,?ListOut,+AtomSelectingSortAlgorithm)
% Select the sorting predicate based on a third argument
% ===

% sort/2 -- Sort to the "standard order of terms".
% Remove duplicates (which one is unimportant as equal terms are undistinguishable) 
% https://eu.swi-prolog.org/pldoc/doc_for?object=sort/2
   
sort_dispatch(Lin,Lout,iso_sort) :- !,sort(Lin,Lout).

% msort/2 -- Sort to the "standard order of terms".
% Keep duplicates. (stability is unimportant as equal terms are undistinguishable)
% https://eu.swi-prolog.org/pldoc/doc_for?object=msort/2

sort_dispatch(Lin,Lout,msort) :- !,msort(Lin,Lout).

% keysort/2 -- Sort a list of Key-Value pairs.
% Works best with library(pairs): https://www.swi-prolog.org/pldoc/man?section=pairs
% It sorts by Key on the "standard order of terms"-
% Duplicates are retained. Sorting is stable.
% https://eu.swi-prolog.org/pldoc/doc_for?object=keysort/2
   
sort_dispatch(Lin,Lout,keysort) :- !,keysort(Lin,Lout).

% sort/4 with @< -- Sort by explicitly sorting on the first element of the "compound term" 
% (i.e. the A in A-B) to the "standard order for terms".
% In this case, remove duplicates, keeping first in-line only.
% https://eu.swi-prolog.org/pldoc/doc_for?object=sort/4
   
sort_dispatch(Lin,Lout,sort4nodups) :- !,sort(1,@<,Lin,Lout).

% sort/4 with @=< -- Sort by explicitly sorting on the first element of the "compound term"
% (i.e. the A in A-B) to the "standard order for terms".
% In this case, duplicates are retained. Sorting is stable.
% https://eu.swi-prolog.org/pldoc/doc_for?object=sort/4

sort_dispatch(Lin,Lout,sort4dups) :- !,sort(1,@=<,Lin,Lout).

% predsort/3 -- Sort by specifying a "sorting predicate".
% Duplicates (those elements for which the sorting predicate returns "=") are 
% removed, keeping the first element in "Lin" only!
% Note the library(yall) lambda expression which calls compare/3 inline.

sort_dispatch(Lin,Lout,predsort) :- !,predsort([X,A-_,B-_]>>compare(X,A,B),Lin,Lout).

% Catchall in case of error.

sort_dispatch(_,_,Behaviour) :- 
   format(string(Buf),"Unhandled behaviour '~q'",[Behaviour]),
   throw(Buf).
   
% ===
% Helpers for unit tests
% ===

% You have two lists:
% - A list of keys
% - A list of values
% Sort the values by their keys

% ---
% Assemble and disassemble keys and values into/from pairs "Key-Value": one predicate!
% ---

ziptwo(A,B,A-B).

% ---
% Do the "Key,Value -> Key-Value" assembly,
% then the sorting according to "Behaviour",
% and then the "Key-Value" -> "Value" disassembly
% ---

sort_keyfully(KeyList,ValList,Lout,Behaviour) :-
   maplist(ziptwo,KeyList,ValList,Lready),
   sort_dispatch(Lready,Lsorted,Behaviour),
   maplist(ziptwo,_,Lout,Lsorted).

% ===
% plunit unit tests, block #1
% ===

:- begin_tests(key_value_sorting).

keylist([b,c,c,a,b,d,b]).
vallist([2,3,2,1,1,4,2]).

do(Behaviour,Expected) :-
   keylist(K),vallist(V),
   sort_keyfully(K,V,Lout,Behaviour),
   write(Lout),
   Lout=Expected.

test(iso_sort_0)    :- do(iso_sort    , [1,1,2,2,3,4]).   % sort by whole term A-B, remove duplicates.
test(msort_0)       :- do(msort       , [1,1,2,2,2,3,4]). % sort by whole term A-B, retain duplicates.
test(keysort_0)     :- do(keysort     , [1,2,1,2,3,2,4]). % sort by keys, retain duplicates, sort is stable.
test(sort4nodups_0) :- do(sort4nodups , [1,2,3,4]).       % sort by subterm (here: by keys), remove duplicates, keeping first only.
test(sort4dups_0)   :- do(sort4dups   , [1,2,1,2,3,2,4]). % sort by subterm (here: by keys), retain duplicates, sort is stable.
test(predsort_0)    :- do(predsort    , [1,2,3,4]).       % sort by predicate (here: by keys), remove duplicates, keeping first only.

:- end_tests(key_value_sorting).

% ===
% Helpers for unit tests
% ===

% You have one list of atoms.
% Sort the list by the third character of each atom.

% ---
% Assemble and disassemble keys and values into/from pairs "Key-Value": one predicate!
% ---

assemble(Lin,Lout) :-
   maplist(
       [Atom,Out]>>
          (atom_chars(Atom,MonoAtoms),
           nth0(2,MonoAtoms,Prefix),
           Out=Prefix-Atom)
       ,Lin,Lout).

disassemble(Lin,Lout) :-
   maplist([_-Atom,Atom]>>true,Lin,Lout).

% ---
% Do the "Atom -> Key-Atom" assembly,
% then the sorting according to "Behaviour",
% and then the "Key-Atom" -> "Atom" disassembly
% ---

sort_by_third_char(Lin,Lout,Behaviour) :-
   assemble(Lin,Lready),
   % format("~q\n",[Lready]),
   sort_dispatch(Lready,Lsorted,Behaviour),
   % format("~q\n",[Lsorted]),
   disassemble(Lsorted,Lout).

% ===
% plunit unit tests, block #2
% ===

:- begin_tests(special_sorting).

input1([aab,aac,aad,aab,ccb]).
input2([zzb,yyb,xxb,xxb]).

do(Which,Behaviour,Expected) :-
   (Which=1 -> input1(Lin) ; input2(Lin)),
   sort_by_third_char(Lin,Lout,Behaviour),
   write(Lout),
   Lout=Expected.
   
test(iso_sort_1) :- do(1,iso_sort,[aab,ccb,aac,aad]).
test(iso_sort_2) :- do(2,iso_sort,[xxb,yyb,zzb]).
                                                                            
test(msort_1) :- do(1,msort,[aab,aab,ccb,aac,aad]).
test(msort_2) :- do(2,msort,[xxb,xxb,yyb,zzb]).

test(keysort_1) :- do(1,keysort,[aab,aab,ccb,aac,aad]).
test(keysort_2) :- do(2,keysort,[zzb,yyb,xxb,xxb]).

test(sort4nodups_1) :- do(1,sort4nodups,[aab,aac,aad]).
test(sort4nodups_2) :- do(2,sort4nodups,[zzb]).

test(sort4dups_1) :- do(1,sort4dups,[aab,aab,ccb,aac,aad]).
test(sort4dups_2) :- do(2,sort4dups,[zzb,yyb,xxb,xxb]).

test(predsort_1) :- do(1,predsort,[aab,aac,aad]).
test(predsort_2) :- do(2,predsort,[zzb]).

:- end_tests(special_sorting).
