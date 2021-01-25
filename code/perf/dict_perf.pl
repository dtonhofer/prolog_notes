% ============================================================================
% Testing retrieval from possibly large SWI-Prolog dicts
% ============================================================================
%
% Call with:
%
% ?- using_dict(50_000,500_000,dicts,builtin).
%
% This will create a dict with atom keys of size 50'000, and perform
% 500'000 lookups on it. 
%
% Arg 1: Size of the dict on which lookup will be performed
% Arg 2: Number of lookups to perform. 
%        The duration that  lookups take will be timed.
% Arg 3: 'dict' or 'list': How the sequence (a list) of lookups to 
%        perform is constructed: by renadom lookup in a dict or 
%        a list of allowed keys. 
%        This takes quite some time.
% Arg 4: 'builtin' or 'loop'; build the dict on which lookups
%        will be performed with 'dict_create/3 or put_dict/4
%        in a loop (foldl)
% Arg 5: 'quiet' or 'verbose'; print progress messages or not 
%
% Load:
%
% ?- [library('perf/dict_perf.pl')].

% Now run:
%
% ?- using_dict(50_000,500_000,builtin,dict,verbose).
%
% This should be rather fast, i.e. the test should be finished in a few
% seconds. If not, the compiler may not have fully compiled the yall
% lambda expressions... reload the code a second time (using consult).
% ============================================================================
% TODO: Add code using library(assoc) and linear lookup in lists, too.
% TODO: Add code using library(rbtrees) 
% TODO: Pull the statistics into the program for better processing. Needs
%       a change to the statistics predicates
% ============================================================================

:- module(perf, [ using_dict/5 ]).

:- use_module(library('heavycarbon/support/meta_helpers.pl')).
:- use_module(library('heavycarbon/utils/random_atom.pl')).
:- use_module(library('snippets/snippets.pl')).
:- use_module(library('snippets/proc_self_status.pl')).

% Do not forget to load these or the test will be SLOW!

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

% ===
% Main predicate
% ===

using_dict(SizeOfDict,LookupCount,BuildDictHow,BuildLookupSequenceHow,Quiet) :-
   assertion(integer_strictly_positive(SizeOfDict)),
   assertion(integer_strictly_positive(LookupCount)),
   assertion(member(BuildLookupSequenceHow,[dict,list])),
   assertion(member(BuildDictHow,[builtin,loop])),
   assertion(member(Quiet,[quiet,verbose])),
   build_list_of_random_atoms(SizeOfDict,10,Keys),
   randomly_valuate(Keys,3,Pairs1), % should we worry about the birthday paradox here?
   switch(
      (BuildDictHow==builtin),
         build_dict_from_pairs_using_builtin(Pairs1,Dict,p),
      (BuildDictHow==loop),
         build_dict_from_pairs_using_loop(Pairs1,Dict,p),
      domain_error([builtin,loop],BuildDictHow)),
   switch(
      (BuildLookupSequenceHow==dict), 
         build_random_lookup_sequence_using_dict(Keys,LookupCount,Sequence,Quiet),
      (BuildLookupSequenceHow==list), 
         build_random_lookup_sequence_using_list(Keys,LookupCount,Sequence,Quiet),
      domain_error([dict,list],BuildLookupSequenceHow)),
   perform_lookup_and_store(Dict,Sequence,Quiet). % main timed test

% ---
% Creating a list of random atoms of fixed length
% ---   

build_list_of_random_atoms(ListLength,AtomLength,Atoms) :-
   length(Atoms,ListLength),
   format("Filling a list of length ~d with random atoms of length ~d\n",[ListLength,AtomLength]),
   time(maplist([Atom]>>random_text(Atom,10,[what(atom)]),Atoms)). 

% ---
% Creating a list of pairs Key-String "PairsOut" where string is a random
% string of fixed length and Key is from the list "KeysIn".
% ---   

randomly_valuate(KeysIn,StringLength,PairsOut) :-
   length(KeysIn,KeysLength),
   format("Creating pairs from ~d keys, where the values are strings of length ~d~n",[KeysLength,StringLength]),
   time((
      maplist({StringLength}/[Key,Key-Value]>>random_text(Value,StringLength,[what(string)]),KeysIn,PairsOut)
   )).

% ---
% Build a dict "DictOut" with tag "Tag" from a list "Pairs" of "Key-Value" pairs. 
% Two ways:
% 'builtin' - use the builtin predicate dict_create/3 
% 'loop'    - use foldl/4 to loop over "Pairs", adding the pairs one-by-one, upticking the dict version
% ---
   
build_dict_from_pairs_using_builtin(PairsIn,DictOut,Tag) :-
   length(PairsIn,Length),
   format("Creating a dict of size ~d, with tag '~a' using built-in predicate dict_create/3~n",[Length,Tag]),
   time(dict_create(DictOut,Tag,PairsIn)).

build_dict_from_pairs_using_loop(PairsIn,DictOut,Tag) :-
   length(PairsIn,Length),
   format("Creating a dict of size ~d, with tag '~a' using a loop calling put_dict/4~n",[Length,Tag]),
   dict_create(EmptyDict,Tag,[]),
   % non-yall alternative for the time goal:
   % time(foldl(inside_foldl_1_A,PairsIn,EmptyDict,DictOut)).
   time(
      % Can't use yall with dict "." notation in body - the program crashes.
      % Use traditional put_dict/4 instead.
      foldl([Key-Value,Din,Dout]>>put_dict(Key,Din,Value,Dout),PairsIn,EmptyDict,DictOut)
   ).

% Non-yall helpers for the above

inside_foldl_1_A(Key-Value,Din,Dout) :- put_dict(Key,Din,Value,Dout).  % Old school
inside_foldl_1_B(Key-Value,Din,Dout) :- Dout = Din.put([Key=Value]).   % Dot notation, not "in-place"
inside_foldl_1_C(Key-Value,Din,Din.put([Key=Value])).                  % Dot notation, "in-place"

% ---
% Build a list of valid dictionary keys that represents the sequence of
% lookups in the big dictionary during the main test (this lookup action is
% what we will time). This means the whole sequence is generated and kept in
% memory. One could alternatively generate it on need using freeze/2, but
% the idea here is to have them ready so that determining the next key
% to use in a lookup does not enter into the timing of the performance.
% Getting the next key off a prepared list seems a cheap way to do this.
%
% Two ways of doing it:
%
% - For each element of the sequence, select a random key from the list of 
%   valid keys with random_member/2. This is very slow.
% - For each element of the sequence, select a random key from a dict that
%   indexes the valid keys by integer (i.e. it works as an array). This
%   is very fast (already indicating that time lookup test will be very
%   fast)
% ---

build_random_lookup_sequence_using_list(Keys,SequenceLength,Sequence,Quiet) :-
   assertion(is_list(Keys)),
   assertion(integer_strictly_positive(SequenceLength)),
   assertion(var(Sequence)),
   length(Keys,KeysLength),
   format("Creating a random lookup sequence of size ~d based on ~d keys (using lists)~n",[SequenceLength,KeysLength]),
   length(Sequence,SequenceLength),
   list_of_integers_between(high_no,0,SequenceLength,Counters),
   time(maplist(inside_1(Keys,SequenceLength,Quiet),Sequence,Counters)).

inside_1(Keys,SequenceLength,Quiet,PickedKey,C) :-
   random_member(PickedKey,Keys),
   emit(Quiet,SequenceLength,C).
 
build_random_lookup_sequence_using_dict(Keys,SequenceLength,Sequence,Quiet) :-
   length(Keys,KeysLength),
   format("Creating a random lookup sequence of size ~d based on ~d keys (using dicts)~n",[SequenceLength,KeysLength]),
   list_of_integers_between(high_no,0,KeysLength,Indexes),
   pairs_keys_values(Pairs,Indexes,Keys),
   dict_create(IndexedKeysDict,foo_tag,Pairs),
   length(Sequence,SequenceLength),
   list_of_integers_between(high_no,0,SequenceLength,Counters),
   time(maplist(inside_maplist_a(IndexedKeysDict,KeysLength,SequenceLength,Quiet),Sequence,Counters)).

inside_maplist_a(IndexedKeysDict,KeysLength,SequenceLength,Quiet,PickedKey,C) :-
   S is KeysLength-1,random_between(0,S,Index),
   get_dict(Index,IndexedKeysDict,PickedKey),
   emit(Quiet,SequenceLength,C).

% ---
% The main test that will be timed: look up values in "Dict", according to
% "Sequence", the sequence of keys to look up. One version stores the
% looked up values, the other does not.
% ---
      
perform_lookup_and_store(Dict,Sequence,Quiet) :-   
   dict_size(Dict,Size),
   length(Sequence,SequenceLength),   
   list_of_integers_between(high_no,0,SequenceLength,Counters),
   format("Looking up ~d entries in a dict of size ~d (storing the result)~n",[SequenceLength,Size]),
   perform_lookup_and_store_timed_1(Dict,Quiet,Sequence,SequenceLength,Counters).

perform_lookup_and_store_timed_1(Dict,Quiet,Sequence,SequenceLength,Counters) :-
   get_time(Start),
   time(perform_lookup_and_store_timed_2(Dict,Quiet,Sequence,SequenceLength,Counters)),
   get_time(End),
   Delta is End-Start,
   PerSecond is integer(SequenceLength/Delta),
   format("Performed ~d lookup/s (delta = ~f s)\n",[PerSecond,Delta]).

/*
perform_lookup_and_store_timed_2(Dict,Quiet,Sequence,SequenceLength,Counters) :-
   maplist(inside_maplist_b(Dict,Quiet,SequenceLength),Sequence,Result,Counters),
   length(Result,ResultLength),
   format("ResultLength is ~d~n",[ResultLength]).

inside_maplist_b(Dict,Quiet,SequenceLength,Key,Value,C) :-
   get_dict(Key,Dict,Value),
   emit(Quiet,SequenceLength,C).
*/

perform_lookup_and_store_timed_2(Dict,Quiet,Sequence,SequenceLength,Counters) :-
   maplist(({Dict,Quiet,SequenceLength}/[Key,Value,C]>>(get_dict(Key,Dict,Value),emit(Quiet,SequenceLength,C))),Sequence,Result,Counters),
   length(Result,ResultLength),
   format("ResultLength is ~d~n",[ResultLength]).

% ---
% Emit a running message to stdout
% ---

emit(quiet,_,_) :- !.

emit(verbose,_,C) :-
   C mod 10000 =\= 0,
   !.

emit(verbose,SequenceLength,C) :-
   proc_self_status(Dict),
   Dict.vm_data = [VmDataVal,VmDataUnit],
   Dict.vm_stk  = [VmStackVal,VmStackUnit],
   Dict.vm_exe  = [VmExeVal,VmExeUnit],
   Dict.vm_lib  = [VmLibVal,VmLibUnit],
   Dict.vm_pte  = [VmPteVal,VmPteUnit],
   Dict.vm_size = [VmSizeVal,VmSizeUnit],
   Dict.vm_rss  = [VmRssVal,VmRssUnit],
   format("~d/~d ",[C,SequenceLength]),
   format("vm_data: ~d ~s, ",[VmDataVal,VmDataUnit]), 
   format("vm_stack: ~d ~s, ",[VmStackVal,VmStackUnit]),
   format("vm_exe: ~d ~s, ",[VmExeVal,VmExeUnit]),
   format("vm_lib: ~d ~s, ",[VmLibVal,VmLibUnit]),
   format("vm_pte: ~d ~s, ",[VmPteVal,VmPteUnit]),
   format("vm_size: ~d ~s, ",[VmSizeVal,VmSizeUnit]),
   format("vm_rss: ~d ~s~n",[VmRssVal,VmRssUnit]).
