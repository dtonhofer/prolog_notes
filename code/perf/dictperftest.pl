% ============================================================================
% Testing retrieval from possibly large SWI-Prolog dicts
%
% Call with:
%
% ?- using_dict(50_000,500_000,dicts).
%
% This will create a dict with atom keys of size 50'000, and perform
% 500'000 lookups on it. 
%
% The lookup sequence (a sequence of atom keys) is created by looking up a
% key randomly in a dict in the first place. Alternatively:
%
% ?- using_dict(50_000,500_000,lists).
%
% will construct the sequence using a random lookup in a list instead. 
% That's very slow, for the above it takes over 10 minutes. Do not use it!
%
% Result:
%
% ?- using_dict(50_000,500_000,dicts).
% 
% says:
% Looking up 500000 entries in a dict of size 50000 (storing the result)
% % 1,500,002 inferences, 0.241 CPU in 0.242 seconds (100% CPU, 6212968 Lips)
% Looking up 500000 entries in a dict of size 50000 (not storing the result)
% % 1,500,002 inferences, 0.241 CPU in 0.242 seconds (100% CPU, 6228728 Lips)
% ============================================================================

:- use_module(library('heavycarbon/utils/random_atom.pl')).
:- use_module(library('snippets/snippets.pl')).
:- use_module(library('heavycarbon/support/meta_helpers.pl')).

using_dict(SizeOfDict,LookupCount,Which) :-
   build_list_of_random_atoms(SizeOfDict,10,Keys),
   randomly_valuate(Keys,20,Pairs1),   % should we worry about the birthday paradox here?
   build_dict(Pairs1,Dict,'perftest'), % will throw on duplicate keys; that's ok for now
   switch(
      (Which==dicts), build_random_lookup_sequence_using_dicts(Keys,LookupCount,Sequence),
      (Which==lists), build_random_lookup_sequence_using_lists(Keys,LookupCount,Sequence),
      domain_error([dicts,lists],Which)),
   perform_lookup_store(Dict,Sequence),
   perform_lookup_nostore(Dict,Sequence).
   
build_list_of_random_atoms(ListLength,AtomLength,Atoms) :-
   length(Atoms,ListLength),
   format("Filling a list of length ~d with random atoms of length ~d\n",[ListLength,AtomLength]),
   time(maplist([Atom]>>random_text(Atom,10,[what(atom)]),Atoms)). 

dict_size(Dict,Size) :-
   assertion(is_dict(Dict)),
   assertion(var(Size);integer(Size)),
   compound_name_arity(Dict,_,Arity),
   Size is (Arity-1)//2.

randomly_valuate(KeysIn,StringLength,PairsOut) :-
   length(KeysIn,KeysLength),
   format("Creating pairs from ~d keys, where the values are strings of length ~d~n",[KeysLength,StringLength]),
   time((
      maplist({StringLength}/[Key,Key-Value]>>random_text(Value,StringLength,[what(string)]),KeysIn,PairsOut)
   )).
   
build_dict(PairsIn,DictOut,Tag) :-
   length(PairsIn,Length),
   format("Creating a dict of size ~d, with tag '~a'~n",[Length,Tag]),
   time(dict_create(DictOut,Tag,PairsIn)).
   
build_random_lookup_sequence_using_lists(Keys,SequenceLength,Sequence) :-
   assertion(is_list(Keys)),
   assertion(integer_strictly_positive(SequenceLength)),
   assertion(var(Sequence)),
   length(Keys,KeysLength),
   format("Creating a random lookup sequence of size ~d based on ~d keys (using lists)~n",[SequenceLength,KeysLength]),
   format("...preparing~n",[]),
   time(length(Sequence,SequenceLength)),
   format("...picking ~d random keys~n",[SequenceLength]),
   time((maplist({Keys}/[X]>>random_member(X,Keys),Sequence))).
   
% Randomly select "SequenceLength" entries from "Keys", and put them into "Sequence". 
% Here, the "Keys" are put into a dict first, where the dict key "PickKey" is an integer index from 0..length("Keys")
% and the dict value "PickValue" is a "Key". We can then pick a "Key" easily at random from the dict.

build_random_lookup_sequence_using_dicts(Keys,SequenceLength,Sequence) :-
   length(Keys,KeysLength),
   format("Creating a random lookup sequence of size ~d based on ~d keys (using dicts)~n",[SequenceLength,KeysLength]),
   format("...preparing~n",[]),
   time((   
      list_of_integers_between(high_no,0,KeysLength,Indexes),
      pairs_keys_values(Pairs,Indexes,Keys),
      dict_create(IndexedKeysDict,foo_tag,Pairs),
      length(Sequence,SequenceLength)
   )),
   format("...picking ~d random keys~n",[SequenceLength]),      
   time((
       maplist(
          {IndexedKeysDict,KeysLength}/[PickedKey]
          >>
          (random_between(high_no,0,KeysLength,Index),
           get_dict(Index,IndexedKeysDict,PickedKey)),
          Sequence)
   )).
      
perform_lookup_store(Dict,Sequence) :-   
   dict_size(Dict,Size),
   length(Sequence,SequenceLength),   
   format("Looking up ~d entries in a dict of size ~d (storing the result)~n",[SequenceLength,Size]),
   time((
      maplist(
         {Dict}/[Key,Value]
         >>
         (get_dict(Key,Dict,Value)
          %,format("~s~n",[Value])
         ),
         Sequence,Result)
   )),         
   length(Result,ResultLength),   
   format("ResultLength is ~d~n",[ResultLength]).

perform_lookup_nostore(Dict,Sequence) :-   
   dict_size(Dict,Size),
   length(Sequence,SequenceLength),   
   format("Looking up ~d entries in a dict of size ~d (not storing the result)~n",[SequenceLength,Size]),
   time(maplist({Dict}/[Key]>>get_dict(Key,Dict,_),Sequence)).
   
