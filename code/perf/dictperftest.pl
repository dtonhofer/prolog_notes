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
% The preparation of the dict can take some time too and the individual
% actions will also be timed.
%
% ?- using_dict(50_000,500_000,builtin,dict,verbose).
%
% ============================================================================
% TODO: Add code using library(assoc) and linear lookup in lists, too.
% TODO: Pull the statistics into the program for better processing. Needs
%       a change to the statistics predicates
% ============================================================================

% :- use_module(library(yall)).
% :- use_module(library(apply_macros)).

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

% ---8<----8<--- MODULE READING IN /proc/self/status COPIED IN BELOW ---8<---8<---

proc_self_status(DictOut) :-
   setup_call_cleanup(
         open("/proc/self/status",read,Stream,[]),
         read_stream_to_codes(Stream,Codes),
         close(Stream)
   ),
   phrase(lines(proc_self_status{},DictOut),Codes),
   !.
   
% Note that even though "XXX" is a string in SWI-Prolog, in the
% DCG rule, "XXX" is interpreted as a list of codes.

lines(Dict,Dict)      --> [].
lines(DictIn,DictOut) --> line(DictIn,Dict2),lines(Dict2,DictOut).

line(DictIn,DictOut)  --> datum(DictIn,DictOut), "\n" , !.
line(Dict,Dict)       --> string_without("\n",_Codes), "\n".
% line(Dict,Dict)       --> string_without("\n",Codes), "\n", { format(user_error,"Unknown: <~s>~n",[Codes]) }.

datum(DictIn,DictIn.put([name=Str]))                 --> "Name:", string_to_eol(Str).
datum(DictIn,DictIn.put([pid=Int]))                  --> "Pid:", just_integer(Int).
datum(DictIn,DictIn.put([ppid=Int]))                 --> "PPid:", just_integer(Int).
datum(DictIn,DictIn.put([tgid=Int]))                 --> "Tgid:", just_integer(Int).
datum(DictIn,DictIn.put([ngid=Int]))                 --> "Ngid:", just_integer(Int).
datum(DictIn,DictIn.put([tracerpid=Int]))            --> "TracerPid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_tgid=Int]))              --> "NStgid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_pid=Int]))               --> "NSpid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_pgid=Int]))              --> "NSpgid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_sid=Int]))               --> "NSsid:", just_integer(Int).
datum(DictIn,DictIn.put([vm_data=[Val,Unit]]))       --> "VmData:", val_and_unit(Val,Unit). % size of private data segments
datum(DictIn,DictIn.put([vm_stk=[Val,Unit]]))        --> "VmStk:", val_and_unit(Val,Unit). % size of stack segments
datum(DictIn,DictIn.put([vm_exe=[Val,Unit]]))        --> "VmExe:", val_and_unit(Val,Unit). % size of text segment
datum(DictIn,DictIn.put([vm_lib=[Val,Unit]]))        --> "VmLib:", val_and_unit(Val,Unit). % size of shared library code
datum(DictIn,DictIn.put([vm_pte=[Val,Unit]]))        --> "VmPTE:", val_and_unit(Val,Unit). % size of page table entries
datum(DictIn,DictIn.put([vm_swap=[Val,Unit]]))       --> "VmSwap:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_peak=[Val,Unit]]))       --> "VmPeak:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_size=[Val,Unit]]))       --> "VmSize:", val_and_unit(Val,Unit). % total program size
datum(DictIn,DictIn.put([vm_lck=[Val,Unit]]))        --> "VmLck:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_pin=[Val,Unit]]))        --> "VmPin:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_hwm=[Val,Unit]]))        --> "VmHWM:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_rss=[Val,Unit]]))        --> "VmRSS:", val_and_unit(Val,Unit).  % size of memory portions. It contains the three following parts (VmRSS = RssAnon + RssFile + RssShmem)
datum(DictIn,DictIn.put([rss_anon=[Val,Unit]]))      --> "RssAnon:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([rss_file=[Val,Unit]]))      --> "RssFile:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([rss_shmem=[Val,Unit]]))     --> "RssShmem:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([hugetlb_pages=[Val,Unit]])) --> "HugetlbPages:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([core_dumping=Int]))         --> "CoreDumping:", just_integer(Int).
datum(DictIn,DictIn.put([thp_enabled=Int]))          --> "THP_enabled:", just_integer(Int).
datum(DictIn,DictIn.put([threads=Int]))              --> "Threads:", just_integer(Int).
datum(DictIn,DictIn.put([umask=Str]))                --> "Umask:", string_to_eol(Str). % actually a string of 4 octals
datum(DictIn,DictIn.put([state=Str]))                --> "State:", string_to_eol(Str).

val_and_unit(Val,Unit) --> whites, integer(Val), whites, string_without("\n",Codes), { string_codes(Unit,Codes) }.
just_integer(Int)      --> whites, integer(Int).
string_to_eol(Str)     --> whites, string_without("\n",Codes), { string_codes(Str,Codes) }.

% ---8<----8<--- PART OF SNIPPETS MODULE COPIED IN BELOW ---8<---8<---

list_of_integers_between(high_yes,Low,High,List) :-
   !,
   bagof(X,between(Low,High,X),List). 
   
list_of_integers_between(high_no,Low,High,List) :-
   !,
   ActualHigh is High-1,
   bagof(X,between(Low,ActualHigh,X),List). 

integer_strictly_positive(X) :- integer(X),X>0.

dict_size(Dict,Size) :-
   assertion(is_dict(Dict)),
   assertion(var(Size);integer(Size)),
   compound_name_arity(Dict,_,Arity),
   Size is (Arity-1)//2. 

% ---8<----8<--- READABILITY SUPPORT PREDICATES COPIED IN BELOW ---8<---8<---

switch(If1,Then1,If2,Then2,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(Else).

if_then(Condition,Then) :-
   call(Condition) -> call(Then) ; true.

if_then_else(Condition,Then,Else) :-
   call(Condition) -> call(Then) ; call(Else).

% ---8<----8<--- MODULE FOR RANDOM ATOMS COPIED IN BELOW ---8<---8<---

:- use_module(library(option)).

% ===
% "char_list/2" always returns the same values computed at runtime, namely:
% Chars: the list of chars ['a','b','c',....,'z'].
% Max:   the maximum allowed 0-based index into the list Chars
% The predicate is tabled so that it is not needlessly executed over and over.
% char_list(-Chars,-Max).
% ===

:- table char_list.

char_list(Chars,Max) :-
   atom_chars('abcdefghijklmnopqrstuvwxyz',Chars),  % explode atom into list of chars
   length(Chars,Length),                            % get its length
   succ(Max,Length).                                % adjust to 0-indexed

% ===
% Generate text, with the length of the result either specified (if L is bound) or
% determined (if L is unbound). Option processing is done through library(option).
% See https://eu.swi-prolog.org/pldoc/man?section=option
% Options is a list which may or may not contain:
% - the option what(W), where W is one of the atoms 'atom', 'string' (default 'atom')
% - the option empty(YN), where YN is one of 'yes', 'no' (default 'no')
% Corresponding "direct-to-atom" and "direct-to-string" predicates also exist.
% The just look at the option empty(YN)
% ===

% EXPORTED
random_text(Text,Len) :-
   random_text(Text,Len,[]).         % only default options

% EXPORTED
random_text(Text,Len,Options) :-
   nonvar(Len),
   !,
   length(Chars,Len),                % throws if Len < 0 or not an integer
   random_char_list(Chars,Options),  % fill the list of known length
   transform(Chars,Text,Options).    % generate appropriately-typed text

% EXPORTED
random_text(Text,Len,Options) :-
   var(Len),
   !,
   random_char_list(Chars,Options),  % fill a list of random length (the length results from a markov process)
   length(Chars,Len),
   transform(Chars,Text,Options).

% ===
% Depending on whether Options indicate to generate a 'string' or an 'atom',
% generate text of the requested type. The default is 'atom'.
% ===

transform(Chars,Text,Options) :-
   option(what(W),Options,atom),
   if_then_else(
      (W==string),                  % non-standard case which must be specified
      string_chars(Text,Chars),
      atom_chars(Text,Chars)).      % else/default case: W==atom or something else or W is var because Options is funky

% ===
% Actually generate the list of characters.
% "Chars" can be an unbound variable.
%    Then:
%    - The length of the resulting list that is eventually unified with "Chars" is chosen
%      according to a Markov process.
%    - The option "empty(YN)" from the "Options" list is considered.
%      YN is one of 'yes', 'no' (default 'no'). The resulting list may by this means be
%      allowed to be empty.
% "Chars" can be a proper list of unbound variables, thus specifying the length.
%    Then:
%    - The option "empty(YN)" from the "Options" list is considered; if the
%      specified length is 0 and "empty(no)" is given, the predicate fails, thus
%      staying consistent with the case of "Chars" an unbound variable.
%
% Interesting thought: These are actually "compressed" clauses for the actual predicate
% random_char_list(Chars,Length,Options)
% where Chars is always unbound, an Length may or may not be bound. If Length is
% bound, the Chars are selected from a smaller subspace of all possible Chars.
% If one could give a set of Length, how would one choose a Chars from the union of
% smaller subspaces with correct relative probabilities?
%
% There is a 3-arg version, which can only be used if "Chars" is an unbound variable.
% It also builds a list holding the changing probability for deciding "more characters"
% and the coin tosses made to make that decision.
% ---

% EXPORT
random_char_list(Chars,Options,Tosses) :-
   var(Chars),
   !,
   if_then_else(
      option(empty(no),Options,no),              % empty(no) first in Options or empty/1 missing from Options
      (random_char_list_inner(L2,1,Tosses2),
       random_char(X),Chars=[X|L2],              % correctly start at length "1"
       Tosses = [1|Tosses2]),
      (random_char_list_inner(Chars,0,Tosses))). % might generate nothing

% EXPORT
random_char_list(Chars,Options) :-
   var(Chars),
   !,
   random_char_list(Chars,Options,_). % call the 3-arg predicate, then dump the "tosses"

% EXPORT
random_char_list(Chars,Options) :-
   nonvar(Chars),
   !,
   assertion(is_list(Chars)),                % Chars must be a closed list
   assertion(maplist(var,Chars)),            % Chars must be a list of unbound variables
   self_defeating_case(Chars,Options),       % another check
   maplist(random_char,Chars).               % fill list

self_defeating_case(Chars,Options) :- 
  (option(empty(no),Options,no),  % Caller doesn't want empty list (empty(no) first in Options or empty/1 missing from Options)
   Chars==[])                     % and Caller demands empty list
  -> fail ; true.
     
% ===
% random_char_list(-Result,+CharsGeneratedAlready)
%
% The length of the list is found by throwing a biased coin before each character
% selection. The coin's probability of saying "one more character" decreases
% according to a sigmoid that is a function of the number of characters
% already in the list (it decreases slowly at first, then has an
% exponential tail going to 0). That gives nicer results than decreasing the
% probability according to a exponential in the length of already-collected
% characters, which would be the case by throwing the same biased coin each time.
%
% The style below is very imperative but easier to understand than if you
% separate it into two clauses, onw for "stop criterium reached + cut" and
% one for "one more character and recursive call". Note that there are two
% open lists in action to which we append, but written in differing style:
% "Chars" and "[Toss|Tosses]" (the latter collects the sigmoid floats, just for
% fun)
% ===

random_char_list_inner(Chars,CharsGot,[Toss|Tosses]) :-
   sigmoid_value(CharsGot,P),    % P becomes smaller and smaller as CharsGot becomes larger (but even for 0, it's not 1.0)
   random(Coin),                 % Coin is a value [0.0..1.0)
   if_then_else(
      (P<Coin),                  % Biased decision on whether to stop, become more probably with length
      (Chars=[],                 % Decision to stop
       Toss=(P<Coin),
       Tosses=[]),
      (random_char(X),           % Otherwise another random character selection
       Chars=[X|More],           % Construction of result list (note this is exactly the procedure of "appending to an open list"
       succ(CharsGot,CharsGot2), % We have 1 more character now
       Toss=(P>=Coin),
       random_char_list_inner(More,CharsGot2,Tosses))).

% ===
% Select a "uniformly random" char from the list of chars given by "char_list/2"
% This is used from inside maplist/1 and the random_char_lits_inner/2 generator.
% One can call it with instantiated X, but it's probably going to fail...
% ===

% EXPORTED
random_char(X) :-
   char_list(Chars,Max),
   random_between(0,Max,Selected),
   nth0(Selected,Chars,X).

% ===
% Sigmoid expressing decreasing probability "P" of continuing with "one
% more character" given there already are "L" characters.
% Plot it at https://www.desmos.com/calculator
% ===

sigmoid_value(L,P) :-
   P is 1-(1/(1+exp(2-(L/2)))).


