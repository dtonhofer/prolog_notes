:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).

:- begin_tests(string_of_spaces_performance).

callcount(100000).
max_string_size(500).

goal_special(SzMax,Spaces) :- 
   random_between(0,SzMax,RL),
   string_of_spaces(RL,Spaces).

goal_direct(SzMax,Spaces) :- 
   random_between(0,SzMax,RL),
   length(Codes,RL),
   maplist(=(0'\s),Codes),
   string_codes(Codes,Spaces).

goal_format(SzMax,Spaces) :- 
   random_between(0,SzMax,RL),
   format(string(Spaces),"~t~*|",[RL]),
   string_length(Spaces,RL).

create_and_drop(Goal) :-
   callcount(CC),
   max_string_size(SzMax),
   with_output_to(string(StatsTxt),time(forall(between(1,CC,_),call(Goal,SzMax,_Spaces)))), 
   % doesn't work: time/1 output is NOT captured in StatsTxt
   format("~s (~d calls) (~d max size) using goal '~s': ~q\n",["drop them immediately",CC,SzMax,Goal,StatsTxt]).

create_and_collect(Goal) :-
   callcount(CC),
   max_string_size(SzMax),
   length(CollectionList,CC),
   with_output_to(string(StatTxt),time(maplist({Goal,SzMax}/[Spaces]>>call(Goal,SzMax,Spaces),CollectionList))),
   % doesn't work: time/1 output is NOT captured in StatsTxt
   format("~s (~d calls) (~d max size) using goal '~s': ~q\n",["collect in list",CC,SzMax,Goal,StatTxt]).

test("generate strings of spaces of random length and drop them immediately after creation") :-
   maplist([Goal]>>create_and_drop(Goal),[goal_special,goal_direct,goal_format]).

test("generate strings of spaces of random length and store them in a list") :-
   maplist([Goal]>>create_and_collect(Goal),[goal_special,goal_direct,goal_format]).

:- end_tests(string_of_spaces_performance).


