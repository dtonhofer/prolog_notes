:- use_module(library('heavycarbon/strings/string_overwrite.pl')).
:- use_module(library('heavycarbon/strings/stringy.pl')).

/*

?- run_tests.
% PL-Unit: string_overwrite_performance 
Looping calls to string_overwrite using runs
% 28,637,889 inferences, 7.443 CPU in 7.477 seconds (100% CPU, 3847478 Lips)
Looping calls to string_overwrite using chars
% 237,865,722 inferences, 41.849 CPU in 42.014 seconds (100% CPU, 5683895 Lips)
. done
% test passed
true.

*/

% :- debug(performance).

:- begin_tests(string_overwrite_performance).

% ---
% Use tabling to cache the "list of chars" 
% ---

:- table prepare_list_of_chars/2.

prepare_list_of_chars(Chars,L) :-
   atom_chars('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',Chars),
   length(Chars,L).

% ---
% Random string of characters, creates an atom of string
% ---
 
create_random_string(S,Len,Want) :-
   length(Selected,Len),
   prepare_list_of_chars(Chars,L),
   succ(Lmax,L),
   maplist({Chars,Lmax}/[Char]>>(random_between(0,Lmax,R),nth0(R,Chars,Char)),Selected),
   stringy_chars(S,Selected,Want).

% ---
% Actual overwriting
% ---

overwrite(Lower,Upper,UpperPos,UpperPosStop,CutLeft,CutRight,Goal,Want) :- 
   UpperPos =< UpperPosStop,!, 
   call(Goal,Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want),
   debug(performance,"~q ~q ~d --> ~q",[Lower,Upper,UpperPos,Result]),
   UpperPosNext is UpperPos+1,
   overwrite(Lower,Upper,UpperPosNext,UpperPosStop,CutLeft,CutRight,Goal,Want).
 
overwrite(_,_,UpperPos,UpperPosStop,_,_,_,_) :- 
   UpperPos > UpperPosStop.

% ---
% Looping runs with a "Lower" of 0..60 chars and an "Upper" of 0..65 chars
% with varying start positions
% ---

loop(Goal) :-
   forall(between(0,60,LowerLen),
      forall(between(0,65,UpperLen),
         (
            create_random_string(Lower,LowerLen,string),
            create_random_string(Upper,UpperLen,string),
            UpperPosStart is -UpperLen-1,
            UpperPosStop  is LowerLen+1,
            overwrite(Lower,Upper,UpperPosStart,UpperPosStop,false,false,Goal,atom)
         ))).


test("Looping") :-
   format("Looping calls to string_overwrite using runs\n",[]),
   time(loop(overwrite_using_runs)),
   format("Looping calls to string_overwrite using chars\n",[]),
   time(loop(overwrite_using_chars)).

:- end_tests(string_overwrite_performance).

   



