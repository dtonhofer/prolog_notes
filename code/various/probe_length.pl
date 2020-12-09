% ===
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ===

% DOESN'T WORK FOR CYCLIC LISTS 

/*
% Non-integer length causes exceptions (even if conversion would make sense):

?- probe_length([a,b,c],3.0,_).
ERROR: Type error: `integer' expected, found `3.0' (a float)

% Negative lengths causes failure:

?- probe_length([],-1,_).
false.

% Unless you assert to be stricter (and less logical)

?- assertz(behaviour(probe_length,throw_on_negative_length)).
?- probe_length([],-1,_).
ERROR: Type error: `nonneg' expected, found `-1' (an integer)

% Otherwise probe_length/3 never fails. As it consider unbound
% variables as part of the data structure, it probes the current
% computational state (it's not always a logical predicate):

probe_length( []        , Length, What). % Length = 0, What = true: A proper list of length 0
probe_length( [a]       , Length, What). % Length = 1, What = true: A proper list of length 1
probe_length( [a,b]     , Length, What). % Length = 2, What = true: A proper list of length 2
probe_length( [a,b,c]   , Length, What). % Length = 3, What = true: A proper list of length 3
probe_length( _         , Length, What). % Length = 0, What = var: An unbound variable, maybe an open list of length 0
probe_length( [a|_]     , Length, What). % Length = 1, What = open: An open list (with a prefix) of length 1
probe_length( [a,b|_]   , Length, What). % Length = 2, What = open: An open list (with a prefix) of length 1
probe_length( [a,b,c|_] , Length, What). % Length = 3, What = open: An open list (with a prefix) of length 1
probe_length( [a|x]     , Length, What). % Length = 1, What = false: An non-list with a listlike prefix of length 1
probe_length( [a,b|x]   , Length, What). % Length = 2, What = false: An non-list with a listlike prefix of length 2
probe_length( [a,b,c|x] , Length, What). % Length = 3, What = false: An non-list with a listlike prefix of length 3
probe_length( foo       , Length, What). % Length = 0, What = false: An non-list with no listlike prefix

% Accept info about list:

?- probe_length( [1,2,3], 3, oclist).
true.

?- probe_length( [1,2,3], 6, oclist).
false.

?- probe_length( [1,2,3], 3, open).
false.

The "What" argument can be:

true    - accept a proper list (you can also use closed and proper)
false   - accept a not an unbound variable and not a list
var     - accept an unbound variable
open    - accept an open list, but not an unbound variable
oclist  - accept an open or closed list, but not an unbound variable
voclist - accept an open or closed list, or an unbound variable
canlist - accept something which can become a list but is not yet: an open list, or an unbound variable 
*/


% ---
% probe_length(@MaybeList, ?Length, ?What)
% ---

:- dynamic behaviour/2.

% behaviour(probe_length,throw_on_negative_length).

probe_length(MaybeList,Length,What) :-
   (nonvar(Length) -> verify_length(Length) ; true),
   (nonvar(What)   -> verify_what(What) ; true),
   probe_length_2(MaybeList,Length,What).
   
verify_length(Length) :-
   must_be(integer,Length),
   (Length<0 -> (behaviour(probe_length,throw_on_negative_length) -> must_be(nonneg,Length) ; fail) ; true).

verify_what(What) :-
   must_be(oneof([true,false,var,open,oclist,voclist,canlist,closed,proper]),What) .
   
% Notice that we dont fail or throw depending on what "MaybeList" turns out to be.
% If the caller wants to fail or throw, he/she can do so him/herself.  In particular,
% by instantiating 'What'

probe_length_2(MaybeList,Length,What) :-   
   var(MaybeList)  -> (Length=0,memberchk(What,[var,voclist,canlist])) ;
   MaybeList=[_|_] -> probe_listlike(MaybeList,1,Length,What) ;
   MaybeList==[]   -> (Length=0,memberchk(What,[true,oclist,voclist,closed,proper])) ;
   (What=false,Length=0). % hit a problem at once -> length is 0

probe_listlike([_|Xs],CurLength,FinLength,What) :-
   var(Xs)  -> (CurLength=FinLength,memberchk(What,[open,oclist,voclist,canlist])) ;
   Xs=[_|_] -> (NextLength is CurLength+1,probe_listlike(Xs,NextLength,FinLength,What)) ;
   Xs==[]   -> (CurLength=FinLength,memberchk(What,[true,oclist,voclist,closed,proper])) ;
   (What=false,CurLength=FinLength). % hit a problem here, at given length
    
% ---
% Time for testing. 
%
% Switch on debug printing with
% :- debug(test_probe_length).
% ---

:- begin_tests(probe_length).

test("Real-value length leads to exception",[error(type_error(_,_))]) :- 
   probe_length([],3.0,_).

% ---
% Cases table contains
% Example        : example to probe
% ShouldLength   : expected computed length (even for outcome "false", where it is the length of the prefix until we hit a problem)
% ShouldGenerate : expected computed "what": one of true, var, open, false
% OnTrue,OnClosed,OnProper,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist : the outcome success/failure if the corresponding
%   "what" is a given, for example "var" for "OnVar".

cases(
  [[ []        , 0   , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ [a]       , 1   , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ [a,b]     , 2   , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ [a,b,c]   , 3   , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ _         , 0   , var      ,  n  ,  n   ,  n  , yes ,  n  ,  n  ,  n   , yes   , yes  ],
   [ [a|_]     , 1   , open     ,  n  ,  n   ,  n  ,  n  , yes ,  n  , yes  , yes   , yes  ],
   [ [a,b|_]   , 2   , open     ,  n  ,  n   ,  n  ,  n  , yes ,  n  , yes  , yes   , yes  ],
   [ [a,b,c|_] , 3   , open     ,  n  ,  n   ,  n  ,  n  , yes ,  n  , yes  , yes   , yes  ],
   [ [a|x]     , 1   , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ],
   [ [a,b|x]   , 2   , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ],
   [ [a,b,c|x] , 3   , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ],
   [ foo       , 0   , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ]]).

test("generation") :-
   cases(Cases),
   maplist(test_generation,Cases). 

test_generation([Example,ShouldLength,ShouldGenerate|_]) :-
   test_generation_on_length_and_what(Example,ShouldLength,ShouldGenerate),
   test_generation_on_what_given_length(Example,ShouldLength,ShouldGenerate).

test_generation_on_length_and_what(Example,ShouldLength,ShouldGenerate) :-
   probe_length(Example,Length,What),
   assertion(Length == ShouldLength),
   assertion(What == ShouldGenerate).

test_generation_on_what_given_length(Example,ShouldLength,ShouldGenerate) :-
   probe_length(Example,ShouldLength,What),
   assertion(What == ShouldGenerate).

   
test("reject on bad length") :-
   cases(Cases),
   maplist(reject_on_bad_length,Cases).
   
reject_on_bad_length([Example,_ShouldLength,ShouldGenerate|_]) :-
   debug(test_probe_length,"reject_on_bad_length received ~q, ~q",[Example,ShouldGenerate]),   
   assertion(\+ probe_length(Example,1000,ShouldGenerate)).
   
   
   
test("acceptance on Length and What") :-
   cases(Cases),
   maplist(every_acceptance_on_length_and_what,Cases).

every_acceptance_on_length_and_what([Example,ShouldLength,_ShouldGenerate,OnTrue,OnClosed,OnProper,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist]) :-
   acceptance_on_length_and_what(Example, ShouldLength, true   , OnTrue),
   acceptance_on_length_and_what(Example, ShouldLength, closed , OnClosed),
   acceptance_on_length_and_what(Example, ShouldLength, proper , OnProper),
   acceptance_on_length_and_what(Example, ShouldLength, var    , OnVar),
   acceptance_on_length_and_what(Example, ShouldLength, open   , OnOpen),
   acceptance_on_length_and_what(Example, ShouldLength, false  , OnFalse),
   acceptance_on_length_and_what(Example, ShouldLength, oclist , OnOclist),
   acceptance_on_length_and_what(Example, ShouldLength, voclist, OnVoclist),
   acceptance_on_length_and_what(Example, ShouldLength, canlist, OnCanlist).

acceptance_on_length_and_what(Example,ShouldLength,What,YesOrNo) :-
   debug(test_probe_length,"acceptance_on_length_and_what received ~q, ~q, ~q, ~q",[Example,ShouldLength,What,YesOrNo]),
   must_be(oneof([yes,n]),YesOrNo),   
   (YesOrNo == yes) -> assertion(probe_length(Example,ShouldLength,What)) ; assertion(\+probe_length(Example,ShouldLength,What)).
   
   
test("acceptance on Length only") :-
   cases(Cases),
   maplist(acceptance_on_length_only,Cases).
   
acceptance_on_length_only([Example,ShouldLength,ShouldGenerate|_]) :-
   debug(test_probe_length,"acceptance_on_length_only received ~q, ~q, ~q",[Example,ShouldLength,ShouldGenerate]),
   probe_length(Example,ShouldLength,What), % call with ShouldLength and capture What
   assertion(What == ShouldGenerate).
   
   
test("acceptance on What only") :-
   cases(Cases),
   maplist(acceptance_on_what_only,Cases).

acceptance_on_what_only([Example,ShouldLength,ShouldGenerate|_]) :-
   debug(test_probe_length,"acceptance_on_what_only received ~q, ~q, ~q",[Example,ShouldLength,ShouldGenerate]),
   probe_length(Example,Length,ShouldGenerate), % call with ShouldGenerate and capture What
   assertion(Length == ShouldLength). 
   
 
:- end_tests(probe_length).
