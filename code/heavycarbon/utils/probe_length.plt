% ===
% ronerycoder@gluino.name (me) says this is licensed under
% https://opensource.org/licenses/0BSD
% ===

:- use_module(library('heavycarbon/utils/probe_length.pl')).

% Switch on debug printing with
% :- debug(test_probe_length).

:- begin_tests(probe_length).

test("Real-value length leads to exception",[error(type_error(_,_))]) :-
   probe_length([],3.0,_).

test("Bad What leads to exception",[error(type_error(_,_))]) :-
   probe_length([],0,foo).

test("'open_nonvar,closed' accepts any open/closed list but not var") :-
   assertion(probe_length([],0,[open_nonvar,closed])),
   assertion(probe_length([1,2],2,[open_nonvar,closed])),
   assertion(probe_length([1,2|_],2,[open_nonvar,closed])),
   assertion(\+probe_length(_,0,[open_nonvar,closed])).

test("'open,closed' accepts any open/closed lists as well as var") :-
   assertion(probe_length([],0,[open,closed])),
   assertion(probe_length([1,2],2,[open,closed])),
   assertion(probe_length([1,2|_],2,[open,closed])),
   assertion(probe_length(_,0,[open,closed])).

test("'can_be_list' accepts any open/closed lists as well as var") :-
   assertion(probe_length([],0,can_be_list)),
   assertion(probe_length([1,2],2,can_be_list)),
   assertion(probe_length([1,2|_],2,can_be_list)),
   assertion(probe_length(_,0,can_be_list)).

test("'nonlist' accepts no lists of any kind") :-
   assertion(\+probe_length([],0,nonlist)),
   assertion(\+probe_length([1,2],2,nonlist)),
   assertion(\+probe_length([1,2|_],2,nonlist)),
   assertion(\+probe_length(_,0,nonlist)),
   assertion(probe_length(foo,0,nonlist)).

test("[nonlist,open,closed] accepts anything") :-
   assertion(probe_length([],0,[nonlist,open,closed])),
   assertion(probe_length([1,2],2,[nonlist,open,closed])),
   assertion(probe_length([1,2|_],2,[nonlist,open,closed])),
   assertion(probe_length(_,0,[nonlist,open,closed])),
   assertion(probe_length(foo,0,[nonlist,open,closed])).

test("[] accepts nothing (NB there is no 'nothing' atom)") :-
   assertion(\+probe_length([],0,[])),
   assertion(\+probe_length([1,2],2,[])),
   assertion(\+probe_length([1,2|_],2,[])),
   assertion(\+probe_length(_,0,[])),
   assertion(\+probe_length(foo,0,[])).

% ---
% Cases table contains
% Example        : example to probe
% ShouldLength   : expected computed "Length"
% ShouldGenerate : expected computed "What"
% OnX            : when "What" is not unbound but one of the allowed keywords (i.e. we don't
%                  test the list) then the corresponding column gives the expected outcome
%                  for the "Example":
%                  OnProper,OnClosed,OnOpenNonvar,OnOpen,OnPartial,OnVar,OnNonlist,OnCanBeList

                                   %             open_nonvar
                                   %       closed |        partial     nonlist
                                   % proper |     |   open    |    var    |  can_be_list
cases(                             %  |     |     |     |     |     |     |     |
  [[ []        , 0   , closed      , yes , yes ,  n  ,  n  ,  n  ,  n  ,  n  , yes ],
   [ [a]       , 1   , closed      , yes , yes ,  n  ,  n  ,  n  ,  n  ,  n  , yes ],
   [ [a,b]     , 2   , closed      , yes , yes ,  n  ,  n  ,  n  ,  n  ,  n  , yes ],
   [ [a,b,c]   , 3   , closed      , yes , yes ,  n  ,  n  ,  n  ,  n  ,  n  , yes ],
   [ _         , 0   , var         ,  n  ,  n  ,  n  , yes , yes , yes ,  n  , yes ],
   [ [a|_]     , 1   , open_nonvar ,  n  ,  n  , yes , yes , yes ,  n  ,  n  , yes ],
   [ [a,b|_]   , 2   , open_nonvar ,  n  ,  n  , yes , yes , yes ,  n  ,  n  , yes ],
   [ [a,b,c|_] , 3   , open_nonvar ,  n  ,  n  , yes , yes , yes ,  n  ,  n  , yes ],
   [ [a|x]     , 1   , nonlist     ,  n  ,  n  ,  n  ,  n  ,  n  ,  n  , yes ,  n  ],
   [ [a,b|x]   , 2   , nonlist     ,  n  ,  n  ,  n  ,  n  ,  n  ,  n  , yes ,  n  ],
   [ [a,b,c|x] , 3   , nonlist     ,  n  ,  n  ,  n  ,  n  ,  n  ,  n  , yes ,  n  ],
   [ foo       , 0   , nonlist     ,  n  ,  n  ,  n  ,  n  ,  n  ,  n  , yes ,  n  ]]).

test("generation") :-
   cases(Cases),
   maplist(test_generation,Cases).

test_generation([Example,ShouldLength,ShouldGenerate|_]) :-
   test_generation_on_length_and_what(Example,ShouldLength,ShouldGenerate),
   test_generation_on_what_given_length(Example,ShouldLength,ShouldGenerate).

test_generation_on_length_and_what(Example,ShouldLength,ShouldGenerate) :-
   probe_length(Example,Length,What),
   assertion(Length == ShouldLength), % assertion/1 lets us test multiple facts with good syntax
   assertion(What == ShouldGenerate). % assertion/1 lets us test multiple facts with good syntax

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

every_acceptance_on_length_and_what([Example,ShouldLength,_,OnProper,OnClosed,OnOpenNonvar,OnOpen,OnPartial,OnVar,OnNonlist,OnCanBeList]) :-
   acceptance_on_length_and_what(Example, ShouldLength, proper      , OnProper),
   acceptance_on_length_and_what(Example, ShouldLength, closed      , OnClosed),
   acceptance_on_length_and_what(Example, ShouldLength, open_nonvar , OnOpenNonvar),
   acceptance_on_length_and_what(Example, ShouldLength, open        , OnOpen),
   acceptance_on_length_and_what(Example, ShouldLength, partial     , OnPartial),
   acceptance_on_length_and_what(Example, ShouldLength, var         , OnVar),
   acceptance_on_length_and_what(Example, ShouldLength, nonlist     , OnNonlist),
   acceptance_on_length_and_what(Example, ShouldLength, can_be_list , OnCanBeList).

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


