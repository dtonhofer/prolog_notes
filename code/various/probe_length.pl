% ===
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ===

% Switch on debug printing with
% :- debug(test_probe_length).
% Switch on runtime printing with
% :- debug(probe_length).

/******************************************************************************
 * Probe the length of an unknown term (generally a list, but not necessarily)
 * 
 * This is a complement to length/2, which only handles proper lists.
 *
 * Problems with this implementation:
 *
 * 1) It's much slower than length/2 because it doesn't call native code.
 * 2) It doesn't handle cyclic structures. That should be added (but will
 *    make it slower or be problematic for multithreading applications
 *    because you have to patch the structure as you examine it.
 *
 * When determining/computing values, the predicate always succeeds.
 * When verifying proposed values, the predicate may fail.
 ******************************************************************************
 *
 * Examples:
 *
 * ?- probe_length([],Length,What). 
 * Length = 0, What = closed.
 *
 * ?- probe_length([a,b,c],Length,What).
 * Length = 3, What = closed. 
 *
 * ?- probe_length(_, Length, What).
 * Length = 0, What = var.
 *
 * ?- probe_length([a,b,c,d|foo], Length, What).
 * Length = 4, What = nonlist.
 *
 * % Propose a length:
 *
 * ?- probe_length([a,b,c],3,What).
 * What = closed.
 *
 * ?- probe_length([a,b,c],10,What). 
 * false.
 *
 * ?- probe_length([a,b,c],-1,What). 
 * false.
 *
 * % Bad length:
 *
 * ?- probe_length([a,b,c],-1,What,strict). % You can also use a list: '[strict]'
 * ERROR: Type error: `nonneg' expected, found `-1' (an integer)
 *
 * ?- probe_length([a,b,c],3.0,What). 
 * ERROR: Type error: `integer' expected, found `3.0' (a float)
 * 
 * ?- probe_length([a|x], Length, What). % Length = 1, What = nonlist: An non-list with a listlike prefix of length 1
 * Length = 1, What = nonlist.
 *
 * ?- probe_length(_, Length, What).
 * Length = 0, What = var.
 *
 * % Propose a class for MaybeList:
 *
 * ?- probe_length([a,b,c,d|foo], Length, nonlist).
 * Length = 4.
 * 
 * ?- probe_length([a,b], Length, [closed,var]).
 * Length = 2.
 *
 ?- probe_length(_, Length, [closed,var]).
 * Length = 0.
 * 
 * ?- probe_length([a,b,c|_], Length, [closed,var]).
 * false.
 */

% ---
% probe_length(@MaybeList, ?Length, ?What)
%
% Identical to 
%
% probe_length(@MaybeList, ?Length, ?What, [])
%
% i.e. the predicate behaves leniently in that a proposed Length that is < 0
% leads to failure instead of an ISO type exception.
% ---

probe_length(MaybeList,Length,What) :-
   probe_length(MaybeList,Length,What,[]).
 
% ---
% probe_length(@MaybeList, ?Length, ?What, @Options)
%
% MaybeList: The term to be examined. Maybe a list.
% Length:    An integer >= 0. The length to be established if
%            a var at call time, or proposed if bound to an integer.
%            Predicate throws if this is nonvar(Lenght),\+integer(Length)
%            Predicate throws if Options is 'strict' or is a list containing 'strict'
%            and Length < 0. Otherwise, if Length < 0 it fails at once.
% What:      Either a var if the class of MaybeList is to be established
%            or an atom or a list of atom if the class of MaybeList is being proposed.
%            The allowed atoms separate the set of possible MaybeList into distinct classes.
%            Allowed are:
%             proper,closed - Class of the proper/closed list
%             open_nonvar   - Class of the open/partial list which however is not a var
%             var           - Class of the unbound variable
%             nonlist       - Class anything else
%            Also allowed is the union class of open_nonvar and var:
%             open,partial  - Class of the open/partial list (which *can* be a var)
%            Also allowed is the union class of open and proper (those terms that "can be list")
%             can_be_list   - Class of open list union proper list (which includes var)
%            What can alos be a list of the above. In that case, it is understood that
%            the caller proposes that MaybeList is a member of the union of the classes
%            corresponding to the listed atoms. An empty list as proposal will always fail.
%            Predicate throws if an atom is encountered that is not one of the above.
% Options:   If this is the atom 'strict' or a list containing the atom 'strict', 
%            then the predicate throws is Length is proposed but < 0. If Options is
%            anything else, this is interpreted as 'lenient', and the predicate fails
%            if Length is proposed but < 0.
% ---

probe_length(MaybeList,Length,What,Options) :-
   verify_what(What,IsClosedQueried,IsVarQueried,IsOpenNonvarQueried,IsNonlistQueried),
   verify_length(Length,Options),
   debug(probe_length,"'What' verified: IsClosed=~q,IsVar=~q,IsOpenNonvar=~q,IsNonlist=~q",
        [IsClosedQueried,IsVarQueried,IsOpenNonvarQueried,IsNonlistQueried]),
   probe_length_2(MaybeList,Length,IsClosedProbed,IsVarProbed,IsOpenNonvarProbed,IsNonlistProbed),
   debug(probe_length,"Length probed: Length=~q, IsClosed=~q,IsVar=~q,IsOpenNonvar=~q,IsNonlist=~q", 
        [Length,IsClosedProbed,IsVarProbed,IsOpenNonvarProbed,IsNonlistProbed]),
   (var(What) -> 
      (build_what_from_probed(IsClosedProbed,IsVarProbed,IsOpenNonvarProbed,IsNonlistProbed,What),
       assertion(must_be(oneof([closed,open_nonvar,open,var,nonlist]),What)))
      ;
      probed_must_match_queried(IsClosedProbed,IsVarProbed,IsOpenNonvarProbed,IsNonlistProbed,
                                IsClosedQueried,IsVarQueried,IsOpenNonvarQueried,IsNonlistQueried)).
   
verify_what(What,IsClosed,IsVar,IsOpenNonvar,IsNonlist) :-
   (var(What)
    -> true  % 'What' will be filled in
    ; (atom(What) % 'What' is a proposal to be accepted/rejected
       -> verify_and_explode_what_atom(What,IsClosed,IsVar,IsOpenNonvar,IsNonlist)
       ; is_list(What) % 'What' is a list of alternative proposals to be accepted/rejected
       -> verify_and_explode_what_list(What,IsClosed,IsVar,IsOpenNonvar,IsNonlist)
       ; type_error("list of atoms or single atom (one of proper,closed,open_nonvar,open,partial,var,nonlist,can_be_list)",What))).

verify_and_explode_what_atom(What,IsClosed,IsVar,IsOpenNonvar,IsNonlist) :-
   ((What==proper;What==closed) -> [IsClosed,IsVar,IsOpenNonvar,IsNonlist] = [true , false, false, false]);
   ((What==open_nonvar)         -> [IsClosed,IsVar,IsOpenNonvar,IsNonlist] = [false, false, true , false]);
   ((What==open;What==partial)  -> [IsClosed,IsVar,IsOpenNonvar,IsNonlist] = [false, true,  true , false]);
   ((What==var)                 -> [IsClosed,IsVar,IsOpenNonvar,IsNonlist] = [false, true,  false, false]);
   ((What==nonlist)             -> [IsClosed,IsVar,IsOpenNonvar,IsNonlist] = [false ,false, false, true ]);
   ((What==can_be_list)         -> [IsClosed,IsVar,IsOpenNonvar,IsNonlist] = [true  ,true , true , false]);
   must_be(oneof([proper,closed,open_nonvar,open,partial,var,nonlist]),What).

verify_and_explode_what_list(WhatList,IsClosedOut,IsVarOut,IsOpenNonvarOut,IsNonlistOut) :-
   verify_and_explode_what_list_2(WhatList,false,false,false,false,IsClosedOut,IsVarOut,IsOpenNonvarOut,IsNonlistOut).

verify_and_explode_what_list_2([What|Whatses],IsClosed,IsVar,IsOpenNonvar,IsNonlist,IsClosedOut,IsVarOut,IsOpenNonvarOut,IsNonlistOut) :-
   verify_and_explode_what_atom(What,IsClosed2,IsVar2,IsOpenNonvar2,IsNonlist2),
   or(IsClosed,     IsClosed2,     IsClosed3),
   or(IsVar,        IsVar2,        IsVar3),
   or(IsOpenNonvar, IsOpenNonvar2, IsOpenNonvar3),
   or(IsNonlist,    IsNonlist2,    IsNonlist3),
   verify_and_explode_what_list_2(Whatses,IsClosed3,IsVar3,IsOpenNonvar3,IsNonlist3,IsClosedOut,IsVarOut,IsOpenNonvarOut,IsNonlistOut).

verify_and_explode_what_list_2([],IsClosed,IsVar,IsOpenNonvar,IsNonlist,IsClosed,IsVar,IsOpenNonvar,IsNonlist).
   
or(false,false,false) :- !.
or(_    ,_    ,true).

verify_length(Length,Options) :-
   (var(Length) 
    -> true % 'Length' will be filled in
    ; (must_be(integer,Length), % Only accepts integers as length
       (Length<0 -> % If length < 0, if Options contains "strict", then throw, otherwise fail 
         (((is_list(Options),memberchk(strict,Options));Options==strict)
          -> must_be(nonneg,Length) 
          ;  fail)
       ; true))).

% build_what_from_probed(++IsClosedProbed,++IsVarProbed,++IsOpenNonvarProbed,++IsNonlistProbed,?What)
   
build_what_from_probed(true,false,false,false,closed)      :- !.
build_what_from_probed(false,true,false,false,var)         :- !.
build_what_from_probed(false,false,true,false,open_nonvar) :- !.
build_what_from_probed(false,false,false,true,nonlist)     :- !.

% probed_must_match_queried(++IsClosedProbed,++IsVarProbed,++IsOpenNonvarProbed,++IsNonlistProbed,
%                           ++IsClosedQueried,++IsVarQueried,++IsOpenNonvarQueried,++IsNonlistQueried)
% Note that exactly one of the "probed" booleans is true, expressing a single class of a mutually disjoint
% set of classes, but multiple ones of the "queried" boolean may be true, as the latter express a union of 
% classes. We basically compute "probed class in set of queried classes"

probed_must_match_queried(IsClosedProbed,IsVarProbed,IsOpenNonvarProbed,IsNonlistProbed,
                          IsClosedQueried,IsVarQueried,IsOpenNonvarQueried,IsNonlistQueried) :-
   (imply(IsClosedProbed,IsClosedQueried),
    imply(IsVarProbed,IsVarQueried),
    imply(IsOpenNonvarProbed,IsOpenNonvarQueried),
    imply(IsNonlistProbed,IsNonlistQueried)).

% Amazingly for a logic programming language, there are scant manipulations of booleans in Prolog

imply(true,false)  :- !,fail.
imply(false,true)  :- !.
imply(false,false) :- !.
imply(true,true)   :- !.

% probe_length(@MaybeList, ?Length, ?IsClosed, ?IsVar, ?IsOpenNonvar, ?IsNonlist)

probe_length_2(MaybeList,Length,IsClosed,IsVar,IsOpenNonvar,IsNonlist) :-   
   var(MaybeList)  -> (Length=0,IsClosed=false,IsVar=true,IsOpenNonvar=false,IsNonlist=false) 
   ;
   MaybeList=[_|_] -> probe_listlike(MaybeList,1,Length,IsClosed,IsVar,IsOpenNonvar,IsNonlist)
   ;
   MaybeList==[]   -> (Length=0,IsClosed=true,IsVar=false,IsOpenNonvar=false,IsNonlist=false)
   ;
   (Length=0,IsClosed=false,IsVar=false,IsOpenNonvar=false,IsNonlist=true). % length of a nonlist is 0
   
probe_listlike([_|Xs],CurLength,FinalLength,IsClosed,IsVar,IsOpenNonvar,IsNonlist) :-
   var(Xs)  -> (CurLength=FinalLength,IsClosed=false,IsVar=false,IsOpenNonvar=true,IsNonlist=false) 
   ;
   Xs=[_|_] -> (NextLength is CurLength+1,probe_listlike(Xs,NextLength,FinalLength,IsClosed,IsVar,IsOpenNonvar,IsNonlist))
   ;
   Xs==[]   -> (CurLength=FinalLength,IsClosed=true,IsVar=false,IsOpenNonvar=false,IsNonlist=false) 
   ;
   (CurLength=FinalLength,IsClosed=false,IsVar=false,IsOpenNonvar=false,IsNonlist=true). % length so far is retained
    
% ===
% Tests
% ===

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

