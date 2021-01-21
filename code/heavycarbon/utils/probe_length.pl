% =============================================================================
% Probe the length of an unknown term (generally a proper list or an 
% open list, but not necessarily). Tells you what it found.
% This is a complement to length/2, which only handles proper lists.
%
% When determining/computing values, the predicate always succeeds.
% When verifying proposed values, the predicate may fail.
%
% Problems with this implementation:
%
% 1) It's much slower than length/2 because it doesn't call native code.
% 2) It doesn't handle cyclic structures. That should be added (but will
%    make it even slower or be problematic for multithreading applications
%    because you have to patch the structure as you examine it.)
%
% =============================================================================
% Running the tests: There should be a file "between_with_step.plt" nearby.
% Then, if the root directory for "code" is on the library path:
%
% ?- use_module(library('heavycarbon/utils/probe_length.pl')).
% ?- load_test_files([]).
% ?- run_tests.
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================

% -----------------------------------------------------------------------------
% Examples:
%
% ?- probe_length([],Length,What).
% Length = 0, What = closed.
%
% ?- probe_length([a,b,c],Length,What).
% Length = 3, What = closed.
%
% ?- probe_length(_, Length, What).
% Length = 0, What = var.
%
% ?- probe_length([a,b,c,d|foo], Length, What).
% Length = 4, What = nonlist.
%
% % Propose a length:
%
% ?- probe_length([a,b,c],3,What).
% What = closed.
%
% ?- probe_length([a,b,c],10,What).
% false.
%
% ?- probe_length([a,b,c],-1,What).
% false.
%
% % Bad length:
%
% ?- probe_length([a,b,c],-1,What,strict). % You can also use a list: '[strict]'
% ERROR: Type error: `nonneg' expected, found `-1' (an integer)
%
% ?- probe_length([a,b,c],3.0,What).
% ERROR: Type error: `integer' expected, found `3.0' (a float)
%
% ?- probe_length([a|x], Length, What). % Length = 1, What = nonlist: An non-list with a listlike prefix of length 1
% Length = 1, What = nonlist.
%
% ?- probe_length(_, Length, What).
% Length = 0, What = var.
%
% % Propose a class for MaybeList:
%
% ?- probe_length([a,b,c,d|foo], Length, nonlist).
% Length = 4.
%
% ?- probe_length([a,b], Length, [closed,var]).
% Length = 2.
%
% ?- probe_length(_, Length, [closed,var]).
% Length = 0.
%
% ?- probe_length([a,b,c|_], Length, [closed,var]).
% false.
% -----------------------------------------------------------------------------

:- module(probe_length,
          [
          probe_length/3  % probe_length(@MaybeList,?Length,?What)
         ,probe_length/4  % probe_length(@MaybeList, ?Length, ?What, @Options)
          ]).

% ---
% Switch on runtime printing with
% :- debug(probe_length).
% ---

% ===
% probe_length(@MaybeList, ?Length, ?What)
%
% Identical to
%
% probe_length(@MaybeList, ?Length, ?What, [])
%
% i.e. the predicate behaves leniently in that a proposed Length that is < 0
% leads to failure instead of an ISO type exception.
% ===

probe_length(MaybeList,Length,What) :- probe_length(MaybeList,Length,What,[]).

% ===
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
% ===

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

