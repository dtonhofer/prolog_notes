% ============================================================================
% 2020-12-23
% https://github.com/dtonhofer/prolog_notes
%
% Testing call_with_depth_limit/3
% https://eu.swi-prolog.org/pldoc/doc_for?object=call_with_depth_limit/3
%
% ----------------------------------------------------------------------------
% ronerycoder@gluino.name (me) says this is licensed under
% https://opensource.org/licenses/0BSD
% ============================================================================


% ===
% Plunit Test Code. Run it with
%
% ?- run_tests.
%
% Or grab the code in the test bodies to run on the Prolog toplevel
% ===

:- begin_tests(depth_test).

test("try various limited depths (some insufficient), but don't limit overall",
   true([ResultList,OverallResult]==[Expected,none])) :-
   depth_test(
      [q{depth_limit:5, target_depth:3},
       q{depth_limit:4, target_depth:3},
       q{depth_limit:3, target_depth:3},
       q{depth_limit:2, target_depth:3},
       q{depth_limit:1, target_depth:3}],
       off,ResultList,OverallResult),
   Expected = [
      r{depth_limit:5, result:4, target_depth:3},
      r{depth_limit:4, result:4, target_depth:3},
      r{depth_limit:3, result:depth_limit_exceeded, target_depth:3},
      r{depth_limit:2, result:depth_limit_exceeded, target_depth:3},
      r{depth_limit:1, result:depth_limit_exceeded, target_depth:3}].

test("try various unlimited depths, but limit overall at depth 11 (this is the max depth needed)",
   true([ResultList,OverallResult]==[Expected,11])) :-
   depth_test(
      [q{depth_limit:off, target_depth:4},
       q{depth_limit:off, target_depth:4},
       q{depth_limit:off, target_depth:4}],
       11,ResultList,OverallResult),
   Expected = [
      r{depth_limit:off, result:off, target_depth:4},
      r{depth_limit:off, result:off, target_depth:4},
      r{depth_limit:off, result:off, target_depth:4}].

test("try various unlimited depths, but limit overall at 10 (this is unsufficient)",
   true(OverallResult==depth_limit_exceeded)) :-
   depth_test(
      [q{depth_limit:off, target_depth:4},
       q{depth_limit:off, target_depth:4},
       q{depth_limit:off, target_depth:4}],10,_,OverallResult).

test("try case of go_deeper/3 depth much larger than overall depth, which fails",
   true(OverallResult==depth_limit_exceeded)) :-
   depth_test([q{depth_limit:off, target_depth:100}],10,_,OverallResult).

test("try case of go_deeper/3 depth much larger than overall depth, but limited by a sufficient limit, which succeeds",
   true([ResultList,OverallResult]==[Expected,6])) :-
   depth_test([q{depth_limit:200, target_depth:100}],10,ResultList,OverallResult),
   Expected = [r{depth_limit:200, result:101, target_depth:100}].

:- end_tests(depth_test).

% ===
% Go through a list of "TargetDepth-DepthLimit" pairs. Each pair causes a
% recursive descent by "go_deep/3" of depth "TargetDepth" (but the actually
% reached maximum depth is "TargetDepth+1". "DepthLimit" limits this descent.
% DepthLimit can be 'off', which means "no limit" but otherwise must be >= 1,
% because 0 means "everything fails".
%
% Additionally, possibly set an overall depth limit on this whole process
% through OverallDepthLimit. OverallDepthLimit can be 'off', which means
% "no limit" but otherwise must be >= 1, because 0 means "everything fails".
% ===

depth_test(DepthList,OverallDepthLimit,ResultList,OverallResult) :-
   if_then_else(
      (OverallDepthLimit==off),
      (adjusted_list_walker(DepthList,ResultList),OverallResult=none),
      (depth_limited_list_walker(DepthList,OverallDepthLimit,ResultList,OverallResult))).

adjusted_list_walker(DepthList,ResultList) :-
   ListPosition = 0,
   ComputedDepth = 1,
   list_walker(DepthList,ListPosition,ComputedDepth,ResultList).

depth_limited_list_walker(DepthList,OverallDepthLimit,ResultList,OverallResult) :-
   must_be(positive_integer,OverallDepthLimit),
   ListPosition = 0,
   ComputedDepth = 1,
   call_with_depth_limit(
      list_walker(DepthList,ListPosition,ComputedDepth,ResultList),
      OverallDepthLimit,
      OverallResult),
   if_then_else(
      (OverallResult == depth_limit_exceeded),
      (format("Overall depth limit of max ~q *** exceeded ***\n",[OverallDepthLimit])), % could also fail this call as there is no ResultList
      (format("Overall depth limit of max ~q maintained with reached = ~q\n",[OverallDepthLimit,OverallResult]))).

% ===
% "DepthList" is a list of "TargetDepth-DepthLimit" pairs.
% ListPosition is the current  position in the DepthList, starting at 0
% ActualDepth counts the actual depth of execution
%
% list_walker(+DepthList,+ListPosition,+ComputedDepth)
% ===

list_walker([],_,ComputedDepth,[]) :-
   format("End of List, ComputedDepth = ~q: \n",[ComputedDepth]).

list_walker([q{target_depth:TargetDepth,depth_limit:DepthLimit}|MorePairs],ListPosition,ComputedDepth,[r{target_depth:TargetDepth,depth_limit:DepthLimit,result:Result}|Results]) :-
   format("ListPosition = ~q, ComputedDepth = ~q: Going deep with TargetDepth = ~q and DepthLimit = ~q\n",[ListPosition,ComputedDepth,TargetDepth,DepthLimit]),
   succ(ComputedDepth,ComputedDepth2), % must be +1 than current computed depth for next calls
   if_then_else(
      (DepthLimit==off),
      (adjusted_go_deep(TargetDepth,ComputedDepth2),Result=off),
      (depth_limited_go_deep(TargetDepth,ComputedDepth2,DepthLimit,Result))),
   succ(ListPosition,ListPosition2),
   list_walker(MorePairs,ListPosition2,ComputedDepth2,Results).

% ===
% Call the predicate go_deep/3 with a depth limit. Result takes up the
% result of the call.
% ===

depth_limited_go_deep(TargetDepth,ComputedDepth,DepthLimit,Result) :-
   must_be(positive_integer,DepthLimit),
   must_be(positive_integer,TargetDepth),
   succ(ComputedDepth,ComputedDepth2),
   call_with_depth_limit(
      go_deep(TargetDepth,1,ComputedDepth2),
      DepthLimit,
      Result),
   if_then_else(
      (Result == depth_limit_exceeded),
      (format("go_deep/3 depth limit of max ~q *** exceeded ***\n",[DepthLimit])), % don't fail this call as we will move to the next list item
      (format("go_deep/3 depth limit of max ~q maintained with reached = ~q\n",[DepthLimit,Result]))).

adjusted_go_deep(TargetDepth,ComputedDepth) :-
   must_be(positive_integer,TargetDepth),
   succ(ComputedDepth,ComputedDepth2),
   go_deep(TargetDepth,1,ComputedDepth2).

% ===
% Try to perform recursive calls as deep as possible.
%
% Called with:
%
% TargetDepth   : the depth to reach, >= 1
% CurrentDepth  : the depth of the called stack frame, starts at 1.
% ComputedDepth : the depth computed by us; increases by 1 on each recursive call
%
% go_deep(+TargetDepth,+CurrentDepth,+ComputedDepth)
% ===

% Note that any call inside go_deep/3 base case demands 1 more "depth unit". So:
% "maximum number of go_deep/3 stack frames" = "target depth" = "depth limit" - 1
% (with 1 meaning only a call to the base case is allowed)

go_deep(TargetDepth,CurrentDepth,ComputedDepth) :-
   TargetDepth == CurrentDepth,
   !,
   format("  Base case: CurrentDepth = ~q, TargetDepth = ~q, ActualDepth = ~q\n",[CurrentDepth,TargetDepth,ComputedDepth]).

go_deep(TargetDepth,CurrentDepth,ComputedDepth) :-
   TargetDepth > CurrentDepth, % This will already go 1 too deep if we are at "maximum depth"; no matter
   !,
   format("  CurrentDepth = ~q, TargetDepth = ~q, ActualDepth = ~q\n",[CurrentDepth,TargetDepth,ComputedDepth]),
   succ(CurrentDepth,CurrentDepth2),
   succ(ComputedDepth,ComputedDepth2),
   % recursive call which might even be subject to tail-call-optimization
   go_deep(TargetDepth,CurrentDepth2,ComputedDepth2).

% ===
% A meta-predicate wrapping the "->/2 + ;" construct to make source code
% readable. Pass three goals.
% ===

if_then_else(Condition,Then,Else) :-
   call(Condition) -> call(Then) ; call(Else).

