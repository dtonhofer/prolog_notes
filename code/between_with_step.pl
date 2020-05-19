% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-05-XX
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% This is free and unencumbered software released into the public domain.
%
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
%
% For more information, please refer to <http://unlicense.org/>
% ============================================================================
% between(+Start,+End,+Step,?Value)
% between(+Start,+End,+Step,?Value,?OptionList)
%
% Start: Start of sequence, must denote an integer (i.e. be an integer or a
%        variable that is bound to an integer).
%        § "Start" will always be the first element in the sequence unless the
%          sequence is actually empty.
%
% End:   End of sequence, must denote an integer. Alternatively, can be one of:
%
%        inf, infinite, +(inf), +(infinite)   : for +infinity (recommended: -inf )
%        minf, minfinite, -(inf), -(infinite) : for -infinity (recommended: +inf )
%
%        § Unless the sequence is empty or "End" denotes one of the infinities,
%          §§ "End" will be the last item in the sequence if there is a "K", K>=0
%             such that Start + K*Step == End.
%          §§ Otherwise, "End" will not appear in the sequence and the last
%             item will be some "Last", Last == Start + K*Step, with K>=0 and
%             Last+Step > End if Step>0, Last+Step < End if Step<0
%          ...in other words, sequence enumeration fails as soon the next
%          sequence element would overshoot "End".
%
% Step:  Integer step of sequence, must denote an integer.
%        § Must have the correct sign to reach End from Start.
%        § It is allowed for Step == 0, as long as Start == End.
%
% Value: Either
%        § a fresh variable (freshvar) that will be bound to the next element
%        § or denotes an integer which for which the predicate verifies that
%          it belongs to the sequence.
%
% OptionList:
%        § May be a freshvar or the empty list for "default".
%          If a freshvar, it is bound to the empty list.
%        § If atom "throw_if_empty" is a member of the list, empty sequences
%          lead to an exception.
%          In default mode, the empty sequence is accepted and leads to
%          immediate predicate failure.
% ============================================================================
% Run unit tests:
% 
% ?- rt(_).
% PL-Unit: between_with_step ........................................ done
% All 80 tests passed
% ============================================================================
% Notes
%
% - The core of the predicate is straightforward but the preparation is 
%   laborious.
% - I try to have the arguments of helper predicates arranged so that the 
%   compiler can quickly decide which clause to use. This means tagging the
%   integer arguments (with int/1, pos/1, neg/1) and moving those to the
%   left (SWI Prolog has a description of its indexing approach here:
%   https://www.swi-prolog.org/pldoc/man?section=jitindex )
%   Still keeping the now possibly non-situation-improving cuts in (in fact
%   they may be useful to naive interpreters?)
% - I'm completely relying on integer arithmetic to not wrap around or go
%   out of bounds. We have 64-bit arithmetic augmented by GNU Multiprecision
%   library - should be generally a good assumption.
% - Kudos to jburse for noticing that I hadn't checked the bounds in VERIFY
%   mode. Bugged! But thanks to the unit test, fixed with confidence.
% ============================================================================

% ===
% Entry point
% ===

between(Start,End,Step,Value) :-
   between(Start,End,Step,Value,[]).

between(Start,End,Step,Value,Options) :-
   (var(Options) -> Options = []; true),                     % Binds Options to empty list if needed
   must_be(integer,Step),                                    % Throws unless Step is an integer (sadly no indication in throw WHICH arg is bad)
   must_be(integer,Start),                                   % Throws unless Start is an integer (sadly no indication in throw WHICH arg is bad)
   cleanup_and_tag_end(End,TaggedEnd),                       % Throws unless End is an integer or one of the "infinites"; also tag value with inf/0, minf/0 or int/1
   tag_step(Step,TaggedStep),                                % Tag step with pos/1, neg/1, zero/0
   check_step(TaggedEnd,TaggedStep,Start,Options,_SeqType),  % Empty sequences yield fail; or exception if "throw_if_empty"
   (nonvar(Value)
    ->                                                       % VERIFY mode
    (must_be(integer,Value),                                 % Throws unless Value an integer (sadly no indication in throw WHICH arg is bad)
     bounded_by(TaggedEnd,Start,Value),                      % Make sure that Value is inside the bounds
     0 =:= ((Value-Start) mod Step))                         % Works correctly with negative Step. Excellent!
    ;                                                        % SEQUENCE ENUMERATION mode
    (between_enum(TaggedStep,TaggedEnd,Start,Value))).       % Help the compiler out by moving the TaggedStep to first position

% ===
% Helper predicates
% ===

% ---
% Better throwing of Domain Error!
% ---

exception_code(e01,changing_from_start_to_end_but_step_is_zero).
exception_code(e02,going_to_plus_infinity_but_step_is_zero).
exception_code(e03,going_to_plus_infinity_but_step_is_negative).
exception_code(e04,going_to_minus_infinity_but_step_is_zero).
exception_code(e05,going_to_minus_infinity_but_step_is_positive).
exception_code(e06,decreasing_from_start_to_end_but_step_is_positive).
exception_code(e07,increasing_from_start_to_end_but_step_is_negative).

% PROBLEM: domain_error/2 , the predicate from library(error) to throw and
% ISO Standard "domain error" expects "domain_error(+Type, +Term)"
% As in: "domain_error(integer, 12.4)" 
% ...and does not leave any way to add more information because the ISO
% standard exception does not have any place for that! 
% We have three equally bad ways out: 
% 1) throw our own Exception
% 2) leave the caller uninformed
% 3) misuse the parameters of domain_error/2
% We go for 3)

throw_or_fail_depending_on_option(ExCode,Start,End,Step,Os) :-
   memberchk(throw_if_empty,Os)
   ->
   (exception_code(ExCode,ExLongDesc),
    domain_error(ExLongDesc, info{start:Start, end:End, step:Step})).

throw_unconditionally(ExCode,Start,End,Step) :-
   (exception_code(ExCode,ExLongDesc),
    domain_error(ExLongDesc, info{start:Start, end:End, step:Step})).
    
% ---
% Cleaning up the value for "end".
% Default behaviour: render "end" matchable by tagging it with int/1; throw unless "end" is an integer.
% The red cuts are there to not backtrack into to default.
% ---

cleanup_and_tag_end(inf         ,inf)    :- !.
cleanup_and_tag_end(infinite    ,inf)    :- !.
cleanup_and_tag_end(+(inf)      ,inf)    :- !.
cleanup_and_tag_end(+(infinite) ,inf)    :- !.
cleanup_and_tag_end(minf        ,minf)   :- !.
cleanup_and_tag_end(-(inf)      ,minf)   :- !.
cleanup_and_tag_end(-(infinite) ,minf)   :- !.
cleanup_and_tag_end(X           ,int(X)) :- must_be(integer,X).

% ---
% Cleaning up the value for "step", which is known to be an integer.
% Default behaviour: render "step" matchable by tagging it with pos/1, neg/1, zero/1 and throw unless "step" is an integer
% ---

tag_step(0 ,zero)    :- !.
tag_step(X ,pos(X))  :- X>0,!.
tag_step(X ,neg(X))  :- X<0,!.

% ---
% Checking the value of "step". Fail means the main predicate will fail.
%
% check_step(+TaggedEnd,+TaggedStep,+Start,+Options,?SeqType)
%
% Note the particular case Start=End with Step=0.
% Employ red cuts to make this a big "if then ... if then ... else."
% although the clauses are already arranged for minimal backtracking.
% The last argument indicates whether the sequence has been found to be empty;
% it just exists for documentation as it is used nowhere.
% ---

check_step(int(X)   ,zero      ,X     ,_  ,empty)    :- !. % rare & borderline: going from X to X by step 0
check_step(int(End) ,zero      ,Start ,_  ,_)        :- !, Start \== End, throw_unconditionally(e01,Start,End,0).
check_step(inf      ,pos(_)    ,_     ,_  ,infinite) :- !.
check_step(inf      ,zero      ,Start ,_  ,_)        :- !, throw_unconditionally(e02,Start,inf,0).
check_step(inf      ,neg(Step) ,Start ,Os ,empty)    :- !, throw_or_fail_depending_on_option(e03,Start,inf,Step,Os).
check_step(minf     ,neg(_)    ,_     ,_  ,infinite) :- !.
check_step(minf     ,zero      ,Start ,_  ,_)        :- !, throw_unconditionally(e04,Start,minf,0).
check_step(minf     ,pos(Step) ,Start ,Os ,empty)    :- !, throw_or_fail_depending_on_option(e05,Start,minf,Step,Os).
check_step(int(End) ,pos(Step) ,Start ,Os ,empty)    :- Start>End, !, throw_or_fail_depending_on_option(e06,Start,End,Step,Os).
check_step(int(End) ,neg(Step) ,Start ,Os ,empty)    :- Start<End, !, throw_or_fail_depending_on_option(e07,Start,End,Step,Os).
check_step(int(_)   ,neg(_)    ,_     ,_  ,usual)    :- !. % the usual case; a cut should not be needed here
check_step(int(_)   ,pos(_)    ,_     ,_  ,usual).         % the usual case

% ===
% Helper predicate to perform limit checks in VERIFY mode
% bounded_by(+TaggedEnd,+Start,+Value)
% ===

bounded_by(int(End),Start,Value) :- Start == End, !, Value == End.
bounded_by(int(End),Start,Value) :- Start < End,  !, Start =< Value, Value =< End.
bounded_by(int(End),Start,Value) :- Start > End,  !, Start >= Value, Value >= End.
bounded_by(inf,Start,Value)      :- Start =< Value.
bounded_by(minf,Start,Value)     :- Value =< Start.

% ===
% Helper predicate workhorse. Be as deterministic as possible!
% between_enum(+TaggedStep,+TaggedEnd,+Start,-Value)
% "Start" is the "Current Value" that is unified with "Value"
% ===

% Special case of the 1-element sequence with "Step" 0

between_enum(zero, int(End), End, End) :- !.

% ---
% Case of "positive step" and "integer end" (i.e. not +infinity end)
% ---

% Exactly hitting "End". Last element in sequence. No backtracking.
% This is the case Start==End

between_enum(pos(_),int(End),End,End) :- !.

% Past end of sequence. Occurs only if the sequence is empty on entry.

between_enum(pos(Step),int(End),Start,Start) :-
   Start < End, Start+Step > End, !.

% More entries exist in sequence because the next element is still <= "End".
% Emit "Start" as "Value" and DO allow backtracking!
% The test "Start < End" is redundant here.
% The test "Start+Step =< End" is redundant here, being the complement of the
% cut-off preceding clause

between_enum(pos(Step),int(End),Start,Start) :-
   Start < End, Start+Step =< End.

% Recursive alternative to the above, digging for more sequence elements.
% Emit "Value" obtained from recursive call as next sequence element.
% The test "Start < End" is redundant here.
% The test "Start+Step =< End" is redundant here.

between_enum(pos(Step),int(End),Start,Value) :-
   Start < End, Start+Step =< End,
   NewStart is Start+Step,
   !, % If the compiler indexes on the first 2 args, this is not needed
   between_enum(pos(Step),int(End),NewStart,Value).

% ---
% Case of "negative step" and "integer end" (i.e. not -infinity end)
% ---

% Exactly hitting "End". Last element in sequence. No backtracking.

between_enum(neg(_),int(End),End,End) :- !.

% Past end of sequence. Occurs only if the sequence is empty on entry.

between_enum(neg(_),int(End),Start,_) :-
   Start < End,!,fail.

% Last element in sequence because the next element would be past "End".
% Emit "Start" as "Value" and don't allow backtracking!
% The test "Start > End" is redundant here.

between_enum(neg(Step),int(End),Start,Start) :-
   Start > End, Start+Step < End, !.

% More entries exist in sequence because the next element is still >= "End".
% Emit "Start" as "Value" and DO allow backtracking!
% The test "Start > End" is redundant here.
% The test "Start+Step >= End" is redundant here, being the complement of the
% cut-off preceding clause

between_enum(neg(Step),int(End),Start,Start) :-
   Start > End, Start+Step >= End.

% Recursive alternative to the above, digging for more sequence elements.
% Emit "Value" obtained from recursive call as next sequence element.
% The test "Start > End" is redundant here.
% The test "Start+Step >= End" is redundant here.

between_enum(neg(Step),int(End),Start,Value) :-
   Start > End, Start+Step >= End,
   NewStart is Start+Step,
   !, % Should not be needed as no further case matches
   between_enum(neg(Step),int(End),NewStart,Value).

% ---
% Case of going to "positive infinity"
% ---

between_enum(pos(_),inf,Start,Start).

between_enum(pos(Step),inf,Start,Value) :-
   NewStart is Start+Step,
   !, % Should not be needed as no further case matches
   between_enum(pos(Step),inf,NewStart,Value).

% ---
% Case of going to "negative infinity"
% ---

between_enum(neg(_),minf,Start,Start).

between_enum(neg(Step),minf,Start,Value) :-
   NewStart is Start+Step,
   !, % Should not be needed as no further case matches
   between_enum(neg(Step),minf,NewStart,Value).

% ============================================================================
% Unit testing between/4 and between/5
% ============================================================================

:- begin_tests(between_with_step).

test(bad_step_1  , error(type_error(_,_),_))      :- between(0,1, foo ,_).
test(bad_step_2  , error(type_error(_,_),_))      :- between(0,1, 0.5 ,_).
test(bad_step_3  , error(instantiation_error,_))  :- between(0,1, _   ,_).

test(empyt_seq_1a  , error(domain_error(_,_),_))  :- between(0,+1, -1 ,_,[throw_if_empty]).
test(empty_seq_1b  , fail)                        :- between(0,+1, -1 ,_).

test(empyt_seq_2a  , error(domain_error(_,_),_))  :- between(0,-1, +1 ,_,[throw_if_empty]).
test(empty_seq_2b  , fail)                        :- between(0,-1, +1 ,_).

test(empyt_seq_3a  , error(domain_error(_,_),_))  :- between(0,-inf,+1,_,[throw_if_empty]).
test(empty_seq_3b  , fail)                        :- between(0,-inf,+1,_).

test(empyt_seq_4a  , error(domain_error(_,_),_))  :- between(0,+inf,-1,_,[throw_if_empty]).
test(empty_seq_4b  , fail)                        :- between(0,+inf,-1,_).

test(bad_start_1 , error(type_error(_,_),_))      :- between(foo  ,1,2,_).
test(bad_start_2 , error(type_error(_,_),_))      :- between(0.5  ,1,2,_).
test(bad_start_3 , error(instantiation_error,_))  :- between(_    ,1,2,_).
test(bad_start_4 , error(type_error(_,_),_))      :- between(inf  ,1,2,_).
test(bad_start_5 , error(type_error(_,_),_))      :- between(minf ,1,2,_).
test(bad_start_6 , error(type_error(_,_),_))      :- between(-inf ,1,2,_).
test(bad_start_7 , error(type_error(_,_),_))      :- between(+inf ,1,2,_).

test(bad_end_1   , error(type_error(_,_),_))      :- between(0,1 , foo,_).
test(bad_end_2   , error(type_error(_,_),_))      :- between(0,1 , 0.5,_).
test(bad_end_3   , error(instantiation_error,_))  :- between(0,1 , _,_).

test(zero_step, true(Value == 10))                :- between(10,10,0,Value).
test(zero_step_fail, error(domain_error(_,_),_))  :- between(10,20,0,_).

test(seq_len_one_1, true(Value ==  10))           :- between( 10,  10,   0,Value).
test(seq_len_one_2, true(Value ==  10))           :- between( 10,  10,  +1,Value).
test(seq_len_one_3, true(Value == -10))           :- between(-10, -10,  -1,Value).
test(seq_len_one_4, true(Value ==  10))           :- between( 10,  20,  11,Value).
test(seq_len_one_5, true(Value == -10))           :- between(-10, -20, -11,Value).

test(seq_len_two_1, all(Value == [ 10, 11]))      :- between( 10,  11,  +1,Value).
test(seq_len_two_2, all(Value == [-10,-11]))      :- between(-10, -11,  -1,Value).
test(seq_len_two_3, all(Value == [ 10, 20]))      :- between( 10,  20,  10,Value).
test(seq_len_two_4, all(Value == [-10,-20]))      :- between(-10, -20, -10,Value).

% There is no feature yet to limit "all" to 6 values say, so we do it ourselves.
% Using limit/3 of library(solution_sequences).

test(seq_enum_zero_1, fail) :- bagof(Value, limit(6,between( 10,  20, -2,Value)),_).
test(seq_enum_zero_2, fail) :- bagof(Value, limit(6,between(-10, -20, +2,Value)),_).

test(seq_enum_lim_1, true(Seq == [10,12,14,16,18,20]))       :- bagof(Value, limit(6,between( 10,  20,  2,Value)),Seq).
test(seq_enum_lim_2, true(Seq == [-10,-12,-14,-16,-18,-20])) :- bagof(Value, limit(6,between(-10, -20, -2,Value)),Seq).
test(seq_enum_lim_3, true(Seq == [10,13,16,19]))             :- bagof(Value, limit(6,between( 10,  20,  3,Value)),Seq).
test(seq_enum_lim_4, true(Seq == [-10,-13,-16,-19]))         :- bagof(Value, limit(6,between(-10, -20, -3,Value)),Seq).
test(seq_enum_lim_5, true(Seq == [10,15,20]))                :- bagof(Value, limit(6,between( 10,  20,  5,Value)),Seq).
test(seq_enum_lim_6, true(Seq == [-10,-15,-20]))             :- bagof(Value, limit(6,between(-10, -20, -5,Value)),Seq).
test(seq_enum_lim_7, true(Seq == [10,17]))                   :- bagof(Value, limit(6,between( 10,  20,  7,Value)),Seq).
test(seq_enum_lim_8, true(Seq == [-10,-17]))                 :- bagof(Value, limit(6,between(-10, -20, -7,Value)),Seq).

test(seq_enum_inf_1, true(Seq == [10,12,14,16,18,20]))       :- bagof(Value, limit(6,between( 10,  inf,  2,Value)),Seq).
test(seq_enum_inf_2, true(Seq == [-10,-12,-14,-16,-18,-20])) :- bagof(Value, limit(6,between(-10, -inf, -2,Value)),Seq).
test(seq_enum_inf_3, true(Seq == [10,13,16,19,22,25]))       :- bagof(Value, limit(6,between( 10,  inf,  3,Value)),Seq).
test(seq_enum_inf_4, true(Seq == [-10,-13,-16,-19,-22,-25])) :- bagof(Value, limit(6,between(-10, -inf, -3,Value)),Seq).
test(seq_enum_inf_5, true(Seq == [10,15,20,25,30,35]))       :- bagof(Value, limit(6,between( 10,  inf,  5,Value)),Seq).
test(seq_enum_inf_6, true(Seq == [-10,-15,-20,-25,-30,-35])) :- bagof(Value, limit(6,between(-10, -inf, -5,Value)),Seq).
test(seq_enum_inf_7, true(Seq == [10,17,24,31,38,45]))       :- bagof(Value, limit(6,between( 10,  inf,  7,Value)),Seq).
test(seq_enum_inf_8, true(Seq == [-10,-17,-24,-31,-38,-45])) :- bagof(Value, limit(6,between(-10, -inf, -7,Value)),Seq).

% Testing verify mode.

test(verify_inside_1)                                :- between(-10,10,1,  0).
test(verify_inside_but_at_limit_1)                   :- between(-10,10,1,-10).
test(verify_inside_but_at_limit_2)                   :- between(-10,10,1,+10).
test(verify_inside_but_not_in_seq,fail)              :- between(-10,10,3, -6).
test(verify_outside_lower_limit,fail)                :- between(-10,10,1,-11).
test(verify_outside_upper_limit,fail)                :- between(-10,10,1,+11).
test(verify_outside_but_in_seq,fail)                 :- between(-10,10,3, 11).
test(verify_inside_going_to_plusinfinity)            :- between(-10,+inf,+1,+1000).
test(verify_inside_going_to_minusinfinity)           :- between(-10,-inf,-1,-1000).
test(verify_inside_infinity_but_not_in_seq_inc,fail) :- between(-10,+inf,+111,455).
test(verify_inside_infinity_but_not_in_seq_dec,fail) :- between(-10,-inf,-111,-455).
test(verify_inside_empty_seq,fail)                   :- between(-10,+10,-1,0).
test(verify_inside_empty_seq_zero_step,fail)         :- between(10,10,0,0).
test(verify_outside_empty_seq_zero_step,fail)        :- between(-10,10,100,0).

back_and_forth(Start,End,Step,Limit) :-
   bagof(Value, limit(Limit,between(Start,End,Step,Value)),Seq),
   maplist(between(Start,End,Step),Seq).
   
% Verify must recognize whatever has been generated

limit(100).

test(back_and_forth_lim_1, true) :- limit(L), back_and_forth( 10,  20,   2, L).
test(back_and_forth_lim_2, true) :- limit(L), back_and_forth(-10, -20,  -2, L).
test(back_and_forth_lim_3, true) :- limit(L), back_and_forth( 10,  20,   3, L).
test(back_and_forth_lim_4, true) :- limit(L), back_and_forth(-10, -20,  -3, L).
test(back_and_forth_lim_5, true) :- limit(L), back_and_forth( 10,  20,   5, L).
test(back_and_forth_lim_6, true) :- limit(L), back_and_forth(-10, -20,  -5, L).
test(back_and_forth_lim_7, true) :- limit(L), back_and_forth( 10,  20,   7, L).
test(back_and_forth_lim_8, true) :- limit(L), back_and_forth(-10, -20,  -7, L).

test(back_and_forth_inf_1, true) :- limit(L), back_and_forth( 10,  inf,  2, L).
test(back_and_forth_inf_2, true) :- limit(L), back_and_forth(-10, -inf, -2, L).
test(back_and_forth_inf_3, true) :- limit(L), back_and_forth( 10,  inf,  3, L).
test(back_and_forth_inf_4, true) :- limit(L), back_and_forth(-10, -inf, -3, L).
test(back_and_forth_inf_5, true) :- limit(L), back_and_forth( 10,  inf,  5, L).
test(back_and_forth_inf_6, true) :- limit(L), back_and_forth(-10, -inf, -5, L).
test(back_and_forth_inf_7, true) :- limit(L), back_and_forth( 10,  inf,  7, L).
test(back_and_forth_inf_8, true) :- limit(L), back_and_forth(-10, -inf, -7, L).

:- end_tests(between_with_step).

rt(between_with_step) :- run_tests(between_with_step).

