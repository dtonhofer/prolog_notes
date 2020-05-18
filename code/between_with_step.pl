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
%        inf, infinite, +(inf), +(infinite)   : for +infinity (recommended: -inf  )
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

% ===
% Entry point
% ===

between(Start,End,Step,Value) :-
   between(Start,End,Step,Value,[]).

between(Start,End,Step,Value,Options) :-
   (var(Options) -> Options = []; true),                     % Binds Options to empty list if needed
   must_be(integer,Step),                                    % Throws unless Step is an integer (sadly no indication in throw WHICH arg is bad)
   must_be(integer,Start),                                   % Throws unless Start is an integer (sadly no indication in throw WHICH arg is bad)
   cleanup_and_tag_end(End,TaggedEnd),                       % Throws unless End is an integer or one of the "infinites"; also tag value
   tag_step(Step,TaggedStep),                                % Tag step with pos/1, neg/1, zero/0
   check_step(TaggedEnd,TaggedStep,Start,Options,_SeqType),  % Empty sequences yield fail; or exception if "throw_if_empty"
   !,                                                        % Cut is not needed; remove if determinism of the preceding calls has been ascertained
   (nonvar(Value)
    ->                                                       % VERIFY mode
    (must_be(integer,Value),                                 % Throws unless Value an integer (sadly no indication in throw WHICH arg is bad)
     0 =:= ((Value-Start) mod Step))                         % Works correctly with negative Step
    ;                                                        % SEQUENCE ENUMERATION mode
    (between_enum(TaggedStep,TaggedEnd,Start,Value))).       % Help the compiler out by moving the TaggedStep to first position

% ===
% Helper predicates
% ===

% ---
% Better throwing!
% Domain error: `going_to_plus_infinity_but_step_is_not_strictly_positive' expected, found `info{end:inf,start:0,step: -1}'
% ---

throw_domain_error(e01,Culprit) :- domain_error(going_to_plus_infinity_but_step_is_not_strictly_positive,Culprit).
throw_domain_error(e02,Culprit) :- domain_error(going_to_minus_infinity_but_step_is_not_strictly_negative,Culprit).
throw_domain_error(e03,Culprit) :- domain_error(increasing_from_start_to_end_but_step_is_negative,Culprit). % empty sequence
throw_domain_error(e04,Culprit) :- domain_error(decreasing_from_start_to_end_but_step_is_positive,Culprit). % empty sequence
throw_domain_error(e05,Culprit) :- domain_error(changing_from_start_to_end_but_step_is_zero,Culprit).

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
% Check_step(+TaggedEnd,+TaggedStep,+Start,+Options,?SeqType)
% Note the particular case Start=End with Step=0.
% Employ red cuts to make this a big "if then ... if then ... else."
% although the clauses are already arranged for minimal backtracking.
% The last argument indicates whether the sequence has been found to be empty;
% it just exists for documentation as it is used nowhere.
% ---

check_step(int(X) ,zero   ,X ,_  ,empty)    :- !. % rare: going from X to X by step 0
check_step(int(E) ,zero   ,S ,_  ,_)        :- !, throw_domain_error(e05, info{start:S, end:E, step:0}).
check_step(inf    ,pos(_) ,_ ,_  ,infinite) :- !.
check_step(inf    ,zero   ,S ,_  ,_)        :- !, throw_domain_error(e01, info{start:S, end:inf, step:0}).
check_step(inf    ,neg(T) ,S ,Os ,empty)    :- !, memberchk(throw_if_empty,Os) -> throw_domain_error(e01, info{start:S, end:inf, step:T}) ; fail.
check_step(minf   ,neg(_) ,_ ,_  ,infinite) :- !.
check_step(minf   ,zero   ,S ,_  ,_)        :- !, throw_domain_error(e02, info{start:S, end:minf, step:0}).
check_step(minf   ,pos(T) ,S ,Os ,empty)    :- !, memberchk(throw_if_empty,Os) -> throw_domain_error(e02, info{start:S, end:minf, step:T}) ; fail.
check_step(int(E) ,pos(T) ,S ,Os ,empty)    :- S>E, !, memberchk(throw_if_empty,Os) -> throw_domain_error(e04, info{start:S, end:E, step:T}) ; fail.
check_step(int(E) ,neg(T) ,S ,Os ,empty)    :- S<E, !, memberchk(throw_if_empty,Os) -> throw_domain_error(e03, info{start:S, end:E, step:T}) ; fail.
check_step(int(_) ,neg(_) ,_ ,_  ,_)        :- !. % the usual case; a cut should not be needed here
check_step(int(_) ,pos(_) ,_ ,_  ,_).             % the usual case

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

between_enum(pos(_),int(End),Start,_) :-
   Start > End,!,fail.

% Last element in sequence because the next element would be past "End".
% Emit "Start" as "Value" and don't allow backtracking!
% The test "Start < End" is redundant here.

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

% ===
% Unit testing between/4 and between/5 (UNDER CONSTRUCTION)
% ===

:- begin_tests(between_45).

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

% There is no feature yet to limit all to 6 values say, so we do it ourselves
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

:- end_tests(between_45).

rt :- run_tests(between_45).

