% ===
% Helper predicates
% ===

% ---
% Better throwing!
% ---

throw_domain_error(e01,Culprit) :- domain_error(going_to_plus_infinity_but_step_is_not_strictly_positive,Culprit).
throw_domain_error(e02,Culprit) :- domain_error(going_to_minus_infinity_but_step_is_not_strictly_negative,Culprit).
throw_domain_error(e03,Culprit) :- domain_error(increasing_from_start_to_end_but_step_is_negative,Culprit). % empty sequence
throw_domain_error(e04,Culprit) :- domain_error(decreasing_from_start_to_end_but_step_is_positive,Culprit). % empty sequence
throw_domain_error(e05,Culprit) :- domain_error(changing_from_start_to_end_but_step_is_zero,Culprit).

% ---
% Cleaning up the value for "end".
% Default behaviour: render "end" matchable by tagging it with int/1; throw unless "end" is an integer
% ---

cleanup_and_tag_end(inf         ,inf).
cleanup_and_tag_end(infinite    ,inf).
cleanup_and_tag_end(+(inf)      ,inf).
cleanup_and_tag_end(+(infinite) ,inf).
cleanup_and_tag_end(minf        ,minf).
cleanup_and_tag_end(-(inf)      ,minf).
cleanup_and_tag_end(-(infinite) ,minf).
cleanup_and_tag_end(X           ,int(X)) :- must_be(integer,X).             

% ---
% Cleaning up the value for "step", which is known to be an integer.
% Default behaviour: render "step" matchable by tagging it with pos/1, neg/1, zero/1 and throw unless "step" is an integer
% ---

tag_step(0 ,zero)    :- !.
tag_step(X ,pos(X))  :- X>0,!.
tag_step(X ,neg(X))  :- X<0,!.

% ---
% Checking the value of "step".
% Note the particular case Start=End with Step=0.
% Employ red cuts to make this a big "if then ... if then ... else.
% ---

check_step(X ,int(X) ,zero,   _) :- !.  % this is allowed (but rare)
check_step(S ,int(E) ,zero,   _) :- !, throw_domain_error(e05, info{start:S, end:E, step:0}).
check_step(_ ,inf    ,pos(_), _) :- !.  % this is allowed! 
check_step(S ,inf    ,zero,   _) :- !, throw_domain_error(e01, info{start:S, end:inf, step:0}).
check_step(S ,inf    ,neg(T), _) :- !, throw_domain_error(e01, info{start:S, end:inf, step:T}).
check_step(_ ,minf   ,neg(_), _) :- !.  % this is allowed! 
check_step(S ,minf   ,zero,   _) :- !, throw_domain_error(e02, info{start:S, end:minf, step:0}).
check_step(S ,minf   ,pos(T), _) :- !, throw_domain_error(e02, info{start:S, end:minf, step:T}).
check_step(S ,int(E) ,pos(T), F) :- S>E, memberchk(throw_if_empty,F), !, throw_domain_error(e04, info{start:S, end:E, step:T}). 
check_step(S ,int(E) ,neg(T), F) :- S<E, memberchk(throw_if_empty,F), !, throw_domain_error(e03, info{start:S, end:E, step:T}). 
check_step(_ ,int(_), neg(_), _).  % the usual case
check_step(_ ,int(_), pos(_), _).  % the usual case

% ===
% Entry point
% ===
% between(+Start,+End,+Step,?Value,?Flags)
% Start: Start of sequence, must denote an integer.
% End:   End of sequence, must denote an integer. Alternatively, can also be one of: 
%        inf, infinite, +(inf), +(infinite)     for +infinity (recommended: inf  )
%        minf, minfinite, -(inf), -(infinite)   for -infinity (recommended: minf )
%        It is allowed for Start = End, as long as Step = 0.
% Step:  Integer step of sequence, must denote an integer.
%        Must have the correct sign to reach End from Start.
%        It is allowed for Step = 0, as long as Start = End.
%        It is not necessary for Start + K*Step = End (if End is not one of the  infinities).
%        Sequence enumeration fails as soon as Current "past" End. 
% Value: Either a fresh variable that will be set to an integer.
%        Or denoates an integer which for which the predicate then verifies that it
%        belongs to the sequence.
% Flags: Behaviour-tuning. May be a freshvar or the empty list for "default"
%        If it contains "throw_if_empty", empty sequences lead to a throw
% ===

between(Start,End,Step,Value) :-
   between(Start,End,Step,Value,[]).
   
between(Start,End,Step,Value,Flags) :-
   (var(Flags) -> Flags = []; true),        % upgrade Flags to nonvar if needed
   must_be(integer,Step),                   % throws unless Step an integer (sadly no indication in trow WHICH arg is bad)
   must_be(integer,Start),                  % throws unless Start an integer (sadly no indication in trow WHICH arg is bad)
   cleanup_and_tag_end(End,TaggedEnd),      % throws unless End is an integer or one of the "infinites"; also tag value
   tag_step(Step,TaggedStep),               % tag step with pos/1, neg/1, zero/0
   check_step(Start,TaggedEnd,TaggedStep,Flags),  % throws unless End can be reached from Start using Step
   !,                                       % Cut is not needed; remove if determinism of workhorse predicate is ascertained
   (nonvar(Value)   
    ->                                      % VERIFY mode
    (must_be(integer,Value),                % throws unless Value an integer (sadly no indication in trow WHICH arg is bad)
     0 =:= ((Value-Start) mod Step))        % works correctly with negative Step
    ;                                       % SEQUENCE ENUMERATION mode    
    (between_enum(Start,TaggedEnd,TaggedStep,Value))).

% ===
% Helper predicate workhorse. Be as deterministic as possible!
% between_enum(+Start,+TaggedEnd,+TaggedStep,-Value)
% ===

% Reached end of Sequence (includes TaggedStep = zero). No backtracking allowed.

between_enum(Limit,int(Limit),_,Limit) :- !.  

% ---
% Case of "positive step" pos(Step) and "integer end" int(End) (not infinite end)
% ---

% Past end of sequence. Occurs only if the sequence is empty on entry.

between_enum(Start,int(End),pos(_),_) :- 
   Start > End,!,fail. 

% Last entry in sequence, emit "Start" as "Value" and don't allow backtracking!
% The test "Start < End" is redundant here.

between_enum(Start,int(End),pos(Step),Start) :- 
   Start < End, Start+Step >  End, !. 

% More entries exist in sequence, emit "Start" as "Value" and DO allow backtracking!
% The test "Start < End" is redundant here.
% The test "Start+Step =< End" is redundant here, being the complement of the cut-off preceding clause

between_enum(Start,int(End),pos(Step),Start) :-
   Start < End, Start+Step =< End.    

% Recursive alternative to the above, digging for more values!
% The test "Start < End" is redundant here.
% The test "Start+Step =< End" is redundant here

between_enum(Start,int(End),pos(Step),Value) :-
   Start < End, Start+Step =< End, 
   NewStart is Start+Step, 
   %!, % NEEDED TO MAINTAIN DETERMINACY ON LAST VALUE
   between_enum(NewStart,int(End),pos(Step),Value).
   
% ---
% Case of "negative step" pos(Step) and "integer end" int(End) (not infinite end)
% ---
                      
 % Past end of sequence. Occurs only if the sequence is empty on entry.
 
between_enum(Start,int(End),neg(_),_) :-
   Start < End,!,fail.

% Last entry in sequence, emit "Start" as "Value" and don't allow backtracking!
% The test "Start > End" is redundant here.
   
between_enum(Start,int(End),neg(Step),Start) :-
   Start > End, Start+Step <  End, !.

% More entries exist in sequence, emit "Start" as "Value" and DO allow backtracking!
% The test "Start > End" is redundant here.
% The test "Start+Step >= End" is redundant here, being the complement of the cut-off preceding clause

between_enum(Start,int(End),neg(Step),Start) :- 
   Start > End, Start+Step >= End.

% Recursive alternative to the above, digging for more values!
% The test "Start > End" is redundant here.
% The test "Start+Step >= End" is redundant here.
   
between_enum(Start,int(End),neg(Step),Value) :- 
   Start > End, Start+Step >= End, 
   NewStart is Start+Step, 
   !, % ACTUALLY NOT NEEDED TO MAINTAIN DETERMINACY ON LAST VALUE
   between_enum(NewStart,int(End),neg(Step),Value).

% ---
% Case of going to "positive infinity"
% ---
   
between_enum(Start,inf,pos(_),Start).

between_enum(Start,inf,pos(Step),Value) :-
   NewStart is Start+Step, 
   !,
   between_enum(NewStart,inf,pos(Step),Value).

% ---
% Case of going to "negative infinity"
% ---

between_enum(Start,minf,neg(_),Start).

between_enum(Start,minf,neg(Step),Value) :-
   NewStart is Start+Step,
   !,
   between_enum(NewStart,minf,neg(Step),Value).

% ===
% Unit testing between/4 and between/5 (UNDER CONSTRUCTION)
%
% Problem: Is there a way to error if the last value for all leaves a choicepoint open?
% ===

:- begin_tests(between_45).

test(bad_step, error(type_error(_,_),_))  :- between(0,1, foo ,_).
test(bad_step, error(type_error(_,_),_))  :- between(0,1, 0.5 ,_).
test(bad_step, error(instantiation_error,_))  :- between(0,1, _   ,_).

test(bad_start, error(type_error(_,_),_)) :- between(foo  ,1,2,_).
test(bad_start, error(type_error(_,_),_)) :- between(0.5  ,1,2,_).
test(bad_start, error(instantiation_error,_)) :- between(_    ,1,2,_).
test(bad_start, error(type_error(_,_),_)) :- between(inf  ,1,2,_).
test(bad_start, error(type_error(_,_),_)) :- between(minf ,1,2,_).
test(bad_start, error(type_error(_,_),_)) :- between(-inf ,1,2,_).
test(bad_start, error(type_error(_,_),_)) :- between(+inf ,1,2,_).

test(bad_end,   error(type_error(_,_),_)) :- between(0,1 , foo,_).
test(bad_end,   error(type_error(_,_),_)) :- between(0,1 , 0.5,_).
test(bad_end,   error(instantiation_error,_)) :- between(0,1 , _,_).

test(zero_step, true(Value  = 10))                       :- between(10,10,0,Value).
test(seq_length_one_step_pos, true(Value  = 10))         :- between(10,10,1,Value).
test(seq_length_one_step_neg, true(Value  = 10))         :- between(10,10,-1,Value).  
test(seq_length_one_step_zero, error(domain_error(_,_),_)) :- between(10,11,0,_).  

test(seq_step_pos_1, all(V = [10,11,12,13,14,15])) :- between(10,15,1,V).
test(seq_step_pos_2, all(V = [10,12,14]))          :- between(10,15,2,V).
test(seq_step_pos_3, all(V = [10,13]))             :- between(10,15,3,V).
test(seq_step_pos_4, all(V = [10,14]))             :- between(10,15,4,V).
test(seq_step_pos_5, all(V = [10,15]))             :- between(10,15,5,V).
test(seq_step_pos_6, all(V = [10]))                :- between(10,15,6,V).
test(seq_step_pos_7, all(V = [10]))                :- between(10,15,7,V).

test(seq_step_neg_1, all(V = [10,9,8,7,6,5])) :- between(10,5,-1,V).
test(seq_step_neg_1, all(V = [10,8,6]))       :- between(10,5,-2,V).
test(seq_step_neg_2, all(V = [10,7]))         :- between(10,5,-3,V).
test(seq_step_neg_3, all(V = [10,6]))         :- between(10,5,-4,V).
test(seq_step_neg_4, all(V = [10,5]))         :- between(10,5,-5,V).
test(seq_step_neg_5, all(V = [10]))           :- between(10,5,-6,V).
test(seq_step_neg_6, all(V = [10]))           :- between(10,5,-7,V).

:- end_tests(between_45).

rt :- run_tests(between_45).
