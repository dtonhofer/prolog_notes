:- use_module(library('heavycarbon/utils/between_with_step.pl')).

:- begin_tests(between_with_step).

test(bad_step_1  , error(type_error(_,_),_))      :- between(0,1, foo ,_).
test(bad_step_2  , error(type_error(_,_),_))      :- between(0,1, 0.5 ,_).
test(bad_step_3  , error(instantiation_error,_))  :- between(0,1, _   ,_).

test(empyt_seq_1a  , throws(my_error(domain_error,_,_)))  :- between(0,+1, -1 ,_,[throw_if_empty]).
test(empty_seq_1b  , fail)                        :- between(0,+1, -1 ,_).

test(empyt_seq_2a  , throws(my_error(domain_error,_,_)))  :- between(0,-1, +1 ,_,[throw_if_empty]).
test(empty_seq_2b  , fail)                        :- between(0,-1, +1 ,_).

test(empyt_seq_3a  , throws(my_error(domain_error,_,_)))  :- between(0,-inf,+1,_,[throw_if_empty]).
test(empty_seq_3b  , fail)                        :- between(0,-inf,+1,_).

test(empyt_seq_4a  , throws(my_error(domain_error,_,_)))  :- between(0,+inf,-1,_,[throw_if_empty]).
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
test(zero_step_fail, throws(my_error(domain_error,_,_)))  :- between(10,20,0,_).

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


