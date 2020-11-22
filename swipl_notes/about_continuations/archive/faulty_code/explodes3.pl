# As used in
# https://github.com/SWI-Prolog/swipl-devel/issues/706
# to demonstrate a segmentation fault problem.
#
# However, this program in itself is faulty!

loop_until(N) :-
   nb_setval(count,0),
   reset(capture_deposit_cmd(N),obtain(Count),Continuation),
   (
      (Continuation == 0) 
      ->
      true
      ;
      (assertion(var(Count)),
       nb_getval(count,Count),
       call(Continuation))
   ).

capture_deposit_cmd(N) :-
   reset(capture_reset_cmd(N),deposit(Count),Continuation),
   (
      (Continuation == 0) 
      ->
      true
      ;
      (assertion(nonvar(Count)),
       nb_setval(count,Count),
       call(Continuation))
   ).

infinitizer(Goal) :-
   call(Goal),
   infinitizer(Goal).
   
capture_reset_cmd(N) :-
   reset(
      infinitizer(in_loop_action(N)),
      escape_from_loop(FinalN),
      _),
   format("The final count is (unsurprisingly) equal to ~d: ~d\n",[N,FinalN]).

   
in_loop_action(N) :-
   shift(obtain(Count)),
   (
      (N == Count) 
      -> 
      shift(escape_from_loop(Count))
      ;
      (format("The count is ~d\n",[Count]),
       NewCount is Count+1,
       shift(deposit(NewCount)))
   ).

