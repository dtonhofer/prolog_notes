:- begin_tests(sub_string).

test(find_single_bravo,[true(T),nondet]) :-
   %      bbbbbb55555aaaaaaaa
   Str = "alpha bravo charlie",
   sub_string(Str, Before, Length, After, "bravo"),
   T = ([Before,Length,After] == [6,5,8]).

test(find_multiple_bravo,[true(T)]) :-
   Str = "alpha bravo charlie bravo delta",
   bagof([Before,Length,After],sub_string(Str, Before, Length, After, "bravo"), Bag),
   T = (Bag == [[6,5,20],[20,5,6]]).

test(fail_finding_micron,[fail]) :-
   Str = "alpha bravo charlie bravo delta",
   sub_string(Str, _Before, _Length, _After, "micron").

test(zero_is_the_first_char,[true(Extract="a")]) :-
   sub_string("axxxx", 0, 1, _, Extract).

test(negative_pos_is_not_allowed,[error(domain_error(not_less_than_zero,_))]) :-
   sub_string("axxxx",-1, 1, _, _).

% edge case which actually succeeds

test(there_is_zero_chars_past_the_end,[true(T)]) :-
   Str = "alpha bravo",
   string_length(Str,StrLen),
   sub_string(Str, StrLen, 0, After, Extract),
   T = ([After,Extract] == [0,""]).

test(getting_the_last_char,[true(T)]) :-
   Str = "alpha bravo",
   string_length(Str,StrLen),
   succ(Pos,StrLen),
   sub_string(Str, Pos, 1, After, Extract),
   T = ([After,Extract] == [0,"o"]).

test(cannot_get_more_chars_than_there_are,[fail]) :-
   Str = "alpha bravo",
   string_length(Str,StrLen),
   succ(Pos,StrLen),
   sub_string(Str, Pos, 2, _, _).

test(there_is_nothing_beyond_past_the_end_not_even_zero_chars,[fail]) :-
   Str = "alpha bravo",
   string_length(Str,StrLen),
   succ(StrLen,BeyondStrLen),
   sub_string(Str, BeyondStrLen, 0, _, _).
 
% so one has to be careful!

test(extract_at_most_5_chars,[true(T)]) :-
   Str = "alpha bravo charlie",
   Want = 5,
   bagof([CurPos,FixedCurPos,CanHave,After,Extract],
         solution(Str,Want,CurPos,FixedCurPos,CanHave,After,Extract),Bag),
   T = (Bag == [[-5, 0,0,19,""     ],
                [-4, 0,1,18,"a"    ],
                [-3, 0,2,17,"al"   ],
                [-2, 0,3,16,"alp"  ],
                [-1, 0,4,15,"alph" ],
                [ 0, 0,5,14,"alpha"],
                [ 1, 1,5,13,"lpha "],
                [ 2, 2,5,12,"pha b"],
                [ 3, 3,5,11,"ha br"],
                [ 4, 4,5,10,"a bra"],
                [ 5, 5,5, 9," brav"],
                [ 6, 6,5, 8,"bravo"],
                [ 7, 7,5, 7,"ravo "],
                [ 8, 8,5, 6,"avo c"],
                [ 9, 9,5, 5,"vo ch"],
                [10,10,5, 4,"o cha"],
                [11,11,5, 3," char"],
                [12,12,5, 2,"charl"],
                [13,13,5, 1,"harli"],
                [14,14,5, 0,"arlie"],
                [15,15,4, 0, "rlie"],
                [16,16,3, 0,  "lie"],
                [17,17,2, 0,   "ie"],
                [18,18,1, 0,    "e"],
                [19,19,0, 0,    ""]]).

solution(Str,Want,CurPos,FixedCurPos,CanHave,After,Extract) :-
   string_length(Str,StrLen),
   StartPos is -Want,
   between(StartPos,StrLen,CurPos),
   CanHave is min(CurPos+Want,min(Want,StrLen-CurPos)),
   FixedCurPos is max(0,CurPos),
   sub_string(Str,FixedCurPos,CanHave,After,Extract).

:- end_tests(sub_string).

rt(sub_string) :- run_tests(sub_string).
