
% ---

working_off_in_correct_order([I|Is],'>>',[I-D|Os],'~',D) :-
   succ(D,Dp),
   working_off_in_correct_order(Is,'>>',Os,'~',Dp).
   
working_off_in_correct_order([],'>>',[],'~',_).

% ---

working_off_in_correct_order_bis([I|Is],'>>',Os,'~',D) :-
   succ(D,Dp),
   working_off_in_correct_order_bis(Is,'>>',Oss,'~',Dp),
   Os = [I-D|Oss].
   
working_off_in_correct_order_bis([],'>>',[],'~',_).

% ---

:- begin_tests(calling).

test(1) :- working_off_in_correct_order([a,b,c,d,e,f,g],'>>',[a-0, b-1, c-2, d-3, e-4, f-5, g-6],'~',0).

test(2) :- working_off_in_correct_order_bis([a,b,c,d,e,f,g],'>>',[a-0, b-1, c-2, d-3, e-4, f-5, g-6],'~',0).

:- end_tests(calling).
