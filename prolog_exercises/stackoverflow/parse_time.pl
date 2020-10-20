# https://stackoverflow.com/questions/64395886/how-can-i-make-this-simple-time-parsing-predicate-work-in-both-directions

:- use_module(library(dcg/basics)).

% ---
% "Generate" direction; note that String may be bound to something
% in which case this clause also verifies whether generating "HH:MM"
% from time(H,M) indeed yields (whatever is denoted by) String.
% ---

process_time(time(H,M),String) :-
   integer(H),                            % Demand that H,M are valid integers inside limits
   integer(M),
   between(0,23,H),
   between(0,59,M),
   !,                                     % Guard passed, commit to this code branch
   phrase(time_g(H,M),Chars,[]),          % Build Codes from time/2 Term
   string_chars(String,Chars).            % Merge Codes into a string, unify with String

% ---
% "Parse" direction. 
% ---

process_time(time(H,M),String) :-
   string(String),                        % Demand that String be a valid string; no demands on H,M  
   !,                                     % Guard passed, commit to this code branch
   string_chars(String,Chars),            % Explode String into characters
   phrase(time_p(H,M),Chars,[]).          % Parse "Codes" into H and M

% ---
% "Generate" DCG
% ---
   
time_g(H,M) --> hour_g(H), [':'], minute_g(M).
hour_g(H)   --> { divmod(H,10,V1,V2), digit_int(D1,V1), digit_int(D2,V2) }, digit(D1), digit(D2).
minute_g(M) --> { divmod(M,10,V1,V2), digit_int(D1,V1), digit_int(D2,V2) }, digit(D1), digit(D2).

% ---
% "Parse" DCG
% ---
   
time_p(H,M) --> hour_p(H), [':'], minute_p(M).
hour_p(H)   --> digit(D1), digit(D2), { digit_int(D1,V1), digit_int(D2,V2), H is V1*10+V2, between(0,23,H) }.
minute_p(M) --> digit(D1), digit(D2), { digit_int(D1,V1), digit_int(D2,V2), M is V1*10+V2, between(0,59,M) }.   
   
% ---
% Do I really have to code this? Oh well!
% ---

digit_int('0',0).
digit_int('1',1).
digit_int('2',2).
digit_int('3',3).
digit_int('4',4).
digit_int('5',5).
digit_int('6',6).
digit_int('7',7).
digit_int('8',8).
digit_int('9',9).

% ---
% Let's add plunit tests!
% ---

:- begin_tests(hhmm).

test("parse 1",    true(T == time(0,0)))   :- process_time(T,"00:00").
test("parse 2",    true(T == time(12,13))) :- process_time(T,"12:13").
test("parse 1",    true(T == time(23,59))) :- process_time(T,"23:59").
test("generate",   true(S == "12:13"))     :- process_time(time(12,13),S).
test("verify",     true)                   :- process_time(time(12,13),"12:13").
test("complete",   true(H == 12))          :- process_time(time(H,13),"12:13").

test("bad parse",    fail)                 :- process_time(_,"66:66").
test("bad generate", fail)                 :- process_time(time(66,66),_).

:- end_tests(hhmm).


