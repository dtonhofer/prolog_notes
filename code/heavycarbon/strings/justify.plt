:- use_module(library('heavycarbon/strings/justify.pl')).

:- begin_tests(justfy).

% ---
% And also: Prolog needs special syntax for PCRE
% ---

space_to_dot(R,O) :- re_replace("\s",".",R,O).

% ---
% Shorty calls, easy to read.
% ---

test("simple left",   true(O == "Hello,.World........")) :- 
   justify_left("Hello, World",20,R,string,_),
   space_to_dot(R,O).

test("simple right",  true(O == "........Hello,.World")) :- 
   justify_right("Hello, World",20,R,string,_),
   space_to_dot(R,O).

test("simple center", true(O == "....Hello,.World....")) :- 
   justify_center("Hello, World",20,R,string,_),
   space_to_dot(R,O).

% ---
% Full-width calls are hard to read.
%
% justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Result,Want,Nocheck)
%
% It is best to create intermediary calls for legibility
%
% And also: Prolog needs name-based parameter passing!
% ---

hello_world_justify(How,Result) :-
   justify("Hello, World", 20, How, _,_,_,_, Result, string, _).

test("full left", true(O == "Hello,.World........") :-
   hello_world_justify(left,R),
   space_to_dot(R,O).

test("full right", true(O == "........Hello,.World") :-
   hello_world_justify(right,R),
   space_to_dot(R,O).

:- end_tests(justify).
