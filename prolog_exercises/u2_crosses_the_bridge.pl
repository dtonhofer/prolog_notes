% ===
% 2019-08-05
% ===
% This is free and unencumbered software released into the public domain.
% 
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
% 
% For more information, please refer to <http://unlicense.org/>
% ===
% References
% - - - - - -
%
% Torsten Sillke has collected a onster page of references:
%
%   "Crossing the bridge in an hour"
%   Torsten Sillke, June 1997/Sept 2001
%   https://www.math.uni-bielefeld.de/~sillke/PUZZLES/crossing-bridge
%
% A problem evoked by Oleg Kiselyov in:
% 
%   "Guess Lazily! making a program guess and guess well"
%   http://okmij.org/ftp/kakuritu/logic-programming.html
%   Strange Loop 2012
%   St Louis, MO September 25, 2012
%
%     ...where it is not attacked with Prolog but used as a
%     example to explain HANSEI.
%
% Paper for the general case:
%
%   "Crossing the Bridge at Night"
%   by Günter Rote (2002-08-21)
%   http://page.mi.fu-berlin.de/rote/Papers/pdf/Crossing+the+bridge+at+night.pdf
%
% Regularly appears on StackOverflow
%
%   https://stackoverflow.com/questions/33469487/prolog-river-crossing
%   https://stackoverflow.com/questions/32978566/bridge-crossing-puzzle-with-clpfd
%   https://stackoverflow.com/questions/10131517/prolog-bridge-puzzle-with-unlimited-people
%   https://stackoverflow.com/questions/1144207/bridge-crossing-puzzle
%
% Description
% - - - - - -
%
% ”U2” has a concert that starts in 17 minutes and they must all
% cross a bridge to get there. They stand on the same side of the
% bridge. It is night. There is one flashlight. A maximum of two
% people can cross at one time, and they must have the flashlight
% with them. The flashlight must be walked back and forth. A
% pair walk together at the rate of the slower man’s pace:
% 
% Bono 1 minute to cross
% Edge 2 minutes to cross
% Adam 5 minutes to cross
% Larry 10 minutes to cross
% 
% For example: if Bono and Larry walk across first, 10 minutes have
% elapsed when they get to the other side of the bridge. If Larry then
% returns with the flashlight, a total of 20 minutes have passed and you
% have failed the mission.
% 
% Allegedly, this is a question for potential Microsoft employees.
% An answer is expected within 5 minutes.
% 
% It is indeed a typical scheduling problem: find a sequence of decisions
% – who should walk in what sequence – subject to a set of constraints,
% optimizing some utility. We assume that there is a schedule and
% describe its properties.
% There are two answers, neither of which are trick answers. Allegedly,
% this is one of the questions for potential Microsoft employees. Some
% people really get caught up trying to solve this problem. Reportedly,
% one guy solved it by writing a C program, although that took him 37
% minutes to develop (compiled and ran on the 1st try though).
% Another guy solved it in three minutes. A group of 50, at Motorola,
% couldn’t figure it out at all.
% ===

% ----8<----8<----8<----8<----8<----8<----8<----8<----8<----

% This program took about 2h to write. I'm not ready for Microsoft.
% 
% Run with 
% 
%    set_prolog_flag(answer_write_options,[max_depth(100)]).
%    solve(X).
% 
% This gives the following solutions (note that the "left group"
% is always the one having the flashlight and the "target side" of
% the bridge switches on ever expanded state)
% 
% X = [ state([adam, larry, bono, edge], [], 17),  - flashlight is with [adam, larry, bono, edge] on target side 
%       state([bono, edge], [adam, larry], 15),    - flashlight is with [bono, edge] on wrong side 
%       state([edge, adam, larry], [bono], 13),    - flashlight is with [edge, adam, larry] on target side 
%       state([adam, larry, bono], [edge], 3),     - flashlight is with [adam, larry, bono] on wrong side 
%       state([bono, edge], [adam, larry], 2),     - flashlight is with [bono,edge] on target side 
%       state([bono, edge, adam, larry], [], 0)]   - START, flashlight on wrong side
%       
% X = [ state([adam, larry, bono, edge], [], 17),  - flashlight is with [adam, larry, bono, edge] on target side 
%       state([edge, bono], [adam, larry], 15),    - flashlight is with [edge, bono] on wrong side 
%       state([bono, adam, larry], [edge], 14),    - flashlight is with [bono, adam, larry] on target side 
%       state([adam, larry, edge], [bono], 4),     - flashlight is with [adam, larry, edge] on wrong side 
%       state([bono, edge], [adam, larry], 2),     - flashlight is with [bono,edge] on target side 
%       state([bono, edge, adam, larry], [], 0)]   - START, flashlight on wrong side
%
% A state in the search space is always: 
% 
% state(ATeam, BTeam, Clock)
% 
% Where ATeam is a list of the people in the ATeam, which is the team holding the flashlight.
% Whether the ATeam is on the wrong or target side of the bridge depends on the history length.
% The BTeam is on the other side of the bridge and has no flashlight.
% The Clock indicates time at that state.

solve(HistoryOut) :- 
    explore([state([bono,edge,adam,larry],[],0)],HistoryOut).

% Explore a list of states (the History) by either detecting a solution or
% expanding the "top of the History" into possible next states, then recursively
% exploring those.

explore(History,History) :- 
    solution(History).

explore([Current|HistoryRest],HistoryOut) :- 
    \+solution([Current|HistoryRest]),
    expand([Current|HistoryRest],[Next,Current|HistoryRest]),
    explore([Next,Current|HistoryRest],HistoryOut).

% A solution is a state where the BTeam is empty and
% the ATeam (the team holding the flashlight) is full.
% and the Clock is <= 17 and the number of crossings of the flashlight is odd.

solution([state(ATeam,[],Clock)|History]) :- 
    length(History,L), L mod 2 =:= 1,
    Clock =< 17,         % no actual need to test this as no state violates this    
    member(bono,ATeam),  % no actual need to test this
    member(edge,ATeam),  % no actual need to test this
    member(adam,ATeam),  % no actual need to test this
    member(larry,ATeam). % no actual need to test this

% Expand a state by adding a a new state on the left of the state list.
% Always seen "from the flashlight's point of view", i.e.
% The "A-Team" has the flashlight and sends people out to the "B-Team".
% Then we do not need to keep track of the flashlight, which saves about 20 lines!

expand([Current|History],[Next,Current|History]) :-
    Current = state(ATeam,BTeam,Clock),                    % deterministic destructuring
    team_selection(ATeam,Team),                            % backtrackable team selection
    crossing_time(Team,CrossingTime),                      % deterministic determination of crossing time
    NextClock is Clock+CrossingTime,
    NextClock =< 17,                                       % selection criterium    
    subtract(ATeam,Team,NextATeam),
    union(BTeam,Team,NextBTeam),
    Next = state(NextBTeam,NextATeam,NextClock).

team_selection(Candidates,[A,B]) :- 
    select(A, Candidates, SmallerCands),select(B, SmallerCands, _), A @< B. 

team_selection(Candidates,[A]) :- 
    select(A, Candidates, _).

% Stipulate crossing time of "persons"

crossing_time_p(bono,1).
crossing_time_p(edge,2).
crossing_time_p(adam,5).
crossing_time_p(larry,10).

% Stipulate crossing time of a team of exactly 1 person.

crossing_time([Who],Time) :- !,crossing_time_p(Who,Time).

% Stipulate crossing time of a team of 2 or more persons.
% This fails if Rest is the empty list, because crossing_time([],_)
% has no solution, as it should!

crossing_time([Who|Rest],Time) :- 
    crossing_time(Rest,Time1),
    crossing_time_p(Who,Time2),
    max_list([Time1,Time2],Time).

