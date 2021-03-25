% https://puzzling.stackexchange.com/questions/6552/puzzle-who-hacked-the-computer
%
% ===
% Four friends have been identified as suspects for an unauthorized 
% access into a computer system.
%
% They have made statements to the investigating authorities.
%
%    Alice said "Carlos did it".
%    John said "I did not do it".
%    Carlos said "Diana did it".
%    Diana said "Carlos lied when he said that I did it".
%
% If the authorities also know that exactly one of the four suspects is
% telling the truth, who did it? Explain
% ===


% Define the persons

persons([alice,john,carlos,diana]).

% Define the statements of the persons

says(alice  , hacker(carlos)).
says(john   , not(hacker(john))).
says(carlos , hacker(diana)).
says(diana  , lies(carlos)).

% Main predicate. We will call this later

solve(Hacker) :-
    selectTruthteller(TT),
    buildWorld(TT,World),
    simplifyWorld(World,SW),          % Run one simplification 
                                      % (this turns out to be enough, 
                                      % no second round needed)
    sort(SW,SimpleWorld),             % Sort and remove duplicates from
                                      % SW, yielding SimpleWorld
    writeln(SimpleWorld),             % Write SimpleWorld
    isConsistent(SimpleWorld,Hacker). % Check that SimpleWorld is
                                      % consistent; if yes, the
                                      % program succeeds and prints the
                                      % "Hacker"

% Select a person as "truthteller". Assuming that person is the
% truthteller, logical consequences will be checked.

selectTruthteller(TT) :-
   persons(Persons),
   member(TT,Persons).

% buildWorld(+Truthteller,-WorldOut).
% Generate "WorldOut" (which is a list of statements) under assumption
% that "Truthteller" is the truthteller.
% We need to iterate over the "Persons", so we need to have a second
% buildWorld/3 predicate in addition to buildWorld/2.

buildWorld(TT,WorldOut) :-
   persons(Persons),
   buildWorld(TT,WorldOut,Persons).

% buildWorld(+Truthteller,-WorldOut, +ListOfPersons).
%
% If there are no persons left in the "ListOfPersons" we are done and 
% the "WorldOut" is empty.
%
% If the next person in the "ListOfPersons" is the truthteller, we add
% the statement that he/she "truthes" as well as his/her
% statement as gospel truth to the "WorldOut".
%
% If the next person in the "ListOfPersons" is NOT the truthteller,
% we add the statement that he/she "lies" as well as his/her
% statement as gospel falsity to the "WorldOut".

buildWorld(_,[],[]).
buildWorld(TT,[truthes(TT),Stmt|WorldOutRest],[TT|PersonsRest]) :-
   says(TT, Stmt),
   buildWorld(TT,WorldOutRest,PersonsRest).
buildWorld(TT,[lies(P),not(Stmt)|WorldOutRest],[P|PersonsRest]) :-
   P \== TT,
   says(P, Stmt),
   buildWorld(TT,WorldOutRest,PersonsRest).

% Go through the world statements and simplify every statement in turn;
% the second line drops a statement if simplifyStmt/2 results in the 
% atom "null"

simplifyWorld([],[]).
simplifyWorld([In|InRest],OutRest) :-
   simplifyStmt(In,null),
   simplifyWorld(InRest,OutRest).
simplifyWorld([In|InRest],[Out|OutRest]) :-
   simplifyStmt(In,Out),
   Out \== null,
   simplifyWorld(InRest,OutRest).

% Simplify a single statement by eliminating "not"; use red cuts to
% simplify code

simplifyStmt(not(not(S)),S)           :- !.
simplifyStmt(not(lies(S)),truthes(S)) :- !.
simplifyStmt(not(hacker(_)),null)     :- !.
simplifyStmt(S,S).

% Check world for consistency

isConsistent(World,Hacker) :- 
   persons(Persons),
   everyPersonLiesOrTruthes(World,Persons),
   \+ someoneLiesAndTruthes(World),
   \+ thereIsMoreThanOneHacker(World),
   thereIsAHacker(World,Hacker).

% Helper predicates to check world for consistency

everyPersonLiesOrTruthes(_,[]).
everyPersonLiesOrTruthes(World,[P|Persons]) :- 
   member(lies(P),World),
   everyPersonLiesOrTruthes(World,Persons).
everyPersonLiesOrTruthes(World,[P|Persons]) :-
   member(truthes(P),World),
   everyPersonLiesOrTruthes(World,Persons).

someoneLiesAndTruthes(World) :-
   member(lies(P),World),
   member(truthes(P),World).

thereIsMoreThanOneHacker(World) :-
   member(hacker(P1),World),
   member(hacker(P2),World),
   P1 \== P2.

thereIsAHacker(World,P) :-
   member(hacker(P),World).
