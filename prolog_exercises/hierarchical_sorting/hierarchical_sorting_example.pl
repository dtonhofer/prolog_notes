% ===
% An example of hierarchical sorting (using predosrt)
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ===

% ---
% Suppose we have:
% employee(++ID,++FirstName,++LastName,++Nickname)
% From https://www.complex.com/pop-culture/most-badass-movie-characters/
% ---

employee(1,  "Sarah"   , "Connor"      , null).
employee(2,  "Richard" , "Callahan"    , "Dirty Harry").
employee(3,  "Brian"   , "Mills"       , null).
employee(4,  null      , null          , "Hit-Girl").
employee(5,  "Alonzo"  , "Harris"      , null).
employee(6,  "Ash"     , "Williams"    , null).
employee(7,  null      , null          , "The Man With No Name").
employee(8,  null      , null          , "Aragorn").
employee(9,  null      , null          , "Wonder Woman").
employee(10, "Jules"   , "Winfield"    , null).
employee(11, "Max"     , "Rockatansky" , "Mad Max").
employee(12, null      , null          , "Furiosa").
employee(13, null      , null          , "Kiyuchiyo").
employee(14, "Léon"    , null          , null).
employee(15, "John"    , "Wick"        , null).
employee(16, "James"   , "Bond"        , "007").
employee(17, "Ellen"   , "Ripley"      , "Ripley").
employee(18, null      , null          , "Robocop").
employee(19, null      , null          , "The Bride").
employee(20, "Robert"  , "Plissken"    , "Snake").
employee(21, "Henry"   , "Walton"      , "Indiana Jones").
employee(22, "T-800"   , "Skynet"      , "The Terminator").
employee(23, "Han"     , "Solo"        , null).
employee(24, "John"    , "McClane"     , null).
employee(25, null      , null          , "Wolverine").

% ---
% Sort by FirstName first, then by LastName.
% Direction is one [ascending,descending] and expresses how to sort
% FinagleNonEquality is one of [true,false] and expresses that we finagle something
% so that records never compare equal, because if two records compare equal, predsort/3
% drops one of them!
% ---

employee_sort(Direction,FinagleNonEquality,EmployeesSorted) :-
   % begin entry checks and data hammering
   (var(Direction) -> (Direction=ascending) ; true),
   must_be(oneof([ascending,descending]),Direction),
   (var(FinagleNonEquality) -> (FinagleNonEquality=true) ; true),
   must_be(boolean,FinagleNonEquality),
   % collect all employees into EmployeesList (fails if there are none)
   bagof(employee(V1,V2,V3,V4),employee(V1,V2,V3,V4),EmployeesUnsorted),
   % call predsort on them
   % the predicate for predosrt is already semi-filled with (Direction,FinagleNonEquality)
   predsort(my_sort(Direction,FinagleNonEquality), EmployeesUnsorted, EmployeesSorted), 
   % some light printing
   maplist([X]>>format("~q\n",[X]),EmployeesSorted).

% ---
% Actual sorting predicate.
% Direction is one of 'ascending', 'descending' and should be ground.
% Delta is determined, but could also be a given, to be accepted or not.
% Employee1 and Employee2 are employee compound terms (records, facts); they should be ground.
% my_sort(++Direction, ?Delta, ++Employee1, ++Employee2)
% Note that the ordering of the last 3 arguments of my_sort is: "Delta, Record1, Record2"
% which is as specified for the predicate that predsort/3 accepts. This is not according to
% style - "output" should be at the back of the argument row:  "Record1, Record2, Delta".
% It makes no sense to have "Delta" as first argument (and thus indexed by the compiler)
% that I can see either. 
% ---

% ---
% LEVEL 1
% ---

my_sort(Direction, FinagleNonEquality, DeltaOut, employee(Id1,FirstName1,LastName1,_), employee(Id2,FirstName2,LastName2,_)) :-
   my_compare(FirstName1,FirstName2,Delta1),   
   my_sort_sublevel(Direction, FinagleNonEquality, Delta1, Id1, LastName1, Id2, LastName2, DeltaOut).

% ---   
% LEVEL 2
% ---

% we already have a inequality; just maybe reverse it 

my_sort_sublevel(Direction, _, '<',_,_,_,_,DeltaOut) :- 
   !,
   adapt_direction(Direction, '<', DeltaOut).  

% we already have a inequality; just maybe reverse it 

my_sort_sublevel(Direction, _, '>',_,_,_,_,DeltaOut) :- 
   !,
   adapt_direction(Direction, '>', DeltaOut).
   
% we have equality, need more comparison!

my_sort_sublevel(Direction, FinagleNonEquality, '=', Id1, LastName1, Id2, LastName2, DeltaOut) :- 
   my_compare(LastName1, LastName2, Delta2),
   finagle_non_equality(FinagleNonEquality, Delta2, Id1, Id2, Delta3),   
   adapt_direction(Direction, Delta3, DeltaOut). 

% A trick if both FirstName and LastName are equal to keep a consistent inequality:
% compare by ID

finagle_non_equality(true,'=',Id1,Id2,NewDelta) :-
   !,
   compare(NewDelta,Id1,Id2).

finagle_non_equality(_,Delta,_,_,Delta).
   
% ---
% If we sort "ascending", keep the comparison result, otherwise reverse the comparison result.
% Which argument is In, which is Out is unimportant.
% ---

adapt_direction(ascending,  '<', '<') :- !.
adapt_direction(ascending,  '>', '>') :- !.
adapt_direction(descending, '<', '>') :- !.
adapt_direction(descending, '>', '<') :- !.
adapt_direction(_         , '=', '=') :- !.
 
% ---
% Although nulls are in principle incomparable, state that they are "equal" among
% themselves and smaller than anything else for our purposes.
% Also assert that we only compare strings (you wouldn't do that in production code,
% or else move such a type check much closer to the "relation").
% In the last case, punt to "comparison according to the standard order of terms"
% ---

my_compare(null,null,'=') :- !.       
my_compare(null,Y,'<')    :- !,assertion(string(Y)).
my_compare(X,null,'>')    :- !,assertion(string(X)).
my_compare(X,Y,D)         :- assertion((string(X),string(Y))),compare(D,X,Y). 
