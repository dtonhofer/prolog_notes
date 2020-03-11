# A Simple Relational Database Operation

 This is not about connecting to a database using SQL. It is inspired by this StackOverflow question:

[Find mutual element in different facts in swi-prolog](https://stackoverflow.com/questions/60582295/find-mutual-element-in-different-facts-in-swi-prolog)

# Problem statement

> Given a database of "actors starring in movies":
> 
> ````
> starsin(a,bob).
> starsin(c,bob).
>
> starsin(a,maria).
> starsin(b,maria).
> starsin(c,maria).
>
> starsin(a,george).
> starsin(b,george).
> starsin(c,george).
> starsin(d,george).
> ````
>
> And given set of movies _M_, find those actors that starred in all the movies of _M_.

# Prolog solution that is "nice"

(For the ugly solution, see further below)

Clarify the problem:

Sets are represented by lists without duplicates, possibly ordered.

> 1. Given a _Set of Movies_ `MovIn` 
> 1. ... Find the _Set of Actors_ `ActOut`
> 1. ... ... Such that: Every _Actor_ in `ActOut` appeared in (at least) all the movies in `MovIn`
> 1. ... ... Reformulated: The _Set of Movies_ `MovAx` for any actor `Ax` of `ActOut` is a superset of `MovIn`.

[setof/3](https://eu.swi-prolog.org/pldoc/doc_for?object=setof/3) seems to be the correct toplevel predicate. An _Ansatz_ for points 1 and 2 is:

````
setof(Ax, (... MovIn ...) , ActOut).
````

If `MovAx` is the _Set of Movies_ that `Ax` appeared in, we can use

 - [subset/2](https://eu.swi-prolog.org/pldoc/doc_for?object=subset/2) of 
[library(lists)](https://eu.swi-prolog.org/pldoc/man?section=lists) or
 - [ord_subset/2](https://eu.swi-prolog.org/pldoc/doc_for?object=ord_subset/2) of
[library(ordset)](https://eu.swi-prolog.org/pldoc/man?section=ordsets) ... if we can ensure evertyhing is an ordset.

Let's use the `subset/2`.

Point 4 seems to make us write:

````
setof(Ax, (..., subset(MovAx, MovIn)) , ActOut).
````

Develop the `...` ...

````
setof(Ax, ( setof(Mx,starsin(Mx,Ax),MovAx) , subset(MovIn, MovAx) ) , ActOut).
````

This seems to be it already! 

The feel when there are λ Expressions but there is
no λ on the keyboard or in the syntax. 

Wrap up into predicate. Note that one must isolate (existentially quantify?) `MovAx` otherwise Prolog 
will start backtracking over possible instantiation of `MovAx` and you will see individual
responses on the toplevel. The construct `+Var^Goal` tells the outer `setof/3` not to bind `MovAx` in goal 
 `setof(Mx,starsin(Mx,Ax),MovAx), subset(MovIn,MovAx)` ... it is not a variable we are interested in at the toplevel.
 
````
actors_appearing_in_movies_nice(MovIn,ActOut) :-
    setof(
        Ax,
        MovAx^(setof(Mx,starsin(Mx,Ax),MovAx), subset(MovIn,MovAx)),
        ActOut
    ).    
````

For clarity, one can also write:

````
subselect(Ax,MovIn) :- 
   setof(Mx,starsin(Mx,Ax),MovAx), subset(MovIn, MovAx).
   
actors_appearing_in_movies_nice(MovIn,ActOut) :- 
   setof(Ax, subselect(Ax,MovIn) , ActOut).
````

Testing is just running a few goals.

Note that for the empty set of movies, we get all the actors. This is arguably correct: 
every actors stars in all the movies of the empty set ([Vacuous Truth](https://en.wikipedia.org/wiki/Vacuous_truth))

Using [`library(plunit)`](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27), we can pack this into a unit test.

(Note the final `!` to make the test goal deterministic. Is that correct? I tdefinitely works. That clause is not really Prolog, it is Meta-Prolog.)

````
:- begin_tests(exercise_nice).

expect([],[bob, george, maria]).
expect([a],[bob, george, maria]).
expect([a,b],[george, maria]).
expect([a,b,c],[george, maria]).
expect([a,b,c,d],[george]).

test("movie stars", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_nice(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_nice).
````

Load the test block above from a file, not through `[user]` (otherwise there will errors when you attempt
to redefine it). 

Run the tests:

````
?- run_tests(exercise_nice).
% PL-Unit: exercise ..... done
% All 5 tests passed
true.
````

# Prolog solution that is "ugly"

What do we need in our toolbox?

- [`bagof/3`](https://www.swi-prolog.org/pldoc/doc_for?object=bagof/3) is the foundation, no recursion needed. On second thoughts, I could have used [`setof/3`](https://www.swi-prolog.org/pldoc/doc_for?object=setof/3) to make a "set" (ordered list with no duplicates) out of a "bag" (list with random stuff).
- [`maplist/3`](https://www.swi-prolog.org/pldoc/doc_for?object=maplist/3) is used to construct an output list from an input list, running each element of the input list through a predicate to get the next element of the output list. The output list does not need to be initialized in any way; `maplist` constrains the corresponding variable to a list of the correct length. 
- Some predicates from [`library(ordsets)`](https://www.swi-prolog.org/pldoc/man?section=ordsets) because we are basically using lists seen as sets.

````
% Create a combination structure of [Actor, List-of-Movies-with-Actor]
% actors_with_movies(X).
% X = [[bob, [a, c]], [george, [a, b, c, d]], [maria, [a, b, c]]]

actors_with_movies(ActorsWithMovies) :-
   all_actors(ActorSet),
   maplist(combine_actor_with_movies,ActorSet,ActorsWithMovies).

% Just rearrange variables into a specific format 

combine_actor_with_movies(Actor,[Actor,MovieSet]) :-
    movies_of_actor(Actor,MovieSet).

% Collect all actors that exist by scanning the facts.
% all_actors(X).
% X = [bob, george, maria]

all_actors(ActorSet) :- 
    bagof(Actor,Movie^starsin(Movie,Actor),ActorBag),
    list_to_ord_set(ActorBag,ActorSet).

% Collect all the movies of a given actor.
% movies_of_actor(bob,[a,c]).
% movies_of_actor(maria,X).
% X = [a,b,c]

movies_of_actor(Actor,MovieSet) :-
    bagof(Movie,starsin(Movie,Actor),MovieBag),
    list_to_ord_set(MovieBag,MovieSet).

% Selection for "Actor".
% Always succeeds, but:
% If the "ActorIn" appears in all the movies listed in "MoviesMust",
% ... then ActorOut is unified with ActorIn
% ... otherwise, ActorOut is unified with []

select(MoviesMust,[ActorIn,MoviesAre],ActorIn) :- ord_subset(MoviesMust,MoviesAre),!.
select(_MoviesMust,[_ActorIn,_MoviesAre],[]).

% ***MAIN PREDICATE***

actors_appearing_in_movies_ugly(MoviesIn,ActorsOut) :-
    list_to_ord_set(MoviesIn,MovieSet),
    actors_with_movies(ActorsWithMovies),
    format("~w\n",[ActorsWithMovies]),
    maplist(select(MovieSet),ActorsWithMovies,ActorsOutWithNulls),
    include(\=([]),ActorsOutWithNulls,ActorsOut).
````

Testing is the same as earlier:

````
:- begin_tests(exercise_ugly).

expect([],[bob, george, maria]).
expect([a],[bob, george, maria]).
expect([a,b],[george, maria]).
expect([a,b,c],[george, maria]).
expect([a,b,c,d],[george]).

test("movie stars", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_ugly(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_ugly).
````

That solution just feels too complex (and as we know, there is a nice, one-liner solution) 

Also, I'm not happy about the the temporary appearance of `[]` to stand for
a NULL, which then has to be filtered out afterwards. It works but feels
weird. It is due to the fact that `maplist/3` must always succeed
and is not free to leave out an element of the output list, which must have the same length
as the input list. Because this is a relation, not a function.

# Addendum: Solution in R

[It's a different approach, still compact](https://github.com/dtonhofer/rstudio_coding/blob/master/rdbms_like_operations.md)

# Addendum: Solution in SQL

This problem is squarely in the domain of relational algebra, and there is a sorta-kinda 
interface to implementations of that, namely SQL.

Here we are using [MariaDB](https://en.wikipedia.org/wiki/MariaDB)/MySQL SQL. [T-SQL](https://en.wikipedia.org/wiki/Transact-SQL) or [PL/SQL](https://en.wikipedia.org/wiki/PL/SQL) are more complete.

SQL loves to YELL AT THE TERMINAL IN PUNCHCARDSPEAK, so we continue in that tradition.

First, set up the facts.

- [Manual page for CREATE TABLE](https://mariadb.com/kb/en/create-table/)

````
CREATE OR REPLACE TABLE starsin 
   (movie CHAR(20) NOT NULL, actor CHAR(20) NOT NULL)
   INDEX (movie, actor) PRIMARY KEY;
INSERT INTO starsin VALUES
   ( "a" , "bob" ),
   ( "c" , "bob" ),
   ( "a" , "maria" ),
   ( "b" , "maria" ),
   ( "c" , "maria" ),
   ( "a" , "george" ),
   ( "b" , "george" ),
   ( "c" , "george" ),
   ( "d",  "george" );
````

Regarding the set of movies given as input, we assume it is given in the form of a (temporary) table.
That would be natural. In MySQL, "temporary tables" are
[local to the session](http://www.geeksengine.com/database/manage-table/create-temporary-table.php). Good.

One can set up this input table, `movies_in` like this:

````
CREATE OR REPLACE TEMPORARY TABLE movies_in
   (movie CHAR(20) PRIMARY KEY);
INSERT INTO movies_in VALUES ("a"), ("b");
````

Query and Test will be wrapped into procedures so that they can be called at will instead of being entered
again and again on the terminal. A [delimiter](https://stackoverflow.com/questions/10259504/delimiters-in-mysql)
is useful here.

## Solution 1 (empty set yields empty output ... bad!)

This one possibility of many. The results can be obtained by getting,

- for each actor
   - the intersection of the set of movies denoted by `movies_in` and
   - the set of movies in which an actor ever appeared (created for each actor via the inner join).
- then counting (for each actor) whether the resulting set has at least as many entries as the set `movies_in`.

- [Manual page for CREATE PROCEDURE](https://mariadb.com/kb/en/create-procedure/)

````
DELIMITER $$

CREATE OR REPLACE PROCEDURE 
   actors_appearing_in_movies()
BEGIN

SELECT 
     d.actor 
   FROM 
     starsin d, movies_in q
   WHERE 
     d.movie = q.movie 
   GROUP BY 
     actor 
   HAVING 
     COUNT(*) >= (SELECT COUNT(*) FROM movies_in);

END$$

DELIMITER ;
````     

Test it. There is no ready-to-use unit testing framework for MariaDB, so we "test by hand" and write a 
procedure, the out of which we check manually. Variadic arguments don't exist
(although once could pass a column maybe?). Let's accept up to 4 movies as input.

````
DELIMITER $$

DROP PROCEDURE IF EXISTS test_movies;

CREATE OR REPLACE PROCEDURE 
   test_movies(IN m1 CHAR(20),IN m2 CHAR(20),IN m3 CHAR(20),IN m4 CHAR(20))
BEGIN

   CREATE OR REPLACE TEMPORARY TABLE movies_in (movie CHAR(20) PRIMARY KEY);   
   CREATE OR REPLACE TEMPORARY TABLE args (movie CHAR(20));
   INSERT INTO args VALUES (m1),(m2),(m3),(m4); -- contains duplicates and NULLs
   INSERT INTO movies_in (SELECT DISTINCT movie FROM args WHERE movie IS NOT NULL); -- clean
   DROP TABLE args;   
   CALL actors_appearing_in_movies();        
END$$

DELIMITER ;
````

Run it:

````
CALL test_movies(NULL,NULL,NULL,NULL);
Empty set (0.002 sec)
````

This should NOT be the empty set, but the set of all actors.

It works otherwise:

````
CALL test_movies(NULL,NULL,"a","b");

+--------+
| actor  |
+--------+
| george |
| maria  |
+--------+
````

````
CALL test_movies("a","b","c","d");
+--------+
| actor  |
+--------+
| george |
+--------+
````

## Solution 2 (empty set yields all actors as output ... good!)

This solution involves counting, because we don't want to "lose" actor rows in joins.

````
DELIMITER $$

DROP PROCEDURE IF EXISTS actors_appearing_in_movies;

CREATE PROCEDURE actors_appearing_in_movies()
BEGIN

   -- collect all the actors
   DROP TABLE IF EXISTS tmp_actors;
   CREATE TEMPORARY TABLE tmp_actors (actor CHAR(20) PRIMARY KEY)
     AS SELECT DISTINCT actor from starsin;

   -- table of all actors x (movies+NULL) and a flag (that would inefficient in real life)
   DROP TABLE IF EXISTS tmp_score;
   CREATE TEMPORARY TABLE tmp_score 
     (actor CHAR(20), movie CHAR(20), needed BOOLEAN NOT NULL DEFAULT TRUE) 
     INDEX (actor, movie) PRIMARY KEY;
     AS SELECT DISTINCT actor from starsin;
   
SELECT 
     d.actor 
   FROM 
     starsin d, movies_in q
   WHERE 
     d.movie = q.movie 
   GROUP BY 
     actor 
   HAVING 
     COUNT(*) >= (SELECT COUNT(*) FROM movies_in);

END$$

DELIMITER ;

DELIMITER ;
````











# All the Prolog code in one place.

## "Nice" but hard-to-understand

````
starsin(a,bob).
starsin(c,bob).

starsin(a,maria).
starsin(b,maria).
starsin(c,maria).

starsin(a,george).
starsin(b,george).
starsin(c,george).
starsin(d,george).

actors_appearing_in_movies_nice(MovIn,ActOut) :-
    setof(
        Ax,
        MovAx^(setof(Mx,starsin(Mx,Ax),MovAx), subset(MovIn,MovAx)),
        ActOut
    ).    
    
:- begin_tests(exercise_nice).

expect([],[bob, george, maria]).
expect([a],[bob, george, maria]).
expect([a,b],[george, maria]).
expect([a,b,c],[george, maria]).
expect([a,b,c,d],[george]).

test("movie stars", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_nice(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_nice).

test_nice :- run_tests(exercise_nice).
````

## "Ugly" but readable

````
starsin(a,bob).
starsin(c,bob).

starsin(a,maria).
starsin(b,maria).
starsin(c,maria).

starsin(a,george).
starsin(b,george).
starsin(c,george).
starsin(d,george).


% Create a combination structure of [Actor, List-of-Movies-with-Actor]
% actors_with_movies(X).
% X = [[bob, [a, c]], [george, [a, b, c, d]], [maria, [a, b, c]]]

actors_with_movies(ActorsWithMovies) :-
   all_actors(ActorSet),
   maplist(combine_actor_with_movies,ActorSet,ActorsWithMovies).

% Just rearrange variables into a specific format 

combine_actor_with_movies(Actor,[Actor,MovieSet]) :-
    movies_of_actor(Actor,MovieSet).

% Collect all actors that exist by scanning the facts.
% all_actors(X).
% X = [bob, george, maria]

all_actors(ActorSet) :- 
    bagof(Actor,Movie^starsin(Movie,Actor),ActorBag),
    list_to_ord_set(ActorBag,ActorSet).

% Collect all the movies of a given actor.
% movies_of_actor(bob,[a,c]).
% movies_of_actor(maria,X).
% X = [a,b,c]

movies_of_actor(Actor,MovieSet) :-
    bagof(Movie,starsin(Movie,Actor),MovieBag),
    list_to_ord_set(MovieBag,MovieSet).

% Selection for "Actor".
% Always succeeds, but:
% If the "ActorIn" appears in all the movies listed in "MoviesMust",
% ... then ActorOut is unified with ActorIn
% ... otherwise, ActorOut is unified with []

select(MoviesMust,[ActorIn,MoviesAre],ActorIn) :- ord_subset(MoviesMust,MoviesAre),!.
select(_MoviesMust,[_ActorIn,_MoviesAre],[]).

% main predicate

actors_appearing_in_movies_ugly(MoviesIn,ActorsOut) :-
    list_to_ord_set(MoviesIn,MovieSet),
    actors_with_movies(ActorsWithMovies),
    format("~w\n",[ActorsWithMovies]),
    maplist(select(MovieSet),ActorsWithMovies,ActorsOutWithNulls),
    include(\=([]),ActorsOutWithNulls,ActorsOut).


:- begin_tests(exercise_ugly).

expect([],[bob, george, maria]).
expect([a],[bob, george, maria]).
expect([a,b],[george, maria]).
expect([a,b,c],[george, maria]).
expect([a,b,c,d],[george]).

test("movie stars", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_ugly(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_ugly).

test_ugly :- run_tests(exercise_ugly).
````

