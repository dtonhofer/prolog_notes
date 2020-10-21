# A discussion of a "Prolog Database" (relational) operation

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

Using [`library(plunit)`](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)), we can pack this into a unit test.

(Note the final `!` to make the test goal deterministic. Is that correct? I tdefinitely works. That clause is not really Prolog, it is Meta-Prolog.)

````
% You can put the "expect/2" fact outside or inside of the begin_tests/end_tests
% block, but if they are outside they cna reused by other tests.

expect([],[bob, george, maria]).
expect([a],[bob, george, maria]).
expect([a,b],[george, maria]).
expect([a,b,c],[george, maria]).
expect([a,b,c,d],[george]).

:- begin_tests(exercise_nice).

test("movie stars, nice", forall(expect(Movies,Actors))) :- 
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

## Alternate Solution

In Stack Overflow question "[Prolog Recursion through Set of](https://stackoverflow.com/questions/60644753/prolog-recursion-through-set-of)", user Rafe proposes the following approache, here slight rewritten:

````
actors_appearing_in_movies_alt(Movies, ActOut) :-
    setof(Actor, starsin_all(Movies, Actor), ActOut).

starsin_all([Movie | RestMovies], Actor) :-
    starsin(Movie, Actor),
    \+ (member(SomeMovie, RestMovies), \+ starsin(SomeMovie, Actor)).

% For "any actor appears in the list of empty movies", we cannot just write
% starsin_all([], _Actor).
% we must anchor the variable to a concrete actor

starsin_all([], Actor) :- starsin(_, Actor).

:- begin_tests(exercise_alt).

test("movie stars, alt", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_alt(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_alt).
````

````
?- run_tests(exercise_alt).
% PL-Unit: exercise_alt ..... done
% All 5 tests passed
true.
````

or

````
actors_appearing_in_movies_alt2(Movies, ActOut) :-
    setof(Actor, starsin_all2(Movies, Actor), ActOut).

starsin_all2([Movie | RestMovies], Actor) :-
    starsin(Movie, Actor),
    forall(member(SomeMovie, RestMovies), starsin(SomeMovie, Actor)).

starsin_all2([], Actor) :- starsin(_, Actor).

:- begin_tests(exercise_alt2).

test("movie stars, alt 2", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_alt2(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_alt2).
````

````
?- run_tests(exercise_alt2).
% PL-Unit: exercise_alt2 ..... done
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

test("movie stars, ugly", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_ugly(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_ugly).
````

Well, it works:

````
?- run_tests(exercise_ugly).
% PL-Unit: exercise_ugly [[bob,[a,c]],[george,[a,b,c,d]],[maria,[a,b,c]]]
.[[bob,[a,c]],[george,[a,b,c,d]],[maria,[a,b,c]]]
.[[bob,[a,c]],[george,[a,b,c,d]],[maria,[a,b,c]]]
.[[bob,[a,c]],[george,[a,b,c,d]],[maria,[a,b,c]]]
.[[bob,[a,c]],[george,[a,b,c,d]],[maria,[a,b,c]]]
. done
% All 5 tests passed
true.
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

Here we are using [MariaDB](https://en.wikipedia.org/wiki/MariaDB)/[MySQL](https://en.wikipedia.org/wiki/MySQL) SQL.
[T-SQL](https://en.wikipedia.org/wiki/Transact-SQL) or [PL/SQL](https://en.wikipedia.org/wiki/PL/SQL) are more complete.

SQL loves to YELL AT THE TERMINAL IN PUNCHCARDSPEAK, so we continue in that tradition.

- [Manual page for CREATE TABLE](https://mariadb.com/kb/en/create-table/)
- [Manual page for CREATE PROCEDURE](https://mariadb.com/kb/en/create-procedure/)
- [Manual page for data types in MariaDB](https://mariadb.com/kb/en/data-types/)

Note that SQL has no vector data types that can be passed to procedures. Gotta work without that.

First, set up the facts.

````
CREATE OR REPLACE TABLE starsin 
   (movie CHAR(20) NOT NULL, actor CHAR(20) NOT NULL, 
    PRIMARY KEY (movie, actor));

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
procedure, the out of which we check manually. Variadic arguments don't exist, vector data types don't exist.
Let's accept up to 4 movies as input and check the result manually. 

````
DELIMITER $$

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
2 rows in set (0.002 sec)
````

````
CALL test_movies("a","b","c","d");
+--------+
| actor  |
+--------+
| george |
+--------+
1 row in set (0.002 sec)
````

## Solution 2 (empty set yields all actors as output ... good!)

This solution involves counting and an `UPDATE`.

**Update**: You can make it basically a one-liner using the SQL from [this answer by Gordon Linoff](https://stackoverflow.com/questions/60604700/prolog-to-sql-any-way-to-improve-sql-code-for-unit-tests-and-fix-an-edge-case-e/60606025#60606025):

````
select 
   a.actor,
from 
   actor a left join starsin si
     on a.actor = si.actor 
        and si.movie in (select * from movies_in)
group 
   by a.actor
having
   count(si.movie) = (select count(*) from movies_in);
````

But here is the multitable solution:

````
DELIMITER $$

CREATE OR REPLACE PROCEDURE actors_appearing_in_movies()
BEGIN

   -- collect all the actors
   CREATE OR REPLACE TEMPORARY TABLE tmp_actor (actor CHAR(20) PRIMARY KEY)
     AS SELECT DISTINCT actor from starsin;

   -- table of "all actors x (input movies + '--' placeholder)"
   -- (combinations that are needed for an actor to show up in the result)
   -- and a flag indicating whether that combination shows up for real
   CREATE OR REPLACE TEMPORARY TABLE tmp_needed 
     (actor CHAR(20), 
      movie CHAR(20), 
      actual TINYINT NOT NULL DEFAULT 0,
     PRIMARY KEY (actor, movie))
   AS 
     (SELECT ta.actor, mi.movie FROM tmp_actor ta, movies_in mi)
     UNION
     (SELECT ta.actor, "--" FROM tmp_actor ta);
   
   -- SELECT * FROM tmp_needed;
   
   -- Mark those (actor, movie) combinations which actually exist with a numeric 1
   UPDATE tmp_needed tn SET actual = 1 WHERE EXISTS
      (SELECT * FROM starsin si WHERE si.actor = tn.actor AND si.movie = tn.movie);

   -- SELECT * FROM tmp_needed;

   -- The result is the set of actors in "tmp_needed" which have as many entries
   -- flagged "actual" as there are entries in "movies_in"
   
   SELECT actor FROM tmp_needed GROUP BY actor 
      HAVING SUM(actual) = (SELECT COUNT(*) FROM movies_in);
   
END$$

DELIMITER ;
````

The above passes all the manual tests, in particular:

````
CALL test_movies(NULL,NULL,NULL,NULL);

+--------+
| actor  |
+--------+
| bob    |
| george |
| maria  |
+--------+
3 rows in set (0.003 sec)
````

For example, for `CALL test_movies("a","b",NULL,NULL);`

First set up the table with all actors against in all the movies in the input set, including the 
"doesn't exist" movie represented by a placeholder `--`. 

````
+--------+--------+-------+
| actual | actor  | movie |
+--------+--------+-------+
|      0 | bob    | --    |
|      0 | bob    | a     |
|      0 | bob    | b     |
|      0 | george | --    |
|      0 | george | a     |
|      0 | george | b     |
|      0 | maria  | --    |
|      0 | maria  | a     |
|      0 | maria  | b     |
+--------+--------+-------+
````

Then mark those rows with a 1 where the actor-movie combination actually exists in `starsin`.

````
+--------+--------+-------+
| actual | actor  | movie |
+--------+--------+-------+
|      0 | bob    | --    |
|      1 | bob    | a     |
|      0 | bob    | b     |
|      0 | george | --    |
|      1 | george | a     |
|      1 | george | b     |
|      0 | maria  | --    |
|      1 | maria  | a     |
|      1 | maria  | b     |
+--------+--------+-------+
````

Finally select an actor for inclusion in the solution if the `SUM(actual)` is equal to the
number of entries in the input movies table (it cannot be larger), as that means that the
actor indeed appears in all movies of the input movies table. In the special case where that
table is empty, the actor-movie combination table will only contain

````
+--------+--------+-------+
| actual | actor  | movie |
+--------+--------+-------+
|      0 | bob    | --    |
|      0 | george | --    |
|      0 | maria  | --    |
+--------+--------+-------+
````

and thus all actors will be selected, which is what we want.

# Collected code

## Prolog "nice" solution

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

expect([],[bob, george, maria]).
expect([a],[bob, george, maria]).
expect([a,b],[george, maria]).
expect([a,b,c],[george, maria]).
expect([a,b,c,d],[george]).


actors_appearing_in_movies_nice(MovIn,ActOut) :-
    setof(
        Ax,
        MovAx^(setof(Mx,starsin(Mx,Ax),MovAx), subset(MovIn,MovAx)),
        ActOut
    ).    

:- begin_tests(exercise_nice).

test("movie stars, nice", forall(expect(Movies,Actors))) :- 
   actors_appearing_in_movies_nice(Movies,ActOut),permutation(ActOut,Actors),!. 

:- end_tests(exercise_nice).
````

````
run_tests(exercise_nice).
````

# All the SQL code in one place.

````
CREATE OR REPLACE TABLE starsin 
   (movie CHAR(20) NOT NULL, actor CHAR(20) NOT NULL, 
    PRIMARY KEY (movie, actor));

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

DELIMITER $$

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

DELIMITER $$

CREATE OR REPLACE PROCEDURE actors_appearing_in_movies()
BEGIN

   -- collect all the actors
   CREATE OR REPLACE TEMPORARY TABLE tmp_actor (actor CHAR(20) PRIMARY KEY)
     AS SELECT DISTINCT actor from starsin;

   -- table of "all actors x (input movies + '--' placeholder)"
   -- (combinations that are needed for an actor to show up in the result)
   -- and a flag indicating whether that combination shows up for real
   CREATE OR REPLACE TEMPORARY TABLE tmp_needed 
     (actor CHAR(20), 
      movie CHAR(20), 
      actual TINYINT NOT NULL DEFAULT 0,
     PRIMARY KEY (actor, movie))
   AS 
     (SELECT ta.actor, mi.movie FROM tmp_actor ta, movies_in mi)
     UNION
     (SELECT ta.actor, "--" FROM tmp_actor ta);
   
   -- SELECT * FROM tmp_needed;
   
   -- Mark those (actor, movie) combinations which actually exist with a numeric 1
   UPDATE tmp_needed tn SET actual = 1 WHERE EXISTS
      (SELECT * FROM starsin si WHERE si.actor = tn.actor AND si.movie = tn.movie);

   -- SELECT * FROM tmp_needed;

   -- The result is the set of actors in "tmp_needed" which have as many entries
   -- flagged "actual" as there are entries in "movies_in"
   
   SELECT actor FROM tmp_needed GROUP BY actor 
      HAVING SUM(actual) = (SELECT COUNT(*) FROM movies_in);
   
END$$

DELIMITER ;

CALL test_movies(NULL,NULL,NULL,NULL);
CALL test_movies(NULL,NULL,NULL,"a");
CALL test_movies(NULL,NULL,"a","b");
CALL test_movies(NULL,"a","b","c");
CALL test_movies("a","b","c","d");
````

