Inspired by this StackOverflow question:

[Find mutual element in different facts in swi-prolog](https://stackoverflow.com/questions/60582295/find-mutual-element-in-different-facts-in-swi-prolog)

which led to:

[setof/3 inside setof/3 not working, but why?](https://stackoverflow.com/questions/60591072/setof-3-inside-setof-3-not-working-but-why)

# Problem statement

Given a database of "actors starring in movies":

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
````

And given set of movies, find those actors that starred in all the movies of said set.

I first had an ugly solution, but then...

# Solution that is nice

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

Wrap up into predicate:

````
actors_appearing_in_movies(MovIn,ActOut) :-
   setof(Ax, ( setof(Mx,starsin(Mx,Ax),MovAx) , subset(MovIn, MovAx) ) , ActOut).
````

Unfortunately the above doesn't work.

There is backtracking going on, apparently I need to wrap everything into another `setof/3`,
but why?? 

````
?- actors_appearing_in_movies([a,b],ActOut).
ActOut = [maria] ;
ActOut = [george].
````

The following **does** work:

````
subselect(Ax,MovIn) :- 
   setof(Mx,starsin(Mx,Ax),MovAx), subset(MovIn, MovAx).
actors_appearing_in_movies(MovIn,ActOut) :- 
   setof(Ax, subselect(Ax,MovIn) , ActOut).
````

````
?- actors_appearing_in_movies([a,b],ActOut).
ActOut = [george, maria].
````

## Testing

Testing is just running a few goals.

Note that for the empty set of movies, we get all the actors. This is arguably correct: 
every actors stars in all the movies of the empty set.

````
actors_appearing_in_movies([],ActOut),permutation([bob, george, maria],ActOut),!. 
actors_appearing_in_movies([a],ActOut),permutation([bob, george, maria],ActOut),!.
actors_appearing_in_movies([a,b],ActOut),permutation([george, maria],ActOut),!.
actors_appearing_in_movies([a,b,c],ActOut),permutation([george, maria],ActOut),!.
actors_appearing_in_movies([a,b,c,d],ActOut),permutation([george],ActOut),!.
````

# Solution that is ugly

What you want are _vector operations_. 

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

% A predicate (in the sense of filter function) to filter the empty list.

not_empty_list([]) :- !,fail.
not_empty_list(_).

% Selection for "Actor".
% Always succeeds, but:
% If the "ActorIn" appears in all the movies listed in "MoviesMust",
% ... then ActorOut is unified with Actor
% ... otherwise, ActorOut is unified with [], a NULL value

select(MoviesMust,[ActorIn,MoviesAre],ActorOut) :-
    format("MoviesMust = ~w, ActorIn = ~w, MoviesAre = ~w\n",
           [MoviesMust,ActorIn,MoviesAre]),
    (ord_subset(MoviesMust,MoviesAre) 
     -> 
     ActorIn = ActorOut
     ;
     ActorOut = []).

% ***MAIN PREDICATE***

actors_appearing_in_movies_ugly(MoviesIn,ActorsOut) :-
    list_to_ord_set(MoviesIn,MovieSet),
    actors_with_movies(ActorsWithMovies),
    format("~w\n",[ActorsWithMovies]),
    maplist(select(MovieSet),ActorsWithMovies,ActorsOutWithNulls),
    include(not_empty_list,ActorsOutWithNulls,ActorsOut).
````

## Testing

Same as earlier:

````
actors_appearing_in_movies_ugly([],ActOut),permutation([bob, george, maria],ActOut),!. 
actors_appearing_in_movies_ugly([a],ActOut),permutation([bob, george, maria],ActOut),!.
actors_appearing_in_movies_ugly([a,b],ActOut),permutation([george, maria],ActOut),!.
actors_appearing_in_movies_ugly([a,b,c],ActOut),permutation([george, maria],ActOut),!.
actors_appearing_in_movies_ugly([a,b,c,d],ActOut),permutation([george],ActOut),!.
````

## Notes:

That solution just feels too complex (and as we know, there is a nice, one-liner solution) 

Also, I'm not happy about the the temporary appearance of `[]` to stand for
a NULL, which then has to be filtered out afterwards. It works but feels
weird. It is actually due to the fact that `maplist/3` must always succeed
and cannot decide to "leave out" en element of the output list, as can be
done in functional programming. Being a relation relating elements of the
input list to elements of the output list, the input list and the output
list must have the same length. Hence, NULL placeholders.
