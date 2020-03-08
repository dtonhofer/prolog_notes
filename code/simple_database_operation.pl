% https://stackoverflow.com/questions/60582295/find-mutual-element-in-different-facts-in-swi-prolog

% actors_appearing_in_movies(MoviesIn,ActorsOut)
%
% This is too complex.
%

starsin(a,bob).
starsin(c,bob).

starsin(a,maria).
starsin(b,maria).
starsin(c,maria).

starsin(a,george).
starsin(b,george).
starsin(c,george).
starsin(d,george).

% actors_with_movies(X).
% X = [[bob, [a, c]], [george, [a, b, c, d]], [maria, [a, b, c]]]

actors_with_movies(ActorsWithMovies) :-
   all_actors(ActorSet),
   maplist(combine_actor_with_movies,ActorSet,ActorsWithMovies).

% just rearrange 

combine_actor_with_movies(Actor,[Actor,MovieSet]) :-
    movies_of_actor(Actor,MovieSet).

% all_actors(X).
% X = [bob, george, maria]

all_actors(ActorSet) :- 
    bagof(Actor,Movie^starsin(Movie,Actor),ActorBag),
    list_to_ord_set(ActorBag,ActorSet).

% movies_of_actor(bob,[a,c]).
% movies_of_actor(maria,X).
% X = [a,b,c]

movies_of_actor(Actor,MovieSet) :-
    bagof(Movie,starsin(Movie,Actor),MovieBag),
    list_to_ord_set(MovieBag,MovieSet).

% filter the empty list

not_empty_list([]) :- !,fail.
not_empty_list(_).

% Selection for "Actor".
% Always succeeds, but:
% If the "ActorIn" appears in all the movies listed in "MoviesMust",
% ActorOut is Actor
% Otherwise, ActorOut is [], a placeholder for NULL.

select(MoviesMust,[ActorIn,MoviesAre],ActorOut) :-
    format("MoviesMust = ~w, ActorIn = ~w, MoviesAre = ~w\n",
           [MoviesMust,ActorIn,MoviesAre]),
    (ord_subset(MoviesMust,MoviesAre) -> 
       ActorIn = ActorOut ; ActorOut = []).

% Call this

actors_appearing_in_movies(MoviesIn,ActorsOut) :-
    list_to_ord_set(MoviesIn,MovieSet),
    actors_with_movies(ActorsWithMovies),
    format("~w\n",[ActorsWithMovies]),
    maplist(select(MovieSet),ActorsWithMovies,ActorsOutWithNulls),
    include(not_empty_list,ActorsOutWithNulls,ActorsOut).
    
