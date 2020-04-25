# Notes on `library(aggregate)`

IN PROGRESS

See: [`library(aggregate)`](https://www.swi-prolog.org/pldoc/man?section=aggregate).

This library provides some aggregation operators on backtrackable predicates. 

It saves you from having to write your own `min`,`max`, etc. aggregators that "work over a predicate" and does so efficiently (to be tested), as opposed tothose "that work over a list". 

On the other hand, you can always transform a list into a backtrackable predicate, and a backtrackable predicate into a list, but at the cost of CPU and RAM.

For testing, some data can be obtain by using the "mtcars" dataset which comes with R, suitable written in Prolog: [mtcars.pl](mtcars.pl).

Load it into Prolog!

Helper predicates in mtcars_help.pl

```logtalk
% Backtrackable predicates that get certain values for each car.
% In relational theory, these are projections (but they keep the duplicates)

car_name_mpg(Name,Mpg) :- mtcars(L),memberchk(name(Name),L),memberchk(mpg(Mpg),L).
car_name_cyl(Name,Cyl) :- mtcars(L),memberchk(name(Name),L),memberchk(cyl(Cyl),L).
car_cyl_mpg(Cyl,Mpg)   :- mtcars(L),memberchk(mpg(Mpg),L),memberchk(cyl(Cyl),L).

% Examine the result of the aggregate predicates, which have varying type

unpump(X) :- is_list(X),!,length(X,LL),format("Result: a list of length ~w\n",LL).
unpump(X) :- number(X),!,format("Result: the value ~w\n",X).
```

Load them all

```
?- [mtcars],[mtcars_help].
```

Several aspects come together:

- `setof`/`bagof` vs. `findall`
- `setof`/`bagof` "existential quantification" to close over certain variables: `X^bagof(...`
   - which is pretty good but also dangerous as you can write Prolog statements that do only make marginal sense and the compiler doesn't warn you
- the "discriminator"
- the templating "set" or "bag"

Notes on stuff that seems useful

- A `limit` parameter that cuts off excessive solutions
- Aggregation possibility to do more complex computation, like computing the standard deviation
- But then maybe connecting to R directly would be good idea? Then you can deploy the tidyverse for dataframe manipulation-

Let's see what we can test.

Example using purely setof/bagof:

```logtalk
?- bagof([N,M],car_name_mpg(N,M),L),unpump(L).
Result: a list of length 32
L = [['Mazda RX4', 21.0], ['Mazda RX4 Wag', 21.0], ['Datsun 710', 22.8], ['Hornet 4 Drive', 21.4],...

?- bagof([C,M],car_cyl_mpg(C,M),L),unpump(L).
Result: a list of length 32
L = [[6, 21.0], [6, 21.0], [4, 22.8], [6, 21.4], [8, 18.7], [6, 18.1],...
```

Aggregating

```logtalk
?- aggregate(bag([C,M]),car_cyl_mpg(C,M),R),unpump(R).
Result: a list of length 32
R = [[6, 21.0], [6, 21.0], [4, 22.8], [6, 21.4], [8, 18.7], [6, 18.1], [8, 14.3], [4|...], [...|...]|...].

?- aggregate(set([C,M]),car_cyl_mpg(C,M),R),unpump(R).
Result: a list of length 27
R = [[4, 21.4], [4, 21.5], [4, 22.8], [4, 24.4], [4, 26.0], [4, 27.3], [4, 30.4], [4|...], [...|...]|...].

?- aggregate(min(M),C^car_cyl_mpg(C,M),R),unpump(R).  % C is kept as "internal variable", otherwise Prolog will backtrack over it
Result: the value 10.4
R = 10.4.

?- aggregate(max(M),C^car_cyl_mpg(C,M),R),unpump(R).  % Same
Result: the value 33.9
R = 33.9.

?- aggregate(avg(M),C^car_cyl_mpg(C,M),R),unpump(R).
false.

% Huh?

?- aggregate(min(sqrt(M)),N^car_name_mpg(N,M),L),unpump(L).
Result: the value 3.22490309931942
L = 3.22490309931942.

% How about the itness?

?- aggregate(min(M,N),car_name_mpg(N,M),L),unpump(L).
false.

% Duh?

```

       

