% Backtrackable predicates that get certain values for each car.
% In relational theory, these are projections (but they keep the duplicates)

car_name_mpg(Name,Mpg) :- mtcars(L),memberchk(name(Name),L),memberchk(mpg(Mpg),L).
car_name_cyl(Name,Cyl) :- mtcars(L),memberchk(name(Name),L),memberchk(cyl(Cyl),L).
car_cyl_mpg(Cyl,Mpg)   :- mtcars(L),memberchk(mpg(Mpg),L),memberchk(cyl(Cyl),L).

% Examine the result of the aggregate predicates, which have varying type

unpump(X) :- is_list(X),!,length(X,LL),format("Result: a list of length ~w\n",LL).
unpump(X) :- number(X),!,format("Result: the value ~w\n",X).

