https://stackoverflow.com/questions/67984333/how-to-split-a-list-into-list-of-list-by-delimiter-in-prolog


% flatten(ListOfLists,ListWithCommas)

flatten([Sublist1|[Sublist2|More]],MergedList) :-
   !, % not needed, but we want to tell Prolog to not 
      % look for more solutions in the next two clauses
      % because there aren't any
   append(Sublist1,[~],Part1),
   flatten([Sublist2|More],Part2),    
   append(Part1,Part2,MergedList).
flatten([Sublist],Sublist).
flatten([],[]).

:- begin_tests(flatten).

test(a1) :- 
   flatten([],Result),
   assertion(Result == []).
   
test(a2) :- 
   flatten([[a,b,c]],Result),
   assertion(Result == [a,b,c]).

test(a3) :- 
   flatten([[]],Result),
   assertion(Result == []).

test(a4) :- 
   flatten([[a,b,c],[d,e,f]],Result),
   assertion(Result == [a,b,c,~,d,e,f]).

test(a5) :- 
   flatten([[a,b,c],[d,e,f],[g,h,i]],Result),
   assertion(Result == [a,b,c,~,d,e,f,~,g,h,i]).

test(a6) :- 
   flatten([[a,b,c],[],[g,h,i]],Result),
   assertion(Result == [a,b,c,~,~,g,h,i]).
   
:- end_tests(flatten).

  
heighten([],[]).  
heighten([Sublist],Sublist) :- 
   \+ member(~,Sublist),
   !.                                % Not needed, but tell Prolog to not look
                                     % for another solution via the next clause because there isn't one
heighten([Sublist1|[Sublist2|More]],MergedList) :-
   append(Part1,Part2,MergedList),   % Propose how to split MergedList into Part1 and Part2   
   append(Sublist1,[~],Part1),       % We demand that Part1 end with ∆, which also gives us Sublist1
   \+ member(~,Sublist1),            % And ~ is itself not member of Sublist1
   !,                                % Not needed, but tell Prolog to not backtrack
                                     % past this point, because there are no more solutions there
   heighten([Sublist2|More],Part2).
   
   
:- begin_tests(heighten).

test(b1) :- 
   bagof(Prior,heighten(Prior,[]),Bag),
   assertion(Bag == [ [] ,  [[]]] ). % clearly this mapping is not bijective
   
test(b2) :- 
   heighten(Prior,[a,b,c]),
   assertion(Prior == [[a,b,c]]).

test(b4) :- 
   heighten(Prior,[a,b,c,~,d,e,f]),
   assertion(Prior == [[a,b,c],[d,e,f]]).

test(b5) :- 
   heighten(Prior,[a,b,c,~,d,e,f,~,g,h,i]),
   assertion(Prior == [[a,b,c],[d,e,f],[g,h,i]]).

test(b6) :- 
   heighten(Prior,[a,b,c,~,~,g,h,i]),
   assertion(Prior == [[a,b,c],[],[g,h,i]]).
   
:- end_tests(heighten).

   


