% ===
% A simple exercise in backtracking
% ===

% ===
% Acquiring the Holy Grail is our only hope
% ===

acquire_grail :- 
   knights(Knights),
   member(Knight,Knights),
   %
   % Printing a death announcement prior to getting the next knight
   % online is a bit hard due to backtracking! How do I do that simply?
   %
   format("~q rides forth!\n",[Knight]),
   search_for_the_grail(Knight,[],Map),
   !, % Victory Cut!
   announce(found,Knight,Map).
                        
acquire_grail :- 
   announce(its_all_over,_,_),fail.

% ===
% The King's Messenger proclaims
% ===
   
announce(found,Who,Map) :- 
  format("The Holy Grail has been found by Brave Knight ~q!\n",[Who]),
  format("Here is how to get there: ~q\n",[Map]).
  
announce(its_all_over,_,_) :- 
  format("Camelot is finished!\n").

% ===
% These Knights hang around the round table usually
% ===

knights([gavain,lancelot,perceval]).

% ===
% The Holy Grail is found after pressing forward 6 times
% But at each move, the Knight has 1/3 chance of dying 
% ===

search_for_the_grail(Knight,Acc,Acc) :-    
   length(Acc,6),
   !, % guard cut
   format("~q: 'God be praised!'\n",[Knight]).
   
search_for_the_grail(Knight,Acc,Map) :- 
   random(0.0,1.0,X), 
   X < 0.666,             
   !, % guard cut
   format("~q: 'The search continues!'\n",[Knight]),
   search_for_the_grail(Knight,[fwd|Acc],Map). 
      
search_for_the_grail(Knight,_,_) :- 
  format("~q: 'Arrgh!'\n",[Knight]),
  fail.
  
