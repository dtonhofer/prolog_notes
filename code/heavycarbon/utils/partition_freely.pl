:- module(heavycarbon_partition_freely,
          [
              partition_freely/4
          ]).

% ---
% Partition elements from "List" into an unspecified number of partitions.
% ---
% The "PartitionPred" is the predicate (or the closure) which yields the 
% partition's "Key" for each "Element" of "List", being called like this:
%
% PartitionPred(Element,Key)
%
% The "PartitionDict" is the result, holding, at each "Key", the list of
% elements from "List" assigned to that "Key" by "PartitionPred". The "Tag"
% of the "PartitionDict" is unified with "Tag".

partition_freely(PartitioningPred, List, Tag, PartitionDict) :-
   PartitionInFoldClosure=partition_in_foldl(PartitioningPred),
   foldl(PartitionInFoldClosure,List,preliminary{},MidDict),  % partition into a preliminary dict, "MidDict"
   dict_pairs(MidDict,_Tag,Pairs),                            % extract "Key-Value" pairs from "MidDict"   
   foldl(rebuild_in_foldl,Pairs,Tag{},PartitionDict).         % rebuild preliminary dict to the final dict
            
% ---
% Called as foldl/4 goal: partition_in_foldl(How,PartitioningPred,Element,FromLeft,ToRight)
% ---
% - "How" indicates wheter we want the offensively coded or defensively
%   coded version.
% - "PartitioningPred" is the predicate that will be called to obtain the
%   partitoning key through a call to "PartitioningPred(Element,Key)".
% - "Element" is the current element from the input list that shall be assigned
%   to its partition.
% - "FromLeft" is a dict with keys the partitioning keys encountered up to now,
%   and values two-element lists "[Tip,Fin]" describing a nonempty open list:
%   "Tip" is the first listbox of a (nonempty) open list, and "Fin" is the cell
%   after the last listbox of that list, a "hole". The open list lists all the
%   values found to belong to the partition identified by "Key" so far.

partition_in_foldl(PartitioningPred,Element,FromLeft,ToRight) :-
   partition_in_foldl_2(PartitioningPred,Element,FromLeft,ToRight)
   -> true
   ; (format(string(Txt),"partition_in_foldl/4: Element = ~q, FromLeft = ~q, ToRight = ~q",[Element,FromLeft,ToRight]),
      throw(Txt)).
   
partition_in_foldl_2(PartitioningPred,Element,FromLeft,ToRight) :-
   call(PartitioningPred,Element,Key),
   (get_dict(Key,FromLeft,Value)
    -> partitioning_key_seen(Element,Key,Value,FromLeft,ToRight)
    ;  partitioning_key_new(Element,Key,FromLeft,ToRight)).
   
partitioning_key_seen(Element,Key,Value,FromLeft,ToRight) :-
   Value   = [Tip,Fin],                         % "Value" holds a 2-element list with the "Tip" and "Fin" of an open list
   assertion(var(Fin)),                         % "Fin" must be an unbound variable at this point
   Fin     = [Element|NewFin],                  % Append "Element" in the partition's list at "Fin", the new fin is the freshvar "NewFin"
   put_dict(Key,FromLeft,[Tip,NewFin],ToRight). % An updated dictionary goes "rightwards" in foldl/4
   
partitioning_key_new(Element,Key,FromLeft,ToRight) :-
   Tip = [Element|Fin],                         % A new open list at "Tip" with "Element" as single element and "Fin" as terminal "hole"
   put_dict(Key,FromLeft,[Tip,Fin],ToRight).    % An updated dictionary goes "rightwards" in foldl/4

% ---
% Called as foldl/4 goal: rebuild_in_foldl(How,Element,FromLeft,ToRight)
% ---
% - "How" indicates wheter we want the offensively coded or defensively
%   coded version.
% - "Element" is a pair "Key-Value" from the initially constructed dict.
%   Here, "Key" and "Value" have the meaning expected for a dict pair.
%   The "Value" is a 2-element list "[Tip,Fin]" where "Tip" is the first 
%   listbox of a (nonempty) open list, and "Fin" is the cell after the last
%   listbox of that list, which, as this is an open list, is a "hole",
%   and not yet "[]".
%   The open list is transformed into a proper list by unifying "Fin" with [].
%   The resulting list is then added to a new dict that is handed rightwards.
% - FromLeft: The previous so-far-rebuilt dict containing only proper lists 
%   as values.
% - ToRight: A new so-far-rebuilt dict containing only proper lists as values,
%   with the new entry for "Key".
% ---

% ~~~
% Defensively coded:
% ~~~

rebuild_in_foldl(Element,FromLeft,ToRight) :-
   rebuild_in_foldl_2(Element,FromLeft,ToRight) 
   -> true                               % If all of that succeeded, we are good!
   ; (format(string(Txt),"rebuild_in_foldl/3 failure: Element = ~q, FromLeft = ~q, ToRight = ~q",[Element,FromLeft,ToRight]),throw(Txt)).

/*   
rebuild_in_foldl_2((Key-[Tip,Fin]),FromLeft,(FromLeft.put([Key=Tip]))) :- 
   Fin = [].
*/

rebuild_in_foldl_2(Element,FromLeft,ToRight) :-
   Element = Key-Value,                 % Element must be a Key-Value pair from the original dict
   Value   = [Tip,Fin],                 % The Value holds a 2-element list with the Tip and Fin of an open list
   assertion(var(Fin)),                 % Fin must be an unbound variable at this point
   Fin     = [],                        % Tip is now a proper list terminating in the empty list at Fin
   ToRight = FromLeft.put([Key=Tip]).   % The dict sent to the right is the dict received from the left plus the new pair




   
