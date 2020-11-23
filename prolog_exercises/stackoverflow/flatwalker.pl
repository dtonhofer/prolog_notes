% https://stackoverflow.com/questions/64936541/splice-in-sublists-in-a-list-without-using-flatten-2

:- debug(flatwalker).

flatwalker(ListIn,ListOut) :-
   Tip=Fin,                            % 2 unbound variables
   %                                   % designating the same memory
   %                                   % cell. Hold the Tip, grow at Fin.
   flatwalker_2(0,ListIn,Fin,TerFin),  % Elements are appended at Fin.
   %                                   % The final Fin is found in TerFin
   %                                   % on success.
   TerFin=[],                          % Unify TerFin with [], closing
   %                                   % the list at Tip.
   keysort(Tip,Sorted),                % Sort the closed list at Tip
   %                                   % by pair key, i.e. by depth.
   %                                   % keysort/2 is stable and keeps
   %                                   % duplicates.
   debug(flatwalker,"Got : ~q",[Tip]),
   maplist([_-V,V]>>true,Sorted,ListOut). % Remove depth values.

% ---
% flatwalker_2(+Depth,+TreeIn,+Fin,+TerFin)
% Depth:  Input integer, indicates current tree depth.
% TreeIn: The list to flatten at this depth (it's a node of the tree,
%         which may or may not contain subtrees, i.e. lists)
% Fin:    Always an unbound variable denoting the end of an open list to
%         which we will append.
%         ("points to an empty memory cell at the fin of the open list")
%         Works as an accumulator as a new Fin, advanced by 1 cell at each
%         append operation is handed to the next flatwalker_2/4
%         activation.
% TerFin: When flatwalker_2/ is done, the final Fin is unified with 
%         TerFin so that it can be passed to flatwalker/2.
% ---

% We make the guards explicit and cut heavily.
% Optimizing the guards (if so desired) is left as an exercise.

flatwalker_2(_,[],Fin,Fin) :- !.       % Done as TreeIn is empty. 
                                       % Unify Fin with TerFin.

flatwalker_2(0,[X|Xs],Fin,TerFin) :-   % Case of X is nonlist at depth 0:
   %                                   % discard!
   \+is_list(X),!,
   flatwalker_2(0,Xs,Fin,TerFin).      % Continue with the rest of the
                                       % list at this depth.

flatwalker_2(D,[X|Xs],Fin,TerFin) :-   % Case of X is nonlist at
   %                                   % depth > 0: keep!
   D>0,\+is_list(X),!,
   Fin=[D-X|Fin2],                     % Grow the result list at its
   %                                   % Fin by D-X.
   flatwalker_2(D,Xs,Fin2,TerFin).     % Continue with the rest of the
                                       % list at this depth.

flatwalker_2(D,[X|Xs],Fin,TerFin) :-   % Case of X is a list at any
   %                                   % depth.
   is_list(X),!,
   DD is D+1,
   flatwalker_2(DD,X,Fin,Fin2),        % Collect one level down
   flatwalker_2(D,Xs,Fin2,TerFin).     % On return, continue with the 
                                       % rest of the list at this depth.
