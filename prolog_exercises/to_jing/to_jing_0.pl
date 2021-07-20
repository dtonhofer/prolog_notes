/*

"Escape to Jing Provice" in Prolog, data for example 0

?- main.
_ _ _ _ ∆
_ ⊞ ∆ _ _
_ _ ⊞ _ _
_ ∿ _ ⊞ ⫛
∆ ∿ ⫛ _ _
% 51,862 inferences, 0.019 CPU in 0.019 seconds (99% CPU, 2742625 Lips)
Soldiers : 21
Cities   : 1
Delay    : 8
Path     : [[5,5,start],[5,4,west],[4,4,north],[4,3,west],[4,2,west],[3,2,north],[3,1,west]]
_ _ _ _ ∆
_ ⊞ ∆ _ _
← ↑ ⊞ _ _
_ ← ← ↑ ⫛
∆ ∿ ⫛ ← ⊗
true.


*/

% Terrain is a matrix of cells.
% The 1-based row is first coordinate going from 1 to nrow (nrow does not need to be specified).
% The 1-based column is second coordinate going from 1 to ncol (ncol does not need to be specified)
% The terrain matrix can be naturally represented as a list of rows, which are lists of terrain type.


terrain( [[ p, p, p, p, m ],
          [ p, c, m, p, p ],
          [ p, p, c, p, p ],
          [ p, r, p, c, f ],
          [ m, r, f, p, p ]] ).

% Each terrain type has an associated delay value.

delay(p,1).
delay(m,9).
delay(f,3).
delay(c,1).
delay(r,2).

% This defines which cells of the terrain belong to Jing Province.

jin(  [[  true, true,  true,  false, false ],
       [  true, false, false, false, false ],
       [  true, false, false, false, false ],
       [ false, false, false, false, false ],
       [ false, false, false, false, false ]] ).

% This defines how many soliders are in each cell of the terrain.
%
% The soldiers matrix gives an optimization goal:
%
% > the sum of the soldiers found in each cell visited on a valid path (including the 1st and last)
%   must be minimized

soldier( [[ 3,1,4,8,1 ],
          [ 2,1,9,5,4 ],
          [ 6,1,4,8,1 ],
          [ 3,1,7,1,2 ],
          [ 6,1,2,4,1 ]] ).

% Where Liu Bei sets off.
% If he starts off at a mountain cell, there is no solution.
% If he starts off in Jing Province, he's done immediately.

const(start_row,5).
const(start_col,5).

% Limits for a valid path:
%
% In a valid path, the sum of the delays for each cell visited (including the 1st and last)
% must be <= timelimit
% In a valid path, the sum of the delays for each cell visited  (including the 1st and last)
% must be <= maxstep

const(timelimit,8).
const(maxstep,8).
