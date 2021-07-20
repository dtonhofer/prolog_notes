/*

"Escape to Jing Provice" in Prolog, data for example 3

?- main.
_ _ _ ∿ ⊞ ⫛ ⫛ ⫛
_ ∆ ∆ ⫛ ∆ ⊞ ∆ _
_ ∆ _ ⫛ ⫛ _ _ _
_ ∆ _ ∆ ∆ ⊞ ∆ ∆
_ ∆ _ ∆ _ ⫛ ∿ ∿
_ ⊞ _ ∆ _ ⫛ ⫛ _
_ _ _ ∆ ⊞ ⫛ ⫛ _
_ _ _ ∆ _ _ _ _
% 95,295 inferences, 0.036 CPU in 0.036 seconds (99% CPU, 2663526 Lips)
Soldiers : 17
Cities   : 1
Delay    : 29
Path     : [[1,1,start],[1,2,east],[1,3,east],[1,4,east],[2,4,south],[3,4,south],[3,5,east],[3,6,east],[4,6,south],[5,6,south],[5,7,east],[6,7,south],[7,7,south],[8,7,south],[8,6,west]]
⊗ → → → ⊞ ⫛ ⫛ ⫛
_ ∆ ∆ ↓ ∆ ⊞ ∆ _
_ ∆ _ ↓ → → _ _
_ ∆ _ ∆ ∆ ↓ ∆ ∆
_ ∆ _ ∆ _ ↓ → ∿
_ ⊞ _ ∆ _ ⫛ ↓ _
_ _ _ ∆ ⊞ ⫛ ↓ _
_ _ _ ∆ _ ← ↓ _
true.

*/

terrain([[ p, p, p, r, c, f, f, f ],
         [ p, m, m, f, m, c, m, p ],
         [ p, m, p, f, f, p, p, p ],
         [ p, m, p, m, m, c, m, m ],
         [ p, m, p, m, p, f, r, r ],
         [ p, c, p, m, p, f, f, p ],
         [ p, p, p, m, c, f, f, p ],
         [ p, p, p, m, p, p, p, p ]]).

delay(p,1).
delay(m,9).
delay(f,3).
delay(c,1).
delay(r,2).

jin([[ false, false, false, false, false, false, false, false ],
     [ false, false, false, false, false, false, false, false ],
     [ false, false, false, false, false, false, false, false ],
     [ false, false, false, false, false, false, false, false ],
     [ false, false, false, false, false, false, false, false ],
     [ false, false, false, false, false, false, false, false ],
     [ false, false, false, false, false, false, false, false ],
     [ false, false, false, true,  true,  true,  false, false ]]).

soldier( [[ 0,2,3,0,0,0,4,5 ],
          [ 0,0,0,5,0,0,3,6 ],
          [ 0,5,0,0,1,0,1,2 ],
          [ 0,0,0,0,0,1,6,1 ],
          [ 0,0,5,3,0,0,2,1 ],
          [ 1,4,0,0,0,8,0,1 ],
          [ 4,2,0,0,0,2,3,3 ],
          [ 0,0,0,0,0,0,0,0 ]] ).

const(start_row,1).
const(start_col,1).

const(timelimit,50).
const(maxstep,20).
