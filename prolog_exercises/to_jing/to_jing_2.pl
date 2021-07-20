/*

"Escape to Jing Provice" in Prolog, data for example 2

?- main.
_ _ _ ∿ ⊞ ⫛
_ ∆ ∆ ⫛ ∆ ⊞
_ ∆ _ ⫛ ⫛ _
_ ∆ _ ∆ ∆ ⊞
_ ∆ _ ∆ _ ⫛
_ ⊞ _ ∆ _ ⫛
% 5,728 inferences, 0.004 CPU in 0.004 seconds (97% CPU, 1458328 Lips)
Soldiers : 7
Cities   : 1
Delay    : 22
Path     : [[1,1,start],[1,2,east],[1,3,east],[1,4,east],[2,4,south],[3,4,south],[3,5,east],[3,6,east],[4,6,south],[5,6,south],[6,6,south]]
⊗ → → → ⊞ ⫛
_ ∆ ∆ ↓ ∆ ⊞
_ ∆ _ ↓ → →
_ ∆ _ ∆ ∆ ↓
_ ∆ _ ∆ _ ↓
_ ⊞ _ ∆ _ ↓
true.

*/

terrain([[ p, p, p, r, c, f ],
         [ p, m, m, f, m, c ],
         [ p, m, p, f, f, p ],
         [ p, m, p, m, m, c ],
         [ p, m, p, m, p, f ],
         [ p, c, p, m, p, f ]]).

delay(p,1).
delay(m,9).
delay(f,3).
delay(c,1).
delay(r,2).

jin([[ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, true, false,  false ],
     [ false, false, false, true, true,   true ]]).

soldier([[ 0,0,0,0,0,0 ],
         [ 0,0,0,5,0,0 ],
         [ 0,0,0,0,1,0 ],
         [ 0,0,0,0,0,1 ],
         [ 0,0,0,0,0,0 ],
         [ 0,0,0,0,0,0 ]]).

const(start_row,1).
const(start_col,1).

const(timelimit,50).
const(maxstep,20).
