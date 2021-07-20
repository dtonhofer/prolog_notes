/*

"Escape to Jing Provice" in Prolog, data for example 1

?- main.
_ _ _ ∿ ⊞ ⫛
_ ∆ ∆ ⫛ ∆ ⊞
_ ∆ _ ⫛ ⫛ _
_ ∆ _ ∆ ∆ ⊞
_ ∆ _ ∆ _ ⫛
_ _ _ ∆ _ ⫛
% 6,926 inferences, 0.003 CPU in 0.003 seconds (99% CPU, 2495146 Lips)
Soldiers : 2
Cities   : 1
Delay    : 25
Path     : [[1,1,start],[2,1,south],[3,1,south],[4,1,south],[5,1,south],[6,1,south],[6,2,east],[6,3,east],[5,3,north],[4,3,north],[3,3,north],[3,4,east],[3,5,east],[3,6,east],[4,6,south],[5,6,south],[6,6,south]]
⊗ _ _ ∿ ⊞ ⫛
↓ ∆ ∆ ⫛ ∆ ⊞
↓ ∆ ↑ → → →
↓ ∆ ↑ ∆ ∆ ↓
↓ ∆ ↑ ∆ _ ↓
↓ → → ∆ _ ↓
true.

*/

terrain( [[ p, p, p, r, c, f ],
          [ p, m, m, f, m, c ],
          [ p, m, p, f, f, p ],
          [ p, m, p, m, m, c ],
          [ p, m, p, m, p, f ],
          [ p, p, p, m, p, f ]]).

delay(p,1).
delay(m,9).
delay(f,3).
delay(c,1).
delay(r,2).

jin([[ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, true,  false, false ],
     [ false, false, false, true,  true,  true  ]]).

soldier( [[ 0,0,0,0,0,0 ],
          [ 0,0,0,5,0,0 ],
          [ 0,0,0,0,1,0 ],
          [ 0,0,0,0,0,1 ],
          [ 0,0,0,0,0,0 ],
          [ 0,0,0,0,0,0 ]] ).

const(start_row,1).
const(start_col,1).

const(timelimit,50).
const(maxstep,20).
