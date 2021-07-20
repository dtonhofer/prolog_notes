/*

"Escape to Jing Provice" in Prolog, data for example 4

?- main.
_ _ _ ∿ ⊞ ⫛ ⫛ ⫛
_ ∆ ∆ ⫛ ∆ ⊞ ∆ _
_ ∆ _ ⫛ ⫛ _ _ _
_ ∆ _ ∆ ∆ ⊞ ∆ ∆
_ ∆ _ ∆ _ ⫛ ∿ ∿
_ ⊞ _ ∿ _ ⫛ ⫛ _
_ _ _ ∆ ⊞ ⫛ ⫛ _
_ _ _ ∆ _ _ _ _
% 167,017 inferences, 0.064 CPU in 0.065 seconds (99% CPU, 2615164 Lips)
Soldiers : 23
Cities   : 1
Delay    : 17
Path     : [[1,1,start],[2,1,south],[3,1,south],[4,1,south],[5,1,south],[6,1,south],[7,1,south],[8,1,south],[8,2,east],[8,3,east],[7,3,north],[6,3,north],[6,4,east],[6,5,east],[7,5,south],[8,5,south]]
⊗ _ _ ∿ ⊞ ⫛ ⫛ ⫛
↓ ∆ ∆ ⫛ ∆ ⊞ ∆ _
↓ ∆ _ ⫛ ⫛ _ _ _
↓ ∆ _ ∆ ∆ ⊞ ∆ ∆
↓ ∆ _ ∆ _ ⫛ ∿ ∿
↓ ⊞ ↑ → → ⫛ ⫛ _
↓ _ ↑ ∆ ↓ ⫛ ⫛ _
↓ → → ∆ ↓ _ _ _
true.

*/

terrain([[ p, p, p, r, c, f, f, f ],
         [ p, m, m, f, m, c, m, p ],
         [ p, m, p, f, f, p, p, p ],
         [ p, m, p, m, m, c, m, m ],
         [ p, m, p, m, p, f, r, r ],
         [ p, c, p, r, p, f, f, p ],
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
     [ false, false, false, true, true, true, false, false  ]]).

soldier([[ 0,2,3,0,0,0,4,5 ],
         [ 0,0,0,5,0,0,3,6 ],
         [ 0,5,0,0,1,0,1,2 ],
         [ 0,0,0,0,0,1,6,1 ],
         [ 0,0,5,3,0,0,2,1 ],
         [ 1,4,0,9,9,8,0,1 ],
         [ 4,2,0,0,0,2,3,3 ],
         [ 0,0,0,0,0,0,0,0 ]]).

const(start_row,1).
const(start_col,1).

const(timelimit,25).
const(maxstep,20).
