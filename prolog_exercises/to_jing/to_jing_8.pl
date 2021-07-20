/*

"Escape to Jing Provice" in Prolog, data for example 8

?- main.
⫛ _ _ ∿ ⫛ ⫛
_ ∆ ∆ ⫛ ∆ ⫛
_ ∆ _ ⫛ ⫛ _
_ ∿ _ ∆ ∆ ⫛
_ ∆ _ ⫛ _ ⫛
_ ⫛ _ ⫛ _ ⫛
% 13,040 inferences, 0.005 CPU in 0.005 seconds (99% CPU, 2839042 Lips)
Soldiers : 21
Cities   : 0
Delay    : 21
Path     : [[1,6,start],[2,6,south],[3,6,south],[3,5,west],[3,4,west],[3,3,west],[4,3,south],[5,3,south],[6,3,south],[6,2,west],[6,1,west]]
⫛ _ _ ∿ ⫛ ⊗
_ ∆ ∆ ⫛ ∆ ↓
_ ∆ ← ← ← ↓
_ ∿ ↓ ∆ ∆ ⫛
_ ∆ ↓ ⫛ _ ⫛
← ← ↓ ⫛ _ ⫛
true.

*/

terrain( [[ f, p, p, r, f, f ],
          [ p, m, m, f, m, f ],
          [ p, m, p, f, f, p ],
          [ p, r, p, m, m, f ],
          [ p, m, p, f, p, f ],
          [ p, f, p, f, p, f ]] ).

delay(p,1).
delay(m,9).
delay(f,3).
delay(c,1).
delay(r,2).

jin([[ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ false, false, false, false, false, false ],
     [ true,  false, false, false, false, false ]]).

soldier([[ 1,3,4,2,1,3 ],
         [ 3,1,2,5,1,3 ],
         [ 1,2,4,3,1,2 ],
         [ 4,3,2,1,4,1 ],
         [ 1,5,0,2,4,1 ],
         [ 1,1,1,0,3,7 ]]).

const(start_row,1).
const(start_col,6).

const(timelimit,22).
const(maxstep,20).
