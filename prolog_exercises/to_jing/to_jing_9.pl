/*

"Escape to Jing Provice" in Prolog, data for example 9

?- main.
_ _ ⊞ ∿ ∿ ⫛ ⊞ ⫛
_ ∆ ∆ ⊞ ∆ ⊞ ∆ _
⊞ ∆ _ ⫛ ⫛ _ _ _
_ ∆ _ ∆ ∆ ⊞ ∆ ⫛
_ ∆ _ ∆ _ ⫛ ∆ ∿
_ ⊞ _ ⊞ _ ⫛ ∆ _
_ _ _ ∆ ⊞ ⫛ ∆ _
_ _ _ ∆ _ _ _ _
% 132,747 inferences, 0.049 CPU in 0.050 seconds (99% CPU, 2693897 Lips)
Soldiers : 32
Cities   : 1
Delay    : 42
Path     : [[5,5,start],[5,6,east],[6,6,south],[7,6,south],[8,6,south],[8,7,east],[8,8,east],[7,8,north],[6,8,north],[5,8,north],[4,8,north],[3,8,north],[3,7,west],[3,6,west],[3,5,west],[3,4,west],[3,3,west],[4,3,south],[5,3,south],[6,3,south],[7,3,south],[7,2,west],[7,1,west],[6,1,north],[5,1,north],[4,1,north],[3,1,north],[2,1,north],[1,1,north]]
↑ _ ⊞ ∿ ∿ ⫛ ⊞ ⫛
↑ ∆ ∆ ⊞ ∆ ⊞ ∆ _
↑ ∆ ← ← ← ← ← ↑
↑ ∆ ↓ ∆ ∆ ⊞ ∆ ↑
↑ ∆ ↓ ∆ ⊗ → ∆ ↑
↑ ⊞ ↓ ⊞ _ ↓ ∆ ↑
← ← ↓ ∆ ⊞ ↓ ∆ ↑
_ _ _ ∆ _ ↓ → →
true.

*/

terrain( [[ p, p, c, r, r, f, c, f ],
          [ p, m, m, c, m, c, m, p ],
          [ c, m, p, f, f, p, p, p ],
          [ p, m, p, m, m, c, m, f ],
          [ p, m, p, m, p, f, m, r ],
          [ p, c, p, c, p, f, m, p ],
          [ p, p, p, m, c, f, m, p ],
          [ p, p, p, m, p, p, p, p ]] ).

delay(p,1).
delay(m,9).
delay(f,3).
delay(c,1).
delay(r,2).

jin( [[ true,  false, false, false, false, false, false, false ],
      [ false, false, false, false, false, false, false, false ],
      [ false, false, false, false, false, false, false, false ],
      [ false, false, false, false, false, false, false, false ],
      [ false, false, false, false, false, false, false, false ],
      [ false, false, false, false, false, false, false, false ],
      [ false, false, false, false, false, false, false, false ],
      [ false, false, false, false, false, false, false, false ]] ).

soldier( [[ 0,2,3,3,4,2,4,5 ],
          [ 0,0,0,5,0,8,0,1 ],
          [ 0,0,0,0,1,0,1,2 ],
          [ 0,0,0,0,0,1,6,1 ],
          [ 0,0,5,3,0,0,2,1 ],
          [ 1,4,0,9,9,8,0,1 ],
          [ 4,2,0,0,0,2,3,3 ],
          [ 0,0,0,0,0,0,0,0 ]] ).

const(start_row,5).
const(start_col,5).

const(timelimit,50).
const(maxstep,30).
