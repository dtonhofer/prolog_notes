/*

"Escape to Jing Provice" in Prolog, data for example 10

?- main.
_ _ ⊞ ∿ ∿ ⫛ ⊞ ⫛
_ ∆ ∆ ∿ ∆ ⊞ ∆ _
⫛ ∆ _ ⫛ ⫛ _ _ _
⫛ ∆ _ ∆ ∆ ∿ ∆ ⫛
⫛ ∆ _ ∆ _ ⫛ ∆ ∿
_ ⊞ _ ∿ _ ⫛ ∆ _
_ _ _ ∆ ⊞ ⫛ ∆ _
_ _ _ ∆ _ _ _ _
% 48,711 inferences, 0.019 CPU in 0.019 seconds (99% CPU, 2627511 Lips)
Soldiers : 7
Cities   : 1
Delay    : 20
Path     : [[5,5,start],[5,6,east],[4,6,north],[3,6,north],[3,5,west],[3,4,west],[2,4,north],[1,4,north],[1,3,west],[1,2,west],[1,1,west]]
← ← ← ↑ ∿ ⫛ ⊞ ⫛
_ ∆ ∆ ↑ ∆ ⊞ ∆ _
⫛ ∆ _ ← ← ↑ _ _
⫛ ∆ _ ∆ ∆ ↑ ∆ ⫛
⫛ ∆ _ ∆ ⊗ → ∆ ∿
_ ⊞ _ ∿ _ ⫛ ∆ _
_ _ _ ∆ ⊞ ⫛ ∆ _
_ _ _ ∆ _ _ _ _
true.

*/

terrain( [[ p, p, c, r, r, f, c, f ],
          [ p, m, m, r, m, c, m, p ],
          [ f, m, p, f, f, p, p, p ],
          [ f, m, p, m, m, r, m, f ],
          [ f, m, p, m, p, f, m, r ],
          [ p, c, p, r, p, f, m, p ],
          [ p, p, p, m, c, f, m, p ],
          [ p, p, p, m, p, p, p, p ]]).

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

soldier( [[ 0,0,0,0,0,0,4,5 ],
          [ 0,0,0,5,0,0,0,1 ],
          [ 0,0,0,0,1,0,1,2 ],
          [ 0,0,0,0,0,1,6,1 ],
          [ 0,0,5,3,0,0,2,1 ],
          [ 1,4,0,9,9,8,0,1 ],
          [ 4,2,0,0,0,2,3,3 ],
          [ 0,0,0,0,0,0,0,0 ]] ).

const(start_row,5).
const(start_col,5).

const(timelimit,50).
const(maxstep,12).
