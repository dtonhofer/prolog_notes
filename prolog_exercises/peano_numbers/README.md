# Peano Numbers Exercises

- [`peano_numbers_as_lists.pl`](peano_numbers_as_lists.pl) - Simple code to implement the bidirectional mapping between Peano Numbers and Natural Numbers, based on [this Stackoverflow question](https://stackoverflow.com/questions/8954435/convert-peano-number-sn-to-integer-in-prolog). The Peano Number of magnitude _n_ is not represented by the traditional recursive structure of an `s/1` function, matrioshka-ed (but not evaluated) _n_ times onto the symbol `zero`, but by the (apparently) linear structure of a list of _n_ atoms `s` (with the empty list corresponding to the symbol `zero`). Much more readable and adapted to Prolog!

- [`peano_add_1.pl`](peano_add_1.pl) - A first attempt at implementing an addition for Peano Numbers, based on [this Stackoverflow question](https://stackoverflow.com/questions/62088500/how-can-i-write-two-predicates-a-division-and-remainder-in-prolog)). In this attempt, the whole equation for natural numbers `NA + NB = NC` is mapped to Peano Number equation `PA + PB = PC`. with any variables in "Peano Number Space" bidirectionally mapped to variables in "Natural Number Space" through a lookup table. The Peano Number equation is then rewritten according to the axioms of Peano Number Addition, and this involves a recursive call with a new equation involving fewer `s/1`. The recursion over the series of new equations continues until it bottoms out. Any variable appearing in the original equation has then been fully defined (i.e. ground). The defined variables are then mapped back to Natural Numbers through the variable lookup table, giving the result of the addition (or of the subtraction, depending on the position of the variable(s)). This seems not inelegant, but I just implemented it for addition. Some tricks in Prolog coding were discovered. 

- [`peano_redux.pl`](peano_redux.pl) - Latest attempt at implementing addition for Peano Numbers, along with multiplication, and quotient/remainder, based on [this Stackoverflow question](https://stackoverflow.com/questions/62132704/times-quotient-and-remainder-predicates-in-prolog)). This is the classical approach, but I try to make the `add/3`, `mult/3`, `pqorem/4` work not only in the usual "function direction", but in "any direction" and with any number of freshvars. Much insight into Prolog's limitations ensues. This code can be simplified. It also contains a huge amount of **unit test code**. Usueful if you should decide to implement your own operations: Reuse the test cases!

```
?- [peano_redux].                   
true.

?- rtall.
% PL-Unit: pm ................... done
% All 19 tests passed                                                                                                                                       
% PL-Unit: pnat ......... done                                                                                                                              
% All 9 tests passed                                                                                                                                        
% PL-Unit: pequal .... done                                                                                                                                 
% All 4 tests passed                                                                                                                                        
% PL-Unit: padd ................................. done                                                                                                      
% All 33 tests passed                                                                                                                                       
% PL-Unit: pless ................. done                                                                                                                     
% All 17 tests passed                                                                                                                                       
% PL-Unit: pmult .......................
% 1,649 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 9016546 Lips)                                                                                     
.
% 3,097 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 9799579 Lips)                                                                                     
.
% 5,813 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 10137704 Lips)                                                                                    
.
% 2,598 inferences, 0.000 CPU in 0.000 seconds (99% CPU, 7588725 Lips)
.
% 768 inferences, 0.000 CPU in 0.000 seconds (101% CPU, 7343450 Lips)
.
% 1,847 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 9842530 Lips)
.
% 8,453,914 inferences, 0.691 CPU in 0.693 seconds (100% CPU, 12239638 Lips)
.
% 4,273 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 10756072 Lips)
.
% 8,389 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 11180131 Lips)
.
% 12,506 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 11491306 Lips)
.
% 45,453 inferences, 0.004 CPU in 0.004 seconds (100% CPU, 11355066 Lips)
. done
% All 34 tests passed
% PL-Unit: pqr ................Found: 50*7+2=352
Found: 21*18+20=398
Found: 16*13+10=218
Found: 13*12+5=161
Found: 28*9+11=263
Found: 15*7+7=112
Found: 25*8+15=215
Found: 40*12+7=487
Found: 48*8+34=418
Found: 44*6+36=300
Found: 14*29+1=407
Found: 3*135+1=406
Found: 3*19+0=57
Found: 28*16+16=464
Found: 43*9+11=398
Found: 31*10+24=334
Found: 16*18+12=300
Found: 45*5+32=257
Found: 36*11+0=396
Found: 28*8+7=231
. done
% All 17 tests passed
true.
```

See also: 

- Wikipedia: [Natural Number](https://en.wikipedia.org/wiki/Natural_number)
- Wikipedia: [Peano Axioms](https://en.wikipedia.org/wiki/Peano_axioms)
- Wikipedia: [Proofs involving the addition of Natural Numbers](https://en.wikipedia.org/wiki/Proofs_involving_the_addition_of_natural_numbers)
