# Peano Numbers Exercises

See also: 

- Wikipedia: [Natural Number](https://en.wikipedia.org/wiki/Natural_number)
- Wikipedia: [Peano Axioms](https://en.wikipedia.org/wiki/Peano_axioms)
- Wikipedia: [Proofs involving the addition of Natural Numbers](https://en.wikipedia.org/wiki/Proofs_involving_the_addition_of_natural_numbers)

In this folder:

- [`peano_numbers_as_lists.pl`](peano_numbers_as_lists.pl) - Simple code to implement the bidirectional mapping between Peano Numbers and Natural Numbers, based on [this Stackoverflow question](https://stackoverflow.com/questions/8954435/convert-peano-number-sn-to-integer-in-prolog). The Peano Number of magnitude _n_ is not represented by the traditional recursive structure of an `s/1` function, matrioshka-ed (but not evaluated) _n_ times onto the symbol `zero`, but by the (apparently) linear structure of a list of _n_ atoms `s` (with the empty list corresponding to the symbol `zero`). Much more readable and adapted to Prolog!

- [`peano_add_1.pl`](peano_add_1.pl) - A first attempt at implementing an addition for Peano Numbers, based on [this Stackoverflow question](https://stackoverflow.com/questions/62088500/how-can-i-write-two-predicates-a-division-and-remainder-in-prolog)). In this attempt, the whole equation for natural numbers `NA + NB = NC` is mapped to Peano Number equation `PA + PB = PC`. with any variables in "Peano Number Space" bidirectionally mapped to variables in "Natural Number Space" through a lookup table. The Peano Number equation is then rewritten according to the axioms of Peano Number Addition, and this involves a recursive call with a new equation involving fewer `s/1`. The recursion over the series of new equations continues until it bottoms out. Any variable appearing in the original equation has then been fully defined (i.e. ground). The defined variables are then mapped back to Natural Numbers through the variable lookup table, giving the result of the addition (or of the subtraction, depending on the position of the variable(s)). This seems not inelegant, but I just implemented it for addition. Some tricks in Prolog coding were discovered. 

- [`peano.pl`](peano.pl) and [`peano.plt`](peano.plt) - Latest attempt at implementing addition for Peano Numbers, along with multiplication, and quotient/remainder, based on [this Stackoverflow question](https://stackoverflow.com/questions/62132704/times-quotient-and-remainder-predicates-in-prolog)). This is the classical approach, but I try to make the `add/3`, `mult/3`, `pqorem/4` work not only in the usual "function direction", but in "any direction" and with any number of unbound variables. Much insight into Prolog's limitations ensues. This has been reviewed 2021-03-09, transformed into a module and the test code has been moved to a separate `.plt` file. To run:


```
?- [peano].
true.

?- load_test_files([]).
true.

?- run_tests.
% PL-Unit: pm ................... done
% PL-Unit: pnat ..... done
% PL-Unit: pnonz .... done
% PL-Unit: pequal .... done
% PL-Unit: padd ................................. done
% PL-Unit: pless ................. done
% PL-Unit: pmult .......................
% 489 inferences, 0.000 CPU in 0.000 seconds (99% CPU, 3935899 Lips)
.
% 792 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 4116617 Lips)
.
% 1,277 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 4009230 Lips)
.
% 719 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 4117890 Lips)
.
% 176 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 2627415 Lips)
.
% 385 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 2534445 Lips)
.
% 1,134,842 inferences, 0.361 CPU in 0.363 seconds (100% CPU, 3141253 Lips)
.
% 848 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 3610708 Lips)
.
% 1,552 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 3650433 Lips)
.
% 2,257 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 3767934 Lips)
.
% 7,887 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 3484011 Lips)
. done
% PL-Unit: pqr ................Found: 22*14+13=321
Found: 10*34+5=345
Found: 3*56+2=170
Found: 36*0+24=24
Found: 17*4+2=70
Found: 46*10+40=500
Found: 37*1+19=56
Found: 38*5+27=217
Found: 46*6+37=313
Found: 41*9+16=385
Found: 21*6+14=140
Found: 42*10+19=439
Found: 19*4+15=91
Found: 5*75+3=378
Found: 7*17+5=124
Found: 27*12+13=337
Found: 31*11+10=351
Found: 14*30+0=420
Found: 3*139+1=418
Found: 21*18+15=393
. done
% All 133 tests passed
true.
```

