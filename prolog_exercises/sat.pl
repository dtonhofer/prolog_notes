% https://stackoverflow.com/questions/66079294/what-is-the-most-elegant-way-to-find-16-bit-numbers-which-satisfy-some-condition

:- use_module(library(clpb)).

% sat(Expr) sets up a constraint over variables
% labeling(ListOfVariables) fixes 0,1 values for variables (several solutions possible)
% atomic_list_concat/3 builds the bitstrings

find(X,Y,Z) :-
    sat(
        *([~(X15 + Y15), % Y | X = 0X49ab (0100100110101011)
            (X14 + Y14),
           ~(X13 + Y13),
           ~(X12 + Y12),
            (X11 + Y11),
           ~(X10 + Y10),
           ~(X09 + Y09),
            (X08 + Y08),
            (X07 + Y07),
           ~(X06 + Y06),
            (X05 + Y05),
           ~(X04 + Y04),
            (X03 + Y03),
           ~(X02 + Y02),
            (X01 + Y01),
            (X00 + Y00),
           ~(0   # X15), % (Y >> 2) ^ X = 0X530b (0101001100001011)
            (0   # X14),
           ~(Y15 # X13),
            (Y14 # X12),
           ~(Y13 # X11),
           ~(Y12 # X10),
            (Y11 # X09),
            (Y10 # X08),
           ~(Y09 # X07),
           ~(Y08 # X06),
           ~(Y07 # X05),
           ~(Y06 # X04),
            (Y05 # X03),
           ~(Y04 # X02),
            (Y03 # X01),
            (Y02 # X00),
           ~(0   * Y15), % (Z >> 1) & Y = 0X0883 (0000100010000011)
           ~(Z15 * Y14),
           ~(Z14 * Y13),
           ~(Z13 * Y12),
            (Z12 * Y11),
           ~(Z11 * Y10),
           ~(Z10 * Y09),
           ~(Z09 * Y08),
            (Z08 * Y07),
           ~(Z07 * Y06),
           ~(Z06 * Y05),
           ~(Z05 * Y04),
           ~(Z04 * Y03),
           ~(Z03 * Y02),
            (Z02 * Y01),
            (Z01 * Y00),
           ~(X13 + Z15), % (X << 2) | Z = 0X1787 (0001011110000111)
           ~(X12 + Z14),
           ~(X11 + Z13),
            (X10 + Z12),
           ~(X09 + Z11),
            (X08 + Z10),
            (X07 + Z09),
            (X06 + Z08),
            (X05 + Z07),
           ~(X04 + Z06),
           ~(X03 + Z05),
           ~(X02 + Z04),
           ~(X01 + Z03),
            (X00 + Z02),
            (  0 + Z01),
            (  0 + Z00) ])),
    labeling([X15,X14,X13,X12,X11,X10,X09,X08,X07,X06,X05,X04,X03,X02,X01,X00,
              Y15,Y14,Y13,Y12,Y11,Y10,Y09,Y08,Y07,Y06,Y05,Y04,Y03,Y02,Y01,Y00,
              Z15,Z14,Z13,Z12,Z11,Z10,Z09,Z08,Z07,Z06,Z05,Z04,Z03,Z02,Z01,Z00]),
    atomic_list_concat([X15,X14,X13,X12,X11,X10,X09,X08,X07,X06,X05,X04,X03,X02,X01,X00],X),
    atomic_list_concat([Y15,Y14,Y13,Y12,Y11,Y10,Y09,Y08,Y07,Y06,Y05,Y04,Y03,Y02,Y01,Y00],Y),
    atomic_list_concat([Z15,Z14,Z13,Z12,Z11,Z10,Z09,Z08,Z07,Z06,Z05,Z04,Z03,Z02,Z01,Z00],Z).


