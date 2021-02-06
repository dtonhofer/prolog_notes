:- use_module(library('heavycarbon/utils/dict_prettyprint.pl')).

:- begin_tests(dict_prettyprint).

test("format empty dict") :-
   dict_pp( _{}, _{}, LinesOut),
   assertion(LinesOut == []).

test("format dict with integers, no tag") :-
   dict_pp( _{w: 10, ww: 100, www: 1000, wwww: 10000},
            _{}, LinesOut),
   assertion(LinesOut ==
      [ "w    : 10   ",
        "ww   : 100  ",
        "www  : 1000 ",
        "wwww : 10000" ]).

test("format dict with integers, no tag, justify right") :-
   dict_pp( _{w: 10, ww: 100, www: 1000, wwww: 10000},
            _{justify_key:right,justify_value:right}, LinesOut),
   assertion(LinesOut ==
      [ "   w :    10",
        "  ww :   100",
        " www :  1000",
        "wwww : 10000" ]).

test("format dict with integers, no tag, justify center") :-
   dict_pp( _{w: 10, ww: 100, www: 1000, wwww: 10000}, 
            _{justify_key:center,justify_value:center}, LinesOut),
   assertion(LinesOut ==
      [ " w   :  10  ",
        " ww  :  100 ",
        "www  : 1000 ",
        "wwww : 10000" ]).

test("format dict with integers, with border") :-
   dict_pp( _{w: 10, ww: 100, www: 1000, wwww: 10000}, 
            _{border:true}, LinesOut),
   assertion(LinesOut ==
      [ "+------------+",
        "|w    : 10   |",
        "|ww   : 100  |",
        "|www  : 1000 |",
        "|wwww : 10000|",
        "+------------+" ]).

test("format dict with integers, with border and tag") :-
   dict_pp( 'this is a tag'{w: 10, ww: 100, www: 1000, wwww: 10000}, 
            _{border:true}, LinesOut),
   assertion(LinesOut ==
      [ "+-------------+",
        "|this is a tag|",
        "+-------------+",
        "|w    : 10    |",
        "|ww   : 100   |",
        "|www  : 1000  |",
        "|wwww : 10000 |",
        "+-------------+" ]).

test("format dict with integers, with border and long tag and right justification") :-
   dict_pp( 'this is a longish tag'{w: 10, ww: 100, www: 1000, wwww: 10000}, 
            _{border:true,justify_key:right,justify_value:right}, LinesOut),
   assertion(LinesOut == 
      [ "+---------------------+",
        "|this is a longish tag|",
        "+---------------------+",
        "|   w :    10         |",
        "|  ww :   100         |",
        "| www :  1000         |",
        "|wwww : 10000         |",
        "+---------------------+" ]).

test("format dict with floats") :-
   dict_pp(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{}, LinesOut),
   assertion(LinesOut ==
      ["w    : 0.259848 ",
       "ww   : 1.458760 ",
       "www  : 643764856",
       "wwww : 400      " ]).

test("format dict with floats, formatted as 'e'") :-
   dict_pp(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{spec_float:e, justify_value:right}, LinesOut),
   assertion(LinesOut = 
      [ "w    : 2.598476e-01",
        "ww   : 1.458760e+00",
        "www  :    643764856",
        "wwww :          400" ]).

test("format dict with floats, formatted as 'g'") :-
   dict_pp(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},
           _{spec_float:g, justify_value:right}, LinesOut),
   assertion(LinesOut ==
      ["w    :  0.259848",
       "ww   :   1.45876",
       "www  : 643764856",
       "wwww :       400" ]).

test("format dict with integers of varying length; justify left") :-
   dict_pp(_{w1: 10, w2: 200, w3: 3000, w4: 40000},
           _{justify_value:left}, LinesOut),
   assertion(LinesOut ==
      [ "w1 : 10   ",
        "w2 : 200  ",
        "w3 : 3000 ",
        "w4 : 40000" ]).

test("format dict with integers of varying length; justify right") :-
   dict_pp(_{w1: 10, w2: 200, w3: 3000, w4: 40000},
           _{justify_value:right},LinesOut),
   assertion(LinesOut ==
      [ "w1 :    10",
        "w2 :   200",
        "w3 :  3000",
        "w4 : 40000" ]).

test("format dict with integers of varying length; justify center") :-
   dict_pp(_{w1: 10, w2: 200, w3: 3000, w4: 40000},
           _{justify_value:center},LinesOut),
   assertion(LinesOut ==
      [ "w1 :  10  ",
        "w2 :  200 ",
        "w3 : 3000 ",
        "w4 : 40000"]).

test("padded left and right by 2 spaces") :-
   dict_pp(
      _{w: 100, ww: 200, www: 300, wwww: 400},
      _{pad:true,pad_left:2,pad_right:2,border:false},LinesOut),
   assertion(LinesOut ==
      ["  w    : 100  ",
       "  ww   : 200  ",
       "  www  : 300  ",
       "  wwww : 400  "]).

test("padded all around by 2 spaces") :-
   dict_pp(
      _{w: 1000, ww: 200, www: 30, wwww: 4},
      _{pad:true,pad_top:2,pad_bottom:2,pad_left:2,pad_right:2,border:false},
      LinesOut),
   assertion(LinesOut ==
      ["               ",
       "               ",
       "  w    : 1000  ",
       "  ww   : 200   ",
       "  www  : 30    ",
       "  wwww : 4     ",
       "               ",
       "               "]).

test("padded all around by 0 spaces") :-
   dict_pp(
      _{w: 1000, ww: 200, www: 30, wwww: 4},
      _{pad:true,border:false},
      LinesOut),
   assertion(LinesOut ==
      ["w    : 1000",
       "ww   : 200 ",
       "www  : 30  ",
       "wwww : 4   "]).

test("unpadded") :-
   dict_pp(
      _{w: 1000, ww: 200, www: 30, wwww: 4},
      _{border:false},
      LinesOut),
   assertion(LinesOut ==
      ["w    : 1000",
       "ww   : 200 ",
       "www  : 30  ",
       "wwww : 4   "]).   % TODO: Probably not what one expects here: trailing whitespace!

test("padded all around by 2 spaces and a border and with a tag") :-
   dict_pp(
      various{w: 100, ww: 200, www: 300, wwww: 400},
      _{pad_top:2,pad_bottom:2,pad_left:2,pad_right:2,pad:true,border:true},
      LinesOut),
   assertion(LinesOut ==
        ["+--------------+",
         "|   various    |",
         "+--------------+",
         "|              |",
         "|              |",
         "|  w    : 100  |",
         "|  ww   : 200  |",
         "|  www  : 300  |",
         "|  wwww : 400  |",
         "|              |",
         "|              |",
         "+--------------+"]).

test("empty dict, padded all around by 2 spaces and a border") :-
   dict_pp(
      _{},
      _{pad_top:2,pad_bottom:2,pad_left:2,pad_right:2,pad:true,border:true},
      LinesOut),
    assertion(LinesOut ==
       ["+----+",
        "|    |",
        "|    |",
        "|    |",
        "|    |",
        "+----+"]).

test("empty dict, enclosed by a border") :-
   dict_pp(
      _{},
      _{border:true},
      LinesOut),
    assertion(LinesOut ==
       ["++",
        "++"]).

test("empty dict, padded left by 1, enclosed by a border") :-
   dict_pp(
      _{},
      _{border:true,pad_left:1,pad_top:1,pad:true},
      LinesOut),
    assertion(LinesOut ==
       ["+-+",
        "| |",
        "+-+"]).

test("recursive dicts with borders (on subdicts)") :-
   dict_pp(
      _{w1: 10, w2: 200, w3: 3000, w4: _{w1: 10, w2: 20, w3: _{ a: 12, b: 13}}},
      _{border:false,sub_border:true},
      LinesOut),
   assertion(LinesOut ==
      ["w1 : 10  ",
       "w2 : 200 ",
       "w3 : 3000",
       "w4 : +-------------+",
       "     |w1 : 10      |",
       "     |w2 : 20      |",
       "     |w3 : +------+|",
       "     |     |a : 12||",
       "     |     |b : 13||",
       "     |     +------+|",
       "     +-------------+"]).

test("recursive dicts with borders (on subdicts) and tags (on subdicts)") :-
   dict_pp(
      alpha{w1: 10, w2: 200, w3: 3000, w4: bravo{w1: 10, w2: 20, w3: charlie{ a: 12, b: 13}}},
      _{border:false,sub_border:true,tag:false,sub_tag:true},
      LinesOut),
   assertion(LinesOut ==
      ["w1 : 10  ",
       "w2 : 200 ",
       "w3 : 3000",
       "w4 : +--------------+",
       "     |    bravo     |",
       "     +--------------+",
       "     |w1 : 10       |",
       "     |w2 : 20       |",
       "     |w3 : +-------+|",
       "     |     |charlie||",
       "     |     +-------+|",
       "     |     |a : 12 ||",
       "     |     |b : 13 ||",
       "     |     +-------+|",
       "     +--------------+"]).


test("recursive dicts with borders full") :-
   dict_pp(
      _{w1: 10, w2: 200, w3: 3000, w4: _{w1: 10, w2: 20, w3: _{ a: 12, b: 13}}},
      _{border:true},
      LinesOut),
   assertion(LinesOut ==
      ["+--------------------+",
       "|w1 : 10             |",
       "|w2 : 200            |",
       "|w3 : 3000           |",
       "|w4 : +-------------+|",
       "|     |w1 : 10      ||",
       "|     |w2 : 20      ||",
       "|     |w3 : +------+||",
       "|     |     |a : 12|||",
       "|     |     |b : 13|||",
       "|     |     +------+||",
       "|     +-------------+|",
       "+--------------------+"]).

test("recursive dicts with borders, justify right") :-
   dict_pp(_{w1: 10, w2: 200, w3: 3000, w4: _{w1: 10, w2: 20, w3: _{ a: 12, b: 13}}},
      _{border:false,sub_border:true,justify_value:right},
      LinesOut),
   assertion(LinesOut ==
      ["w1 :   10",
       "w2 :  200",
       "w3 : 3000",
       "w4 : +-------------+",
       "     |w1 : 10      |",
       "     |w2 : 20      |",
       "     |w3 : +------+|",
       "     |     |a : 12||",
       "     |     |b : 13||",
       "     |     +------+|",
       "     +-------------+"]).

:- end_tests(dict_prettyprint).

