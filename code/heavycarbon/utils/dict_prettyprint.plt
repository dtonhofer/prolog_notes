:- use_module(library('heavycarbon/utils/dict_prettyprint.pl')).

:- begin_tests(dict_prettyprint).

test("format empty dict") :- 
   dict_pp_lines(_{},Lines),
   assertion(Lines == []).

test("format dict with integers") :- 
   dict_pp_lines(_{w: 100, ww: 200, www: 300, wwww: 400},Lines),
   assertion(Lines == 
      [ "w    : 100",
        "ww   : 200",
        "www  : 300",
        "wwww : 400" ]).

test("format dict with floats") :- 
   dict_pp_lines(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},Lines),
   assertion(Lines == 
      [ "w    : 0.259848",
        "ww   : 1.458760",
        "www  : 643764856",
        "wwww : 400" ]).

test("format dict with floats; format as g") :- 
   dict_pp_lines(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},_{float:g},Lines),
   assertion(Lines == 
      [ "w    : 0.259848",
        "ww   : 1.45876",
        "www  : 643764856",
        "wwww : 400" ]).

test("format dict with floats; format as e") :- 
   dict_pp_lines(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},_{float:e},Lines),
   assertion(Lines == 
      [ "w    : 2.598476e-01",
        "ww   : 1.458760e+00",
        "www  : 643764856",    % stays int
        "wwww : 400" ]).       % stays int
 
test("format dict with integers of varying length; justify left") :-
   dict_pp_lines(_{w1: 10, w2: 200, w3: 3000, w4: 40000},_{justify:left},Lines),
   assertion(Lines ==
      [ "w1 : 10   ",
        "w2 : 200  ",
        "w3 : 3000 ",
        "w4 : 40000" ]).

test("format dict with integers of varying length; justify right") :-
   dict_pp_lines(_{w1: 10, w2: 200, w3: 3000, w4: 40000},_{justify:right},Lines),
   assertion(Lines ==
      [ "w1 :    10",
        "w2 :   200",
        "w3 :  3000",
        "w4 : 40000" ]).
 
test("format dict with integers of varying length; justify center") :-
   dict_pp_lines(_{w1: 10, w2: 200, w3: 3000, w4: 40000},_{justify:center},Lines),
   assertion(Lines ==
      [ "w1 :  10  ",
        "w2 :  200 ",
        "w3 : 3000 ",
        "w4 : 40000"]).

test("padded left and right by 2 spaces") :-
   dict_pp_lines_padded(
      _{w: 100, ww: 200, www: 300, wwww: 400},
      _{pad_top:0,pad_btm:0,pad_left:2,pad_right:2,border:false},
      PaddedLines),
   assertion(PaddedLines ==
      ["  w    : 100  ",
       "  ww   : 200  ",
       "  www  : 300  ",
       "  wwww : 400  "]).

test("padded all around by 2 spaces") :-
   dict_pp_lines_padded(
      _{w: 100, ww: 200, www: 300, wwww: 400},
      _{pad_top:2,pad_btm:2,pad_left:2,pad_right:2,border:false},
      PaddedLines),
   assertion(PaddedLines = 
      ["              ",
       "              ",
       "  w    : 100  ",
       "  ww   : 200  ",
       "  www  : 300  ",
       "  wwww : 400  ",
       "              ",
       "              "]).

test("padded all around by 2 spaces and a border") :-
   dict_pp_lines_padded(
      _{w: 100, ww: 200, www: 300, wwww: 400},
      _{pad_top:2,pad_btm:2,pad_left:2,pad_right:2,border:true},
      PaddedLines),
   assertion(PaddedLines = 
      ["+--------------+",
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
   dict_pp_lines_padded(
      _{},
      _{pad_top:2,pad_btm:2,pad_left:2,pad_right:2,border:true},
      PaddedLines),
    assertion(PaddedLines =
       ["+----+",
        "|    |",
        "|    |",
        "|    |",
        "|    |",
        "+----+"]).

test("empty dict, enclosed by a border") :-
   dict_pp_lines_padded(
      _{},
      _{border:true},
      PaddedLines),
    assertion(PaddedLines =
       ["++",
        "++"]).

test("empty dict, padded left by 1, enclosed by a border") :-
   dict_pp_lines_padded(
      _{},
      _{border:true,pad_left:1,pad_top:1},
      PaddedLines),
    assertion(PaddedLines =
       ["+-+",
        "| |",
        "+-+"]).

test("recursive dicts with borders (on subdicts)") :-
   dict_pp_lines(
      _{w1: 10, w2: 200, w3: 3000, w4: _{w1: 10, w2: 20, w3: _{ a: 12, b: 13}}},
      _{border:true},
      Lines),
   assertion(Lines =
      ["w1 : 10",
       "w2 : 200",
       "w3 : 3000",
       "w4 : +-------------+",
       "     |w1 : 10      |",
       "     |w2 : 20      |",
       "     |w3 : +------+|",
       "     |     |a : 12||",
       "     |     |b : 13||",
       "     |     +------+|",
       "     +-------------+"]).

test("recursive dicts with borders full") :-
   dict_pp_lines_padded(
      _{w1: 10, w2: 200, w3: 3000, w4: _{w1: 10, w2: 20, w3: _{ a: 12, b: 13}}},
      _{border:true},
      Lines),
   assertion(Lines =
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
   dict_pp_lines(_{w1: 10, w2: 200, w3: 3000, w4: _{w1: 10, w2: 20, w3: _{ a: 12, b: 13}}},
      _{border:true,justify:right},
      Lines),
   assertion(Lines = 
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

