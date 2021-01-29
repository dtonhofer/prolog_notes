:- use_module(library('heavycarbon/utils/dict_prettyprint.pl')).

:- begin_tests(dict_prettyprint).

test("format empty dict") :- 
   dict_lines(_{},s,Lines),
   assertion(Lines == []).

test("format dict with integers (placeholder d)") :- 
   dict_lines(_{w: 100, ww: 200, www: 300, wwww: 400},d,Lines),
   assertion(Lines == 
      [ "w    : 100",
        "ww   : 200",
        "www  : 300",
        "wwww : 400" ]).

test("format dict with floats (placeholder e)") :- 
   dict_lines(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},e,Lines),
   assertion(Lines == 
      [ "w    : 2.598476e-01",
        "ww   : 1.458760e+00",
        "www  : 6.437649e+08",
        "wwww : 4.000000e+02" ]).
 
test("format random names (placeholder s)") :- 
   name_dict(Dict), 
   dict_lines(Dict,s,Lines),
   assertion(Lines ==
      [  "alpha    : Micha Verrier"
        ,"bravo    : Fay Weibel"
        ,"charlie  : Glady Stallworth"
        ,"delta    : Lexie Latshaw"
        ,"echo     : Liana Alewine"
        ,"foxtrott : Rickey Weigand"
        ,"golf     : Lizbeth Elsner"
        ,"india    : Jamel Slifer"
        ,"juliett  : Jeni Salvaggio"
        ,"kilo     : Phoebe Cora" ]).

test("framed left and right by 2 spaces") :-
   dict_lines_framed(
      _{w: 100, ww: 200, www: 300, wwww: 400},
      d,
      _{pad_top:0,pad_btm:0,pad_left:2,pad_right:2,border:false},
      FramedLines),
   assertion(FramedLines ==
      ["  w    : 100  ",
       "  ww   : 200  ",
       "  www  : 300  ",
       "  wwww : 400  "]).

test("framed all around by 2 spaces") :-
   dict_lines_framed(
      _{w: 100, ww: 200, www: 300, wwww: 400},
      d,
      _{pad_top:2,pad_btm:2,pad_left:2,pad_right:2,border:false},
      FramedLines),
   assertion(FramedLines = 
      ["              ",
       "              ",
       "  w    : 100  ",
       "  ww   : 200  ",
       "  www  : 300  ",
       "  wwww : 400  ",
       "              ",
       "              "]).

test("framed all around by 2 spaces and a border") :-
   dict_lines_framed(
      _{w: 100, ww: 200, www: 300, wwww: 400},
      d,
      _{pad_top:2,pad_btm:2,pad_left:2,pad_right:2,border:true},
      FramedLines),
   assertion(FramedLines = 
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

test("empty dict, framed all around by 2 spaces and a border") :-
   dict_lines_framed(
      _{},
      d,
      _{pad_top:2,pad_btm:2,pad_left:2,pad_right:2,border:true},
      FramedLines),
    assertion(FramedLines =
       ["+----+",
        "|    |",
        "|    |",
        "|    |",
        "|    |",
        "+----+"]).

test("empty dict, framed all around by just a border") :-
   dict_lines_framed(
      _{},
      d,
      _{border:true},
      FramedLines),
    assertion(FramedLines =
       ["++",
        "++"]).

test("empty dict, padded left by 1, framed all around by a border") :-
   dict_lines_framed(
      _{},
      d,
      _{border:true,pad_left:1,pad_top:1},
      FramedLines),
    assertion(FramedLines =
       ["+-+",
        "| |",
        "+-+"]).

% Random names from http://www.listofrandomnames.com/
% This will be funny if someone gets here because of this

name_dict(_{  alpha    : "Micha Verrier"
             ,bravo    : "Fay Weibel"
             ,charlie  : "Glady Stallworth"
             ,delta    : "Lexie Latshaw"
             ,echo     : "Liana Alewine"
             ,foxtrott : "Rickey Weigand"
             ,golf     : "Lizbeth Elsner"
             ,india    : "Jamel Slifer"
             ,juliett  : "Jeni Salvaggio"
             ,kilo     : "Phoebe Cora" }).

:- end_tests(dict_prettyprint).

