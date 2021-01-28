:- use_module(library('snippets/prettyprint_dict.pl')).

:- begin_tests(prettyprint_dict).

test("format empty dict",fail) :- 
   dict_lines(_{},_,s).

test("format random names") :- 
   name_dict(N), 
   dict_lines(N,Lines,s),
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
        ,"kilo     : Phoebe Cora"
        ,"lima     : Maranda Kriebel"
        ,"mike     : Kylee Joshua"
        ,"november : Angelia Pollak"
        ,"oscar    : Orlando Kriegel"
        ,"papa     : Breann Schorr"
        ,"quebec   : Mozelle Obryan"
        ,"romeo    : Lorean Alba"
        ,"sierra   : Antonette Shaughnessy"
        ,"tango    : Frederick Seese"
        ,"uniform  : Anjelica Romero" ]).

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
             ,kilo     : "Phoebe Cora"
             ,lima     : "Maranda Kriebel"
             ,mike     : "Kylee Joshua"
             ,november : "Angelia Pollak"
             ,oscar    : "Orlando Kriegel"
             ,papa     : "Breann Schorr"
             ,quebec   : "Mozelle Obryan"
             ,romeo    : "Lorean Alba"
             ,sierra   : "Antonette Shaughnessy"
             ,tango    : "Frederick Seese"
             ,uniform  : "Anjelica Romero" }).

:- end_tests(prettyprint_dict).
