:- use_module(library('heavycarbon/strings/tablify.pl')).

test_tablify(Lines) :-
   NEX=visit_next,
   PRO=probed,
   VID=visited,
   dict_create(DataSet,dataset,[
      a:vertex{name:a, prior:'', local_cost:100, overall_cost:1000000, state:NEX, hops:100},
      b:vertex{name:b, prior:calpha,  local_cost:10 , overall_cost:100, state:PRO, hops:100}
   ]),
   dict_create(TableDesc,tabledesc,[
      1:col{key:name,         header:"Name",         justify:center},
      2:col{key:prior,        header:"Prior",        justify:center},
      3:col{key:local_cost,   header:"Local cost",   justify:right},
      4:col{key:overall_cost, header:"Overall cost", justify:right},
      5:col{key:state,        header:"State",        justify:center, enum:[NEX,PRO,VID]},
      6:col{key:hops,         header:"Hops",         justify:right}
   ]),
   tablify(TableDesc,DataSet,Lines,_MaxWidthsDict).

:- begin_tests(tablify).

test(1) :- 
   test_tablify(Lines),
   assertion(nth0(0,Lines,"+------+--------+------------+--------------+------------+------+")),
   assertion(nth0(1,Lines,"| Name | Prior  | Local cost | Overall cost |   State    | Hops |")),
   assertion(nth0(2,Lines,"+------+--------+------------+--------------+------------+------+")),
   assertion(nth0(3,Lines,"|  a   |        |        100 |      1000000 | visit_next |  100 |")),
   assertion(nth0(4,Lines,"|  b   | calpha |         10 |          100 |   probed   |  100 |")),
   assertion(nth0(5,Lines,"+------+--------+------------+--------------+------------+------+")).

:- end_tests(tablify).
