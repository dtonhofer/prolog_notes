:- use_module(library('heavycarbon/strings/string_overwrite.pl')).

% :- debug(repeatedly_overwrite).

:- begin_tests(string_overwrite).

repeatedly_overwrite(
      range(StartPos,EndPos),
      goal(Goal),
      strings(Lower,Upper),
      cutflags(CutLeft,CutRight),
      out(UpperPos,Result),
      aux(Want)) :-
   between(StartPos,EndPos,UpperPos), % generate new position on redo
   call(Goal,Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want),
   debug(repeatedly_overwrite,"[~d,~q],",[UpperPos,Result]).

repeatedly_overwrite_the_empty_string(Goal,T) :-
   Upper = "XxX",
   stringy_length(Upper,UpperLen), 
   StartPos is -UpperLen-1, 
   EndPos is 1,
   bagof([UpperPos,Result],
         repeatedly_overwrite(
            range(StartPos,EndPos),
            goal(Goal), 
            strings("",Upper),
            cutflags(false,false),
            out(UpperPos,Result),
            aux(string)), % result shall be string, not atom
         Bag),
   T = (Bag ==
   [[-4,"XxX "],                                                                                                      
    [-3,"XxX"],                                                                                                       
    [-2,"XxX"],                                                                                                       
    [-1,"XxX"],                                                                                                       
    [0,"XxX"],                                                                                                        
    [1," XxX"]]).

repeatedly_overwrite_lorem_ipsum_no_cutting(Goal,T) :-
   Lower = "Lorem ipsum",
   stringy_length(Lower,LowerLen),
   Upper = "[perspiciatis]",
   stringy_length(Upper,UpperLen),
   StartPos is -UpperLen-3,
   EndPos is LowerLen+3,
   bagof([UpperPos,Result],
         repeatedly_overwrite(
            range(StartPos,EndPos),
            goal(Goal),
            strings(Lower,Upper),
            cutflags(false,false),
            out(UpperPos,Result),
            aux(string)), % result shall be string, not atom
         Bag),
   T = (Bag ==  
   [[-17,"[perspiciatis]   Lorem ipsum"],
    [-16,"[perspiciatis]  Lorem ipsum"],
    [-15,"[perspiciatis] Lorem ipsum"],
    [-14,"[perspiciatis]Lorem ipsum"],
    [-13,"[perspiciatis]orem ipsum"],
    [-12,"[perspiciatis]rem ipsum"],
    [-11,"[perspiciatis]em ipsum"],
    [-10,"[perspiciatis]m ipsum"],
    [-9,"[perspiciatis] ipsum"],
    [-8,"[perspiciatis]ipsum"],
    [-7,"[perspiciatis]psum"],
    [-6,"[perspiciatis]sum"],
    [-5,"[perspiciatis]um"],
    [-4,"[perspiciatis]m"],
    [-3,"[perspiciatis]"],
    [-2,"[perspiciatis]"],
    [-1,"[perspiciatis]"],
    [0,"[perspiciatis]"],
    [1,"L[perspiciatis]"],
    [2,"Lo[perspiciatis]"],
    [3,"Lor[perspiciatis]"],
    [4,"Lore[perspiciatis]"],
    [5,"Lorem[perspiciatis]"],
    [6,"Lorem [perspiciatis]"],
    [7,"Lorem i[perspiciatis]"],
    [8,"Lorem ip[perspiciatis]"],
    [9,"Lorem ips[perspiciatis]"],
    [10,"Lorem ipsu[perspiciatis]"],
    [11,"Lorem ipsum[perspiciatis]"],
    [12,"Lorem ipsum [perspiciatis]"],
    [13,"Lorem ipsum  [perspiciatis]"],
    [14,"Lorem ipsum   [perspiciatis]"]]).

repeatedly_overwrite_lorem_ipsum_cutting_upper(Goal,T) :-
   Lower = "Lorem ipsum",
   stringy_length(Lower,LowerLen),
   Upper = "[perspiciatis]",
   stringy_length(Upper,UpperLen),
   StartPos is -UpperLen-3,
   EndPos is LowerLen+3,
   bagof([UpperPos,Result],
         repeatedly_overwrite(
            range(StartPos,EndPos),
            goal(Goal),
            strings(Lower,Upper),
            cutflags(false,true),
            out(UpperPos,Result),
            aux(string)), % result shall be string, not atom
         Bag),
   T = (Bag == 
   [[-17,"[perspiciatis]   Lorem ipsum"],
    [-16,"[perspiciatis]  Lorem ipsum"],
    [-15,"[perspiciatis] Lorem ipsum"],
    [-14,"[perspiciatis]Lorem ipsum"],
    [-13,"[perspiciatis]orem ipsum"],
    [-12,"[perspiciatis]rem ipsum"],
    [-11,"[perspiciatis]em ipsum"],
    [-10,"[perspiciatis]m ipsum"],
    [-9,"[perspiciatis] ipsum"],
    [-8,"[perspiciatis]ipsum"],
    [-7,"[perspiciatis]psum"],
    [-6,"[perspiciatis]sum"],
    [-5,"[perspiciatis]um"],
    [-4,"[perspiciatis]m"],
    [-3,"[perspiciatis]"],
    [-2,"[perspiciatis"],
    [-1,"[perspiciati"],
    [0,"[perspiciat"],
    [1,"L[perspicia"],
    [2,"Lo[perspici"],
    [3,"Lor[perspic"],
    [4,"Lore[perspi"],
    [5,"Lorem[persp"],
    [6,"Lorem [pers"],
    [7,"Lorem i[per"],
    [8,"Lorem ip[pe"],
    [9,"Lorem ips[p"],
    [10,"Lorem ipsu["],
    [11,"Lorem ipsum"],
    [12,"Lorem ipsum"],
    [13,"Lorem ipsum"],
    [14,"Lorem ipsum"]]).

repeatedly_overwrite_lorem_ipsum_cutting_lower(Goal,T) :-
   Lower = "Lorem ipsum",
   stringy_length(Lower,LowerLen),
   Upper = "[perspiciatis]",
   stringy_length(Upper,UpperLen),
   StartPos is -UpperLen-1,
   EndPos is LowerLen+1,
   bagof([UpperPos,Result],
         repeatedly_overwrite(
            range(StartPos,EndPos),
            goal(Goal),
            strings(Lower,Upper),
            cutflags(true,false),
            out(UpperPos,Result),
            aux(string)), % result shall be string, not atom
         Bag),
   T = (Bag == 
   [[-15,"Lorem ipsum"],                                                                                              
    [-14,"Lorem ipsum"],                                                                                              
    [-13,"]orem ipsum"],                                                                                              
    [-12,"s]rem ipsum"],                                                                                              
    [-11,"is]em ipsum"],                                                                                              
    [-10,"tis]m ipsum"],                                                                                              
    [-9,"atis] ipsum"],                                                                                               
    [-8,"iatis]ipsum"],                                                                                               
    [-7,"ciatis]psum"],                                                                                               
    [-6,"iciatis]sum"],                                                                                               
    [-5,"piciatis]um"],                                                                                               
    [-4,"spiciatis]m"],                                                                                               
    [-3,"rspiciatis]"],                                                                                               
    [-2,"erspiciatis]"],                                                                                              
    [-1,"perspiciatis]"],                                                                                             
    [0,"[perspiciatis]"],                                                                                             
    [1,"L[perspiciatis]"],                                                                                            
    [2,"Lo[perspiciatis]"],                                                                                           
    [3,"Lor[perspiciatis]"],                                                                                          
    [4,"Lore[perspiciatis]"],                                                                                         
    [5,"Lorem[perspiciatis]"],                                                                                        
    [6,"Lorem [perspiciatis]"],                                                                                       
    [7,"Lorem i[perspiciatis]"],                                                                                      
    [8,"Lorem ip[perspiciatis]"],                                                                                     
    [9,"Lorem ips[perspiciatis]"],                                                                                    
    [10,"Lorem ipsu[perspiciatis]"],                                                                                  
    [11,"Lorem ipsum[perspiciatis]"],                                                                                 
    [12,"Lorem ipsum [perspiciatis]"]]).

repeatedly_overwrite_lorem_ipsum_cutting_both_sides(Goal,T) :-
   Lower = "Lorem ipsum",
   stringy_length(Lower,LowerLen),
   Upper = "~X~",
   stringy_length(Upper,UpperLen),
   StartPos is -UpperLen-1,
   EndPos is LowerLen+1,
   bagof([UpperPos,Result],
         repeatedly_overwrite(
            range(StartPos,EndPos),
            goal(Goal),
            strings(Lower,Upper),
            cutflags(true,true),
            out(UpperPos,Result),
            aux(string)), % result shall be string, not atom
         Bag),
   T = (Bag == 
   [[-4,"Lorem ipsum"],
    [-3,"Lorem ipsum"],
    [-2,"~orem ipsum"],
    [-1,"X~rem ipsum"],
    [0,"~X~em ipsum"],
    [1,"L~X~m ipsum"],
    [2,"Lo~X~ ipsum"],
    [3,"Lor~X~ipsum"],
    [4,"Lore~X~psum"],
    [5,"Lorem~X~sum"],
    [6,"Lorem ~X~um"],
    [7,"Lorem i~X~m"],
    [8,"Lorem ip~X~"],
    [9,"Lorem ips~X"],
    [10,"Lorem ipsu~"],
    [11,"Lorem ipsum"],
    [12,"Lorem ipsum"]]).

repeatedly_overwrite_lorem_ipsum_with_empty_string_cutting_both_sides(Goal,T) :-
   Lower = "Lorem ipsum",
   stringy_length(Lower,LowerLen),
   Upper = "",
   stringy_length(Upper,UpperLen),
   StartPos is -UpperLen-1,
   EndPos is LowerLen+1,
   bagof([UpperPos,Result],
         repeatedly_overwrite(
            range(StartPos,EndPos),
            goal(Goal),
            strings(Lower,Upper),
            cutflags(true,true),
            out(UpperPos,Result),
            aux(string)), % result shall be string, not atom
         Bag),
   T = (Bag == 
       [[-1,"Lorem ipsum"],
        [0,"Lorem ipsum"],
        [1,"Lorem ipsum"],
        [2,"Lorem ipsum"],
        [3,"Lorem ipsum"],
        [4,"Lorem ipsum"],
        [5,"Lorem ipsum"],
        [6,"Lorem ipsum"],
        [7,"Lorem ipsum"],
        [8,"Lorem ipsum"],
        [9,"Lorem ipsum"],
        [10,"Lorem ipsum"],
        [11,"Lorem ipsum"],
        [12,"Lorem ipsum"]]).

repeatedly_overwrite_lorem_ipsum_with_empty_string_no_cutting(Goal,T) :-
   Lower = "Lorem ipsum",
   stringy_length(Lower,LowerLen),
   Upper = "",
   stringy_length(Upper,UpperLen),
   StartPos is -UpperLen-5,
   EndPos is LowerLen+5,
   bagof([UpperPos,Result],
         repeatedly_overwrite(
            range(StartPos,EndPos),
            goal(Goal),
            strings(Lower,Upper),
            cutflags(false,false),
            out(UpperPos,Result),
            aux(string)), % result shall be string, not atom
         Bag),
   T = (Bag == 
       [[-5,"     Lorem ipsum"],
        [-4,"    Lorem ipsum"],                                                                                           
        [-3,"   Lorem ipsum"],                                                                                            
        [-2,"  Lorem ipsum"],                                                                                             
        [-1," Lorem ipsum"],                                                                                              
        [0,"Lorem ipsum"],                                                                                                
        [1,"Lorem ipsum"],                                                                                                
        [2,"Lorem ipsum"],                                                                                                
        [3,"Lorem ipsum"],
        [4,"Lorem ipsum"],
        [5,"Lorem ipsum"],
        [6,"Lorem ipsum"],
        [7,"Lorem ipsum"],
        [8,"Lorem ipsum"],
        [9,"Lorem ipsum"],
        [10,"Lorem ipsum"],
        [11,"Lorem ipsum"],
        [12,"Lorem ipsum "],
        [13,"Lorem ipsum  "],
        [14,"Lorem ipsum   "],
        [15,"Lorem ipsum    "],
        [16,"Lorem ipsum     "]]).

test("Char-by-Char 1",[true(T)]) :- 
   repeatedly_overwrite_the_empty_string(overwrite_char_by_char,T).

test("Runs 1",[true(T)]) :- 
   repeatedly_overwrite_the_empty_string(overwrite_using_runs,T).

test("Char-by-Char 2",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_no_cutting(overwrite_char_by_char,T).

test("Runs 2",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_no_cutting(overwrite_using_runs,T).

test("Char-by-Char 3",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_cutting_upper(overwrite_char_by_char,T).

test("Runs 3",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_cutting_upper(overwrite_using_runs,T).

test("Char-by-Char 4",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_cutting_lower(overwrite_char_by_char,T).

test("Runs 4",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_cutting_lower(overwrite_using_runs,T).

test("Char-by-Char 5",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_cutting_both_sides(overwrite_char_by_char,T).

test("Runs 5",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_cutting_both_sides(overwrite_using_runs,T).

test("Char-by-Char 6",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_with_empty_string_cutting_both_sides(overwrite_char_by_char,T).

test("Runs 6",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_with_empty_string_cutting_both_sides(overwrite_using_runs,T).
 
test("Char-by-Char 7",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_with_empty_string_no_cutting(overwrite_char_by_char,T).

test("Runs 7",[true(T)]) :- 
   repeatedly_overwrite_lorem_ipsum_with_empty_string_no_cutting(overwrite_using_runs,T).

:- end_tests(string_overwrite).

