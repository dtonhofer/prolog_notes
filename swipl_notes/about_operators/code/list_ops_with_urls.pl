% =============================================================================
% Code to list the operators configured at call time.
%
% Just call list_ops_with_urls/0.
%
% Output is in pldoc format (more or less markdown format).
%
% For known operators, a description and a link into the manual are provided
% via predicate op_info/4. If they exist, description and link are bashed
% into a pldoc URL and printed.
% =============================================================================
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% =============================================================================

list_ops_with_urls :-
   bagof(PrecVal,Type^Name^current_op(PrecVal,Type,Name),AllPrecVals),
   sort(AllPrecVals,AllPrecValsSorted),  % duplicates have been removed!
   AllPrecValsSorted=[MinPrecVal|_],
   last(AllPrecValsSorted,MaxPrecVal),
   % use dict_create to be able to use values in variables as keys
   dict_create(Annotations,attr,[MinPrecVal-"strongest precedence, most 'leaflike'",MaxPrecVal-"weakest precedence, most 'rootlike'"]),
   reverse(AllPrecValsSorted,AllPrecValsSorted2), % "rootlike" precedence first
   by_precedence(AllPrecValsSorted2,Annotations).
   
% ---
% Section of the given precedence
% ---

by_precedence([Precedence|More],Annotations) :-
   format("**Precedence value ~q**",[Precedence]),
   (get_dict(Precedence, Annotations, Text) ->  format(" : ~s~n",[Text]) ; format("~n",[])),   
   handle_ops_with_precedence(Precedence),
   by_precedence(More,Annotations).
   
by_precedence([],_).

% ---
% All ops of the given precedence
% ---

handle_ops_with_precedence(Precedence) :-
   bagof(
      Name-Type,
      current_op(Precedence,Type,Name),
      Ops),
   keysort(Ops,OpsSorted), % sorted by name
   max_name_length(OpsSorted,MaxNameLength),
   format_ops(OpsSorted,MaxNameLength).
   
format_ops([Name-Type|More],MaxNameLength) :-
   FieldWidth is MaxNameLength+4,
   % set tab ~|, print datum ~a, add tab expanding-filler (will thus left justify) ~t,
   % set next tab stop FieldWidth positions after the previous tab stop ~FieldWidth+,
   % print datum ~a, add tab expanding-filler (will thus left justify) ~t,    
   % set next tab stop 5 positions after the previous tab stop, add newline ~n
   atomics_to_string(["   - ","~|=| ~a~t~",FieldWidth,"+~a~t~5+|= "],FormatString),
   % format("Format string ~q\n",[FormatString]), % for debugging, writes the format string
   format(FormatString,[Name,Type]), 
   append_op_info(Name,Type),
   format("~n",[]),
   format_ops(More,MaxNameLength).
   
format_ops([],_).

max_name_length(OpsSorted,MaxNameLength) :-
   foldl(
      ([Name-_,FromLeft,ToRight]>>(atom_length(Name,Len),ToRight is max(FromLeft,Len))),
      OpsSorted,
      0,
      MaxNameLength).

append_op_info(Name,Type) :-
   op_info(Name,Type,Text,Link),!,   
   re_replace("\\("/g,"%28",Link,Link2),
   re_replace("\\)"/g,"%29",Link2,Link3),
   (
      (Link == "") 
      -> format("~s",[Text])
      ;  format("[~s](~s~s)",[Text,"https://eu.swi-prolog.org/pldoc/",Link3])).

append_op_info(_,_).

op_info('-->'  , xfx , "DCG head-body separator" , "man?section=DCG").
op_info(':-'   , fx  , "Directive prefix as used in source code" , "").
op_info(':-'   , xfx , "Clause head-body separator (left-pointing material implication of constructive logic or positive classical logic)" , "").
op_info('?-'   , fx  , "Query prefix as used on the command line", "").

op_info('|'    , xfy , "Old-school disjunction meta-predicate", "doc_for?object=('|')/2").
op_info(';'    , xfy , "Meta-predicate to combine goals into a (constructive) disjunction. Also 'else' part of 'if-then-else'","doc_for?object=(%3B)/2").
op_info('*->'  , xfy , "Soft-cut","doc_for?object=(*->)/2").
op_info('->'   , xfy , "If-Then (and maybe Else)","doc_for?object=(->)/2").
op_info(','    , xfy , "Meta-predicate to combine goals into a conjunction","doc_for?object=(%27,%27)/2").
op_info('\\+'  , fy  , "Negation-as-failure operator","doc_for?object=(%5C%2B)/1").
op_info(':='   , xfx , "",""). % what is this?
op_info(':'    , xfy , "Module qualification","man?section=metapred").

op_info('discontiguous'         , fx , "Declares a predicate as discontinguous", "doc_for?object=(discontiguous)/1").
op_info('dynamic'               , fx , "Declares a predicate as dynamic", "doc_for?object=(dynamic)/1").
op_info('initialization'        , fx , "Marks goal to be called after source file has been loaded", "doc_for?object=(initialization)/1").
op_info('meta_predicate'        , fx , "Declares a meta-predicate for module export", "doc_for?object=(meta_predicate)/1").
op_info('module_transparent'    , fx , "Declares a predicate as meta-predicate. Deprecated.", "doc_for?object=(module_transparent)/1").
op_info('multifile'             , fx , "Informs the system that the specified predicate(s) may be defined over more than one file.", "doc_for?object=(multifile)/1").
op_info('public'                , fx , "Instructs the cross-referencer that the predicate can be called","doc_for?object=(public)/1").
op_info('table'                 , fx , "Declares a predicate as subject to tabling", "doc_for?object=(table)/1"). 
op_info('thread_initialization' , fx , "Marks goal to be called when thread is started", "doc_for?object=(thread_initialization)/1").
op_info('thread_local'          , fx , "Declares a predicate as dynamic, but local to the thread","doc_for?object=(thread_local)/1").
op_info('volatile'              , fx , "Declares a predicate as not to be saved into a state file","doc_for?object=(volatile)/1").
op_info('+'    , yfx , "Arithmetic addition" , "doc_for?object=f((%2B)/2)"). 
op_info('-'    , yfx , "Arithmetic subtraction" , "doc_for?object=f((-)/2)").
op_info('/\\'  , yfx , "Bitwise AND" , "doc_for?object=f((/%5C)/2)").
op_info('\\/'  , yfx , "Bitwise OR" , "doc_for?object=f((%5C/)/2)").

op_info('*'    , yfx , "Arithmetic multiplication" , "doc_for?object=f((*)/2)").
op_info('/'    , yfx , "Arithmetic division" , "doc_for?object=f((/)/2)").
op_info('//'   , yfx , "Arithmetic default-rounded integer division" , "doc_for?object=f((//)/2)").
op_info('<<'   , yfx , "Bitwise shift left" , "doc_for?object=f((<<)/2)").
op_info('>>'   , yfx , "Bitwise shift right" , "doc_for?object=f((>>)/2)").
op_info('div'  , yfx , "Arithmetic integer division" , "doc_for?object=f((div)/2)").
op_info('mod'  , yfx , "Arithmetic modulo (remainder of floored division)" , "doc_for?object=f((mod)/2)").
op_info('rdiv' , yfx , "Arithmetic rational number division" , "doc_for?object=f((rdiv)/2)").
op_info('rem'  , yfx , "Arithmetic remainder of integer division" , "doc_for?object=f((rem)/2)").
op_info('xor'  , yfx , "Bitwise XOR" , "doc_for?object=f((xor)/2)").

op_info(':<'   , xfx , "Dict pair selection", "doc_for?object=(%3A%3C)/2"). 
op_info('<'    , xfx , "Arithmetic evaluation followed by less-than test", "doc_for?object=(<)/2").
op_info('='    , xfx , "Unification", "doc_for?object=(%3D)/2").
op_info('=..'  , xfx , "Univ operator to assemble/disassemble a compound term", "doc_for?object=(%3D..)/2").
op_info('=:='  , xfx , "Arithmetic evaluation followed by equality test", "doc_for?object=(%3D%3A%3D)/2").
op_info('=<'   , xfx , "Arithmetic evaluation followed by less-or-equal-than test", "doc_for?object=(%3D<)/2").
op_info('=='   , xfx , "Term equivalence; also standard order of term equality test", "doc_for?object=(%3D%3D)/2").
op_info('=@='  , xfx , "Term structural equivalence", "doc_for?object=(%3D@%3D)/2").
op_info('=\\=' , xfx , "Arithmetic evaluation followed by disequality test", "doc_for?object=(%3D%5C%3D)/2").
op_info('>'    , xfx , "Arithmetic evaluation followed by greater-than test", "doc_for?object=(>)/2").
op_info('>:<'  , xfx , "Dict partial unification", "doc_for?object=(>%3A<)/2").
op_info('>='   , xfx , "Arithmetic evaluation followed by greater-or-equal-than test", "doc_for?object=(>%3D)/2").
op_info('@<'   , xfx , "Standard order of terms less-than test", "doc_for?object=(@<)/2").
op_info('@=<'  , xfx , "Standard order of terms less-than-or-equal test", "doc_for?object=(@%3D<)/2").
op_info('@>'   , xfx , "Standard order of terms greater-than test", "doc_for?object=(@>)/2").
op_info('@>='  , xfx , "Standard order of terms greater-than-or-equal test", "doc_for?object=(@>%3D)/2").
op_info('\\='  , xfx , "Unification failure", "doc_for?object=(%5C%3D)/2").
op_info('\\==' , xfx , "Term non-equivalence", "doc_for?object=(%5C%3D%3D)/2").
op_info('\\=@=', xfx , "Term structural non-equivalence","doc_for?object=(%5C%3D@%3D)/2").
op_info('as'   , xfx , "Particle used to add tabling options after table/1", "doc_for?object=(table)/1").
op_info('is'   , xfx , "Arithmetic evaluation of RHS followed by unification with LHS","doc_for?object=(is)/2").

op_info('**'   , xfx , "Arithmetic exponentiation (float or integer) ('power' in ISO standard)","doc_for?object=f((**)/2)").
op_info('+'    , fy  , "Arithmetic unary '+' sign", "doc_for?object=f((%2B)/1)").
op_info('-'    , fy  , "Arithmetic unary '-' sign", "doc_for?object=f((-)/1)").
op_info('\\'   , fy  , "Bitwise negation (one complement)", "doc_for?object=f((%5C)/1)").
op_info('^'    , xfy , "Arithmetic exponentiation (float or integer), also existential quantifier for bagof/3, setof/3","doc_for?object=f((^)/2)").

op_info('.'    , yfx , "Dict entry dereferencing and dict function call (also char-to-charcode conversion in arithmetic expressions)","man?section=bidicts").
op_info('$'    , fx  , "At the toplevel, reuse of bindings created by previous goals, as in '$Var'", "man?section=topvars").
