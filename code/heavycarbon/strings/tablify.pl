% =============================================================================
% Print tabular data.
% =============================================================================
% Table description
% -----------------
%
% TableDesc (table description) is an SWI-Prolog dict mapping an integer index
% (which is thus necessarily unique but need not be contiguous; it just gives
% the sorting order of columns) to a ColDesc (column description). The ColDesc
% is an SWI-Prolog dict with the following mappings:
%
% key     -> an atom giving the key of the column; corresponds to the 
%            key of the record field
% header  -> a string giving the text to be printed in the header
% justify -> how to justify column text an atom 'left','center','right'; if 
%            unrecognized or missing, defaults to 'right'
%
% TableDesc:dict
% --------------
%     0              
%     1 -----------> ColDesc:dict (for column at position 1)
%     2              ------------
%     3              key:atom      -------> column key in dataset (called "ColKey" in code)
%     .              header:string -------> column header text (may be missing)
%     .              justify:enum  -------> 'left', 'center', 'right' (or missing)
%     .              enum:List     -------> if there is a list of known values,they can be listed here; the column width will respect this
%     .
%
% Data set
% --------
%
% DataSet is an SWI-Prolog dict. Its keys form a set of arbitrary values that
% can be sorted according to the standard order of terms (e.g. they can be 
% integer, or atoms). They need not be continguous, but they give the sorting
% order of rows. The keys map to DataRecords, which correspond to rows.
% The DataRecord is an SWI-Prolog dict holding arbitrary key-value pairs
% (DataRecord fields). The keys of the DataRecord fields correspond to the
% values found for ColDesc 'key', which gives the order of appearance of the
% fields in a row.
%
% DataSet:dict
% --------------
%     0              
%     1 -----------> DataRecord:dict (for row at position 1)
%     2              ---------------
%     3              ColKey1:SomeValue1  -------> SomeValue1 will be printed
%     .              .                            in the column whose ColDesc
%     .              .                            'key'  has ColKey1 as key
%     .              ColKey2:SomeValue2  -------> SomeValue2 will be printed 
%     .              .                            in the column whose ColDesc
%     .              .                            'key'  has ColKey2 as key
%     .              ColKey3:SomeValue3  -------> SomeValue3 will be printed
%     .                                           in the column whose ColDesc
%     .                                           'key'  has ColKey3 as key
%     .             
% =============================================================================
% Copyright 2021 David Tonhofer (ronerycoder@gluino.name)
% This code is licensed under: The 3-Clause BSD License
% https://opensource.org/licenses/BSD-3-Clause
% =============================================================================
% Latest review: Tue 19 January 2021 (this is still alpha)
% =============================================================================

:- module(heavycarbon_stringy_tablify,
          [
          tablify/4
          ]).
 
:- use_module(library('heavycarbon/strings/justify.pl')).      % string justification
:- use_module(library('heavycarbon/support/meta_helpers.pl')). % if_then_else/3 & friends

% ---
% Generate a printable representation of the table-
% The representation is an SWI-Prolog dict, which the values being the lines
% (strings with no terminating newline), keyed by line number (not 
% necessarily continuous numbers, but monotonically increasing)
%
% tablify(+TableDesc,+DataSet,-LinesDict)
% ---

tablify(TableDesc,DataSet,Lines,MaxWidthsDict) :-
   assertion(is_dict(TableDesc)),
   assertion(is_dict(DataSet)),
   assertion(var(Lines)),   
   prepare_maxwidthsdict(TableDesc,MaxWidthsDict),  % collect first "max widths" for columns by looking at headers
   format("MaxWidthsDict is: ~q~n",[MaxWidthsDict]),
   % We now have something like: max_widths{hops:4,local_cost:10,name:4,overall_cost:12,prior:5,state:10}
   % Now loop over all the "data records" in the "DataSet", for each upticking 
   % MaxWidthsDict on each of its entries (which correspond to columns)
   uptick_maxwidthsdict_with_dataset(DataSet,TableDesc,MaxWidthsDict,MaxWidthsDictFull),
   horizontal_line(MaxWidthsDictFull,TableDesc,'+',2,HorizontalLine),
   header_line(MaxWidthsDictFull,TableDesc,2,HeaderLine),
   % format("Currently: ~s~n",[HorizontalLine]),
   % format("Currently: ~s~n",[HeaderLine]),
   Lines=[HorizontalLine,HeaderLine,HorizontalLine|FinIn],
   append_record_lines(MaxWidthsDictFull,DataSet,TableDesc,2,FinIn,FinOut),
   FinOut=[HorizontalLine],
   maplist(([Line]>>format("~s~n",[Line])),Lines).
   
uptick_maxwidthsdict_with_dataset(DataSet,TableDesc,MaxWidthsDictIn,MaxWidthsDictOut) :-
   dict_keys(DataSet,Keys), % Keys is an ordered list of the DataSet keys   
   foldl(uptick_maxwidthsdict_with_datarecord(DataSet,TableDesc),Keys,MaxWidthsDictIn,MaxWidthsDictOut).

% ---
% Append a line (a string) for each DataRecord in the DataSet based on the
% column widths in "MaxWidthsDict" and the header labels in "TableDesc".
% Additional whitespace is catered for by adding "Spacing" dashes for each 
% column ("Spacing" is supposed to be an even integer).
% ---
   
append_record_lines(MaxWidthsDictFull,DataSet,TableDesc,Spacing,FinIn,FinOut) :-
   dict_keys(DataSet,RowKeys),        % RowsKeys is now an ordered list of the keys of DataSet (probably numeric)
   foldl(append_record_line_in_foldl(MaxWidthsDictFull,DataSet,TableDesc,Spacing),RowKeys,FinIn,FinOut).
   
append_record_line_in_foldl(MaxWidthsDict,DataSet,TableDesc,Spacing,RowKey,FinIn,FinOut) :-   
   get_dict(RowKey,DataSet,DataRecord),
   dict_keys(TableDesc,Keys),      % ColKeys is now an ordered list of the keys of TableDesc (probably numeric)
   LineTip=['|'|LineFin],
   foldl(append_record_stuff(MaxWidthsDict,TableDesc,DataRecord,Spacing),Keys,LineFin,[]),
   atomics_to_string(LineTip,Line),
   FinIn=[Line|FinOut].
   
append_record_stuff(MaxWidthsDict,TableDesc,DataRecord,Spacing,Key,FinIn,FinOut) :-
   get_dict(Key,TableDesc,ColDesc),      % get the current ColDesc (the one corresponding to Key in TableDesc)   
   get_dict(key,ColDesc,ColKey),         % get the column key, which is an atom (must exist)
   get_dict(ColKey,MaxWidthsDict,Width), % now get the width (TODO: precompute the sequence of ColKey)
   if_then_else(
      get_dict(ColKey,DataRecord,Datum), % id there is a datum
      true,                              % then use
      Datum=""                           % otherwise just use the column key as text
   ),
   atom_string(Datum,Text),              % stringification (good enough for now)
   if_then_else(
      get_dict(justify,ColDesc,Justify), 
      true,
      Justify=center
   ),
   switch(
      (Justify=left),  justify_left(Text,Width,FinalText),   
      (Justify=right), justify_right(Text,Width,FinalText),
      justify_center(Text,Width,FinalText)
   ),   
   WiderWidth is Width+Spacing,
   justify_center(FinalText,WiderWidth,FinalTextWithSpacing),
   FinIn=[FinalTextWithSpacing,'|'|FinOut]. 

% ---
% Create a line for the table header (a string) based on the column widths
% in "MaxWidthsDict" and the header labels in "TableDesc".
% Additional whitespace is catered for by adding "Spacing" dashes for each
% column ("Spacing" is supposed to be an even integer).
% ---

header_line(MaxWidthsDict,TableDesc,Spacing,Line) :-
   dict_keys(TableDesc,Keys),         % Keys is now an ordered list of the keys of TableDesc (probably numeric)
   Tip=['|'|Fin],                     % Create an open list to append stuff to; it starts with "|"
   foldl(append_header_stuff(MaxWidthsDict,TableDesc,Spacing),Keys,Fin,[]),  
   atomics_to_string(Tip,Line).       % Flatten the list of characters into a String
      
append_header_stuff(MaxWidthsDict,TableDesc,Spacing,Key,FinIn,FinOut) :-
   get_dict(Key,TableDesc,ColDesc),      % get the current ColDesc (the one corresponding to Key in TableDesc)   
   get_dict(key,ColDesc,ColKey),         % get the column key, which is an atom (must exist)
   get_dict(ColKey,MaxWidthsDict,Width), % now get the width (TODO: precompute the sequence of ColKey)
   WiderWidth is Width + Spacing,
   if_then_else(
      get_dict(header,ColDesc,Header),   % if there is an ColDesc entry for 'header' (and there should be)
      Text=Header,                       % then use it as Text
      Text=ColKey                        % otherwise just use the column key as text
   ),
   justify_center(Text,WiderWidth,FinalText),
   FinIn=[FinalText,'|'|FinOut]. 

% ---
% Create a horizontal line (a string) based on the column widths in 
% "MaxWidthsDict". The line consists of dashes with a "Sep" character 
% separating the columns. 
% Additional whitespace is catered for by adding "Spacing" dashes for each 
% column ("Spacing" is supposed to be an even integer).
% ---

horizontal_line(MaxWidthsDict,TableDesc,Sep,Spacing,Line) :-
   dict_keys(TableDesc,Keys),         % Keys is now an ordered list of the keys of TableDesc (probably numeric)
   Tip=[Sep|Fin],                     % Create an open list to append characters to; it starts with "Sep"   
   % Add dashes and Seps according to the MaxWidthsDict; the the list closing using '[]'
   foldl(append_hline_chars(MaxWidthsDict,TableDesc,Sep,Spacing),Keys,Fin,[]),  
   atomics_to_string(Tip,Line).       % Flatten the list of characters into a String

append_hline_chars(MaxWidthsDict,TableDesc,Sep,Spacing,Key,FinIn,FinOut) :-
   get_dict(Key,TableDesc,ColDesc),      % get the current ColDesc (the one corresponding to Key in TableDesc)   
   get_dict(key,ColDesc,ColKey),         % get the column key, which is an atom (must exist)
   get_dict(ColKey,MaxWidthsDict,Width), % now get the width (TODO: precompute the sequence of ColKey)
   WiderWidth is Width + Spacing,
   append_dashes(WiderWidth,FinIn,FinMid),
   FinMid=[Sep|FinOut]. % termninate with "Sep" character

append_dashes(0,Fin,Fin) :- !.
append_dashes(K,FinIn,FinOut) :- FinIn=['-'|FinMid],succ(Km,K),append_dashes(Km,FinMid,FinOut).

% ---
% Building the initial "dict of max widths", which is a dict
% in which the maximum width over all "data record dicts" for the
% value of a given "key" (i.e. over the whole column) is stored,
% indexed by "key". Initially this can be set to the width of the header
% texts as given by TableDesc.
% ---

prepare_maxwidthsdict(TableDesc,DictOfMaxWidths) :-
   dict_keys(TableDesc,Keys),   % Keys is now an ordered list of the keys of TableDesc (probably numeric)
   foldl(collect_initial_width_value_in_foldl(TableDesc),Keys,max_widths{},DictOfMaxWidths). 
   
collect_initial_width_value_in_foldl(TableDesc,Key,DictOfMaxWidthsIn,DictOfMaxWidthsOut) :-
   get_dict(Key,TableDesc,ColDesc),      % get the current ColDesc (the one corresponding to Key in TableDesc)   
   get_dict(key,ColDesc,ColKey),         % get the column key, which is an atom (must exist)
   if_then_else(
      get_dict(header,ColDesc,Header),   % if there is an ColDesc entry for 'header' (and there should be)
      string_length(Header,Width1),      % then determine the width if Header is transformed to string
      string_length(ColKey,Width1)       % otherwise just use the key
   ),
   if_then_else(
      get_dict(enum,ColDesc,Enum),       % if there is an ColDesc entry for 'enum' (which is rare)
      max_width_over_list(Enum,Width2),  % then find a max width over the 'enum' values
      Width2=0                           % otherwise just use 0
   ),
   Width is max(Width1,Width2),          % maxify
   put_dict(ColKey,DictOfMaxWidthsIn,Width,DictOfMaxWidthsOut). 

% ---
% Go through a list of "text" and determine the "largest text width".
% The "text" can be anything "stringifiable", really. For example, floats.
% ---

max_width_over_list(List,Width) :-
   foldl(max_width,List,0,Width).

max_width(Text,WidthIn,WidthOut) :-
   atom_string(Text,String),     % stringification of Text; good enough for now!
   string_length(String,Width),
   WidthOut is max(Width,WidthIn).
   
% ---
% Uptick the "MaxWidthsDict" by scanning through a data record (i.e. by
% scanning through all fields of the data record)
% ---

uptick_maxwidthsdict_with_datarecord(DataSet,TableDesc,Key,MaxWidthsDictIn,MaxWidthsDictOut) :-
   get_dict(Key,DataSet,DataRecord),  % get the DataRecord
   dict_keys(TableDesc,Keys),         % Keys is now an ordered list of the keys of TableDesc (probably numeric)
   foldl(uptick_maxwidthsdict_with_field(DataRecord,TableDesc),Keys,MaxWidthsDictIn,MaxWidthsDictOut).
   
% ---
% Uptick the "MaxWidthsDict" with the width of a single field.
% Note that it becomes more and more like that the Dict does not need an uptick
% as we get deeper into the DataSet.
% ---

uptick_maxwidthsdict_with_field(DataRecord,TableDesc,Key,MaxWidthsDictIn,MaxWidthsDictOut) :-
   get_dict(Key,TableDesc,ColDesc),      % get the current ColDesc (the one corresponding to Key in TableDesc)   
   get_dict(key,ColDesc,ColKey),         % get the column key, which is an atom (must exist)
   get_dict(ColKey,MaxWidthsDictIn,MaxSoFar), % now get the width (TODO: precompute the sequence of ColKey)
   if_then_else(
      get_dict(ColKey,DataRecord,Value),   % fails if entry does not exist; *can* happen!
      max_width(Value,MaxSoFar,NewMax),    % if "Value" exists, compute the new "max"
      max_width('',MaxSoFar,NewMax)        % otherwise, assume empty string (though maybe NULL will be printed or something?)
   ),
   uptick_maxwidthsdict_with_field_maybe(ColKey,NewMax,MaxSoFar,MaxWidthsDictIn,MaxWidthsDictOut). % be efficient and don't replace always
   
uptick_maxwidthsdict_with_field_maybe(ColKey,NewMax,MaxSoFar,MaxWidthsDictIn,MaxWidthsDictOut) :-
   NewMax > MaxSoFar,
   !, % replace
   put_dict(ColKey,MaxWidthsDictIn,NewMax,MaxWidthsDictOut).
  
uptick_maxwidthsdict_with_field_maybe(_,NewMax,MaxSoFar,MaxWidthsDict,MaxWidthsDict) :-
   NewMax =< MaxSoFar. % no need to replace anything





