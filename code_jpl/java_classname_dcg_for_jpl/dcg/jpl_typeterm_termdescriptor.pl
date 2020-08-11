
% ===
%! jpl_typeterm_typedescriptor(TypeTerm)//1
%
% Map a "typedescriptor" (as used to denote type in JNI, serialization etc)
% into a Prolog-side "type term" and vice-versa.
%
% ~~~
%       Typedescriptor                        Prolog-side Type Term
%           Atom  <-------------------------------> Term
%     Ljava/util/Date;                     class([java,util],['Date'])
% ~~~
%
% Previously (pre 2020-08) called: `jpl_type_descriptor_1//1`
%
% Now called: `jpl_typeterm_typedescriptor//1`
%
% Called from:
%
% * `jpl_type_to_descriptor/2`
% ===

jpl_typeterm_typedescriptor(T) --> jpl_tt_td_primitive(T),!.
jpl_typeterm_typedescriptor(T) --> jpl_tt_td_class_descriptor(T),!.
jpl_typeterm_typedescriptor(T) --> jpl_tt_td_array_descriptor(T),!.
jpl_typeterm_typedescriptor(T) --> jpl_tt_td_method_descriptor(T). 
