:- consult('helpers/write_class_inheritance.pl').

jpl_class_to_raw_classname(Cobj, CN) :-
    jpl_classname_to_class('java.lang.Class', CC),      % cached?
    % https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#getName()
    jGetMethodID(CC, getName, method([],class([java,lang],['String'])), MIDgetName),
    jCallObjectMethod(Cobj, MIDgetName, [], [], S),
    S = CN.


first_uno_contact :-
    use_module(library(jpl)),  % SWI-Prolog: use_module/1 in predicate role, as the God of Logic Programming intended
    % XComponentContext is an interface!
    % Java: com.sun.star.uno.XComponentContext xContext = com.sun.star.comp.helper.Bootstrap.bootstrap();
    jpl_call('com.sun.star.comp.helper.Bootstrap','bootstrap',[],XContext),
    format("Connected to a running office with XContext = ~q\n",[XContext]),
    (jpl_is_object(XContext) -> format("XContext is an object\n") ; format("XContext is NOT an object\n")),
    write_class_inheritance_path_of_object(XContext),        
    % Java: com.sun.star.lang.XMultiComponentFactory xMCF = xContext.getServiceManager();
    jpl_object_to_class(XContext,Cobj),
    format("Object ~q has class ~q\n",[XContext,Cobj]),
    jpl_class_to_raw_classname(Cobj, CN), 
    format("Class ~q has raw classname (name of the entity)' ~q\n",[Cobj,CN]),
    atom_codes(CN, CsCN),

    jpl_classname_chars_to_type(CsCN, Tr), % goes FALSE name is 'com.sun.proxy.$Proxy1'

    format("Classname ~q gives type ~q\n",[Cs,Tr]),
 
    %jpl_type_to_canonical_type(Tr, Tx),      


    (jpl_object_to_type(XContext,Type) -> format("~q is of type ~q\n",[XContext,Type]) ; format("~q has no type!\n",[XContext])).


  jpl_type_bare_classname(class(Ps,Cs)) -->
    jpl_type_dotted_package_parts(Ps), jpl_type_class_parts(Cs). 
