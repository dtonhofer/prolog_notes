first_uno_contact :-
    use_module(library(jpl)),  % SWI-Prolog: use_module/1 in predicate role, as the God of Logic Programming intended
    % XComponentContext is an interface!
    % Java: com.sun.star.uno.XComponentContext xContext = com.sun.star.comp.helper.Bootstrap.bootstrap();
    jpl_call('com.sun.star.comp.helper.Bootstrap','bootstrap',[],XContext),
    format("Connected to a running office with XContext = ~q\n",[XContext]),
    (jpl_is_object(XContext) -> format("XContext is an object\n") ; format("XContext is NOT an object\n")),
    write_class_inheritance_path_of_object(XContext),        
    % Java: com.sun.star.lang.XMultiComponentFactory xMCF = xContext.getServiceManager();
    (jpl_object_to_type(XContext,Type) -> format("~q is of type ~q\n",[XContext,Type]) ; format("~q has no type!\n",[XContext])),
    jpl_call_entrycheck(XContext,'getServiceManager',[],_XMcf). % THIS THROWS


jpl_call_entrycheck(X, Mspec, Params, R) :-
    (   jpl_object_to_type(X, Type)         % the usual case (goal fails safely if X is var or rubbish)
    ->  Obj = X,
        Kind = instance
    ;   var(X)
    ->  throw(error(instantiation_error,context(jpl_call/4,'1st arg must be bound to an object, classname, descriptor or type')))
    ;   atom(X)
    ->  (   jpl_classname_to_type(X, Type)     % does this attempt to load the class?
        ->  (   jpl_type_to_class(Type, ClassObj)
            ->  Kind = static
            ;   throw(error(existence_error(class,X),context(jpl_call/4,'the named class cannot be found')))
            )
        ;   throw(error(type_error(class_name_or_descriptor,X),context(jpl_call/4,'1st arg must be an object, classname, descriptor or type')))
        )
    ;   X = class(_,_)
    ->  Type = X,
        jpl_type_to_class(Type, ClassObj),
        Kind = static
    ;   X = array(_)
    ->  throw(error(type_error(object_or_class,X),context(jpl_call/4,'cannot call a static method of an array type, as none exists')))
    ;   throw(error(domain_error(object_or_class,X),context(jpl_call/4,'1st arg must be an object, classname, descriptor or type')))
    ).
    
% ===
% A Java String is Prologified into an atom
% ===

simple(str) :-
    use_module(library(jpl)),
    jpl_new('java.lang.String',['Hello, World'],JS),
    format("Obtained Java String = ~q\n",[JS]),
    (jpl_is_object(JS) -> format("JS is an object\n") ; format("JS is NOT an object\n")),
    (atom(JS) -> format("JS is an atom\n") ; format("JS is NOT an atom\n")).
    
% ===
% A Gregorian Calendar stays a Gregorian Calendar
% ===

simple(cal) :-
    use_module(library(jpl)),
    jpl_new('java.util.GregorianCalendar',[],JGC),
    format("Obtained Java GregorianCalendar = ~q\n",[JGC]),
    (jpl_is_object(JGC) -> format("JGC is an object\n") ; (format("JGC is NOT an object\n"),fail)),
    write_class_inheritance_path_of_object(JGC).
    
write_class_inheritance_path_of_object(JObj) :- 
    assertion(jpl_is_object(JObj)),
    jpl_object_to_class(JObj, JClass),
    format("Object is of Class ~q\n",[JClass]),
    write_class_inheritance_path(JClass).
    
write_class_inheritance_path(JX) :-     
    assertion(jpl_is_object(JX)), % it's an object
    assertion((jpl_object_to_class(JX,JXClass),jpl_class_to_classname(JXClass,'java.lang.Class'))), % of type java.lang.Class
    jpl_class_to_classname(JX, ClassName),
    format("Class ~q has name ~q\n",[JX,ClassName]),
    jpl_call(JX,'getSuperclass',[],JSuperClass),
    (jpl_null(JSuperClass)
     -> 
     true
     ;     
     (format("Class ~q has superclass ~q\n",[JX,JSuperClass]),
      write_class_inheritance_path(JSuperClass))).
    
