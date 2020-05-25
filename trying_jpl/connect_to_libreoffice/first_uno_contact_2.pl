first_uno_contact :-
    use_module(library(jpl)),  % SWI-Prolog: use_module/1 in predicate role, as the God of Logic Programming intended
    % XComponentContext is an interface!
    % Java: com.sun.star.uno.XComponentContext xContext = com.sun.star.comp.helper.Bootstrap.bootstrap();
    jpl_call('com.sun.star.comp.helper.Bootstrap','bootstrap',[],XContext),
    format("Connected to a running office with XContext = ~q\n",[XContext]),
    (jpl_is_object(XContext) -> format("XContext is an object\n") ; format("XContext is NOT an object\n")),
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
    

