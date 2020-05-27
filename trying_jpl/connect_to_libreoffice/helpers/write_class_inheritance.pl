% ===
% Given the reference to a Java Object, write the Java Object's
% class inheritance path out
% ===
   
write_class_inheritance_path_of_object(JObj) :- 
    assertion(jpl_is_object(JObj)),
    jpl_object_to_class(JObj, JClass),
    format("Object is of Class ~q\n",[JClass]),
    write_class_inheritance_path(JClass).
    
write_class_inheritance_path(JX) :-     
    %
    % it's an Object of type java.lang.Class
    %
    assertion(jpl_is_object(JX)), 
    assertion((jpl_object_to_class(JX,JXClass),jpl_class_to_classname(JXClass,'java.lang.Class'))),
    %
    % print current class name
    %
    jpl_class_to_classname(JX, ClassName),
    format("Class ~q has name ~q\n",[JX,ClassName]),
    %
    % move to parent if it exists 
    %
    jpl_call(JX,'getSuperclass',[],JSuperClass),
    (jpl_null(JSuperClass) -> true ; write_class_inheritance_path(JSuperClass)).
    
