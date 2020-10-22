% ---
% Define a list of allowed values for "zone names" once, centrally:
% ---

const(zone_names,[zone_a,zone_b,zone_c]).

% ---
% A dynamic predicate through which verification code can be disabled on demand:
% ---

:- dynamic verify/2.

verify(foo/4).   % It's on for foo/4!

% ---
% The predicate to verify all the arguments that will be passed to core_foo/4.
% It either succeeds (deterministically) or throws.
% ---

before_foo(X,Y,Z,ZoneName) :-
   const(zone_names,ZoneNames),                        % Retrieve the allowed values for Zone names
   (nonvar(X)  -> true ; instantiation_error('X')),    % X needs to be instantiated
   (nonvar(Y)  -> true ; instantiation_error('Y')),    % Y needs to be instantiated
   (var(Z)     -> true ; uninstantiation_error('Z')),  % Z needs to be uninstantiated (to take up numeric value)
   (number(X)  -> true ; type_error(number,X)),        % X,Y must be a number: that's a condition on the type
   (number(Y)  -> true ; type_error(number,Y)),
   ((0=<X,X<2) -> true ; domain_error("[0,2[",X)),     % X,Y must be within a known 1-D range: that's a condition on the domain
   ((0=<Y,Y<2) -> true ; domain_error("[0,2[",Y)),     % ISO standard says nothing about how to *express* the domain :-(
   (\+ (1=<X,X<2,1=<Y,Y=<2)                            % X,Y must be within a known 2-D range: that's a condition on the domain
       -> true
       ;  domain_error("valid zone",[X,Y])),           % Again, how do we *express* the domain in the exception term??
   ((var(ZoneName);atom(ZoneName))                     % ZoneName must be uninstantiated or an atom (to compare against)
       -> true
       ;  type_error(atom,ZoneName)),                  % ... that's a condition on the type
   (nonvar(ZoneName)                                   % If Zone has been given
      -> (memberchk(ZoneName,ZoneNames)                % ... it must be one of the allowed zone values ("enum")
         -> true
         ;  domain_error(ZoneNames,ZoneName))          % Otherwise it's a domain error.
      ; true).                                         % Again, expressing "what's wrong" and "what would be right" is nearly impossible

% ---
% Core foo/4 code
% ---

guard_zone_a(X,Y) :- 0=<X, X<1, 0=<Y, Y<1.
guard_zone_c(X,Y) :- 0=<X, X<1, 1=<Y, Y<2.
guard_zone_b(X,Y) :- 1=<X, X<2, 0=<Y, Y<1.

core_foo(X,Y,Z,zone_a) :- guard_zone_a(X,Y) -> (Z is X+Y;Z is X*Y).
core_foo(X,Y,Z,zone_c) :- guard_zone_c(X,Y) -> Z is X*Y*Y.
core_foo(X,Y,Z,zone_b) :- guard_zone_b(X,Y) -> Z is X*X*Y.

% ---
% Actual foo/4 code, which throws if arguments are no to its liking, and otherwise
% proceeds with core_foo/4, which succeeds or fails in its proper domain
% ---

foo(X,Y,Z,ZoneName) :-
   (verify(foo/4) -> \+ \+ before_foo(X,Y,Z,ZoneName) ; true),
   core_foo(X,Y,Z,ZoneName).

% ---
% Completely lenient version which fails instead of throwing.
% ---

foo_lenient(X,Y,Z,ZoneName) :-
   catch(foo(X,Y,Z,ZoneName),_Catcher,fail).
