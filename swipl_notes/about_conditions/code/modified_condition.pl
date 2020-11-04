% ===
% Code originally from package (condition), slightly
% modified
% ===


:- module(condition, [ handle/2
                     , handle/3
                     , signal/2
                     , signal/3
                     , add_handler/2
                     , rm_handler/1
                     ]).

% handle/2 adds clauses to this predicate dynamically
:- thread_local handler/2.

%%  signal(+Condition, -Restart) is multi.
%
%   Signal a Condition and allow handlers to bind Restart.
%   This predicate is the mechanism
%   by which code indicates that it doesn't know how to
%   proceed and is requesting assistance in choosing a path forward.
%
%   It's possible for ancestors to disagree about Restart (aka,
%   different values on backtracking). In this scenario, it's acceptable
%   to choose the first (innermost) Restart, iterate each Restart in turn or
%   consider all Restart values together (quorum?).
%
%   If no ancestor has an opinion, signal/2 calls =|throw(Condition)|=.
%   If you'd rather use a default value for Restart in this case,
%   use signal/3 instead.
%
%   It's quite common for signal/2 to leave dangling, `false`
%   choicepoints. If you're only interested in the first Restart value,
%   use once/1 or a similar construct to explicitly state that intent.
signal(Condition,Restart) :-
    ( handler(Condition,Restart) *-> true ; throw(Condition) ).


%% signal(+Condition,+Default,-Restart) is multi.
%
%  Like signal/2 but unifies Restart with Default if nobody handles
%  this Condition.  It can be helfpul to use Restart as its own default
%  allowing you to check =|var(Restart)|= to see if a restart was provided.
%  For example,
%
%      signal(oops, Restart, Restart),
%      ( var(Restart) ->
%          print_message(warning,"Nobody provided a restart")
%      ; ...
%      )
signal(Condition,Default,Restart) :-
    catch(signal(Condition,Restart),Condition,Restart=Default).


%%  handle(:Goal, +Condition, +Restart)
%
%   Like handle/2 with unification as the restarter.  This
%   builds a restarter which only uses Restart if the condition unifies
%   with Condition. It addresses the common situation where one knows
%   both Condition and Restart before calling Goal.  For example,
%
%       % if stuff signals oops(_) then restart with carry_on
%       handle(stuff, oops(_), carry_on)
:- meta_predicate condition:handle(0,?,?).
handle(Goal, Condition, Restart) :-
    handle(Goal, {Condition,Restart}/[C,R]>>(C=Condition -> R=Restart)).


%%  handle(:Goal, +Restarter)
%
%   Handles a condition signaled by Goal. Goal is called as with
%   call/1. If Goal, or a child goal thereof, signals a condition
%   (via signal/2) execute `call(Restarter, Condition, Restart)` to
%   determine which Restart value should be sent to the signaler.
%   Restarter may fail which allows other handlers higher up the call
%   stack to address this condition. Restarter may produce multiple
%   Restart values on backtracking, if multiple restarts are plausible.
%
%   When using handle/2, consult the documentation of those predicates
%   that might signal conditions. They'll explain which Restart values
%   are acceptable, what they do and whether generating Restart values
%   on backtracking makes sense.
%
%   If more than one handle/2 is in effect within the current call
%   stack, Restarter values are executed from the innermost to
%   the outermost ancestor. This allows those "closest" to the signal a
%   chance to handle it before it propagates outward.  Of course, if
%   a signaler looks at multiple solutions, other handlers will be
%   executed too.
:- meta_predicate condition:handle(0,2).
handle(Goal, Restarter) :-
    setup_call_cleanup(
        add_handler(Restarter,Ref),
        Goal,
        rm_handler(Ref)
    ).


%% add_handler(+Restarter, -Ref) is det.
%
%  Add a handler for all subsequent signals.  Used in combination with
%  rm_handler/1.  This can be helpful for setting a handler for
%  multiple goals.  For example,
%
%      add_handler(restarter, Ref),
%      foo(a),  % signals handled by restarter/2
%      foo(b),  % ditto
%      rm_handler(Ref).
%
%  Of course, to be safe, one should use the built in setup_call_cleanup/3
%  or cleanup/1 provided by library(sweet).  That makes sure the handler
%  is removed no matter how foo/1 misbehaves.
add_handler(Restarter,Ref) :-
    condition:asserta((handler(C,R) :- call(Restarter,C,R)), Ref).


%% rm_handler(+Ref) is det.
%
%  Remove a handler from all subsequent signals.  Used in combination with
%  add_handler/2.
rm_handler(Ref) :-
    erase(Ref).
