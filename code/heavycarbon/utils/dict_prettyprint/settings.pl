:- module(heavycarbon_utils_dict_prettyprint_settings,
          [
           get_setting/3
          ,get_setting/4
          ,get_padding_settings/5
          ,get_padding_settings_modulated/6 
          ]).

% ----------

/** <module> dict prettyprinter helper predicates

Predicates to extract settings from "SettingsDict".

Predicates in this module are used by the "dict prettyprinter" code.

If you need to load this module manually, run:

```
?- use_module(library('heavycarbon/utils/dict_prettyprint/settings.pl')).
```

@license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
@author David Tonhofer (ronerycoder@gluino.name)

*/

:- use_module(library('heavycarbon/utils/dict_prettyprint/checking.pl')).

% ----------

%! get_setting(+SettingsDict,+Key,?Value,+Default)
% 
% Instantiate Value to the value stored under Key in SettingsDict. If it
% is missing, unifies Value with Default.

get_setting(SettingsDict,Key,Value,_Default) :-
   get_dict(Key,SettingsDict,Value),!.            % get_dict/3 succeeds if entry with "Key" exists

get_setting(_,_,ValueIsDefault,ValueIsDefault).   % fallback to "Default"

% ----------

%! get_setting(+SettingsDict,+Key,?Value)
%
% Instantiate Value to the value stored under Key in SettingsDict. If it
% is missing an exception is thrown.

get_setting(SettingsDict,Key,Value) :-            % get_dict/3 succeeds if entry with "Key" exists
   get_dict(Key,SettingsDict,Value),!.

get_setting(SettingsDict,Key,_) :-                % otherwise explicitly throw
   format(string(Msg),"dict with key ~q",[Key]), 
   domain_error(Msg,SettingsDict). 

% ----------

%! get_padding_settings(+SettingDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
%
% Get multiple specific values, those for paddings, in one call.
% This predicate also asserts that the retrieved values are all integers >= 0.

get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight) :-
   get_setting(SettingsDict,pad_top,    PadTop    ,0),
   get_setting(SettingsDict,pad_bottom, PadBottom ,0),
   get_setting(SettingsDict,pad_left,   PadLeft   ,0),
   get_setting(SettingsDict,pad_right,  PadRight  ,0),
   assertion(is_list_of_positive_integers([PadTop,PadBottom,PadLeft,PadRight])).

% ----------

%! get_padding_settings_modulated(+DecisionForPadding,+SettingsDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
%
% Get the settings for the padding, but clamp them all to 0 if DecisionForPadding is =false=.

get_padding_settings_modulated(false,_,0,0,0,0) :- !.

get_padding_settings_modulated(true,SettingsDict,PadTop,PadBottom,PadLeft,PadRight) :-
   get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight).

