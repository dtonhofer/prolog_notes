% ============================================================================
% An as yet incomplete reading of file /proc/self/status. 
% On Linux systems, this gives process information
% 
% See https://www.kernel.org/doc/html/latest/filesystems/proc.html?highlight=VmPTE
% ============================================================================
% Load module with:
% 
% ?- use_module(library('snippets/proc_self_status.pl')).
% ============================================================================

:- module(proc_self_status,
          [
          proc_self_status/1
          ]).
          
proc_self_status(DictOut) :-
   setup_call_cleanup(
         open("/proc/self/status",read,Stream,[]),
         read_stream_to_codes(Stream,Codes),
         close(Stream)
   ),
   phrase(lines(proc_self_status{},DictOut),Codes),
   !.
   
% Note that even though "XXX" is a string in SWI-Prolog, in the
% DCG rule, "XXX" is interpreted as a list of codes.

lines(Dict,Dict)      --> [].
lines(DictIn,DictOut) --> line(DictIn,Dict2),lines(Dict2,DictOut).

line(DictIn,DictOut)  --> datum(DictIn,DictOut), "\n" , !.
line(Dict,Dict)       --> string_without("\n",_Codes), "\n".
% line(Dict,Dict)       --> string_without("\n",Codes), "\n", { format(user_error,"Unknown: <~s>~n",[Codes]) }.

datum(DictIn,DictIn.put([name=Str]))                 --> "Name:", string_to_eol(Str).
datum(DictIn,DictIn.put([pid=Int]))                  --> "Pid:", just_integer(Int).
datum(DictIn,DictIn.put([ppid=Int]))                 --> "PPid:", just_integer(Int).
datum(DictIn,DictIn.put([tgid=Int]))                 --> "Tgid:", just_integer(Int).
datum(DictIn,DictIn.put([ngid=Int]))                 --> "Ngid:", just_integer(Int).
datum(DictIn,DictIn.put([tracerpid=Int]))            --> "TracerPid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_tgid=Int]))              --> "NStgid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_pid=Int]))               --> "NSpid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_pgid=Int]))              --> "NSpgid:", just_integer(Int).
datum(DictIn,DictIn.put([ns_sid=Int]))               --> "NSsid:", just_integer(Int).
datum(DictIn,DictIn.put([vm_data=[Val,Unit]]))       --> "VmData:", val_and_unit(Val,Unit). % size of private data segments
datum(DictIn,DictIn.put([vm_stk=[Val,Unit]]))        --> "VmStk:", val_and_unit(Val,Unit). % size of stack segments
datum(DictIn,DictIn.put([vm_exe=[Val,Unit]]))        --> "VmExe:", val_and_unit(Val,Unit). % size of text segment
datum(DictIn,DictIn.put([vm_lib=[Val,Unit]]))        --> "VmLib:", val_and_unit(Val,Unit). % size of shared library code
datum(DictIn,DictIn.put([vm_pte=[Val,Unit]]))        --> "VmPTE:", val_and_unit(Val,Unit). % size of page table entries
datum(DictIn,DictIn.put([vm_swap=[Val,Unit]]))       --> "VmSwap:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_peak=[Val,Unit]]))       --> "VmPeak:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_size=[Val,Unit]]))       --> "VmSize:", val_and_unit(Val,Unit). % total program size
datum(DictIn,DictIn.put([vm_lck=[Val,Unit]]))        --> "VmLck:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_pin=[Val,Unit]]))        --> "VmPin:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_hwm=[Val,Unit]]))        --> "VmHWM:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([vm_rss=[Val,Unit]]))        --> "VmRSS:", val_and_unit(Val,Unit).  % size of memory portions. It contains the three following parts (VmRSS = RssAnon + RssFile + RssShmem)
datum(DictIn,DictIn.put([rss_anon=[Val,Unit]]))      --> "RssAnon:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([rss_file=[Val,Unit]]))      --> "RssFile:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([rss_shmem=[Val,Unit]]))     --> "RssShmem:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([hugetlb_pages=[Val,Unit]])) --> "HugetlbPages:", val_and_unit(Val,Unit).
datum(DictIn,DictIn.put([core_dumping=Int]))         --> "CoreDumping:", just_integer(Int).
datum(DictIn,DictIn.put([thp_enabled=Int]))          --> "THP_enabled:", just_integer(Int).
datum(DictIn,DictIn.put([threads=Int]))              --> "Threads:", just_integer(Int).
datum(DictIn,DictIn.put([umask=Str]))                --> "Umask:", string_to_eol(Str). % actually a string of 4 octals
datum(DictIn,DictIn.put([state=Str]))                --> "State:", string_to_eol(Str).

val_and_unit(Val,Unit) --> whites, integer(Val), whites, string_without("\n",Codes), { string_codes(Unit,Codes) }.
just_integer(Int)      --> whites, integer(Int).
string_to_eol(Str)     --> whites, string_without("\n",Codes), { string_codes(Str,Codes) }.
