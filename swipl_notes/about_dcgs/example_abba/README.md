# Example: Count occurrences of 'ab' and 'ba' in a string

DCGs are actually too powerful for this example, as the string can be recognized by a 
regular expression (regex), i.e. a  (nondeterministic) finite state machine.

On the other hand, this allow us to compare the DCG approach and the Regex approach.

Write this in Perl first. The problem is extracting the counts of `ab` and `ba`. It's 
done in a loop, with the same regular expression applied repeatedly to the ever-smaller
tail of the input string:

Here it is, complete with test cases: [`fsm_perl.pl`](fsm_perl.pl)

Run `fsm_perl.pl` on the command line. It outputs the number of `ab` and `ba` substrings
found with the approach being that those substrings are recognized greedily when scanning
left-to-right and the same character cannot appear in different substrings. The perl 
program outputs the counts for the `ab` and `ba` substrings as well as a string telling how
the input string was cut up into groups:

```
$ perl fsm_perl.pl bab
'bab' contains 0 'ab' and 1 'ba': ba-b

$ perl fsm_perl.pl aba
'aba' contains 1 'ab' and 0 'ba': ab-a

$ perl fsm_perl.pl abbaayybbaba
'abbaayybbaba' contains 1 'ab' and 3 'ba': ab-ba-ayyb-ba-ba

$ perl fsm_perl.pl abbaabbaba
'abbaabbaba' contains 2 'ab' and 3 'ba': ab-ba-ab-ba-ba

$ perl fsm_perl.pl abbayyabbaba
'abbayyabbaba' contains 2 'ab' and 3 'ba': ab-ba-yy-ab-ba-ba
```

Now build the DCG-based Prolog program. There is some subtlety involved because, during the
DCG predicate processing (as opposed to after recognition), we want to:

1. Determine the number of `ab` and `ba` substrings
2. Generate the string telling how the input string was cut up into groups:

Playing manual regex-to-FSM compiler, we come up with the following. It makes heavy use
of state transitions that consume no character (_epsilon_). It is also a non-deterministic
automaton, which must be made (more) deterministic by prioritizing the state transitions
through their clause ordering and committing to paths taken using the `!`.  In particular
`ab` could be consumed by going to intermediate state `F`, then `ab found`, or cycling 
twice through back to `start`, which would be undesired.

![regex_fsm_1](regex_fsm_1.svg)

We can get rid of the intermediate `F` state and soon find a simpler alternative:

[![regex_fsm_2](regex_fsm_2.svg)

Coding this into a Prolog DCG is a bit tricky:

Here it is, complete with test cases: [`fsm_prolog.pl`](fsm_prolog.pl)



