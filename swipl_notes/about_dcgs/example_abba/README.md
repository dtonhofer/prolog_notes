# Example: Count occurrences of 'ab' and 'ba' in a string

DCGs are actually too powerful for this example, as the string can be recognized by a 
regular expression (as given by a simple regex), i.e. a  (nondeterministic) finite state machine.

On the other hand, this allow us to compare the DCG approach and the regex approach.

In this, we use Perl Regular Expressions.

- [Cheatsheet for Perl Regular Expressions](https://perlmaven.com/regex-cheat-sheet)
- Wikipedia: [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression)
- Wikipedia: [PCRE: Perl Compatible Regular Expressions Library](https://en.wikipedia.org/wiki/Perl_Compatible_Regular_Expressions)
- SWI-Prolog's wrapper around PCRE [SWI-Prolog Regular Expression library](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pcre.html%27))
- A regex-testing tool: https://regex101.com/

Note that, as said in the Wikipedia article on "Regular Expressions":

> It is worth noting that many real-world "regular expression" engines implement features that cannot be
> described by the regular expressions in the sense of formal language theory; rather, they implement _regexes_. 

## In Perl, using Perl Regular Expressions

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

## In Prolog, using DCGs 

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

![regex_fsm_2](regex_fsm_2.svg)

Here it is, complete with test cases: [`fsm_prolog.pl`](fsm_prolog.pl)

Note that:

- Non-DCG Calls to Prolog have to be made inside the DCG clauses to compute the latest list of runs (`Pieces`).
- CLP(FD) is used to make the recursive DCG call on last position (not sure it helps).

Run it on the Prolog Toplevel:

```
?- [fsm_prolog].
true.

?- fsm_parse('bab',AB,BA,D,[]).
AB = 0,
BA = 1,
D = 'ba-b'.

?- fsm_parse('aba',AB,BA,D,[]).
AB = 1,
BA = 0,
D = 'ab-a'.

?- fsm_parse('yyabyybayy',AB,BA,D,[]).
AB = BA, BA = 1,
D = 'yy-ab-yy-ba-yy' ;
false.

?- rt(dcg_chars).
% PL-Unit: dcg_chars ............... done
% All 15 tests passed
true.
```

## In Prolog, using Perl Regular Expressions

SWI-Prolog comes with a library wrapping the PCRE library: 
[SWI-Prolog Regular Expression library](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pcre.html%27)).
Let's try it!



