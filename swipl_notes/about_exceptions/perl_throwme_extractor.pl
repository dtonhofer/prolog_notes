#!/usr/bin/perl

use strict;
use warnings;

main();

# ---
# Read stdin line by line and extract "throwme()" calls.
# Don't bother to detect comments.
# ---

sub main {
   # First argument is an integer (or missing)
   # Output will be indented by that many spaces on output
   my $indent=$ARGV[1];
   if (defined $indent) { $indent=int($indent*1) } else { $indent = 0 }
   my $test_case_counter = 1;
   while (my $line = <>) {
      chomp $line;
      if ($line =~ /\bthrowme\((.*)$/) {
         my $args = extract_args($1);
         my $call = "throwme(" . $args;
         output($test_case_counter,$indent,$call);
         $test_case_counter++;
      }
   }
}

sub output {
   my($test_case_counter,$indent,$call) = @_;
   for (my $i=0;$i<$indent;$i++) { print " " }
   print
      "test(",
      sprintf("%2d",$test_case_counter),
      ") :- exc_test(",
      $call,
      ").\n";
}

sub extract_args {
   my($argstr) = @_;
   my $depth = 1; # the first opening parenthesis has already been grabbed
   my $i = 0;
   my $extracted = "";
   my $sof = 1; # boolean; if true, expect a "start of term"
   while ($i < length($argstr) && $depth > 0) {
      my $ch = substr($argstr,$i,1);
      if ($ch eq '(') {
         $depth++;
         $sof = 1;
         $extracted .= $ch
      }
      elsif ($ch eq ')') {
         $depth--;
         $sof = 1;
         $extracted .= $ch
      }
      elsif ($ch =~ /[A-Za-z0-9_]/) {
         if ($sof) {
            if ($ch =~ /[A-Z]/ && $ch ne '_') {
               # looks like a variable, anonymize it!
               $extracted .= "_";
            }
            $sof = 0
         }
         $extracted .= $ch  
      }
      else {
         # probabyl punctuation characters etc; don't analyze too closely for now
         $extracted .= $ch;  
         $sof = 1
      }
      $i++
   }
   die "Non zero depth $depth at end of string $argstr" unless $depth == 0; # probably an error in the Prolog code
   return $extracted
}

