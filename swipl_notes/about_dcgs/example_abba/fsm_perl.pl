#!/usr/bin/perl

use strict;
use warnings;

# The string matches this pattern:
#    a minimal amount of chaff
#    ...followed by an optional (because there may not be an 'ab' or 'ba')...
#       ('ab' or 'ba') followed by the rest up to the end of the string

sub process {
   my ($str) = @_;
   my $cur      = $str;
   my $ab_count = 0;
   my $ba_count = 0;
   my $pieces   = [];
   while ($cur ne '') {
      ($cur =~ /^(.*?)((ab|ba)(.*))?$/) || die "Did not recognize '$cur'";
      my $chaff = $1;
      my $hay   = $2;
      push @$pieces, $chaff unless $chaff eq '';   
      if ($hay) {
         my $abba  = $3;
         my $rest  = $4;
         $ab_count++ if ($abba eq 'ab');
         $ba_count++ if ($abba eq 'ba');
         push @$pieces, $abba;
         $cur = $rest
      }
      else {
         $cur = ''
      }
   }
   my $recons_str = join(''  , @$pieces);
   if ($recons_str ne $str) {
      my $recons_dashed_str = join('-' , @$pieces);
      die "The reconstituted string '$recons_str' ($recons_dashed_str) does not match the input string '$str'"
   }
   return { ab_count => $ab_count,
            ba_count => $ba_count,
            pieces   => $pieces }
}

sub test {
   my($str,$ab_count_exp,$ba_count_exp,$dashed_exp) = @_;
   my $result = process($str);
   my $ab_count = $$result{ab_count};
   my $ba_count = $$result{ba_count};
   my $pieces   = $$result{pieces};
   my $recons_dashed_str = join('-' , @$pieces);
   my $fail = 0;
   if (($ab_count != $ab_count_exp) || ($ba_count != $ba_count_exp)) {
      print STDERR "Test failure:\n";
      print STDERR "   Expected $ab_count_exp 'ab', $ba_count_exp 'ba', $dashed_exp\n";
      print STDERR "   Got $ab_count 'ab', $ba_count 'ba', $recons_dashed_str\n";
   } 
}

# ===
# main
# ===

my $str = $ARGV[0]; 

die "You must provide a string to recognize" unless (defined $str);

if ($str eq "test") {
   test(''                ,0,0,'');
   test('bab'             ,0,1,'ba-b');
   test('aba'             ,1,0,'ab-a');
   test('yyabyybayy'      ,1,1,'yy-ab-yy-ba-yy');
   test('yyabbayyabaabaab',4,1,'yy-ab-ba-yy-ab-a-ab-a-ab');
   test('abbaayybbaba'    ,1,3,'ab-ba-ayyb-ba-ba');
   test('abbaabbaba'      ,2,3,'ab-ba-ab-ba-ba');
   test('abbayyabbaba'    ,2,3,'ab-ba-yy-ab-ba-ba');
   test('abbaabbaba'      ,2,3,'ab-ba-ab-ba-ba');
   test('yabybayyyy'      ,1,1,'y-ab-y-ba-yyyy');
   test('yyyyyyyyya'      ,0,0,'yyyyyyyyya');
   test('yyyyyyybaa'      ,0,1,'yyyyyyy-ba-a');
   test('yyyyyyyba'       ,0,1,'yyyyyyy-ba');
   test('yaybyayba'       ,0,1,'yaybyay-ba');
   test('yaabby'          ,1,0,'ya-ab-by');
   print STDERR "All tests passed\n"
}
else {
   my $result            = process($str);
   my $ab_count          = $$result{ab_count};
   my $ba_count          = $$result{ba_count};
   my $pieces            = $$result{pieces};
   my $recons_dashed_str = join('-' , @$pieces);
   print "'$str' contains $ab_count 'ab' and $ba_count 'ba': $recons_dashed_str\n";
   # print "test('$str',$ab_count,$ba_count,'$recons_dashed_str')\n"
}






