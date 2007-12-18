use strict;
use warnings;
use feature ":5.10";
use English;
use lib "../lib";
use Parse::Marpa;
use Carp;
use Fatal qw(open close);

sub usage {
   die("usage: $0 < raw-file > compile-file\n");
}

usage() unless scalar @ARGV == 0;

my $raw_grammar; { local($RS) = undef; $raw_grammar = <STDIN>; }

my $grammar;
{
   local $SIG{__WARN__} = sub {
       croak(
	   $_[0],
	   "Marpa got a warning from Data::Dumper and can't continue"
       );
   };
   eval $raw_grammar;
}

print ${$grammar->compile()};
