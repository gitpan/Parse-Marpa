require 5.009005;
use strict;
use warnings;
use Fatal qw(chdir open);
use English;
use lib "lib";
use Parse::Marpa;

our $PREAMBLE; open(PREAMBLE, "<", "lib/Parse/Marpa/preamble_Source.pm");
my $preamble; { local($RS) = undef; $preamble = <PREAMBLE> }

if( @ARGV ) {
   open( STDOUT, ">", $ARGV[0] ) or die "Cannot write '$ARGV[0]' - $!";
}

print "# This file was automatically generated\n";
print "# on ".(scalar localtime)."\n";
print "\n";

print $preamble;
print ${Parse::Marpa::create_compiled_source_grammar()};
