#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

diag( "Testing Parse::Marpa $Parse::Marpa::VERSION, Perl $], $^X" );
