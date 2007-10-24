#!perl

# An amibguous equation

use strict;
use warnings;

use Test::More tests => 2;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $g = new Parse::Marpa(
    start => "E",
    rules => [
	[qw/E E Op E/],
	[qw/E Number/],
	[ "Number" => qr/\d+/],
	[ "Op" => qr/[-+*] /],
    ],
);

my $parse = new Parse::Marpa::Parse($g);

my $op = $g->get_symbol("Op");
my $number = $g->get_symbol("Number");
$parse->token([$number, 1, 1]);
$parse->token([$op, "-", 1]);
$parse->token([$number, 3, 1]);
$parse->token([$op, "*", 1]);
$parse->token([$number, 5, 1]);
$parse->token([$op, "+", 1]);
$parse->token([$number, 7, 1]);
$parse->token();

print $g->show_rules(), "\n";
print $g->show_ii_SDFA(), "\n";

print $parse->show_status(1);

TODO: {
    local $TODO = "Not yet debugged";
my $evaluator = new Parse::Marpa::Evaluator($parse);
if ($evaluator) {
    is( $evaluator->show_evaluator(1), '', "Aycock/Horspool Evaluator Tree" );
} else {
    fail("Valid parse in evaluator");
    diag("No valid parse in evaluator");
}

}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
