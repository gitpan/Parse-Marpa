#!perl

# An amibguous equation

use strict;
use warnings;

use Test::More tests => 1;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $g = new Parse::Marpa(
    start => "E",
    rules => [
	[ "E", [qw/E Op E/] ],
	[ "E", [qw/Number/] ],
	[ "Number" => qr/\d+/],
	[ "Op" => qr/[-+*] /],
    ],
);

my $parse = new Parse::Marpa::Parse($g);

my $op = $g->get_symbol("Op");
my $number = $g->get_symbol("Number");
$parse->token([$number, 2, 1]);
$parse->token([$op, "-", 1]);
$parse->token([$number, 0, 1]);
$parse->token([$op, "*", 1]);
$parse->token([$number, 3, 1]);
$parse->token([$op, "+", 1]);
$parse->token([$number, 1, 1]);
$parse->token();

print $g->show_rules(), "\n";
print $g->show_ii_SDFA(), "\n";

# print $parse->show_status(1);

TODO: {
    local $TODO = "Not yet debugged";
    $parse->initial();
    print $parse->show_status(1);
    # if ($evaluator) {
        # is( $evaluator->show_evaluator(1), '', "Aycock/Horspool Evaluator Tree" );
    # } else {
        # fail("Valid parse in evaluator");
        # diag("No valid parse in evaluator");
    # }
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
