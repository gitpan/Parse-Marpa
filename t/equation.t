#!perl

# An amibguous equation

use strict;
use warnings;

use Test::More tests => 4;

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
    default_closure => sub {
         my $v_count = scalar @Parse::Marpa::v;
         return "" if $v_count <= 0;
         return $Parse::Marpa::v[0] if $v_count == 1;
         "(" . join(";", @Parse::Marpa::v) . ")";
    },
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

# print $g->show_rules(), "\n";
is( $g->show_rules(), <<'END_RULES', "Ambiguous Equation Rules" );
0: E -> E Op E
1: E -> Number
2: E['] -> E
END_RULES

# print $g->show_ii_SDFA(), "\n";
is( $g->show_ii_SDFA(), <<'END_SDFA', "Ambiguous Equation SDFA" );
St0: 1,5
E ::= . E Op E
E ::= . Number
 <E> => St1 (2)
 <Number> => St4 (6)
St1: 2
E ::= E . Op E
 <Op> => St2 (3)
St2: 3
E ::= E Op . E
 empty => St0 (1,5)
 <E> => St3 (4)
St3: 4
E ::= E Op E .
St4: 6
E ::= Number .
St5: 7
E['] ::= . E
 empty => St0 (1,5)
 <E> => St6 (8)
St6: 8
E['] ::= E .
END_SDFA

# print $parse->show_status(1);

# TODO: {
    # local $TODO = "Not yet debugged";
    $parse->initial();
    # print $parse->show_status(1);
    is($parse->value(), "(((2;-;0);*;3);+;1)", "Ambiguous Equation Value");
# }

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
