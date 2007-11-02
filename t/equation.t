#!perl

# An ambiguous equation

use strict;
use warnings;

use Test::More tests => 4;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $g = new Parse::Marpa(
    start => "E",
    rules => [
	[ "E", [qw/E Op E/], sub {
            my ($right_string, $right_value)
                = ($Parse::Marpa::v[2] =~ /^(.*)==(.*)$/);
            my ($left_string, $left_value)
                = ($Parse::Marpa::v[0] =~ /^(.*)==(.*)$/);
            my $op = $Parse::Marpa::v[1];
            my $value;
            if ($op eq "+") {
               $value = $left_value + $right_value;
            } elsif ($op eq "*") {
               $value = $left_value * $right_value;
            } elsif ($op eq "-") {
               $value = $left_value - $right_value;
            } else {
               croak("Unknown op: $op");
            }
            "(" . $left_string . $op . $right_string . ")==" . $value;
        } ],
	[ "E", [qw/Number/], sub {
           my $v0 = pop @Parse::Marpa::v;
           $v0 . "==" . $v0;
        } ],
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
    is($parse->value(), "(((2-0)*3)+1)==7", "Ambiguous Equation Value");
# }

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
