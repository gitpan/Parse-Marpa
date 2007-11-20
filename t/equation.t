#!perl

# An ambiguous equation

use 5.009005;
use strict;
use warnings;

use Test::More tests => 8;

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
	[ "E", [qw/E Op E/],
<<'EOCODE'
            my ($right_string, $right_value)
                = ($Parse::Marpa::This::v->[2] =~ /^(.*)==(.*)$/);
            my ($left_string, $left_value)
                = ($Parse::Marpa::This::v->[0] =~ /^(.*)==(.*)$/);
            my $op = $Parse::Marpa::This::v->[1];
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
EOCODE
        ],
	[ "E", [qw/Number/],
<<'EOCODE'
           my $v0 = pop @$Parse::Marpa::This::v;
           $v0 . "==" . $v0;
EOCODE
        ],
    ],
    terminals => [
	[ "Number" => [qr/\d+/] ],
	[ "Op" => [qr/[-+*]/] ],
    ],
    default_closure => sub {
         my $v_count = scalar @$Parse::Marpa::This::v;
         return "" if $v_count <= 0;
         return $Parse::Marpa::This::v->[0] if $v_count == 1;
         "(" . join(";", @$Parse::Marpa::This::v) . ")";
    },
);

my $parse = new Parse::Marpa::Parse($g);

my $op = $g->get_symbol("Op");
my $number = $g->get_symbol("Number");
$parse->lex_earleme([$number, 2, 1]);
$parse->lex_earleme([$op, "-", 1]);
$parse->lex_earleme([$number, 0, 1]);
$parse->lex_earleme([$op, "*", 1]);
$parse->lex_earleme([$number, 3, 1]);
$parse->lex_earleme([$op, "+", 1]);
$parse->lex_earleme([$number, 1, 1]);
$parse->lex_end();

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
lexables: Number
 <E> => St1 (2)
 <Number> => St4 (6)
St1: 2
E ::= E . Op E
lexables: Op
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

my @expected = (
    '(((2-0)*3)+1)==7',
    '((2-(0*3))+1)==3',
    '((2-0)*(3+1))==8',
    '(2-((0*3)+1))==1',
    '(2-(0*(3+1)))==2',
);
$parse->initial();

# Set max at 10 just in case there's an infinite loop.
# This is for debugging, after all
PARSE: for my $i (0 .. 10) {
    my $value = $parse->value();
    if ($i > $#expected) {
       fail("Ambiguous equation has extra value: $value\n");
    } else {
        is($value, $expected[$i], "Ambiguous Equation Value $i");
    }
    last PARSE unless $parse->next();
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
