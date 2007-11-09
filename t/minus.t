#!perl

# An ambiguous equation

use strict;
use warnings;

use Test::More tests => 11;

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
	[ "E", [qw/E Minus E/], sub {
            my ($right_string, $right_value)
                = ($Parse::Marpa::This::v[2] =~ /^(.*)==(.*)$/);
            my ($left_string, $left_value)
                = ($Parse::Marpa::This::v[0] =~ /^(.*)==(.*)$/);
            my $value = $left_value - $right_value;
            "(" . $left_string . "-" . $right_string . ")==" . $value;
        } ],
	[ "E", [qw/E Minus Minus/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[0] =~ /^(.*)==(.*)$/);
            "(" . $string . "--" . ")==" . $value--;
        } ],
	[ "E", [qw/Minus Minus E/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[2] =~ /^(.*)==(.*)$/);
            "(" . "--" . $string . ")==" . --$value;
        } ],
	[ "E", [qw/Minus E/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[1] =~ /^(.*)==(.*)$/);
            "(" . "-" . $string . ")==" . -$value;
        } ],
	[ "E", [qw/Number/], sub {
            my $value = $Parse::Marpa::This::v[0];
            "$value==$value";
        } ],
    ],
    terminals => [
	[ "Number" => qr/\d+/],
	[ "Minus" => qr/[-] /],
    ],
    default_closure => sub {
         my $v_count = scalar @Parse::Marpa::This::v;
         return "" if $v_count <= 0;
         return $Parse::Marpa::This::v[0] if $v_count == 1;
         "(" . join(";", @Parse::Marpa::This::v) . ")";
    },
);

my $parse = new Parse::Marpa::Parse($g);

my $minus = $g->get_symbol("Minus");
my $number = $g->get_symbol("Number");
$parse->earleme([$number, 6, 1]);
$parse->earleme([$minus, "+", 1]);
$parse->earleme([$minus, "+", 1]);
$parse->earleme([$minus, "+", 1]);
$parse->earleme([$minus, "+", 1]);
$parse->earleme([$minus, "+", 1]);
$parse->earleme([$number, 1, 1]);
$parse->earleme();

# print $g->show_rules(), "\n";
is( $g->show_rules(), <<'END_RULES', "Minuses Equation Rules" );
0: E -> E Minus E
1: E -> E Minus Minus
2: E -> Minus Minus E
3: E -> Minus E
4: E -> Number
5: E['] -> E
END_RULES

# print $g->show_ii_SDFA(), "\n";
is( $g->show_ii_SDFA(), <<'END_SDFA', "Minuses Equation SDFA" );
St0: 1,5,9,13,16
E ::= . E Minus E
E ::= . E Minus Minus
E ::= . Minus Minus E
E ::= . Minus E
E ::= . Number
 <E> => St8 (2,6)
 <Minus> => St1 (10,14)
 <Number> => St5 (17)
St1: 10,14
E ::= Minus . Minus E
E ::= Minus . E
 empty => St0 (1,5,9,13,16)
 <E> => St4 (15)
 <Minus> => St2 (11)
St2: 11
E ::= Minus Minus . E
 empty => St0 (1,5,9,13,16)
 <E> => St3 (12)
St3: 12
E ::= Minus Minus E .
St4: 15
E ::= Minus E .
St5: 17
E ::= Number .
St6: 18
E['] ::= . E
 empty => St0 (1,5,9,13,16)
 <E> => St7 (19)
St7: 19
E['] ::= E .
St8: 2,6
E ::= E . Minus E
E ::= E . Minus Minus
 <Minus> => St9 (3,7)
St9: 3,7
E ::= E Minus . E
E ::= E Minus . Minus
 empty => St0 (1,5,9,13,16)
 <E> => St10 (4)
 <Minus> => St11 (8)
St10: 4
E ::= E Minus E .
St11: 8
E ::= E Minus Minus .
END_SDFA

# print $parse->show_status(1);

# $Parse::Marpa::This::trace = 1;

my @expected = (
    '(((6--)--)-1)==5',
    '((6--)-(--1))==6',
    '(6-(--(--1)))==7',
    '(6-(--(-(-1))))==6',
    '((6--)-(-(-1)))==5',
    '(6-(-(-(--1))))==6',
    '(6-(-(-(-(-1)))))==5',
    '(6-(-(--(-1))))==4',
);
$parse->initial();

# Set max at 20 just in case there's an infinite loop.
# This is for debugging, after all
PARSE: for my $i (0 .. 20) {
    my $value = $parse->value();
    if ($i > $#expected) {
       fail("Minuses equation has extra value: $value\n");
    } else {
        is($value, $expected[$i], "Minuses Equation Value $i");
    }
    last PARSE unless $parse->next();
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
