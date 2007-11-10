#!perl

# An ambiguous equation,
# this time using the lexer

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
	[ "E", [qw/E MinusMinus/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[0] =~ /^(.*)==(.*)$/);
            "(" . $string . "--" . ")==" . $value--;
        } ],
	[ "E", [qw/MinusMinus E/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[1] =~ /^(.*)==(.*)$/);
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
	[ "Minus" => qr/[-]/],
	[ "MinusMinus" => qr/[-][-]/],
    ],
    default_closure => sub {
         my $v_count = scalar @Parse::Marpa::This::v;
         return "" if $v_count <= 0;
         return $Parse::Marpa::This::v[0] if $v_count == 1;
         "(" . join(";", @Parse::Marpa::This::v) . ")";
    },
);

my $parse = new Parse::Marpa::Parse($g);

# print $g->show_rules(), "\n";
is( $g->show_rules(), <<'END_RULES', "Minuses Equation Rules" );
0: E -> E Minus E
1: E -> E MinusMinus
2: E -> MinusMinus E
3: E -> Minus E
4: E -> Number
5: E['] -> E
END_RULES

# print $g->show_ii_SDFA(), "\n";
is( $g->show_ii_SDFA(), <<'END_SDFA', "Minuses Equation SDFA" );
St0: 1,5,8,11,14
E ::= . E Minus E
E ::= . E MinusMinus
E ::= . MinusMinus E
E ::= . Minus E
E ::= . Number
 <E> => St7 (2,6)
 <Minus> => St2 (12)
 <MinusMinus> => St11 (9)
 <Number> => St4 (15)
St1: 10
E ::= MinusMinus E .
St2: 12
E ::= Minus . E
 empty => St0 (1,5,8,11,14)
 <E> => St3 (13)
St3: 13
E ::= Minus E .
St4: 15
E ::= Number .
St5: 16
E['] ::= . E
 empty => St0 (1,5,8,11,14)
 <E> => St6 (17)
St6: 17
E['] ::= E .
St7: 2,6
E ::= E . Minus E
E ::= E . MinusMinus
 <Minus> => St8 (3)
 <MinusMinus> => St10 (7)
St8: 3
E ::= E Minus . E
 empty => St0 (1,5,8,11,14)
 <E> => St9 (4)
St9: 4
E ::= E Minus E .
St10: 7
E ::= E MinusMinus .
St11: 9
E ::= MinusMinus . E
 empty => St0 (1,5,8,11,14)
 <E> => St1 (10)
END_SDFA

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

SKIP: {
    skip "Not yet debugged", (scalar @expected);

    my $minus = $g->get_symbol("Minus");
    my $number = $g->get_symbol("Number");
    $parse->lex(\("6-----1"));
    $parse->lex_end();

# Set max at 20 just in case there's an infinite loop.
# This is for debugging, after all
PARSE: for my $i (0 .. 20) {
    my $value = $parse->value();
    # print $parse->show();
    if ($i > $#expected) {
       fail("Minuses equation has extra value: $value\n");
    } else {
        is($value, $expected[$i], "Minuses Equation Value $i");
    }
    last PARSE unless $parse->next();
}

} # SKIP

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
