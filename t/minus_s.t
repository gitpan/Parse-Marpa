use 5.009005;

# An ambiguous equation,
# this time using the lexer

use strict;
use warnings;
use lib "../lib";
use English;

use Test::More tests => 10;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $grammar_source; { local($RS) = undef; $grammar_source = <DATA> };

my $text = "6-----1";

my @values = Parse::Marpa::marpa(\$grammar_source, \$text);

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

my $expected_count = @expected;
my $values_count = @values;

is($expected_count, $values_count, "Expected number of values");

for (my $i = 0; $i <= $#expected; $i++) {
    is(${$values[$i]}, $expected[$i], "Minuses Equation Value $i");
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

__DATA__
semantics are perl5.  version is 0.1.59.

the start symbol is E.

E: E, Minus, E.
q{
    my ($right_string, $right_value)
        = ($Parse::Marpa::This::v->[2] =~ /^(.*)==(.*)$/);
    my ($left_string, $left_value)
        = ($Parse::Marpa::This::v->[0] =~ /^(.*)==(.*)$/);
    my $value = $left_value - $right_value;
    "(" . $left_string . "-" . $right_string . ")==" . $value;
}.
 
E: E, Minus Minus.
q{
    my ($string, $value)
        = ($Parse::Marpa::This::v->[0] =~ /^(.*)==(.*)$/);
    "(" . $string . "--" . ")==" . $value--;
}.

E: Minus Minus, E.
q{
    my ($string, $value)
        = ($Parse::Marpa::This::v->[1] =~ /^(.*)==(.*)$/);
    "(" . "--" . $string . ")==" . --$value;
}.

E: Minus, E.
q{
    my ($string, $value)
        = ($Parse::Marpa::This::v->[1] =~ /^(.*)==(.*)$/);
    "(" . "-" . $string . ")==" . -$value;
}.

E: Number.
q{
    my $value = $Parse::Marpa::This::v->[0];
    "$value==$value";
}.

Number matches qr/\d+/.

Minus matches qr/[-]/.

Minus Minus matches qr/[-][-]/.

the default action is q{
     my $v_count = scalar @$Parse::Marpa::This::v;
     return "" if $v_count <= 0;
     return $Parse::Marpa::This::v->[0] if $v_count == 1;
     "(" . join(";", @$Parse::Marpa::This::v) . ")";
}.
