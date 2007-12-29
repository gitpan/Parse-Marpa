# An ambiguous equation

use 5.009005;
use strict;
use warnings;
use lib "../lib";
use English;

use Test::More tests => 6;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $source; { local($RS) = undef; $source = <DATA> };

my $g = new Parse::Marpa(
    source => \$source,
);

my $parse = new Parse::Marpa::Parse($g);

my $op = $g->get_symbol("Op");
my $number = $g->get_symbol("Number");
$parse->earleme([$number, 2, 1]);
$parse->earleme([$op, "-", 1]);
$parse->earleme([$number, 0, 1]);
$parse->earleme([$op, "*", 1]);
$parse->earleme([$number, 3, 1]);
$parse->earleme([$op, "+", 1]);
$parse->earleme([$number, 1, 1]);

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
       fail("Ambiguous equation has extra value: " . $$value . "\n");
    } else {
        is($$value, $expected[$i], "Ambiguous Equation Value $i");
    }
    last PARSE unless $parse->next();
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__
semantics are perl5.  version is 0.1.65.

start symbol is E.

	E: E, Op, E.
q{
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
}.

E: Number.
q{
           my $v0 = pop @$Parse::Marpa::This::v;
           $v0 . "==" . $v0;
}.

Number matches qr/\d+/.

Op matches qr/[-+*]/.
 
the default action is q{
         my $v_count = scalar @$Parse::Marpa::This::v;
         return "" if $v_count <= 0;
         return $Parse::Marpa::This::v->[0] if $v_count == 1;
         "(" . join(";", @$Parse::Marpa::This::v) . ")";
    }.
