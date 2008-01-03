use 5.010_000;
use strict;
use warnings;
use lib "../lib";

# The Wall Series: a sequence of number generated by an especially
# ambiguous section of Perl syntax, relaxed to ignore precedence
# and lvalue restricitons.

use Test::More tests => 13;

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
    volatile => 0,
    rules => [
        [ "E", [qw/E Minus E/],
<<'EOCODE'
    my ($right_string, $right_value)
        = ($Parse::Marpa::Read_Only::v->[2] =~ /^(.*)==(.*)$/);
    my ($left_string, $left_value)
        = ($Parse::Marpa::Read_Only::v->[0] =~ /^(.*)==(.*)$/);
    my $value = $left_value - $right_value;
    "(" . $left_string . "-" . $right_string . ")==" . $value;
EOCODE
        ],
        [ "E", [qw/E Minus Minus/],
<<'EOCODE'
    my ($string, $value)
        = ($Parse::Marpa::Read_Only::v->[0] =~ /^(.*)==(.*)$/);
    "(" . $string . "--" . ")==" . $value--;
EOCODE
        ],
        [ "E", [qw/Minus Minus E/],
<<'EOCODE'
    my ($string, $value)
        = ($Parse::Marpa::Read_Only::v->[2] =~ /^(.*)==(.*)$/);
    "(" . "--" . $string . ")==" . --$value;
EOCODE
        ],
        [ "E", [qw/Minus E/],
<<'EOCODE'
    my ($string, $value)
        = ($Parse::Marpa::Read_Only::v->[1] =~ /^(.*)==(.*)$/);
    "(" . "-" . $string . ")==" . -$value;
EOCODE
        ],
        [ "E", [qw/Number/],
<<'EOCODE'
            my $value = $Parse::Marpa::Read_Only::v->[0];
            "$value==$value";
EOCODE
        ],
    ],
    terminals => [
        [ "Number" => { regex => qr/\d+/ } ],
        [ "Minus" => { regex => qr/[-] / } ],
    ],
    default_action =>
<<'EOCODE'
     my $v_count = scalar @$Parse::Marpa::Read_Only::v;
     return "" if $v_count <= 0;
     return $Parse::Marpa::Read_Only::v->[0] if $v_count == 1;
     "(" . join(";", @$Parse::Marpa::Read_Only::v) . ")";
EOCODE
);

my @expected = qw(0 1 1 3 4 8 12 21 33 55 88 144 232 );

for my $n (1 .. 12) {

    my $parse = new Parse::Marpa::Parse($g);
    my $minus = $g->get_symbol("Minus");
    my $number = $g->get_symbol("Number");
    $parse->earleme([$number, 6, 1]);
    for my $i (1 .. $n) {
        $parse->earleme([$minus, "-", 1]);
    }
    $parse->earleme([$number, 1, 1]);

    $parse->initial();

    my $parse_count = 0;
    # Set max at 20 just in case there's an infinite loop.
    # This is for debugging, after all
    PARSE: for (my $i = 0;  1; $i++) {
        my $value = $parse->value();
        $parse_count++;
        last PARSE unless $parse->next();
    }

    is($expected[$n], $parse_count, "Wall Series Number $n");

}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
