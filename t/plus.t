#!perl

# An ambiguous equation

use strict;
use warnings;

use Test::More tests => 10;

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
	[ "E", [qw/E Plus E/], sub {
            my ($right_string, $right_value)
                = ($Parse::Marpa::This::v[2] =~ /^(.*)==(.*)$/);
            my ($left_string, $left_value)
                = ($Parse::Marpa::This::v[0] =~ /^(.*)==(.*)$/);
            my $value = $left_value + $right_value;
            "(" . $left_string . "+" . $right_string . ")==" . $value;
        } ],
	[ "E", [qw/E Plus Plus/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[0] =~ /^(.*)==(.*)$/);
            "(" . $string . "++" . ")==" . $value++;
        } ],
	[ "E", [qw/Plus Plus E/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[2] =~ /^(.*)==(.*)$/);
            "(" . "++" . $string . ")==" . ++$value;
        } ],
	[ "E", [qw/Plus E/], sub {
            my ($string, $value)
                = ($Parse::Marpa::This::v[1] =~ /^(.*)==(.*)$/);
            "(" . "+" . $string . ")==" . +$value;
        } ],
	[ "E", [qw/Var/], sub {
           '$one==1';
        } ],
	[ "Var" => qr/\$x/],
	[ "Plus" => qr/[+] /],
    ],
    default_closure => sub {
         my $v_count = scalar @Parse::Marpa::This::v;
         return "" if $v_count <= 0;
         return $Parse::Marpa::This::v[0] if $v_count == 1;
         "(" . join(";", @Parse::Marpa::This::v) . ")";
    },
);

my $parse = new Parse::Marpa::Parse($g);

my $plus = $g->get_symbol("Plus");
my $var = $g->get_symbol("Var");
$parse->token([$var, 1, 1]);
$parse->token([$plus, "+", 1]);
$parse->token([$plus, "+", 1]);
$parse->token([$plus, "+", 1]);
$parse->token([$plus, "+", 1]);
$parse->token([$plus, "+", 1]);
$parse->token([$var, 1, 1]);
$parse->token();

# print $g->show_rules(), "\n";
is( $g->show_rules(), <<'END_RULES', "Pluses Equation Rules" );
0: E -> E Plus E
1: E -> E Plus Plus
2: E -> Plus Plus E
3: E -> Plus E
4: E -> Var
5: E['] -> E
END_RULES

# print $g->show_ii_SDFA(), "\n";
is( $g->show_ii_SDFA(), <<'END_SDFA', "Pluses Equation SDFA" );
St0: 1,5,9,13,16
E ::= . E Plus E
E ::= . E Plus Plus
E ::= . Plus Plus E
E ::= . Plus E
E ::= . Var
 <E> => St8 (2,6)
 <Plus> => St1 (10,14)
 <Var> => St5 (17)
St1: 10,14
E ::= Plus . Plus E
E ::= Plus . E
 empty => St0 (1,5,9,13,16)
 <E> => St4 (15)
 <Plus> => St2 (11)
St2: 11
E ::= Plus Plus . E
 empty => St0 (1,5,9,13,16)
 <E> => St3 (12)
St3: 12
E ::= Plus Plus E .
St4: 15
E ::= Plus E .
St5: 17
E ::= Var .
St6: 18
E['] ::= . E
 empty => St0 (1,5,9,13,16)
 <E> => St7 (19)
St7: 19
E['] ::= E .
St8: 2,6
E ::= E . Plus E
E ::= E . Plus Plus
 <Plus> => St9 (3,7)
St9: 3,7
E ::= E Plus . E
E ::= E Plus . Plus
 empty => St0 (1,5,9,13,16)
 <E> => St10 (4)
 <Plus> => St11 (8)
St10: 4
E ::= E Plus E .
St11: 8
E ::= E Plus Plus .
END_SDFA

# print $parse->show_status(1);

# $Parse::Marpa::This::trace = 1;

my @expected = (
    '((($one++)++)+$one)==2',
    '(($one++)+(++$one))==3',
    '($one+(++(++$one)))==4',
    '($one+(++(+(+$one))))==3',
    '(($one++)+(+(+$one)))==2',
    '($one+(+(+(++$one))))==3',
    '($one+(+(++(+$one))))==3',
);
$parse->initial();

# Set max at 20 just in case there's an infinite loop.
# This is for debugging, after all
PARSE: for my $i (0 .. 20) {
    my $value = $parse->value();
    if ($i > $#expected) {
       fail("Pluses equation has extra value: $value\n");
    } else {
        is($value, $expected[$i], "Pluses Equation Value $i");
    }
    last PARSE unless $parse->next();
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
