#!perl

# An ambiguous equation,
# this time using the lexer

use strict;
use warnings;

use Test::More tests => 1;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

sub canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-\s]+/-/g;
    $symbol;
}

sub optional {
    my $lhs = shift;
    my $rhs = shift;
    my $closure = shift;

    my $main_rule = [
       $lhs,
        [ $rhs ],
        sub {
            # no change to @Parse::Marpa::This::v
            $closure->();
        }
    ];
    my $nulling_rule = [
       $lhs,
        [ ],
        sub { "" }
    ];
    return ($main_rule, $nulling_rule);
}

sub one_or_more {
    my $lhs = shift;
    my $rhs = shift;
    my $closure = shift;

    my $sequence = $lhs . ":Seq 1-*";
    my $main_rule = [
       $lhs,
        [ $sequence ],
        sub {
            @Parse::Marpa::This::v = @{$Parse::Marpa::This::v[0]};
            $closure->();
        }
    ];
    my $more_rule = [
        $sequence,
        [ $sequence, $rhs ],
        sub { [
            @{$Parse::Marpa::This::v[0]},
            $Parse::Marpa::This::v[1]
        ] }
    ];
    my $one_rule = [
        $sequence,
        [ $rhs ],
        sub { [ $Parse::Marpa::This::v[0] ] }
    ];
    return ($main_rule, $one_rule, $more_rule);
}

my $rules = [
    [ "S", [
        "optional whitespace",
        "left hand side",
        "optional whitespace",
        "colon",
        "optional whitespace",
        "right hand side",
        "optional whitespace",
        ],
        sub {
           join(" ", @Parse::Marpa::This::v[1,3,5])
        }
    ],
    [ "left hand side",
        [ "symbol" ]
    ],
    [ "right hand side",
        [ "symbol" ]
    ],
    [ "optional whitespace" ],
    [ "optional whitespace", [ "whitespace" ] ]
];

my $default_closure = sub {
     my $v_count = scalar @Parse::Marpa::This::v;
     return "" if $v_count <= 0;
     return $Parse::Marpa::This::v[0] if $v_count == 1;
     "(" . join(";", map { defined $_ ? $_ : "undef" } @Parse::Marpa::This::v) . ")";
};

push(@$rules, one_or_more("symbol", "symbol word", $default_closure));

my $terminals = [
    [ "symbol word" => qr/\w+/ ],
    [ "colon" => qr/[:]/ ],
    [ "whitespace" => qr/\s+/ ],
];

for my $terminal_rule (@$terminals) {
    $terminal_rule->[0] = canonical_symbol_name($terminal_rule->[0]);
}

for my $rule (@$rules) {
    $rule->[0] = canonical_symbol_name($rule->[0]);
    my $rhs = $rule->[1];
    for my $ix (0 .. $#$rhs) {
        $rhs->[$ix] = canonical_symbol_name($rhs->[$ix]);
    }
}

my $g = new Parse::Marpa(
    start => canonical_symbol_name("S"),
    rules => $rules,
    terminals => $terminals,
    default_closure => $default_closure
);

my $parse = new Parse::Marpa::Parse($g);

# print $g->show_rules(), "\n";

# print $g->show_SDFA(), "\n";

$parse->lex_string(\("lhs : rhs"));
$parse->lex_end();

$parse->initial();

my $value = $parse->value();
print $value, "\n";

# print $parse->show();
# print $parse->show_status();

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__

specification: statement sequence

statement: production.

statement: terminal declaration.

production: rule closure, optional-period.

production: rule period.

rule: lhs ":" rhs

lhs: symbol

rhs: "optional" symbol "separated" symbol "sequence".

rhs: symbol "separated" symbol "sequence".

rhs: "optional" symbol "sequence".

rhs: symbol "sequence".

rhs: optional comma separated symbol sequence.

rhs: optional symbol sequence.

symbol: terminal

symbol: non-terminal

non-terminal: word sequence.

terminal: string.

terminal: "optional" string.

word matches "[-\w]+"

terminal declaration: symbol "matches" string.

terminal declaration: symbol "matches" closure.

closure matches { # TO DO
}

string matches { # TO DO
}

period matches "[.]"

