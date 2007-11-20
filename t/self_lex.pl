# Marpa compiling its own specification language

use strict;
use warnings;
use feature ":5.10";
use English;

use Test::More tests => 1;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $discard = q{ "" };

my $default_closure = q{
    my $v_count = scalar @$Parse::Marpa::This::v;
    return "" if $v_count <= 0;
    return $Parse::Marpa::This::v->[0] if $v_count == 1;
    "(" . join(";", map { defined $_ ? $_ : "undef" } @$Parse::Marpa::This::v) . ")";
};

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

sub counted_rule {
    my $lhs = shift;
    my $rhs = shift;
    my $closure = shift;
    my $options = shift;
    my ($min, $max, $separator);
    my $return;

    while (my ($option, $value) = each(%$options)) {
	given ($option) {
	    when ("min") { $min = $value }
	    when ("max") { $max = $value }
	    when ("separator") { $separator = $value }
	    default { croak("Unknown option in counted rule: $option") }
	}
    }

    if (not defined $min
        or $max <= $min and $max < 2
	or defined $max and $max < $min) {
	croak("Illegal min, max in counted rule: ",
	    ($min // "undef"), " ", 
	    ($max // "undef")
	)
    }

    # specifically counted rules
    for my $count ( ($min ? $min : 1) .. ($max // 1) ) {
	my @separated_rhs;
	push(@separated_rhs, $separator) if defined $separator;
	push(@separated_rhs, $rhs) if defined $separator;
	push (@$return,
	    [
	       $lhs, [ $rhs, @separated_rhs x ($count - 1) ],
		    # no change to @Parse::Marpa::This::v
	       $closure,
	    ],
	);
    }

    # nulling rule
    if ($min <= 0) {
	push (@$return,
	    [
	        $lhs, [ ],
	        (defined $closure
	            ? ( q{ $Parse::Marpa::This::v = []; } . $closure )
		    : undef
		)
	    ],
	);
    }

    my $sequence = ($lhs . ":Seq 1-*");

}


sub one_or_more {
    my $lhs = shift;
    my $rhs = shift;
    my $closure = shift;

    my $sequence = ($lhs . ":Seq 1-*");
    my $main_rule = [
       $lhs, [ $sequence ],
        q{ $Parse::Marpa::This::v = $Parse::Marpa::This::v->[0]; }
            . $closure
    ];
    my $more_rule = [
        $sequence, [ $sequence, $rhs ],
        q{ 
            my $r = $Parse::Marpa::This::v->[0];
            push(@$r, $Parse::Marpa::This::v->[1]);
	    $r;
        }
    ];
    my $one_rule = [
        $sequence, [ $rhs ],
        q{ $Parse::Marpa::This::v }
    ];
    return ($main_rule, $one_rule, $more_rule);
}

sub zero_or_more {
    my $lhs = shift;
    my $rhs = shift;
    my $closure = shift;

    my $sequence = ($lhs . ":Seq 0-*");
    my $main_rule = [
       $lhs, [ $sequence ],
        q{ $Parse::Marpa::This::v = $Parse::Marpa::This::v->[0]; }
	. $closure
    ];
    my $more_rule = [
        $sequence, [ $sequence, $rhs ],
        q{ 
            my $r = $Parse::Marpa::This::v->[0];
            push(@$r, $Parse::Marpa::This::v->[1]);
	    $r;
        }
    ];
    my $one_rule = [
        $sequence, [ $rhs ],
        q{ $Parse::Marpa::This::v }
    ];
    my $nulling_rule = [
       $lhs, [ ],
        q{ @Parse::Marpa::This::v = (); }
	. $closure
    ];
    return ($main_rule, $nulling_rule, $one_rule, $more_rule);
}

my $rules = [
    [ "stuff", [ "pod block" ], $discard, ],
    [ "stuff", [ "urtoken" ], $default_closure, ],
    [ "stuff", [ "whitespace" ], $default_closure, ],
    [ "stuff", [ "comment" ], $discard, ],
    [ "stuff", [ "q square bracket string" ], $default_closure, ],
    [ "stuff", [ "q curly bracket string" ], $default_closure, ],
    [ "stuff", [ "q angle bracket string" ], $default_closure, ],
    [ "stuff", [ "q parenthesis string" ], $default_closure, ],
    [ "stuff", [ "q string" ], $default_closure, ],
    [ "stuff", [ "double quoted string" ], $default_closure, ],
    [ "stuff", [ "single quoted string" ], $default_closure, ],
    [ "stuff", [ "code block" ], $default_closure, ],
    [ "pod block",
        [
	    "pod head",
	    "pod body",
	    "pod cut",
	],
	$discard,
    ],
];

push(@$rules, zero_or_more("start", "stuff", $default_closure));
push(@$rules, zero_or_more("pod body", "pod line", $discard));

my $terminals = [
    [ "code block"           => [ qr/%{.*?}%/s ] ],
    [ "q square bracket string" => [ qr/q\[.*?\]/s ] ],
    [ "q curly bracket string" => [ qr/q\{.*?\}/s ] ],
    [ "q angle bracket string" => [ qr/q\<.*?\>/s ] ],
    [ "q parenthesis string" => [ qr/q\(.*?\)/s ] ],
    [ "q string" => [ qr/q(\S).*?\g{-1}/s ] ],
    [ "double quoted string" => [ qr/\".*?\"/s ] ],
    [ "single quoted string" => [ qr/\'.*?\'/s ] ],
    [ "pod head"             => [qr/^=[a-zA-Z_].*$/m ] ],
    [ "pod cut"              => [qr/^=cut.*$/m ] ],
    [ "pod line"             => [qr/.*\n/m ] ],
    [ "comment"              => [qr/#.*\n/], ],
    [ "urtoken"              => [qr/\S+/ ] ],
    [ "whitespace"           => [qr/\s+/ ] ], # when in doubt, throw away whitespcae
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
    start => canonical_symbol_name("start"),
    rules => $rules,
    terminals => $terminals,
    default_closure => $default_closure,
    ambiguous_lex => 0,
    default_lex_prefix => qr/\s*/,
);

my $parse = new Parse::Marpa::Parse($g);
$parse->trace("lex");

# print $g->show_rules(), "\n";

# print $g->show_SDFA(), "\n";

{
    local($RS) = undef;
    my $spec = <DATA>;
    if ($parse->lex_string(\$spec) >= 0) {
	print $parse->show_status();
	print "Parse exhausted\n";
	exit 1;
    }
    $parse->lex_end();
}

$parse->initial();

my $value = $parse->value();
print $value, "\n";

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__

=begin Some Pod

Some pod text for testing

=end Some Pod

=cut 

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

closure matches %{ # TO DO
}%

string matches %{ # TO DO
}%

period matches "[.]"

