# Marpa compiling its own specification language

use strict;
use warnings;
use feature ":5.10";
use English;
use lib "../lib";

use Test::More tests => 1;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $discard = q{ undef };

my $default_closure = q{
    my $v_count = scalar @$Parse::Marpa::This::v;
    return undef if $v_count <= 0;
    join("\n", grep { $_ } @$Parse::Marpa::This::v);
};

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

sub canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-_\s]+/-/g;
    $symbol;
}

my $rules = [
    {
       lhs => "grammar",
       rhs => [ "text segment" ],
       closure => $default_closure,
       min => 0,
    },
    [ "text segment", [ "statement", "optional period" ], $default_closure, ],
    [ "text segment", [ "pod block" ], $discard, ],
    # [ "stuff", [ "urtoken" ], $default_closure, ],
    [ "text segment", [ "whitespace" ], $default_closure, ],
    [ "text segment", [ "comment" ], $discard, ],
    # [ "stuff", [ "string" ], $default_closure, ],
    [ "string", [ "q square bracket string" ], $default_closure, ],
    [ "string", [ "q curly bracket string" ], $default_closure, ],
    [ "string", [ "q angle bracket string" ], $default_closure, ],
    [ "string", [ "q parenthesis string" ], $default_closure, ],
    [ "string", [ "q string" ], $default_closure, ],
    [ "string", [ "double quoted string" ], $default_closure, ],
    [ "string", [ "single quoted string" ], $default_closure, ],
    # [ "stuff", [ "code block" ], $default_closure, ],
    # [ "stuff", [ "symbol" ], $default_closure ],
    [ "pod block",
        [
	    "pod head",
	    "pod body",
	    "pod cut",
	],
	$discard,
    ],
    {
       lhs => "pod body",
       rhs => [ "pod line" ],
       closure => $discard,
       min => 0,
    },
    [ "statement", [ "production" ],  $default_closure, ],
    [ "production", [ "lhs", "colon", "rhs" ], $default_closure ],
    {
	lhs => "symbol phrase",
	rhs => [ "symbol word" ],
	closure => q{ "SYMBOL=" . ::canonical_symbol_name(join("-", @$Parse::Marpa::This::v)) },
	min => 1,
    },
    {
        lhs => "lhs",
	rhs => [ "symbol phrase" ],
	closure => q{ "LHS?=" . join(", ", @$Parse::Marpa::This::v) },
    },
    {
        lhs => "rhs",
	rhs => [ "rhs element" ],
	closure => q{ "RHS?=" . join(", ", @$Parse::Marpa::This::v) },
	min => 1,
    },
    {
        lhs => "rhs",
	rhs => [ "rhs element" ],
	closure => q{ "RHS?=" . join(", ", @$Parse::Marpa::This::v) },
	min => 1,
	separator => "comma",
    },
    {
        lhs => "rhs element",
	rhs => [ "symbol phrase" ],
	closure => $default_closure,
    },
    {
        lhs => "rhs element",
	rhs => [ "string", ],
	closure => $default_closure,
    },
    {
        lhs => "optional period",
	rhs => [ "period" ],
	closure => $discard,
	min => 0,
	max => 1,
    }
];


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
    [ "symbol word"          => [ qr/[a-zA-Z_][a-zA-Z0-9_-]+/ ], ], 
    [ "period"               => [ qr/\./ ], ], 
    [ "colon"                => [ qr/\:/ ], ], 
    [ "comma"                => [ qr/\,/ ], ], 
    [ "urtoken"              => [qr/\S+/ ] ],
    [ "whitespace"           => [qr/\s+/ ] ], # when in doubt, throw away whitespcae
];

for my $terminal_rule (@$terminals) {
    $terminal_rule->[0] = canonical_symbol_name($terminal_rule->[0]);
}

for my $rule (@$rules) {
    given (ref $rule) {
        when ("ARRAY") {
	    $rule->[0] = canonical_symbol_name($rule->[0]);
	    my $rhs = $rule->[1];
	    my $new_rhs = [];
	    for my $symbol (@$rhs) {
		push(@$new_rhs, canonical_symbol_name($symbol));
	    }
	    $rule->[1] = $new_rhs;
	}
	when ("HASH") {
	     for (keys %$rule) {
	         when ("lhs") { $rule->{$_} = canonical_symbol_name($rule->{$_}) }
	         when ("rhs") {
		     my $new_rhs = [];
		     for my $symbol (@{$rule->{$_}}) {
			 push(@$new_rhs, canonical_symbol_name($symbol));
		     }
		     $rule->{$_} = $new_rhs;
		 }
	         when ("separator") { $rule->{$_} = canonical_symbol_name($rule->{$_}) }
	     }
	}
	default { croak ("Invalid rule ref: ", ($_ ? $_ : "undefined")) }
    }
}

my $g = new Parse::Marpa(
    start => canonical_symbol_name("grammar"),
    rules => $rules,
    terminals => $terminals,
    default_closure => $default_closure,
    # ambiguous_lex => 0,
    default_lex_prefix => qr/\s*/,
    # trace_rules => 1,
);

my $parse = new Parse::Marpa::Parse($g);
$parse->trace("iterations");

# print $g->show_rules(), "\n";

print $g->show_SDFA(), "\n";

sub binary_search {
    my ($target, $data) = @_;  
    my ($lower, $upper) = (0, $#$data); 
    my $i;                       
    while ($lower <= $upper) {
	my $i = int(($lower + $upper)/2);
	given ($data->[$i]) {
	    when ($_ < $target) { $lower = $i; }
	    when ($_ > $target) { $upper = $i; }
	    default { return $i }
	} 
    }
    $lower
}

sub locator {
    my $earleme = shift;
    my $string = shift;

    state @lines = (0);
    my $pos = 0;
    NL: while ($$string =~ /\n/g) {
	$pos = pos $$string;
        push(@lines, $pos);
	last NL if $pos > $earleme;
    }
    my $line = @lines - ($pos > $earleme ? 2 : 1);
    my $line_start = $lines[$line];
    return ($line, $line_start);
}

{
    local($RS) = undef;
    my $spec = <DATA>;
    if ((my $earleme = $parse->lex_string(\$spec)) >= 0) {
	# print $parse->show_status();
	my ($line, $line_start) = locator($earleme, \$spec);
	print "Parse exhausted at line $line, earleme $earleme\n";
	given (index($spec, "\n", $line_start)) {
	    when (undef) { say substr($spec, $line_start) }
	    default { say substr($spec, $line_start, $_-$line_start) }
	}
	say +(" " x ($earleme-$line_start)), "^";
	exit 1;
    }
    $parse->lex_end();
}

unless ($parse->initial()) {
    print $parse->show_status();
    say "No parse";
    exit 1;
}

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

