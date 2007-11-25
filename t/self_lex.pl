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

my $discard = undef;

my $concatenate_lines = q{
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
       action => $concatenate_lines,
       min => 0,
    },
    [ "text segment", [ "statement", "optional period" ], $concatenate_lines, ],
    [ "text segment", [ "pod block" ], $discard, ],
    [ "text segment", [ "comment" ], $discard, ],
    [ "string", [ "q square bracket string" ], $concatenate_lines, ],
    [ "string", [ "q curly bracket string" ], $concatenate_lines, ],
    [ "string", [ "q angle bracket string" ], $concatenate_lines, ],
    [ "string", [ "q parenthesis string" ], $concatenate_lines, ],
    [ "string", [ "q string" ], $concatenate_lines, ],
    [ "string", [ "double quoted string" ], $concatenate_lines, ],
    [ "string", [ "single quoted string" ], $concatenate_lines, ],
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
       action => $discard,
       min => 0,
    },
    [ "statement", [ "production" ],  $concatenate_lines, ],
    [ "statement", [ "terminal declaration" ],  $concatenate_lines, ],
    [ "statement", [ "null statement" ],  $discard, ],
    {
	lhs => "production",
	rhs => [ "lhs", "colon", "rhs" ],
	# tell perl NNN counter is in special package
	action => q(
	    our $uniq_number;
	    sprintf('my $rule%d', $uniq_number++)
	    . "= {\n"
	    . join(",\n", @{$Parse::Marpa::This::v}[0,2])
	    . "\n};" 
	),
    },
    {
	lhs => "symbol phrase",
	rhs => [ "symbol word" ],
	action => q{ ::canonical_symbol_name(join("-", @$Parse::Marpa::This::v)) },
	min => 1,
    },
    {
        lhs => "lhs",
	rhs => [ "symbol phrase" ],
	action => q{ "    lhs => " . join("-", @$Parse::Marpa::This::v) },
    },
    {
        lhs => "rhs",
	rhs => [ "rhs element" ],
	action => q{ "    rhs => [" . join(", ", @$Parse::Marpa::This::v) . "]" },
	min => 1,
    },
    {
        lhs => "rhs",
	rhs => [ "rhs element" ],
	action => q{ "    rhs => " . join(", ", @$Parse::Marpa::This::v) },
	min => 1,
	separator => "comma",
    },
    {
        lhs => "rhs element",
	rhs => [ "symbol phrase" ],
	action => $concatenate_lines,
    },
    {
        lhs => "rhs element",
	rhs => [ "string", ],
	action => $concatenate_lines,
    },
    {
        lhs => "optional period",
	rhs => [ "period" ],
	action => $discard,
	min => 0,
	max => 1,
    },
    {
        lhs => "terminal declaration",
	rhs => [ "symbol phrase", "matches keyword", "string", ],
	action => q{
	    our $uniq_number;
	    sprintf('my $terminal%d', $uniq_number++)
	    . ' = [ "'
	    . $Parse::Marpa::This::v->[0]
	    . '" => [ qr/'
	    . $Parse::Marpa::This::v->[2]
	    . '/ ] ];'
	}
    },
    {
        lhs => "terminal declaration",
	rhs => [ "symbol phrase", "matches keyword", "code block", ],
	action => $concatenate_lines,
    },
    {
        lhs => "null statement",
	rhs => [ "whitespace", ],
	action => $discard,
    },
];


my $terminals = [
    [ "matches keyword"      => [ qr/matches/i ] ],
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
    # default_action => $default_action,
    # ambiguous_lex => 0,
    default_lex_prefix => qr/\s*/,
    # trace_rules => 1,
);

my $parse = new Parse::Marpa::Parse($g);
# $parse->trace("iterations");

# print $g->show_rules(), "\n";

# print $g->show_SDFA(), "\n";

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
    my $pos = pos $$string = 0;
    NL: while ($$string =~ /\n/g) {
	$pos = pos $$string;
        push(@lines, $pos);
	last NL if $pos > $earleme;
    }
    my $line = @lines - ($pos > $earleme ? 2 : 1);
    my $line_start = $lines[$line];
    return ($line, $line_start);
}

my $spec;

{
    local($RS) = undef;
    $spec = <DATA>;
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
    # print $parse->show_status();
    say "No parse";
    my ($earleme, $lhs) = $parse->find_complete_rule();
    unless (defined $earleme) {
	say "No rules completed";
	exit 1;
    }
    my ($line, $line_start) = locator($earleme, \$spec);
    say "Parses completed at line $line, earleme $earleme for symbols:";
    say join(", ", @$lhs);
    given (index($spec, "\n", $line_start)) {
	when (undef) { say substr($spec, $line_start) }
	default { say substr($spec, $line_start, $_-$line_start) }
    }
    say +(" " x ($earleme-$line_start)), "^";
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

period matches "[.]"

