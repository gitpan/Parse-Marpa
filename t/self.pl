# Marpa compiling its own specification language

use strict;
use warnings;
use feature ":5.10";
use English;
use lib "../lib";
use Parse::Marpa;

my $discard = undef;

my $concatenate_lines = q{
    my $v_count = scalar @$Parse::Marpa::This::v;
    return undef if $v_count <= 0;
    join("\n", grep { $_ } @$Parse::Marpa::This::v);
};

		    # old default_lex_prefix => qr/\s*/,
our $default_lex_prefix = qr/[ \t]*(\#[^\n]*\n|\n)?([ \t]*\#[^\n]*\n)*[ \t]*/;

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
        rhs => [ "paragraph" ],
        action => q{
	    q{
		use 5.9.5;
		use strict;
		use warnings;
		use Parse::Marpa;

		sub canonical_symbol_name {
		    my $symbol = lc shift;
		    $symbol =~ s/[-_\s]+/-/g;
		    $symbol;
		}

		my $terminals = [];
		my $rules = [];
	    }
	    . join("\n", grep { $_ } @$Parse::Marpa::This::v)
	    . "\n"
	    .q{
		my $g = new Parse::Marpa(
		    start => canonical_symbol_name("grammar"),
		    rules => $rules,
		    terminals => $terminals,
		    default_lex_prefix => $default_lex_prefix,
		);

		my $parse = new Parse::Marpa::Parse(
		   grammar=> $g,
		);
	    }
	   },
	   min => 1,
	   separator => "paragraph break",
	   action => $concatenate_lines,
    },
    # change definition to another word?
    [ "paragraph", [ "definition paragraph" ] ],
    [ "paragraph", [ "production paragraph" ], $concatenate_lines ],
    [ "paragraph", [ "terminal paragraph" ], $concatenate_lines ],
    {
	lhs => "definition paragraph",
	rhs => [ "definition" ],
	min => 1,
	action => $concatenate_lines,
    },
    [ "production paragraph",
       [
	 "non structural production sentences",
         "production sentence",
	 "non structural production sentences",
         "optional action sentence",
	 "non structural production sentences",
	],
	$concatenate_lines,
    ],
    { 
       lhs => "non structural production sentences",
       rhs => [ "non structural production sentence" ],
       min => 0,
       action => $concatenate_lines,
    },
    {
       lhs => "optional action sentence",
       rhs => [ "action sentence" ],
       min => 0,
       max => 1,
       action => $concatenate_lines,
    },
    [ "action sentence",
	[
	    "optional the keyword",
	    "action keyword",
	    "is keyword",
	    "action specifier",
	    "period",
	],
	$concatenate_lines,
    ],
    [ "action sentence",
	[
	    "action specifier",
	    "period",
	],
	$concatenate_lines,
    ],
    [ "action specifier", [ "string" ], $concatenate_lines ],
    [ "action specifier", [ "symbol phrase" ], $concatenate_lines ],
    [ "non-structural production sentence", [ "comment sentence", ], $concatenate_lines, ],
    [ "non-structural terminal sentence", [ "comment sentence", ], $concatenate_lines, ],
    [ "definition", [ "setting", "period" ], $concatenate_lines, ],
    [ "definition", [ "preamble keyword", "string", "period" ], $concatenate_lines, ],
    [ "definition", [ "comment sentence", ], $concatenate_lines, ],
    [ "definition", [ "bracketed comment", ], $concatenate_lines, ],
    [ "definition", [ "string definition", ], $concatenate_lines, ],
    [ "definition", [ "default action definition", ], $concatenate_lines, ],
    [ "setting", [ "semantics setting" ], $concatenate_lines, ],
    [ "setting", [ "start symbol setting" ], $concatenate_lines, ],
    [ "setting", [ "default lex prefix setting" ], $concatenate_lines, ],
    [ "semantics setting",
	[
 	  "optional the keyword",
  	  "semantics keyword", 
	  "copula",
  	  "perl5 keyword", 
	],
	$concatenate_lines,
    ],
    [ "semantics setting",
	[
  	  "perl5 keyword", 
	  "copula",
 	  "optional the keyword",
  	  "semantics keyword", 
	],
	$concatenate_lines,
    ],
    [ "start symbol setting",
	[
 	  "optional the keyword",
  	  "start keyword", 
  	  "symbol keyword", 
	  "copula",
  	  "symbol phrase", 
	],
	$concatenate_lines,
    ],
    [ "start symbol setting",
	[
  	  "symbol phrase", 
	  "copula",
 	  "optional the keyword",
  	  "start keyword", 
  	  "symbol keyword", 
	],
	$concatenate_lines,
    ],
    [ "default lex prefix setting",
	[
  	  "regex", 
	  "copula",
 	  "optional the keyword",
  	  "default keyword", 
  	  "lex keyword", 
  	  "prefix keyword", 
	],
	$concatenate_lines,
    ],
    [ "default lex prefix setting",
	[
 	  "optional the keyword",
  	  "default keyword", 
  	  "lex keyword", 
  	  "prefix keyword", 
	  "copula",
  	  "regex", 
	],
	$concatenate_lines,
    ],
    [ "copula", [ "is keyword" ], $concatenate_lines ],
    [ "copula", [ "are keyword" ], $concatenate_lines ],
    [ "string definition",
	[ "define keyword", "optional the keyword", "action keyword", "symbol phrase", "as keyword", "string",
	  "period"
        ],
	$concatenate_lines
    ],
    [ "default action definition",
	[ "symbol phrase", "is keyword", "optional the keyword", "default keyword",
	  "action keyword",
	  "period",
	  ],
	$concatenate_lines
    ],
    {
       lhs => "optional the keyword",
       rhs => [ "the keyword" ],
       action => $discard,
       min => 0,
       max => 1,
    },
    [ "comment sentence", [ "comment tag", "colon", "comment word sequence", "period" ], $discard ],
    {
       lhs => "comment word sequence",
       rhs => [ "comment word" ],
       action => $discard,
       min => 0,
    },
    [ "text segment", [ "pod block" ], $discard, ],
    [ "text segment", [ "comment" ], $discard, ],
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
    {
	lhs => "production sentence",
	rhs => [ "lhs", "colon", "rhs", "period" ],
	# tell perl NNN counter is in special package
	action => q{
	    'push(@$rules, '
	    . "{\n"
	    . join(",\n", @{$Parse::Marpa::This::v}[0,2])
	    . "\n});" 
	},
    },
    {
	lhs => "symbol phrase",
	rhs => [ "symbol word" ],
	action => q{ q{"} . ::canonical_symbol_name(join("-", @$Parse::Marpa::This::v)) . q{"} },
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
	action => q{ "    rhs => [" . join(", ", @$Parse::Marpa::This::v) . "]" },
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
    [ "terminal paragraph",
       [
	 "non structural terminal sentences",
         "terminal sentence",
	 "non structural terminal sentences",
	],
	$concatenate_lines,
    ],
    { 
       lhs => "non structural terminal sentences",
       rhs => [ "non structural terminal sentence" ],
       min => 0,
       action => $concatenate_lines,
    },
    {
        lhs => "terminal sentence",
	rhs => [ "symbol phrase", "matches keyword", "regex", "period" ],
	action => q{
	    '{ my $regex_string = '
	    . $Parse::Marpa::This::v->[2]
	    . ";\n"
	    . 'push(@$terminals, '
	    . '[ '
	    . $Parse::Marpa::This::v->[0]
	    . ' => [ qr/$regex_string/ ] ]);'
	    . "\n}"
	}
    },
];


my $terminals = [
    [ "action keyword"       => [ qr/action/ ] ],
    [ "as keyword"           => [ qr/as/ ] ],
    [ "are keyword"          => [ qr/are/ ] ],
    [ "default keyword"      => [ qr/default/ ] ],
    [ "define keyword"       => [ qr/define/ ] ],
    [ "is keyword"           => [ qr/is/ ] ],
    [ "lex keyword"          => [ qr/lex/ ] ],
    [ "matches keyword"      => [ qr/matches/ ] ],
    [ "perl5 keyword"        => [ qr/perl5/ ] ],
    [ "preamble keyword"     => [ qr/preamble/ ] ],
    [ "prefix keyword"       => [ qr/prefix/ ] ],
    [ "semantics keyword"    => [ qr/semantics/ ] ],
    [ "start keyword"        => [ qr/start/ ] ],
    [ "symbol keyword"       => [ qr/symbol/ ] ],
    [ "the keyword"          => [ qr/the/ ] ],
    [ "q string" => "Parse::Marpa::Lex::lex_q_quote" ],
    [ "regex" => "Parse::Marpa::Lex::lex_regex" ],
    [ "paragraph break"       => [ qr/^[ \t]*\n\s*/m ] ],
    [ "bracketed comment"     => [ qr/\x{5b}[^\x{5d}]*\x{5d}/ ] ],
    # change to lex_q_quote
    [ "single quoted string" => q{
	    state $prefix_regex = qr/\G$main::default_lex_prefix'/o;
	    return unless $$STRING =~ /$prefix_regex/g;
            state $regex = qr/\G[^'\0134]*('|\0134')/;
	    MATCH: while ($$STRING =~ /$regex/gc) {
		next MATCH unless defined $1;
		if ($1 eq q{'}) {
		    my $length = (pos $$STRING) - $START;
		    return (substr($$STRING, $START, $length), $length);
		}
	    }
	    return;
        }
    ],
    [ "double quoted string" => q{
	    # say "pos STRING=", (pos $$STRING), "; ", substr($$STRING, (pos $$STRING), 10);
	    state $prefix_regex = qr/\G$main::default_lex_prefix"/o;
	    return unless $$STRING =~ /$prefix_regex/g;
            state $regex = qr/\G[^"\0134]*("|\0134")/;
	    MATCH: while ($$STRING =~ /$regex/gc) {
		next MATCH unless defined $1;
		if ($1 eq q{"}) {
		    my $length = (pos $$STRING) - $START;
		    return (substr($$STRING, $START, $length), $length);
		}
	    }
	    return;
        }
    ],
    [ "pod head"             => [qr/^=[a-zA-Z_].*$/m ] ],
    [ "pod cut"              => [qr/^=cut.*$/m ] ],
    [ "pod line"             => [qr/.*\n/m ] ],
    [ "comment"              => [qr/#.*\n/], ],
    [ "symbol word"          => [ qr/[a-zA-Z_][a-zA-Z0-9_-]*/ ], ], 
    [ "period"               => [ qr/\./ ], ], 
    [ "colon"                => [ qr/\:/ ], ], 

    # Do I want to allow comments between "to" and "do" ?
    [ "comment tag" => [ qr/(to\s+do|note|comment)/ ], ],

    # Includes all non-whitespace printable characters except period
    [ "comment word" => [ qr/[\x{21}-\x{2d}\x{2f}-\x{7e}]+/ ], ],

    [ "comma"                => [ qr/\,/ ], ], 
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
    # default_lex_prefix => qr/[ \t]*(\#.*)\n[ \t]*/,
    default_lex_prefix => qr/[ \t]*(\#[^\n]*\n|\n)?([ \t]*\#[^\n]*\n)*[ \t]*/,
    # trace_rules => 1,
);

my $parse = new Parse::Marpa::Parse(
   grammar=> $g,
   # trace_actions => 1,
);
# $parse->trace("lex");

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
# print $parse->show();

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__

semantics are perl5.

comment: Comments can be broken with a period
or a double newline.

comment: I need to implement preamble code.
preamble q{
    use 5.9.5;
    use strict;
    use warnings;
    use Parse::Marpa;

    my $terminals = [];
    my $rules = [];

    sub canonical_symbol_name {
	my $symbol = lc shift;
	$symbol =~ s/[-_\s]+/-/g;
	$symbol;
    }
}.

define the action concatenate lines as q{
    my $v_count = scalar @$Parse::Marpa::This::v;
    return undef if $v_count <= 0;
    join("\n", grep { $_ } @$Parse::Marpa::This::v);
}.
concatenate lines is the default action. # perl5 style comment
grammar is the start symbol.

grammar: double-newline separated paragraph sequence.
# Traditional comments still work
the action is
         q{
	    join("\n", grep { $_ } @$Parse::Marpa::This::v)
	    . "\n"
	    .q{
		my $g = new Parse::Marpa(
		    start => canonical_symbol_name("grammar"),
		    rules => $rules,
		    terminals => $terminals,
		    default_lex_prefix => qr/\s*/,
		);

		my $parse = new Parse::Marpa::Parse(
		   grammar=> $g,
		);
	    }
	   }.

paragraph: pod block. discard.

paragraph: comment.  discard.

commented paragraph: comment, uncommented paragraph.  concatenate lines.

comment sentences: "comment", colon, comment sentence sequence. discard.

string: q or qq string.

[ q string lexer is a built-in.  match q or qq string using q string lexer. ]

string: double quoted string.  concatenate lines.

string: single quoted string.  concatenate lines.

pod block: pod head, pod body, pod cut.  discard.

pod body: optional pod line sequence.  discard.

uncommmented paragraph: production, action specifier.

uncommmented paragraph: terminal declaration.

terminal declaration: "match" symbol-phrase "using" lexing-closure.

terminal declaration: symbol-phrase "matches" string.
	 q{
	    '{ my $regex_string = '
	    . $Parse::Marpa::This::v->[2]
	    . ";\n"
	    . 'push(@$terminals, '
	    . '[ '
	    . $Parse::Marpa::This::v->[0]
	    . ' => [ qr/$regex_string/ ] ]);'
	    . "\n}"
	}.

lexing closure: string.

production: lhs ":" rhs.  q{
	    'push(@$rules, '
	    . "{\n"
	    . join(",\n", @{$Parse::Marpa::This::v}[0,2])
	    . "\n});" 
	}.
    
symbol phrase: symbol word sequence.
    q{ q{"} . ::canonical_symbol_name(join("-", @$Parse::Marpa::This::v)) . q{"} }.
    
lhs: symbol phrase.  q{ "    lhs => " . join("-", @$Parse::Marpa::This::v) }.
    
rhs: rhs element sequence.
    q{ "    rhs => [" . join(", ", @$Parse::Marpa::This::v) . "]" }.
    
rhs: comma separated rhs element sequence.
    q{ "    rhs => [" . join(", ", @$Parse::Marpa::This::v) . "]" }.
	
rhs element: symbol phrase.
    
rhs element: string.
    
null statement: optional whitespace.  discard.

period matches qr"[.]".

to do: Make regexes different from strings. 
note: Allow qr and / form of regexes.
to do: Have modifiers at end of regexes, a la perl5.
double quoted string matches /\".*?\"/s.

single quoted string matches /\'.*?\'/s.

whitespace matches qr"\s+".

comma matches qr','.

colon matches qr':'.

period matches qr'\.'.

pod head matches qr/^=[a-zA-Z_].*$/m.

pod cut matches qr/^=cut.*$/m.

pod line matches qr/.*\n/m.

comment matches qr/#.*\n/.

symbol word matches qr/[a-zA-Z_][a-zA-Z0-9_-]+/.

the default lex prefix is qr/\s*/.

