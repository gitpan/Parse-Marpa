# Marpa compiling its own specification language

use strict;
use warnings;
use feature ":5.10";
use English;
use lib "../lib";
use Parse::Marpa;
use Carp;

sub usage {
   die("usage: $0 grammar-file\n");
}

usage() unless scalar @ARGV == 1;

my $grammar_file_name = shift @ARGV;

open(GRAMMAR, "<", $grammar_file_name) or die("Cannot open $grammar_file_name: $!");

my $discard = undef;

my $concatenate_lines = q{
    my $v_count = scalar @$Parse::Marpa::This::v;
    return undef if $v_count <= 0;
    join("\n", grep { $_ } @$Parse::Marpa::This::v);
};

our $whitespace = qr/(?:[ \t]*(?:\n|(?:\#[^\n]*\n)))*[ \t]*/;
our $default_lex_prefix = $whitespace;

sub canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-_\s]+/-/g;
    $symbol;
}

sub canonical_version {
    my $version = shift;
    my @version = split(/\./, $version);
    my $result = sprintf("%d.", (shift @version));
    for my $subversion (@version) {
       $result .= sprintf("%03d", $subversion);
    }
    $result;
}

our $preamble =<<'EOCODE';
use 5.9.5;
use strict;
use warnings;
use Parse::Marpa;
use Carp;

sub canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-_\s]+/-/g;
    $symbol;
}

my $terminals = [];
my $rules = [];
my $preamble = "";
my %strings = ();
EOCODE

our $compile_code = <<'EOCODE';

$start_symbol //= "(undefined start symbol)";
$semantics //= "not defined";
$version //= -1;

croak("Version requested is ", $version, "\nVersion must match ", $Parse::Marpa::VERSION, " exactly.")
   unless $version == $Parse::Marpa::VERSION;

croak("Semantics are ", $semantics, "\nThe only semantics currently available are perl5.")
   unless $semantics eq "perl5";

my $g = new Parse::Marpa(
    start => $start_symbol,
    rules => $rules,
    terminals => $terminals,
    default_lex_prefix => $default_lex_prefix,
);

my $parse = new Parse::Marpa::Parse(
   grammar=> $g,
);
EOCODE

my $rules = [
    [ "grammar", [ "paragraph sequence" ], $concatenate_lines ],
    [ "grammar", [ "paragraph sequence", "whitespace lines" ], $concatenate_lines ],
    {
        lhs => "paragraph sequence",
        rhs => [ "paragraph" ],
        action => q{
            "\n$::preamble\n"
            . join("\n", grep { $_ } @$Parse::Marpa::This::v)
            . "\n"
            . "\n$::compile_code\n"
        },
        min => 1,
        separator => "empty line",
    },
    # change definition to another word?
    [ "paragraph", [ "empty paragraph" ] ],
    [ "paragraph", [ "definition paragraph" ], $concatenate_lines ],
    [ "paragraph", [ "production paragraph" ], $concatenate_lines ],
    [ "paragraph", [ "terminal paragraph" ], $concatenate_lines ],
    [ "empty paragraph", [ "whitespace" ] ],
    {
	lhs => "comment paragraph",
	rhs => [ "comment" ],
	min => 1,
	action => $concatenate_lines,
    },
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
        q{
            my $action = $Parse::Marpa::This::v->[3];
	    'push(@$rules, '
	    . "{\n"
	    . $Parse::Marpa::This::v->[1] . ",\n"
	    . (defined $action ? ($action . ",\n") : "")
	    . "\n});" 
	}
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
	q{
           "    action => "
           . $Parse::Marpa::This::v->[3]
        }
    ],
    [ "action sentence",
	[
	    "action specifier",
	    "period",
	],
	q{
           "    action => "
           . $Parse::Marpa::This::v->[0]
        }
    ],
    [ "action specifier", [ "string specifier" ], $concatenate_lines ],
    [ "non-structural production sentence", [ "comment sentence", ], $concatenate_lines, ],
    [ "non-structural terminal sentence", [ "comment sentence", ], $concatenate_lines, ],
    [ "definition", [ "preamble definition", ], $concatenate_lines, 1000 ],
    [
        "definition",
        [ "setting", "period" ],
        q{ $Parse::Marpa::This::v->[0] }
    ],
    [ "definition", [ "comment sentence", ], $concatenate_lines, ],
    [ "definition", [ "bracketed comment", ], $concatenate_lines, ],
    [ "definition", [ "default action definition", ], $concatenate_lines, ],
    [ "definition", [ "string definition", ], $concatenate_lines, ],
    [ "setting", [ "semantics setting" ], $concatenate_lines, ],
    [ "setting", [ "version setting" ], $concatenate_lines, ],
    [ "setting", [ "start symbol setting" ], $concatenate_lines, ],
    [ "setting", [ "default lex prefix setting" ], $concatenate_lines, ],
    [ "semantics setting",
	[
 	  "optional the keyword",
  	  "semantics keyword", 
	  "copula",
  	  "perl5 keyword", 
	],
        q{
            q{my $semantics = '}
            . $Parse::Marpa::This::v->[3]
            . qq{';\n}
        }
    ],
    [ "semantics setting",
	[
  	  "perl5 keyword", 
	  "copula",
 	  "optional the keyword",
  	  "semantics keyword", 
	],
        q{
            q{my $semantics = '}
            . $Parse::Marpa::This::v->[0]
            . qq{';\n}
        }
    ],
    [ "version setting",
	[
 	  "optional the keyword",
  	  "version keyword", 
	  "copula",
  	  "version number", 
	],
        q{
            q{my $version = '}
            . ::canonical_version($Parse::Marpa::This::v->[3])
            . qq{';\n}
        }
    ],
    [ "version setting",
	[
  	  "version number", 
	  "copula",
 	  "optional the keyword",
  	  "version keyword", 
	],
        q{
            q{my $version = '}
            . ::canonical_version($Parse::Marpa::This::v->[0])
            . qq{';\n}
        }
    ],
    [ "start symbol setting",
	[
 	  "optional the keyword",
  	  "start keyword", 
  	  "symbol keyword", 
	  "copula",
  	  "symbol phrase", 
	],
        q{
            q{my $start_symbol = }
            . $Parse::Marpa::This::v->[4]
            . qq{;\n}
        }
    ],
    [ "start symbol setting",
	[
  	  "symbol phrase", 
	  "copula",
 	  "optional the keyword",
  	  "start keyword", 
  	  "symbol keyword", 
	],
        q{
            q{my $start_symbol = }
            . $Parse::Marpa::This::v->[0]
            . qq{;\n}
        }
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
        q{
            q{my $default_lex_prefix = }
            . $Parse::Marpa::This::v->[0]
            . qq{;\n}
        }
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
        q{
            q{my $default_lex_prefix = }
            . $Parse::Marpa::This::v->[5]
            . qq{;\n}
        }
    ],
    [ "copula", [ "is keyword" ] ],
    [ "copula", [ "are keyword" ] ],
    [ "string definition",
	[ "symbol phrase", "is keyword", "string specifier",
	# [ "symbol phrase", "is keyword", "literal string",
	  "period"
        ],
        q{
            '$strings{'
            . $Parse::Marpa::This::v->[0]
            . '} = '
            . $Parse::Marpa::This::v->[2]
            . qq{;\n}
        }
    ],
    [
        "preamble definition",
        [ "a keyword", "preamble keyword", "is keyword", "string specifier", "period" ],
        q{
            q{$preamble .= }
            . $Parse::Marpa::This::v->[3]
            . qq{;\n}
        }
    ],
    [ "default action definition",
	[
            "symbol phrase",
            "is keyword",
            "optional the keyword",
            "default keyword",
            "action keyword",
	    "period",
	],
        q{
            q{my $default_action = }
            . $Parse::Marpa::This::v->[0]
            . qq{;\n}
        }
    ],
    {
       lhs => "optional the keyword",
       rhs => [ "the keyword" ],
       min => 0,
       max => 1,
    },
    [ "comment sentence", [ "comment tag", "colon", "comment word sequence", "period" ], $discard ],
    {
       lhs => "comment word sequence",
       rhs => [ "comment word" ],
       # action => $discard,
       min => 0,
    },
    [ "text segment", [ "pod block" ], $discard, ],
    [ "text segment", [ "comment" ], $discard, ],
    [
        "literal string",
        [ "q string" ],
        q{ $Parse::Marpa::This::v->[0] }
    ],
    [ "literal string", [ "double quoted string" ], $concatenate_lines, ],
    [ "literal string", [ "single quoted string" ], $concatenate_lines, ],
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
       # action => $discard,
       min => 0,
    },
    {
	lhs => "production sentence",
	rhs => [ "lhs", "colon", "rhs", "period" ],
	# tell perl NNN counter is in special package
	action => q{
	    join(",\n", @{$Parse::Marpa::This::v}[0,2])
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
	rhs => [ "literal string", ],
	action => $concatenate_lines,
    },
    {
        lhs => "optional period",
	rhs => [ "period" ],
	# action => $discard,
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
	    'push(@$terminals, [ '
	    . $Parse::Marpa::This::v->[0]
	    . ' => [ '
	    . $Parse::Marpa::This::v->[2]
            . "] ] );\n"
	}
    },
    {
        lhs => "terminal sentence",
	rhs => [ "match keyword", "symbol phrase", "using keyword", "string specifier", "period" ],
	action => q{
	    'push(@$terminals, [ '
	    . $Parse::Marpa::This::v->[1]
	    . ' => '
	    . $Parse::Marpa::This::v->[3]
            . " ] );\n"
	}
    },
    [ "string specifier", [ "literal string" ], $concatenate_lines ],
    [ "string specifier", [ "symbol phrase" ], $concatenate_lines ],
    { 
        lhs => "whitespace lines",
        rhs => [ "whitespace line" ],
        min => 1,
        # action => $discard,
    }
];


my $terminals = [
    [ "a keyword"            => [ qr/a/ ] ],
    [ "action keyword"       => [ qr/action/ ] ],
    [ "as keyword"           => [ qr/as/ ] ],
    [ "are keyword"          => [ qr/are/ ] ],
    [ "default keyword"      => [ qr/default/ ] ],
    [ "define keyword"       => [ qr/define/ ] ],
    [ "is keyword"           => [ qr/is/ ] ],
    [ "lex keyword"          => [ qr/lex/ ] ],
    [ "match keyword"        => [ qr/match/ ] ],
    [ "matches keyword"      => [ qr/matches/ ] ],
    [ "perl5 keyword"        => [ qr/perl5/ ] ],
    [ "preamble keyword"     => [ qr/preamble/ ] ],
    [ "prefix keyword"       => [ qr/prefix/ ] ],
    [ "semantics keyword"    => [ qr/semantics/ ] ],
    [ "start keyword"        => [ qr/start/ ] ],
    [ "symbol keyword"       => [ qr/symbol/ ] ],
    [ "the keyword"          => [ qr/the/ ] ],
    [ "using keyword"        => [ qr/using/ ] ],
    [ "version keyword"      => [ qr/version/ ] ],
    [ "q string" => "Parse::Marpa::Lex::lex_q_quote" ],
    [ "regex" => "Parse::Marpa::Lex::lex_regex" ],
    [ "empty line"       => [ qr/^[ \t]*\n/m ] ],
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
    [ "version number"       => [ qr/(\d+\.)*\d+/ ], ],
    [ "comment"              => [qr/#.*\n/], ],
    [ "symbol word"          => [ qr/[a-zA-Z_][a-zA-Z0-9_-]*/ ], ], 
    [ "period"               => [ qr/\./ ], ], 
    [ "colon"                => [ qr/\:/ ], ], 

    # Do I want to allow comments between "to" and "do" ?
    [ "comment tag" => [ qr/(to\s+do|note|comment)/ ], ],

    # Includes all non-whitespace printable characters except period
    [ "comment word" => [ qr/[\x{21}-\x{2d}\x{2f}-\x{7e}]+/ ], ],

    [ "comma"                => [ qr/\,/ ], ], 
    [ "whitespace line"      => [ qr/^[ \t]*(?:\#[^\n]*)?\n/m ], ],
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
    # default_lex_prefix => qr/[ \t]*(\#[^\n]*\n|\n)?([ \t]*\#[^\n]*\n)*[ \t]*/,
    default_lex_prefix => $default_lex_prefix,
    # trace_rules => 1,
);

my $parse = new Parse::Marpa::Parse(
   grammar=> $g,
   # trace_actions => 1,
);
# $parse->trace("evaluation choices");

# print STDERR $g->show_rules(), "\n";

# print STDERR $g->show_SDFA(), "\n";

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
    $spec = <GRAMMAR>;
    if ((my $earleme = $parse->lex_string(\$spec)) >= 0) {
	# print $parse->show_status();
        # for the editors, line numbering starts at 1
        # do something about this?
	my ($line, $line_start) = locator($earleme, \$spec);
	say "Parse exhausted at line ", $line+1, ", earleme $earleme";
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
# print STDERR $parse->show_status();

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
