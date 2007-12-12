# Marpa compiling its own specification language

use strict;
use warnings;
use feature ":5.10";
use English;
use lib "../lib";
use Parse::Marpa;
use Carp;
use Fatal;

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

my $preamble = <<'EOCODE';
    our $whitespace = qr/(?:[ \t]*(?:\n|(?:\#[^\n]*\n)))*[ \t]*/;
    our $default_lex_prefix = $whitespace;
EOCODE

my %regex;

# URL escaping
sub gen_symbol_from_regex {
    my $regex = shift;
    state $uniq_number;
    $uniq_number //= 0;
    given ($regex) {
        when (/^qr/) { $regex = substr($regex, 3, -1); }
        default { $regex = substr($regex, 1, -1); }
    }
    my $symbol = $regex{$regex};
    return $symbol if defined $symbol;
    $symbol = substr($regex, 0, 20);
    $symbol =~ s/%/%%/g;
    $symbol =~ s/([^[:alnum:]_-])/sprintf("%%%.2x", ord($1))/ge;
    $symbol .= sprintf(":k%x", $uniq_number++);
    $regex{$regex} = $symbol;
    ($symbol, 1);
}

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

my $rules = [
    [ "grammar", [ "paragraph sequence" ], $concatenate_lines ],
    [ "grammar", [ "paragraph sequence", "whitespace lines" ], $concatenate_lines ],
    {
        lhs => "paragraph sequence",
        rhs => [ "paragraph" ],
        action => $concatenate_lines,
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
            my $other_key_value = join(",\n", map { $_ // "" } @{$Parse::Marpa::This::v}[0,2,4]);
	    my $result =
                'push(@$rules, '
                . "{\n"
                . $Parse::Marpa::This::v->[1] . ",\n"
                . (defined $action ? ($action . ",\n") : "")
                . $other_key_value
                . "\n});"
            ;
            our @implicit_terminals;
            if (@implicit_terminals) {
                $result .= "\n" . 'push(@$terminals,' . "\n";
                while (my $implicit_terminal = shift @implicit_terminals) {
                    $result .= "    [" . $implicit_terminal . "],\n";
                }
                $result .= ");\n";
            }
            our @implicit_rules;
            if (@implicit_rules) {
                $result .= "\n" . 'push(@$rules, ' . "\n";
                while (my $implicit_production = shift @implicit_rules) {
                    $result .= "    {" . $implicit_production . "},\n";
                }
                $result .= " );\n";
            }
            $result;
	}
    ],
    { 
       lhs => "non structural production sentences",
       rhs => [ "non structural production sentence" ],
       min => 0,
       action => $concatenate_lines,
    },
    {
       lhs => "non structural production sentence",
       rhs => [ "priority keyword", "integer", "period" ],
       action => q{ q{ priority => } . $Parse::Marpa::This::v->[1] },
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
    [
        "definition",
        [ "setting", "period" ],
        q{ $Parse::Marpa::This::v->[0] }
    ],
    [ "definition", [ "comment sentence", ] ],
    [ "definition", [ "bracketed comment", ] ],
    [ "definition", [ "default action definition", ], $concatenate_lines, ],
    [ "definition", [ "string definition", ], $concatenate_lines, ],
    [ "definition", [ "preamble definition", ], $concatenate_lines, 1000 ],
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
            q{my $start_symbol = "}
            . $Parse::Marpa::This::v->[4]
            . qq{";\n}
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
            q{our $default_lex_prefix = }
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
            q{our $default_lex_prefix = }
            . $Parse::Marpa::This::v->[5]
            . qq{;\n}
        }
    ],
    [ "copula", [ "is keyword" ] ],
    [ "copula", [ "are keyword" ] ],
    [ "string definition",
	[ "symbol phrase", "is keyword", "string specifier",
	  "period"
        ],
        q{
            '$strings{"'
            . $Parse::Marpa::This::v->[0]
            . '"} = '
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
            "action specifier",
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
	action => q{ ::canonical_symbol_name(join("-", @$Parse::Marpa::This::v)) },
	min => 1,
    },
    {
        lhs => "lhs",
	rhs => [ "symbol phrase" ],
	action => q{ '    lhs => "' . $Parse::Marpa::This::v->[0] . q{"} },
    },
    {
        lhs => "rhs",
	rhs => [ "rhs element" ],
	action => q{ "    rhs => [" . join(", ", @$Parse::Marpa::This::v) . "]" },
	min => 1,
	separator => "comma",
    },
    {
        lhs => "rhs",
	rhs => [ "symbol phrase", "sequence keyword" ],
        priority => 1000,
        action => q{
            q{rhs => ["}
            . $Parse::Marpa::This::v->[0]
            . qq{"],\n}
            . qq{min => 1,\n}
        }
    },
    {
        lhs => "rhs",
	rhs => [ "optional keyword", "symbol phrase", "sequence keyword" ],
        priority => 2000,
        action => q{
            q{rhs => ["}
            . $Parse::Marpa::This::v->[1]
            . qq{"],\n}
            . qq{min => 0,\n}
        }
    },
    {
        lhs => "rhs",
	rhs => [ "symbol phrase", "separated keyword", "symbol phrase", "sequence keyword" ],
        priority => 2000,
        action => q{
            q{rhs => ["}
            . $Parse::Marpa::This::v->[2]
            . qq{"],\n}
            . q{separator => "}
            . $Parse::Marpa::This::v->[0]
            . qq{",\n}
            . qq{min => 1,\n}
        }
    },
    {
        lhs => "rhs",
	rhs => [
            "optional keyword",
            "symbol phrase", "separated keyword",
            "symbol phrase", "sequence keyword"
        ],
        priority => 3000,
        action => q{
            q{rhs => ["}
            . $Parse::Marpa::This::v->[3]
            . qq{"],\n}
            . q{separator => "}
            . $Parse::Marpa::This::v->[1]
            . qq{",\n}
            . qq{min => 0,\n}
        }
    },
    {
        lhs => "rhs element",
        rhs => [ "mandatory rhs element", ],
        action => $concatenate_lines,
    },
    {
        lhs => "rhs element",
        rhs => [ "optional rhs element", ],
        action => $concatenate_lines,
    },
    {
        lhs => "mandatory rhs element",
	rhs => [ "rhs symbol specifier" ],
        action => q{ q{"} . $Parse::Marpa::This::v->[0] . q{"} },
    },
    {
        lhs => "optional rhs element",
	rhs => [ "optional keyword", "rhs symbol specifier" ],
	action => q{
            my $symbol_phrase = $Parse::Marpa::This::v->[1];
            my $optional_symbol_phrase = $symbol_phrase . ":optional";
            our %implicit_rules;
            if (not defined $implicit_rules{$optional_symbol_phrase}) {
                $implicit_rules{$optional_symbol_phrase} = 1;
                our @implicit_rules;
                push(
                    @implicit_rules,
                    q{ lhs => "} . $optional_symbol_phrase . q{", }
                    . q{ rhs => [ "} . $symbol_phrase . qq{" ], }
                    . q{
                            min => 0,
                            max => 1,
                            action => q{ $Parse::Marpa::This::v->[0] }
                    }
                );
            }
            q{"} . $optional_symbol_phrase . q{"};
        },
    },
    {
        lhs => "rhs symbol specifier",
	rhs => [ "symbol phrase" ],
	action => q{ $Parse::Marpa::This::v->[0] },
    },
    {
        lhs => "rhs symbol specifier",
	rhs => [ "regex", ],
	action => q{
            my $regex = $Parse::Marpa::This::v->[0];
            my ($symbol, $new) = ::gen_symbol_from_regex($regex);
            our @implicit_terminals;
            if ($new) {
                push(@implicit_terminals,
                    q{"}
                    . $symbol
                    . '" => [ '
                    . $regex
                    . " ]"
                );
            }
            $symbol;
        }
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
	    q{push(@$terminals, [ "}
	    . $Parse::Marpa::This::v->[0]
	    . q{" => [ }
	    . $Parse::Marpa::This::v->[2]
            . qq{] ] );\n}
	}
    },
    {
        lhs => "terminal sentence",
	rhs => [ "match keyword", "symbol phrase", "using keyword", "string specifier", "period" ],
	action => q{
	    q{push(@$terminals, [ "}
	    . $Parse::Marpa::This::v->[1]
	    . q{" => }
	    . $Parse::Marpa::This::v->[3]
            . qq{ ] );\n}
	}
    },
    [ "string specifier", [ "literal string" ], $concatenate_lines ],
    [
        "string specifier",
        [ "symbol phrase" ],
        q{
            '$strings{ "'
	    . $Parse::Marpa::This::v->[0]
            . '" }'
        }
    ],
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
    [ "optional keyword"     => [ qr/optional/ ] ],
    [ "perl5 keyword"        => [ qr/perl5/ ] ],
    [ "prefix keyword"       => [ qr/prefix/ ] ],
    [ "preamble keyword"     => [ qr/preamble/ ] ],
    [ "priority keyword"     => [ qr/priority/ ] ],
    [ "separated keyword"    => [ qr/separated/ ] ],
    [ "sequence keyword"     => [ qr/sequence/ ] ],
    [ "semantics keyword"    => [ qr/semantics/ ] ],
    [ "start keyword"        => [ qr/start/ ] ],
    [ "symbol keyword"       => [ qr/symbol/ ] ],
    [ "the keyword"          => [ qr/the/ ] ],
    [ "using keyword"        => [ qr/using/ ] ],
    [ "version keyword"      => [ qr/version/ ] ],
    [ "q string"             => "lex_q_quote" ],
    [ "regex"                => "lex_regex" ],
    [ "empty line"           => [ qr/^[ \t]*\n/m ] ],
    [ "bracketed comment"    => [ qr/\x{5b}[^\x{5d}]*\x{5d}/ ] ],
    # change to lex_q_quote
    [ "single quoted string" => q{
            our $default_lex_prefix;
	    state $prefix_regex = qr/\G$default_lex_prefix'/o;
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
            our $default_lex_prefix;
	    state $prefix_regex = qr/\G$default_lex_prefix"/o;
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
    [ "integer"              => [ qr/\d+/ ], ],

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
    default_lex_prefix => $default_lex_prefix,
    # trace_rules => 1,
   preamble => $preamble,
);

my $parse = new Parse::Marpa::Parse(
   grammar=> $g,
   # trace_actions => 1,
);
# $parse->trace("lex");

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

    state $lines;
    $lines //= [0];
    my $pos = pos $$string = 0;
    NL: while ($$string =~ /\n/g) {
	$pos = pos $$string;
        push(@$lines, $pos);
	last NL if $pos > $earleme;
    }
    my $line = (@$lines) - ($pos > $earleme ? 2 : 1);
    my $line_start = $lines->[$line];
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
	say STDERR "Parse exhausted at line ", $line+1, ", earleme $earleme";
	given (index($spec, "\n", $line_start)) {
	    when (undef) { say STDERR substr($spec, $line_start) }
	    default { say STDERR substr($spec, $line_start, $_-$line_start) }
	}
	say STDERR +(" " x ($earleme-$line_start)), "^";
	exit 1;
    }
    $parse->lex_end();
}

unless ($parse->initial()) {
    say STDERR "No parse";
    my ($earleme, $lhs) = $parse->find_complete_rule();
    unless (defined $earleme) {
	say STDERR "No rules completed";
	exit 1;
    }
    my ($line, $line_start) = locator($earleme, \$spec);
    say STDERR "Parses completed at line $line, earleme $earleme for symbols:";
    say STDERR join(", ", @$lhs);
    given (index($spec, "\n", $line_start)) {
	when (undef) { say STDERR substr($spec, $line_start) }
	default { say STDERR substr($spec, $line_start, $_-$line_start) }
    }
    say STDERR +(" " x ($earleme-$line_start)), "^";
    exit 1;
}

my $headers;
{ local($RS) = undef; open(HEADERS, "<", "headers.pl"); $headers = <HEADERS>; }

my $trailers;
{ local($RS) = undef; open(TRAILERS, "<", "trailers.pl"); $trailers = <TRAILERS>; }

my $value = $parse->value();
print $headers, $value, $trailers;

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
