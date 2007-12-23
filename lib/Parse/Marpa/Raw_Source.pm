require 5.009005;

use feature ":5.10";
use warnings;
use strict;

# It's all integers, except for the version number
use integer;

package Parse::Marpa::Internal::Source_Raw;

my $new_terminals = [];
my $new_rules = [];
my $new_preamble = "";
my $new_start_symbol;
my $new_semantics;
my $new_version;
my $new_default_action;
my $new_default_null_value;
my $new_default_lex_prefix;
my %strings;

$new_semantics = 'perl5';

 $new_version = '0.001062';

$new_start_symbol = "grammar";

$new_preamble .=  q{
    our $whitespace = qr/(?:[ \t]*(?:\n|(?:\#[^\n]*\n)))*[ \t]*/;
    our $default_lex_prefix = $whitespace;
    our %strings;
    my $new_start_symbol;
    my $new_version;
    my $new_semantics;
    my $new_default_action;
    my $new_default_null_value;
    my $new_default_lex_prefix;
};

$new_default_lex_prefix = qr/(?:[ \t]*(?:\n|(?:\#[^\n]*\n)))*[ \t]*/;

$strings{"concatenate-lines"} =  q{
    my $v_count = scalar @$Parse::Marpa::This::v;
    return undef if $v_count <= 0;
    join("\n", grep { $_ } @$Parse::Marpa::This::v);
};

push(@$new_rules, {
    lhs => "grammar",
    rhs => ["paragraphs"],
    action =>   q{ $Parse::Marpa::This::v->[0] },
,
,

});
push(@$new_rules, {
    lhs => "grammar",
    rhs => ["paragraphs", "whitespace-lines"],
    action =>   q{ $Parse::Marpa::This::v->[0] },
,
,

});
push(@$new_rules, {
    lhs => "paragraphs",
rhs => ["paragraph"],
separator => "empty-line",
min => 1,
,
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "paragraph",
    rhs => ["definition-paragraph"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "paragraph",
    rhs => ["production-paragraph"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "paragraph",
    rhs => ["terminal-paragraph"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "definition-paragraph",
rhs => ["definition"],
min => 1,
,
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "production-paragraph",
    rhs => ["non-structural-production-sentences", "production-sentence", "non-structural-production-sentences", "action-sentence:optional", "non-structural-production-sentences"],
    action => 
    q{
        my $action = $Parse::Marpa::This::v->[3];
        my $other_key_value = join(",\n", map { $_ // "" } @{$Parse::Marpa::This::v}[0,2,4]);
        my $result =
            'push(@$new_rules, '
             . "{\n"
             . $Parse::Marpa::This::v->[1] . ",\n"
             . (defined $action ? ($action . ",\n") : "")
             . $other_key_value
             . "\n});"
         ;
         our @implicit_terminals;
         if (@implicit_terminals) {
             $result .= "\n" . 'push(@$new_terminals,' . "\n";
             while (my $implicit_terminal = shift @implicit_terminals) {
                 $result .= "    [" . $implicit_terminal . "],\n";
             }
             $result .= ");\n";
         }
         our @implicit_rules;
         if (@implicit_rules) {
             $result .= "\n" . 'push(@$new_rules, ' . "\n";
             while (my $implicit_production = shift @implicit_rules) {
                 $result .= "    {" . $implicit_production . "},\n";
             }
             $result .= " );\n";
         }
         $result;
    },
,
,

});
push(@$new_rules, 
    { lhs => "action-sentence:optional",  rhs => [ "action-sentence" ], 
                     min => 0,
                     max => 1,
                     action => q{ $Parse::Marpa::This::v->[0] }
             },
 );

push(@$new_rules, {
    lhs => "non-structural-production-sentences",
rhs => ["non-structural-production-sentence"],
min => 0,
,
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "non-structural-production-sentence",
    rhs => ["priority:k18", "integer", "period"],
    action => 
q{ q{ priority => } . $Parse::Marpa::This::v->[1] },
,
,

});
push(@$new_terminals,
    ["priority:k18" => [ qr/priority/ ]],
);

push(@$new_rules, {
    lhs => "action-sentence",
    rhs => ["the:ka:optional", "action:k8", "is:k7", "action-specifier", "period"],
    action => 
q{
    "    action => "
    . $Parse::Marpa::This::v->[3]
},
,
,

});
push(@$new_rules, {
    lhs => "action-sentence",
    rhs => ["action-specifier", "period"],
    action => 
q{
    "    action => "
    . $Parse::Marpa::This::v->[0]
},
,
,

});
push(@$new_rules, {
    lhs => "action-specifier",
    rhs => ["string-specifier"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "non-structural-production-sentence",
    rhs => ["comment-sentence"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "non-structural-terminal-sentence",
    rhs => ["comment-sentence"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "definition",
    rhs => ["setting", "period"],
    action =>   q{ $Parse::Marpa::This::v->[0] },
,
,
 priority => 1000
});
push(@$new_rules, {
    lhs => "definition",
    rhs => ["comment-sentence"],
,
,

});
push(@$new_rules, {
    lhs => "definition",
    rhs => ["bracketed-comment"],
,
,

});
push(@$new_rules, {
    lhs => "definition",
    rhs => ["string-definition"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "setting",
    rhs => ["default-action-setting"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "setting",
    rhs => ["default-null-value-setting"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "setting",
    rhs => ["preamble-setting"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "setting",
    rhs => ["semantics-setting"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "setting",
    rhs => ["version-setting"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "setting",
    rhs => ["start-symbol-setting"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "setting",
    rhs => ["default-lex-prefix-setting"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "semantics-setting",
    rhs => ["the:ka:optional", "semantics:k16", "copula", "perl5:k17"],
    action => 
     q{
         q{$new_semantics = '}
         . $Parse::Marpa::This::v->[3]
        . qq{';\n}
},
,
,

});
push(@$new_rules, {
    lhs => "semantics-setting",
    rhs => ["perl5:k17", "copula", "the:ka:optional", "semantics:k16"],
    action => 
q{
    q{$new_semantics = '}
    . $Parse::Marpa::This::v->[0]
    . qq{';\n}
},
,
,

});
push(@$new_terminals,
    ["semantics:k16" => [ qr/semantics/ ]],
    ["perl5:k17" => [ qr/perl5/ ]],
);

push(@$new_rules, {
    lhs => "version-setting",
    rhs => ["the:ka:optional", "version:k14", "copula", "version-number"],
    action => 
q{
    q{ $new_version = '}
    . Parse::Marpa::Source::canonical_version($Parse::Marpa::This::v->[3])
    . qq{';\n}
},
,
,

});
push(@$new_rules, {
    lhs => "version-setting",
    rhs => ["version%20number:k15", "copula", "the:ka:optional", "version:k14"],
    action => 
q{
    q{ $new_version = '}
    . Parse::Marpa::Source::canonical_version($Parse::Marpa::This::v->[0])
    . qq{';\n}
},
,
,

});
push(@$new_terminals,
    ["version:k14" => [ qr/version/ ]],
    ["version%20number:k15" => [ qr/version number/ ]],
);

push(@$new_rules, {
    lhs => "start-symbol-setting",
    rhs => ["the:ka:optional", "start:k13", "symbol:k12", "copula", "symbol-phrase"],
    action => 
q{
    q{$new_start_symbol = "}
    . $Parse::Marpa::This::v->[4]
    . qq{";\n}
},
,
,

});
push(@$new_rules, {
    lhs => "start-symbol-setting",
    rhs => ["symbol-phrase", "copula", "the:ka:optional", "start:k13", "symbol:k12"],
    action => 
q{
    q{$new_start_symbol = }
    . $Parse::Marpa::This::v->[0]
    . qq{;\n}
},
,
,

});
push(@$new_terminals,
    ["symbol:k12" => [ qr/symbol/ ]],
    ["start:k13" => [ qr/start/ ]],
);

push(@$new_rules, {
    lhs => "default-lex-prefix-setting",
    rhs => ["regex", "copula", "the:ka:optional", "default:k9", "lex:k11", "prefix:k10"],
    action => 
q{
             q{$new_default_lex_prefix = }
             . $Parse::Marpa::This::v->[0]
             . qq{;\n}
},
,
,

});
push(@$new_rules, {
    lhs => "default-lex-prefix-setting",
    rhs => ["the:ka:optional", "default:k9", "lex:k11", "prefix:k10", "copula", "regex"],
    action => 
q{
    q{$new_default_lex_prefix = }
    . $Parse::Marpa::This::v->[5]
    . qq{;\n}
},
,
,

});
push(@$new_terminals,
    ["prefix:k10" => [ qr/prefix/ ]],
    ["lex:k11" => [ qr/lex/ ]],
);

push(@$new_rules, {
    lhs => "default-null-value-setting",
    rhs => ["string-specifier", "copula", "the:ka:optional", "default:k9", "null:kf", "value:ke"],
    action => 
q{
             q{$new_default_null_value = }
             . $Parse::Marpa::This::v->[0]
             . qq{;\n}
},
,
,

});
push(@$new_rules, {
    lhs => "default-null-value-setting",
    rhs => ["the:ka:optional", "default:k9", "null:kf", "value:ke", "copula", "string-specifier"],
    action => 
q{
    q{$new_default_null_value = }
    . $Parse::Marpa::This::v->[5]
    . qq{;\n}
},
,
,

});
push(@$new_terminals,
    ["value:ke" => [ qr/value/ ]],
    ["null:kf" => [ qr/null/ ]],
);

push(@$new_rules, {
    lhs => "preamble-setting",
    rhs => ["a:kd", "preamble:kc", "is:k7", "string-specifier"],
    action => 
q{
    q{$new_preamble .= }
    . $Parse::Marpa::This::v->[3]
    . qq{;\n}
},
,
 priority => 1000,

});
push(@$new_rules, {
    lhs => "preamble-setting",
    rhs => ["string-specifier", "is:k7", "a:kd", "preamble:kc"],
    action => 
q{
    q{$new_preamble .= }
    . $Parse::Marpa::This::v->[0]
    . qq{;\n}
},
,
 priority => 1000,

});
push(@$new_terminals,
    ["preamble:kc" => [ qr/preamble/ ]],
    ["a:kd" => [ qr/a/ ]],
);

push(@$new_rules, {
    lhs => "copula",
    rhs => ["is:k7"],
,
,

});
push(@$new_rules, {
    lhs => "copula",
    rhs => ["are:kb"],
,
,

});
push(@$new_terminals,
    ["are:kb" => [ qr/are/ ]],
);

push(@$new_rules, {
    lhs => "string-definition",
    rhs => ["symbol-phrase", "is:k7", "string-specifier", "period"],
    action => 
q{
    '$strings{"'
    . $Parse::Marpa::This::v->[0]
    . '"} = '
    . $Parse::Marpa::This::v->[2]
    . qq{;\n}
},
,
,

});
push(@$new_rules, {
    lhs => "default-action-setting",
    rhs => ["action-specifier", "is:k7", "the:ka:optional", "default:k9", "action:k8"],
    action => 
q{
    q{ $new_default_action = }
    . $Parse::Marpa::This::v->[0]
    . qq{;\n}
},
,
,

});
push(@$new_rules, {
    lhs => "default-action-setting",
    rhs => ["the:ka:optional", "default:k9", "action:k8", "is:k7", "action-specifier"],
    action => 
q{
    q{ $new_default_action = }
    . $Parse::Marpa::This::v->[4]
    . qq{;\n}
},
,
,

});
push(@$new_terminals,
    ["is:k7" => [ qr/is/ ]],
    ["action:k8" => [ qr/action/ ]],
    ["default:k9" => [ qr/default/ ]],
    ["the:ka" => [ qr/the/ ]],
);

push(@$new_rules, 
    { lhs => "the:ka:optional",  rhs => [ "the:ka" ], 
                     min => 0,
                     max => 1,
                     action => q{ $Parse::Marpa::This::v->[0] }
             },
 );

push(@$new_rules, {
    lhs => "comment-sentence",
    rhs => ["comment-tag", "%3a:k6", "comment-words", "period"],
,
,

});
push(@$new_rules, {
    lhs => "comment-words",
rhs => ["comment-word"],
min => 1,
,
,
,

});
push(@$new_rules, {
    lhs => "literal-string",
    rhs => ["q-string"],
    action =>   q{ $Parse::Marpa::This::v->[0] },
,
,

});
push(@$new_rules, {
    lhs => "literal-string",
    rhs => ["double-quoted-string"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "literal-string",
    rhs => ["single-quoted-string"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "production-sentence",
    rhs => ["lhs", "%3a:k6", "rhs", "period"],
    action => 
q{ join(",\n", @{$Parse::Marpa::This::v}[0,2]) },
,
,

});
push(@$new_terminals,
    ["%3a:k6" => [ qr/:/ ]],
);

push(@$new_rules, {
    lhs => "symbol-phrase",
rhs => ["symbol-word"],
min => 1,
,
    action => 
q{ Parse::Marpa::Source::canonical_symbol_name(join("-", @$Parse::Marpa::This::v)) },
,
,

});
push(@$new_rules, {
    lhs => "lhs",
    rhs => ["symbol-phrase"],
    action => 
q{ '    lhs => "' . $Parse::Marpa::This::v->[0] . q{"} },
,
,

});
push(@$new_rules, {
    lhs => "rhs",
rhs => ["rhs-element"],
separator => "comma",
min => 0,
,
    action => 
q{ "    rhs => [" . join(", ", @$Parse::Marpa::This::v) . "]" },
,
,

});
push(@$new_rules, {
    lhs => "rhs",
    rhs => ["symbol-phrase", "sequence:k4"],
    action => 
q{
    q{rhs => ["}
    . $Parse::Marpa::This::v->[0]
    . qq{"],\n}
    . qq{min => 1,\n}
},
,
 priority => 1000,

});
push(@$new_rules, {
    lhs => "rhs",
    rhs => ["optional:k3", "symbol-phrase", "sequence:k4"],
    action => 
q{
    q{rhs => ["}
    . $Parse::Marpa::This::v->[1]
    . qq{"],\n}
    . qq{min => 0,\n}
},
,
 priority => 2000,

});
push(@$new_rules, {
    lhs => "rhs",
    rhs => ["symbol-phrase", "separated:k5", "symbol-phrase", "sequence:k4"],
    action => 
q{
    q{rhs => ["}
    . $Parse::Marpa::This::v->[2]
    . qq{"],\n}
    . q{separator => "}
    . $Parse::Marpa::This::v->[0]
    . qq{",\n}
    . qq{min => 1,\n}
},
,
 priority => 2000,

});
push(@$new_rules, {
    lhs => "rhs",
    rhs => ["optional:k3", "symbol-phrase", "separated:k5", "symbol-phrase", "sequence:k4"],
    action => 
q{
    q{rhs => ["}
    . $Parse::Marpa::This::v->[3]
    . qq{"],\n}
    . q{separator => "}
    . $Parse::Marpa::This::v->[1]
    . qq{",\n}
    . qq{min => 0,\n}
},
,
 priority => 3000,

});
push(@$new_terminals,
    ["sequence:k4" => [ qr/sequence/ ]],
    ["separated:k5" => [ qr/separated/ ]],
);

push(@$new_rules, {
    lhs => "rhs-element",
    rhs => ["mandatory-rhs-element"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "rhs-element",
    rhs => ["optional-rhs-element"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "mandatory-rhs-element",
    rhs => ["rhs-symbol-specifier"],
    action => 
q{ q{"} . $Parse::Marpa::This::v->[0] . q{"} },
,
,

});
push(@$new_rules, {
    lhs => "optional-rhs-element",
    rhs => ["optional:k3", "rhs-symbol-specifier"],
    action => 
q{
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
,
,

});
push(@$new_terminals,
    ["optional:k3" => [ qr/optional/ ]],
);

push(@$new_rules, {
    lhs => "rhs-symbol-specifier",
    rhs => ["symbol-phrase"],
    action => 
q{ $Parse::Marpa::This::v->[0] },
,
,

});
push(@$new_rules, {
    lhs => "rhs-symbol-specifier",
    rhs => ["regex"],
    action => 
q{
    my $regex = $Parse::Marpa::This::v->[0];
    my ($symbol, $new) = Parse::Marpa::Source::gen_symbol_from_regex($regex);
    if ($new) {
        our @implicit_terminals;
        push(@implicit_terminals,
            q{"}
            . $symbol
            . '" => [ '
            . $regex
            . " ]"
        );
    }
    $symbol;
},
,
,

});
push(@$new_rules, {
    lhs => "terminal-paragraph",
    rhs => ["non-structural-terminal-sentences", "terminal-sentence", "non-structural-terminal-sentences"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "non-structural-terminal-sentences",
rhs => ["non-structural-terminal-sentence"],
min => 0,
,
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "terminal-sentence",
    rhs => ["symbol-phrase", "matches:k2", "regex", "period"],
    action => 
q{
    q{push(@$new_terminals, [ "}
    . $Parse::Marpa::This::v->[0]
    . q{" => [ }
    . $Parse::Marpa::This::v->[2]
    . qq{] ] );\n}
},
,
,

});
push(@$new_terminals,
    ["matches:k2" => [ qr/matches/ ]],
);

push(@$new_rules, {
    lhs => "terminal-sentence",
    rhs => ["match:k1", "symbol-phrase", "using:k0", "string-specifier", "period"],
    action => 
q{
    q{push(@$new_terminals, [ "}
    . $Parse::Marpa::This::v->[1]
    . q{" => }
    . $Parse::Marpa::This::v->[3]
    . qq{ ] );\n}
},
,
,

});
push(@$new_terminals,
    ["using:k0" => [ qr/using/ ]],
    ["match:k1" => [ qr/match/ ]],
);

push(@$new_rules, {
    lhs => "string-specifier",
    rhs => ["literal-string"],
    action => $strings{ "concatenate-lines" },
,
,

});
push(@$new_rules, {
    lhs => "string-specifier",
    rhs => ["symbol-phrase"],
    action => 
q{
    '$strings{ "'
    . $Parse::Marpa::This::v->[0]
    . '" }'
},
,
,

});
push(@$new_rules, {
    lhs => "whitespace-lines",
rhs => ["whitespace-line"],
min => 1,
,
,
,

});
push(@$new_terminals, [ "q-string" =>  "lex_q_quote" ] );

push(@$new_terminals, [ "regex" =>  "lex_regex" ] );

push(@$new_terminals, [ "empty-line" => [ qr/^[ \t]*\n/m] ] );

push(@$new_terminals, [ "bracketed-comment" => [ qr/\x{5b}[^\x{5d}]*\x{5d}/] ] );

push(@$new_terminals, [ "single-quoted-string" =>  q{
    state $prefix_regex = qr/\G(?:[ \t]*(?:\n|(?:\#[^\n]*\n)))*[ \t]*'/o;
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
} ] );

push(@$new_terminals, [ "double-quoted-string" =>  q{
    state $prefix_regex = qr/\G(?:[ \t]*(?:\n|(?:\#[^\n]*\n)))*[ \t]*"/o;
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
} ] );

push(@$new_terminals, [ "version-number" => [ qr/(\d+\.)*\d+/] ] );

push(@$new_terminals, [ "symbol-word" => [ qr/[a-zA-Z_][a-zA-Z0-9_-]*/] ] );

push(@$new_terminals, [ "period" => [ qr/\./] ] );

push(@$new_terminals, [ "integer" => [ qr/\d+/] ] );

push(@$new_terminals, [ "comment-tag" => [ qr/(to\s+do|note|comment)/] ] );

push(@$new_terminals, [ "comment-word" => [ qr/[\x{21}-\x{2d}\x{2f}-\x{7e}]+/] ] );

push(@$new_terminals, [ "comma" => [ qr/\,/] ] );

push(@$new_terminals, [ "whitespace-line" => [ qr/^[ \t]*(?:\#[^\n]*)?\n/m] ] );


sub Parse::Marpa::Internal::raw_source_grammar {

    my $raw_source_grammar = new Parse::Marpa(
        start => $new_start_symbol,
        rules => $new_rules,
        terminals => $new_terminals,
        version => $new_version,
        warnings => 1,
    );  
        
    $raw_source_grammar->set(
        default_lex_prefix => $new_default_lex_prefix
    ) if defined $new_default_lex_prefix;

    $raw_source_grammar->set(
        default_action => $new_default_action
    ) if defined $new_default_action;

    $raw_source_grammar->set(
        default_null_value => $new_default_null_value
    ) if defined $new_default_null_value;

    $raw_source_grammar;

}
        
1;

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4: