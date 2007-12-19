package Parse::Marpa;
require 5.009005;

# the public namespace

# TO THE (POTENTIAL) READER:

# At this point it's my suggestion that
# reading the code and documentation below will serve no purpose.
# This is a developer-only version,
# not useful except for my own testing.
# What applies to the code applies also to the documentation
# -- it's an early draft.  In particular, the draft
# acknowledgments might be incomplete or just plain inaccurate.

# The above is simply a humble and non-binding personal request.
# Where there seems to be a conflict between the above suggestion
# and the standard Perl license, the standard Perl license
# prevails.

# thanks, Jeffrey Kegler

use feature ":5.10";
use warnings;
no warnings "recursion";
use strict;

our $VERSION = '0.001_058';
$VERSION = eval $VERSION;

package Parse::Marpa::Internal;

use Carp;
use Data::Dumper;
use Scalar::Util qw(weaken);

use Parse::Marpa::Lex;
use Parse::Marpa::Source;

=begin Apology:

An APOLOGY to the READER:

Please don't conclude that this is my idea of what Perl code
should usually look like.  It's not.  The aim of this module is
nonstandard, and those aims have forced on this module a style that
I don't in general use or recommend.

The aim of this code is easy translation into time-efficient C.
This because parsers run inside tight loops.  In particular the rap
against Earley's has always been speed.  Readability is not a good
reason to writing a uselessly slow module.  In a sense, if you hate
the style, I've done my job -- you probably wouldn't be reading the
code unless the module proved to be of use.

I've written very C-ish Perl -- lots of references, avoidances of
hashes, no internal OO, etc.  To repeat, I don't think that trying
to make Perl look like C is, in general, a good idea.  But as the
lawyers say, circumstances make cases.

C conversion is important because one of two things are going to
happen to Marpa: it turns out to be so slow it's difficult to use,
or it does not.  If Marpa is slow, the next thing to try is conversion
to C.  If it's fast, Marpa will be highly useful, and there will
almost certainly be demand for a yet faster version -- in C.  As
of this writing, my guess is that Marpa is doomed to a C
re-implementation, or oblivion.

The candid reader, having read the above and perused the below,
might not be interested in my recommendations on books about Perl
style, but for what it's worth, I recommend Damian Conway's _Perl
Best Practices_.  Damian is the best starting point for thinking
about Perl style, whether you agree with him or not.  I've made
many exceptions due to necessity, as described above.   Many more
I've no doubt made out of ignorance.  A few other exceptions are
because I can't agree with Damian.

Especially noticeable will be that I don't append "_ref" to the
name references -- almost every variable name in the below is a
reference.  I don't for a moment imagine it's easy code to read,
but I can't believe having 90% of the variable names end in "_ref"
will make it any easier.

=end Apology:

=cut

=begin Implementation:

Structures and Objects: The design is to present an object-oriented
interface, but internally to avoid overheads.  So internally, where
objects might be used, I use array with constant indices to imitate
what in C would be structures.

=end Implementation:

=cut

# It's all integers, except for the version number
use integer;

package Parse::Marpa::Internal::Symbol;

use constant ID              => 0;
use constant NAME            => 1;
use constant LHS             => 2;     # rules with this as the lhs,
                                       # as a ref to an array of rule refs
use constant RHS             => 3;     # rules with this in the rhs,
                                       # as a ref to an array of rule refs
use constant ACCESSIBLE      => 4;     # reachable from start symbol?
use constant PRODUCTIVE      => 5;     # reachable from input symbol?
use constant START           => 6;     # is one of the start symbols?
use constant REGEX           => 7;     # regex, for terminals; undef otherwise
use constant NULLING         => 8;     # always is null?
use constant NULLABLE        => 9;     # can match null?
use constant NULL_VALUE      => 10;    # value when null
use constant NULL_ALIAS      => 11;    # for a non-nulling symbol,
                                       # ref of a its nulling alias,
                                       # if there is one
                                       # otherwise undef
use constant TERMINAL        => 12;    # terminal?
use constant CLOSURE         => 13;    # closure to do lexing
use constant PRIORITY        => 14;    # order, for lexing
use constant COUNTED         => 15;    # used on rhs of counted rule?
use constant ACTION          => 16;    # lexing action specified by user
use constant PREFIX          => 17;    # lexing prefix specified by user
use constant SUFFIX          => 18;    # lexing suffix specified by user

package Parse::Marpa::Internal::Rule;

use constant ID              => 0;
use constant NAME            => 1;
use constant LHS             => 2;     # ref of the left hand symbol
use constant RHS             => 3;     # array of symbol refs
use constant NULLABLE        => 4;     # can match null?
use constant ACCESSIBLE      => 5;     # reachable from start symbol?
use constant PRODUCTIVE      => 6;     # reachable from input symbol?
use constant NULLING         => 7;     # always matches null?
use constant USEFUL          => 8;     # use this rule in NFA?
use constant ACTION          => 9;     # action for this rule
use constant CLOSURE         => 10;    # closure for evaluating this rule
use constant ORIGINAL_RULE   => 11;    # for a rewritten rule, the original

# use constant ORDER                    => 12;   # the order in which rules are to
# be tried -- not necessarily unique
use constant HAS_CHAF_LHS => 13;       # has CHAF internal symbol as lhs?
use constant HAS_CHAF_RHS => 14;       # has CHAF internal symbol on rhs?
use constant PRIORITY     => 15;       # rule priority

package Parse::Marpa::Internal::NFA;

use constant ID         => 0;
use constant NAME       => 1;
use constant ITEM       => 2;          # an LR(0) item
use constant TRANSITION => 3;          # the transitions, as a hash
                                       # from symbol name to NFA states

package Parse::Marpa::Internal::SDFA;

use constant ID             => 0;
use constant NAME           => 1;
use constant NFA_STATES     => 2;   # in an SDFA: an array of NFA states
use constant TRANSITION     => 3;   # the transitions, as a hash
                                    # from symbol name to SDFA states
use constant COMPLETE_LHS   => 4;   # an array of the lhs's of complete rules
use constant COMPLETE_RULES => 5;   # an array of lists of the complete rules,
                                    # indexed by lhs
use constant START_RULE     => 6;   # the start rule
use constant TAG            => 7;   # implementation-independant tag

package Parse::Marpa::Internal::LR0_item;

use constant RULE     => 0;
use constant POSITION => 1;

package Parse::Marpa::Internal::Grammar;

use constant ID              => 0;    # number of this grammar
use constant NAME            => 1;    # namespace special to this grammar
use constant RULES           => 2;    # array of rule refs
use constant SYMBOLS         => 3;    # array of symbol refs
use constant RULE_HASH       => 4;    # hash by name of symbol refs
use constant SYMBOL_HASH     => 5;    # hash by name of symbol refs
use constant START           => 6;    # ref to start symbol
use constant NFA             => 7;    # array of states
use constant SDFA            => 8;    # array of states
use constant SDFA_BY_NAME    => 9;    # hash from SDFA name to SDFA reference
use constant NULLABLE_SYMBOL => 10;   # array of refs of the nullable symbols
use constant ACADEMIC        => 11;   # true if this is a textbook grammar,
                                      # for checking the NFA and SDFA, and NOT
                                      # for actual Earley parsing
use constant DEFAULT_NULL_VALUE => 12; # default value for nulling symbols
use constant DEFAULT_ACTION     => 13; # action for rules without one
use constant DEFAULT_LEX_PREFIX => 14; # default prefix for lexing
use constant DEFAULT_LEX_SUFFIX => 15; # default suffix for lexing
use constant AMBIGUOUS_LEX      => 16; # lex ambiguously? (the default)
use constant TRACE_RULES        => 17;
use constant TRACE_FILE_HANDLE  => 18;
use constant LOCATION_CALLBACK  => 19; # default callback for showing location
use constant VOLATILE           => 20; # default for volatility
use constant PROBLEMS           => 21; # fatal problems
use constant PREAMBLE           => 22; # default preamble
use constant STATE              => 23; # the grammar's state
use constant WARNINGS           => 24; # print warnings about grammar?
use constant VERSION            => 25; # Marpa version this grammar was compiled from
use constant CODE_LINES         => 26; # max lines to display on failure
use constant SEMANTICS          => 27; # semantics (currently perl5 only)
use constant TRACING            => 28; # master flag, set if any tracing is being done
                                       # (to control overhead for non-tracing processes)
use constant TRACE_STRINGS      => 29; # trace strings defined in marpa grammar
use constant TRACE_PREDEFINEDS  => 30; # trace predefineds in marpa grammar
use constant TRACE_PRIORITIES   => 31;
use constant TRACE_LEX_TRIES          => 32;
use constant TRACE_LEX_MATCHES        => 33;
use constant TRACE_ITERATION_SEARCHES => 34;
use constant TRACE_ITERATION_CHANGES  => 35;
use constant TRACE_EVALUATION_CHOICES => 36;
use constant TRACE_COMPLETIONS        => 37;
use constant TRACE_ACTIONS            => 38;
use constant TRACE_VALUES             => 39;

# values for state
use constant NEW          => "new grammar";
use constant SOURCE_RULES => "grammar with rules entered from source";
use constant PERL_RULES   => "grammar with rules entered from Perl";
use constant PRECOMPUTED  => "precomputed grammar";
use constant COMPILED     => "compiled grammar";
use constant EVALED       => "evaled grammar";
use constant IN_USE       => "in use grammar";

package Parse::Marpa::This;

# Public namespace reserved for dynamic globals, that is local() variables.
# No actual globals should reside here

package Parse::Marpa::Internal::This;

# Internal namespace reserved for dynamic globals, that is local() variables.
# No actual globals should reside here

package Parse::Marpa::Internal;

sub die_on_problems {
    my $fatal_error = shift;
    my $warnings = shift;
    my $where = shift;
    my $long_where = shift;
    my $code = shift;

    $long_where //= $where;
    my $grammar = $Parse::Marpa::Internal::This::grammar;
    my $code_lines = 30;
    $code_lines = $grammar->[ Parse::Marpa::Internal::Grammar::CODE_LINES ]
        if defined $grammar;
    my @msg;
    if (defined $code and defined $$code and $code_lines) {
        my $position = 0;
        if ($code_lines >= 0) {
            LINE: for (my $line = $code_lines; $line > 0; $line--) {
                $position = index($$code, "\n", $position);
                last LINE if $position < 0;
                $position++;
            }
        } else {
            $position = -1;
        }
        my $code_piece = $code;
        if ($position > 0) {
            $code_piece = \(
                substr($$code, 0, $position+1)
                . "[ Code truncated after $code_lines lines ]"
            )
        }
        push(@msg,
            "Problems in "
            . $long_where
            . ", code:\n"
            . $$code_piece
            . "\n"
        );
    }
    my $warnings_count = @$warnings;
    if ($warnings_count) {
        push(@msg, "Warnings ($warnings_count) in $where:\n", @$warnings);
        unless ($fatal_error) {
            $fatal_error = "Marpa will not continue due to warnings";
        }
    }
    push(@msg,
        "Fatal problem in $long_where\n",
        $fatal_error,
    );
    croak(@msg);
}

package Parse::Marpa::Internal::Source_Eval;

sub Parse::Marpa::Internal::raw_grammar_eval {
     my $grammar = shift;
     my $raw_grammar = shift;

     my ($trace_fh, $trace_strings, $trace_predefineds);
     if ($grammar-> [ Parse::Marpa::Internal::Grammar::TRACING  ]) {
         $trace_fh
             = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
         $trace_strings
             = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_STRINGS ];
         $trace_predefineds
             = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_PREDEFINEDS ];
     }

     my $new_start_symbol;
     my $new_semantics;
     my $new_version;
     my $new_preamble;
     my $new_default_lex_prefix;
     my $new_default_action;
     my $new_default_null_value;
     my $new_rules;
     my $new_terminals;
     my %strings;

     {
         my @warnings;
         local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
         eval $$raw_grammar;
         my $fatal_error = $@;
         if ($fatal_error or @warnings) {
              Parse::Marpa::Internal::die_on_problems(
                  $fatal_error,
                  \@warnings,
                  "evaluating gramar",
                  "evaluating gramar",
                  $raw_grammar,
              );
         }
     }

     if ($trace_strings) {
         for my $string (keys %strings) {
             say $trace_fh qq{String "$string" set to '}, $strings{$string}, q{'};
         }
     }

     if (defined $new_start_symbol) {
         $grammar->[ Parse::Marpa::Internal::Grammar::START ] = $new_start_symbol;
         say $trace_fh "Start symbol set to ", $new_start_symbol
             if $trace_predefineds;
     }

     Carp::croak("Semantics must be set to perl5 in marpa grammar")
         if not defined $new_semantics or $new_semantics ne "perl5"; 
     $grammar->[ Parse::Marpa::Internal::Grammar::SEMANTICS ] = $new_semantics;
     say $trace_fh "Semantics set to ", $new_semantics
         if $trace_predefineds;

     Carp::croak("Version must be set in marpa grammar")
         if not defined $new_version;
     Carp::croak(
         "Version in marpa grammar does not match: $new_version vs. ",
         $Parse::Marpa::VERSION
     ) if $new_version != $Parse::Marpa::VERSION;
     $grammar->[ Parse::Marpa::Internal::Grammar::VERSION ] = $new_version;
     say $trace_fh "Version set to ", $new_version
          if $trace_predefineds;

     if (defined $new_preamble) {
         $grammar->[ Parse::Marpa::Internal::Grammar::PREAMBLE ] = $new_preamble;
         say $trace_fh "Preamble set to '", $new_preamble, q{'}
             if defined $trace_predefineds;
     }

     if (defined $new_default_lex_prefix) {
         $grammar->[ Parse::Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX ] = $new_default_lex_prefix;
         say $trace_fh "Default lex prefix set to '", $new_default_lex_prefix, q{'}
             if defined $trace_predefineds;
     }

     if (defined $new_default_action) {
         $grammar->[ Parse::Marpa::Internal::Grammar::DEFAULT_ACTION ] = $new_default_action;
         say $trace_fh "Default action set to '", $new_default_action, q{'}
             if $trace_predefineds;
     }

     if (defined $new_default_null_value) {
         $grammar->[ Parse::Marpa::Internal::Grammar::DEFAULT_NULL_VALUE ] = $new_default_null_value;
         say $trace_fh "Default null_value set to '", $new_default_null_value, q{'}
             if $trace_predefineds;
     }

     Parse::Marpa::Internal::add_user_rules($grammar, $new_rules);
     Parse::Marpa::Internal::add_user_terminals($grammar, $new_terminals);

     $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
         Parse::Marpa::Internal::Grammar::PERL_RULES;

}

package Parse::Marpa::Internal;

sub Parse::Marpa::marpa {
    my $grammar = shift;
    my $text = shift;
    my $options = shift;

    my $ref = ref $grammar;
    croak(qq{grammar arg to marpa() was ref type "$ref", must be string ref})
        unless $ref eq "SCALAR";

    $ref = ref $text;
    croak(qq{text arg to marpa() was ref type "$ref", must be string ref})
        unless $ref eq "SCALAR";

    $options //= {};
    $ref = ref $options;
    croak(qq{text arg to marpa() was ref type "$ref", must be hash ref})
        unless $ref eq "HASH";

    my $g = new Parse::Marpa(
        source => $grammar,
        %{$options}
    );
    my $parse = new Parse::Marpa::Parse($g);
    $parse->text($text);

    $parse->initial();
    my @values;
    push(@values, $parse->value());
    return $values[0] unless wantarray;
    push(@values, $parse->value()) while $parse->next;
    @values;
}

sub Parse::Marpa::new {
    my $class = shift;
    my %args  = @_;

    my $grammar = [];
    bless( $grammar, $class );
    local($Parse::Marpa::Internal::This::grammar) = $grammar;

    # set the defaults and the default defaults
    $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE]
        = *STDERR;
    state $grammar_number //= 0;
    $grammar->[Parse::Marpa::Internal::Grammar::ID] = $grammar_number++;

    # Note: this limits the number of grammar to the number of integers --
    # not likely to be a big problem.
    $grammar->[Parse::Marpa::Internal::Grammar::NAME] =
        sprintf( "Parse::Marpa::G_%x", $grammar_number );

    $grammar->[Parse::Marpa::Internal::Grammar::ACADEMIC]           = 0;
    $grammar->[Parse::Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX] = "";
    $grammar->[Parse::Marpa::Internal::Grammar::DEFAULT_LEX_SUFFIX] = "";
    $grammar->[Parse::Marpa::Internal::Grammar::AMBIGUOUS_LEX]      = 1;
    $grammar->[Parse::Marpa::Internal::Grammar::TRACE_RULES]        = 0;
    $grammar->[Parse::Marpa::Internal::Grammar::LOCATION_CALLBACK] =
        q{ "Earleme " . $earleme };
    $grammar->[Parse::Marpa::Internal::Grammar::VOLATILE] = 1;
    $grammar->[Parse::Marpa::Internal::Grammar::WARNINGS] = 1;
    $grammar->[Parse::Marpa::Internal::Grammar::CODE_LINES] = 30;
    $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
        Parse::Marpa::Internal::Grammar::NEW;
    $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS]      = [];
    $grammar->[Parse::Marpa::Internal::Grammar::SYMBOL_HASH]  = {};
    $grammar->[Parse::Marpa::Internal::Grammar::RULES]        = [];
    $grammar->[Parse::Marpa::Internal::Grammar::RULE_HASH]    = {};
    $grammar->[Parse::Marpa::Internal::Grammar::SDFA_BY_NAME] = {};

    $grammar->set(%args);
}

our $compiled_source_grammar;

# some day use to make locator more efficient on repeated calls
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

sub die_with_parse_failure {
    my $source = shift;
    my $earleme = shift;

    # print $parse->show_status();

    # for the editors, line numbering starts at 1
    # do something about this?
    my ($line, $line_start) = locator($earleme, $source);
    my @msg = ("Parse failed at line ", $line+1, ", earleme $earleme\n");
    given (index($$source, "\n", $line_start)) {
        when (undef) { push(@msg, substr($$source, $line_start), "\n") }
        default { push(@msg, substr($$source, $line_start, $_-$line_start), "\n") }
    }
    push(@msg, (" " x ($earleme-$line_start)), "^\n");
    croak(@msg);
}


# First arg is the current grammar, that is, the one being
# built.
# Second arg is ref to string containing Marpa source
sub source_grammar {
    my $grammar = shift;
    my $source  = shift;
    my $source_grammar = Parse::Marpa::decompile($Parse::Marpa::Internal::compiled_source_grammar);
    my $grammar_version = $source_grammar->[ Parse::Marpa::Internal::Grammar::VERSION ];
    no integer;
    if ($VERSION != $grammar_version) {
        croak("Version mismatch between Marpa ($VERSION) and its source grammar ($grammar_version)");
    }
    use integer;
    my $parse = new Parse::Marpa::Parse(
        grammar => $source_grammar,
    );

    # print STDERR $source_grammar->show_rules();
    # print STDERR $source_grammar->show_SDFA();
 
    my $failed_at_earleme = $parse->text($source);
    if ($failed_at_earleme >= 0) {
        die_with_parse_failure($source, $failed_at_earleme);
    }
    $parse->initial();
    my $value = $parse->value();
    raw_grammar_eval($grammar, $value);
    # croak("sourcing grammars not yet implemented");
}

sub Parse::Marpa::set {
    my $grammar = shift;
    my %args    = @_;

    local ($Parse::Marpa::Internal::This::grammar) = $grammar;
    my $tracing = $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }

    my $precomputed = 1;
    my $state = $grammar->[Parse::Marpa::Internal::Grammar::STATE];
    given ($state) {
        when (Parse::Marpa::Internal::Grammar::NEW)          {$precomputed = 0}
        when (Parse::Marpa::Internal::Grammar::PERL_RULES)   {$precomputed = 0}
        when (Parse::Marpa::Internal::Grammar::SOURCE_RULES) {$precomputed = 0}
    }

    # value of source needs to be a *REF* to a string
    my $source = $args{"source"};
    if ( defined $source ) {
        croak("Cannot source grammar with some rules already defined")
            if $state ne Parse::Marpa::Internal::Grammar::NEW;
        source_grammar( $grammar, $source );
    }

    while ( my ( $option, $value ) = each %args ) {
        given ($option) {
            when ("source") {;}    # already dealt with
            when ("rules") {
                croak("Perl rules not allowed with sourced grammar")
                    if $state eq
                        Parse::Marpa::Internal::Grammar::SOURCE_RULES;
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                add_user_rules( $grammar, $value );
                $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
                    Parse::Marpa::Internal::Grammar::PERL_RULES;
            }
            when ("terminals") {
                croak("Perl terminals not allowed with sourced grammar")
                    if $state eq
                        Parse::Marpa::Internal::Grammar::SOURCE_RULES;
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                add_user_terminals( $grammar, $value );
                $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
                    Parse::Marpa::Internal::Grammar::PERL_RULES;
            }
            when ("start") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar->[Parse::Marpa::Internal::Grammar::START] = $value;
                $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
                    Parse::Marpa::Internal::Grammar::PERL_RULES;
            }
            when ("academic") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar->[Parse::Marpa::Internal::Grammar::ACADEMIC] =
                    $value;
            }
            when ("default_null_value") {
                $grammar
                    ->[Parse::Marpa::Internal::Grammar::DEFAULT_NULL_VALUE] =
                    $value;
            }
            when ("default_action") {
                $grammar->[Parse::Marpa::Internal::Grammar::DEFAULT_ACTION] =
                    $value;
            }
            when ("default_lex_prefix") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar
                    ->[Parse::Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX] =
                    $value;
            }
            when ("default_lex_suffix") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar
                    ->[Parse::Marpa::Internal::Grammar::DEFAULT_LEX_SUFFIX] =
                    $value;
            }
            when ("ambiguous_lex") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar->[Parse::Marpa::Internal::Grammar::AMBIGUOUS_LEX] =
                    $value;
            }
            when ("trace_file_handle") {
                $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE]
                    = $value;
            }
            when ("trace_actions") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_ACTIONS ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ] = 1;
            }
            when ("trace_lex") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_LEX_TRIES ]
                    = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_LEX_MATCHES ]
                    = $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ] = 1;
            }
            when ("trace_lex_tries") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_LEX_TRIES ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ] = 1;
            }
            when ("trace_lex_matches") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_LEX_MATCHES ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ] = 1;
            }
            when ("trace_values") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_VALUES ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ] = 1;
            }
            when ("trace_rules") {
                $grammar->[Parse::Marpa::Internal::Grammar::TRACE_RULES] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING  ] = 1;
            }
            when ("trace_strings") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_STRINGS ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING  ] = 1;
            }
            when ("trace_predefineds") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_PREDEFINEDS ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING  ] = 1;
            }
            when ("trace_evaluation_choices") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_EVALUATION_CHOICES ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING  ] = 1;
            }
            when ("trace_priorities") {
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_PRIORITIES ] =
                    $value;
                $grammar->[ Parse::Marpa::Internal::Grammar::TRACING  ] = 1;
            }
            when ("location_callback") {
                croak("location callback not yet implemented");
            }
            when ("volatile") {
                $grammar->[Parse::Marpa::Internal::Grammar::VOLATILE] =
                    $value;
            }
            when ("warnings") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar->[Parse::Marpa::Internal::Grammar::WARNINGS] =
                    $value;
            }
            when ("code_lines") {
                $grammar->[Parse::Marpa::Internal::Grammar::CODE_LINES] =
                    $value;
            }
            when ("version") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar->[ Parse::Marpa::Internal::Grammar::VERSION ] =
                    $value;
            }
            when ("semantics") {
                croak("$option option not allowed after grammar is precomputed")
                    if $precomputed;
                $grammar->[ Parse::Marpa::Internal::Grammar::SEMANTICS ] =
                    $value;
            }
            when ("preamble") {
                $grammar->[Parse::Marpa::Internal::Grammar::PREAMBLE] =
                    $value;
            }
            default {
                croak("$_ is not an available Marpa option");
            }
        }
    }

    $grammar;
}

=begin Implementation:

In order to automatically ELIMINATE inaccessible and unproductive
productions from a grammar, you have to first eliminate the
unproductive productions, THEN the inaccessible ones.  I don't do
this in the below.

The reason is my purposes are primarily diagnostic.  The difference
shows in the case of an unproductive start symbol.  Following the
correct procedure for automatically cleaning the grammar, I would
have to regard the start symbol and its productions as eliminated
and therefore go on to report every other production and symbol as
inaccessible.  Almost certainly all these inaccessiblity reports,
while theoretically correct, are irrelevant, since the user will
probably respond by making the start symbol productive, and the
extra "information" would only get in the way.

The downside is that in a few uncommon cases, a user relying entirely
on the Marpa warnings to clean up his grammar will have to go through
more than a single pass of the diagnostics.  I think even those
users unlucky enough to hit upon such cases may prefer simpler
diagnostics, and in any case, simpler diagnostics are clearly best
for the most common problems.

=end Implementation:

=cut

# returns undef if there was a problem
sub Parse::Marpa::precompute {
    my $grammar = shift;

    my $tracing = $grammar->[Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }

    my $state = $grammar->[ Parse::Marpa::Internal::Grammar::STATE ];
    given ($state) {
        when (Parse::Marpa::Internal::Grammar::PERL_RULES) { ; }
        when (Parse::Marpa::Internal::Grammar::SOURCE_RULES) { ; }
        default {
            croak(
                "Attempt to precompute grammar in inappropriate state\nAttempt to precompute ",
                $state
            );
        }
    }

    nulling($grammar);
    nullable($grammar) or return $grammar;
    productive($grammar);

    my $start = $grammar->[Parse::Marpa::Internal::Grammar::START];
    croak("No start symbol specified") unless defined $start;

    set_start( $grammar, $start ) or return $grammar;

    accessible($grammar);
    if ( $grammar->[Parse::Marpa::Internal::Grammar::ACADEMIC] ) {
        setup_academic_grammar($grammar);
    }
    else {
        rewrite_as_CHAF($grammar);
    }
    create_NFA($grammar);
    create_SDFA($grammar);
    if ( $grammar->[Parse::Marpa::Internal::Grammar::WARNINGS] ) {
        my $trace_fh =
            $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        for my $symbol ( @{ Parse::Marpa::inaccessible_symbols($grammar) } ) {
            say $trace_fh "Inaccessible symbol: $symbol";
        }
        for my $symbol ( @{ Parse::Marpa::unproductive_symbols($grammar) } ) {
            say $trace_fh "Unproductive symbol: $symbol";
        }
    }

    $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
        Parse::Marpa::Internal::Grammar::PRECOMPUTED;
    $grammar;
}

sub Parse::Marpa::show_problems {
    my $grammar = shift;

    my $problems = $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS];
    "Grammar has these problems:\n"
        . join("\n", @$problems)
        . "\n"
}

# Deep Copy Grammar
#
# Could this be made more efficient with a custom routine?
# Maybe not, Data::Dumper uses XS
# Note that the copying strengthens weak refs
sub Parse::Marpa::compile {
    my $grammar = shift;

    my $tracing = $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }

    my $problems = $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS];
    if ($problems) {
        croak(
            show_problems($grammar),
            "Attempt to compile grammar with fatal problems\n",
            "Marpa cannot proceed"
        );
    }

    my $state = $grammar->[ Parse::Marpa::Internal::Grammar::STATE ];
    given ($state) {
        when (Parse::Marpa::Internal::Grammar::PERL_RULES) { Parse::Marpa::precompute($grammar); }
        when (Parse::Marpa::Internal::Grammar::SOURCE_RULES) { Parse::Marpa::precompute($grammar); }
        when (Parse::Marpa::Internal::Grammar::PRECOMPUTED) { ; }
        default {
            croak(
                "Attempt to compile grammar in inappropriate state\nAttempt to compile ",
                $state
            );
        }
    }
    my $d = Data::Dumper->new( [$grammar], ["grammar"] );
    $d->Purity(1);
    $d->Indent(1);
    # returns a ref -- dumps can be long
    return \($d->Dump());
}

sub Parse::Marpa::decompile {
    # doesn't set trace file handle -- no subroutines called
    # uses a ref -- grammars can be long
    my $compiled_grammar = shift;
    my $grammar;
    {
        my @warnings;
        local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
        eval $$compiled_grammar;
        my $fatal_error = $@;
        if ($fatal_error or @warnings) {
            die_on_problems(
                $fatal_error,
                \@warnings,
                "decompiling gramar",
                "decompiling gramar",
                $compiled_grammar,
            );
        }
    }

    # Eliminate or weaken all circular references
    my $symbol_hash =
        $grammar->[Parse::Marpa::Internal::Grammar::SYMBOL_HASH];
    while ( my ( $name, $ref ) = each %{$symbol_hash} ) {
        weaken( $symbol_hash->{$name} = $ref );
    }

    # these were weak references, but aren't used anyway, so
    # free up the memory
    for my $symbol (
        @{ $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS] } )
    {
        $symbol->[Parse::Marpa::Internal::Symbol::LHS] = undef;
        $symbol->[Parse::Marpa::Internal::Symbol::RHS] = undef;
    }
    $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
        Parse::Marpa::Internal::Grammar::COMPILED;
    $grammar;

}

sub Parse::Marpa::show_symbol {
    my $symbol = shift;
    my $text   = "";
    $text .= sprintf "%d: %s, lhs=[%s], rhs=[%s]",
        $symbol->[Parse::Marpa::Internal::Symbol::ID],
        $symbol->[Parse::Marpa::Internal::Symbol::NAME],
        join( " ",
        map { $_->[Parse::Marpa::Internal::Rule::ID] }
            @{ $symbol->[Parse::Marpa::Internal::Symbol::LHS] } ),
        join( " ",
        map { $_->[Parse::Marpa::Internal::Rule::ID] }
            @{ $symbol->[Parse::Marpa::Internal::Symbol::RHS] } );
    if ( not $symbol->[Parse::Marpa::Internal::Symbol::PRODUCTIVE] ) {
        $text .= " unproductive";
    }
    if ( not $symbol->[Parse::Marpa::Internal::Symbol::ACCESSIBLE] ) {
        $text .= " inaccessible";
    }
    if ( $symbol->[Parse::Marpa::Internal::Symbol::NULLABLE] ) {
        $text .= " nullable";
    }
    if ( $symbol->[Parse::Marpa::Internal::Symbol::NULLING] ) {
        $text .= " nulling";
    }
    if ( $symbol->[Parse::Marpa::Internal::Symbol::TERMINAL] ) {
        $text .= " terminal";
    }
    $text .= "\n";
}

sub Parse::Marpa::show_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS];
    my $text    = "";
    for my $symbol_ref (@$symbols) {
        $text .= Parse::Marpa::show_symbol($symbol_ref);
    }
    $text;
}

sub Parse::Marpa::show_nulling_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS];
    join( " ",
        sort map { $_->[Parse::Marpa::Internal::Symbol::NAME] }
            grep { $_->[Parse::Marpa::Internal::Symbol::NULLING] }
            @$symbols );
}

sub Parse::Marpa::show_nullable_symbols {
    my $grammar = shift;
    my $symbols =
        $grammar->[Parse::Marpa::Internal::Grammar::NULLABLE_SYMBOL];
    join( " ",
        sort map { $_->[Parse::Marpa::Internal::Symbol::NAME] } @$symbols );
}

sub Parse::Marpa::show_productive_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS];
    join( " ",
        sort map { $_->[Parse::Marpa::Internal::Symbol::NAME] }
            grep { $_->[Parse::Marpa::Internal::Symbol::PRODUCTIVE] }
            @$symbols );
}

sub Parse::Marpa::show_accessible_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS];
    join( " ",
        sort map { $_->[Parse::Marpa::Internal::Symbol::NAME] }
            grep { $_->[Parse::Marpa::Internal::Symbol::ACCESSIBLE] }
            @$symbols );
}

sub Parse::Marpa::inaccessible_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS];
    [   sort map { $_->[Parse::Marpa::Internal::Symbol::NAME] }
            grep { !$_->[Parse::Marpa::Internal::Symbol::ACCESSIBLE] }
            @$symbols
    ];
}

sub Parse::Marpa::unproductive_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Internal::Grammar::SYMBOLS];
    [   sort map { $_->[Parse::Marpa::Internal::Symbol::NAME] }
            grep { !$_->[Parse::Marpa::Internal::Symbol::PRODUCTIVE] }
            @$symbols
    ];
}

sub Parse::Marpa::brief_rule {
    my $rule = shift;
    my ( $lhs, $rhs, $rule_id ) = @{$rule}[
        Parse::Marpa::Internal::Rule::LHS,
        Parse::Marpa::Internal::Rule::RHS,
        Parse::Marpa::Internal::Rule::ID
    ];
    my $text .= $rule_id . ": "
        . $lhs->[Parse::Marpa::Internal::Symbol::NAME] . " ->";
    if (@$rhs) {
        $text .= " "
            . join( " ",
            map { $_->[Parse::Marpa::Internal::Symbol::NAME] } @$rhs );
    }
    $text;
}

sub Parse::Marpa::brief_original_rule {
    my $rule          = shift;
    my $original_rule = $rule->[Parse::Marpa::Internal::Rule::ORIGINAL_RULE]
        // $rule;
    Parse::Marpa::brief_rule($original_rule);
}

sub Parse::Marpa::show_rule {
    my $rule = shift;

    my ( $rhs, $productive, $accessible, $nullable, $nulling,
        $useful, $priority, )
        = @{$rule}[
        Parse::Marpa::Internal::Rule::RHS,
        Parse::Marpa::Internal::Rule::PRODUCTIVE,
        Parse::Marpa::Internal::Rule::ACCESSIBLE,
        Parse::Marpa::Internal::Rule::NULLABLE,
        Parse::Marpa::Internal::Rule::NULLING,
        Parse::Marpa::Internal::Rule::USEFUL,
        Parse::Marpa::Internal::Rule::PRIORITY,
        ];
    my $text    = Parse::Marpa::brief_rule($rule);
    my @comment = ();

    if ( not(@$rhs) )           { push( @comment, "empty" ); }
    if ( not $productive ) { push( @comment, "unproductive" ); }
    if ( not $accessible ) { push( @comment, "inaccessible" ); }
    if ($nullable)              { push( @comment, "nullable" ); }
    if ($nulling)               { push( @comment, "nulling" ); }
    if ( not $useful )          { push( @comment, "!useful" ); }
    if ($priority)              { push( @comment, "priority=$priority" ); }
    if (@comment) {
        $text .= " " . join( " ", "/*", @comment, "*/" );
    }
    $text .= "\n";
}

sub Parse::Marpa::show_rules {
    my $grammar = shift;
    my $rules   = $grammar->[Parse::Marpa::Internal::Grammar::RULES];
    my $text;

    for my $rule (@$rules) {
        $text .= Parse::Marpa::show_rule($rule);
    }
    $text;
}

sub Parse::Marpa::show_item {
    my $item = shift;
    my $text = "";
    if ( not defined $item ) {
        $text .= "/* empty */";
    }
    else {
        my ( $rule, $position ) = @{$item}[
            Parse::Marpa::Internal::LR0_item::RULE,
            Parse::Marpa::Internal::LR0_item::POSITION
        ];
        my @names =
            ( $rule->[Parse::Marpa::Internal::Rule::LHS]
                ->[Parse::Marpa::Internal::Symbol::NAME] );
        push( @names,
            map { $_->[Parse::Marpa::Internal::Symbol::NAME] }
                @{ $rule->[Parse::Marpa::Internal::Rule::RHS] } );
        splice( @names, $position + 1, 0, "." );
        splice( @names, 1, 0, "::=" );
        $text .= join( " ", @names );
    }
    $text;
}

sub Parse::Marpa::show_NFA_state {
    my $state = shift;
    my ( $name, $item, $transition ) = @{$state}[
        Parse::Marpa::Internal::NFA::NAME,
        Parse::Marpa::Internal::NFA::ITEM,
        Parse::Marpa::Internal::NFA::TRANSITION
    ];
    my $text .= $name . ": " . Parse::Marpa::show_item($item) . "\n";
    for my $symbol_name ( sort keys %$transition ) {
        my $transition_states = $transition->{$symbol_name};
        $text
            .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => "
            . join( " ",
            map { $_->[Parse::Marpa::Internal::NFA::NAME] }
                @$transition_states )
            . "\n";
    }
    $text;
}

sub Parse::Marpa::show_NFA {
    my $grammar = shift;
    my $text    = "";
    my $NFA     = $grammar->[Parse::Marpa::Internal::Grammar::NFA];
    for my $state (@$NFA) {
        $text .= Parse::Marpa::show_NFA_state($state);
    }
    $text;
}

sub Parse::Marpa::show_SDFA_state {
    my $state = shift;
    my $tags  = shift;

    my $text = "";
    my ( $id, $name, $NFA_states, $transition, $tag, $lexables, ) = @{$state}[
        Parse::Marpa::Internal::SDFA::ID,
        Parse::Marpa::Internal::SDFA::NAME,
        Parse::Marpa::Internal::SDFA::NFA_STATES,
        Parse::Marpa::Internal::SDFA::TRANSITION,
        Parse::Marpa::Internal::SDFA::TAG,
    ];

    $text .= defined $tags ? "St" . $tag : "S" . $id;
    $text .= ": " . $name . "\n";
    for my $NFA_state (@$NFA_states) {
        my $item = $NFA_state->[Parse::Marpa::Internal::NFA::ITEM];
        $text .= Parse::Marpa::show_item($item) . "\n";
    }

    for my $symbol_name ( sort keys %$transition ) {
        my ( $to_id, $to_name ) = @{ $transition->{$symbol_name} }[
            Parse::Marpa::Internal::SDFA::ID,
            Parse::Marpa::Internal::SDFA::NAME
        ];
        $text
            .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => "
            . ( defined $tags ? "St" . $tags->[$to_id] : "S" . $to_id ) . " ("
            . $to_name . ")\n";
    }
    $text;
}

sub tag_SDFA {
    my $grammar = shift;
    my $SDFA    = $grammar->[Parse::Marpa::Internal::Grammar::SDFA];
    return if defined $SDFA->[0]->[Parse::Marpa::Internal::SDFA::TAG];
    my $tag = 0;
    for my $state (
        sort {
            $a->[Parse::Marpa::Internal::SDFA::NAME]
                cmp $b->[Parse::Marpa::Internal::SDFA::NAME]
        } @$SDFA
        )
    {
        $state->[Parse::Marpa::Internal::SDFA::TAG] = $tag++;
    }
}

sub Parse::Marpa::show_SDFA {
    my $grammar = shift;
    my $text    = "";
    my $SDFA    = $grammar->[Parse::Marpa::Internal::Grammar::SDFA];
    for my $state (@$SDFA) { $text .= Parse::Marpa::show_SDFA_state($state); }
    $text;
}

sub Parse::Marpa::show_ii_SDFA {
    my $grammar = shift;
    my $text    = "";
    my $SDFA    = $grammar->[Parse::Marpa::Internal::Grammar::SDFA];
    my $tags;
    tag_SDFA($grammar);

    for my $state (@$SDFA) {
        $tags->[ $state->[Parse::Marpa::Internal::SDFA::ID] ] =
            $state->[Parse::Marpa::Internal::SDFA::TAG];
    }
    for my $state (
        map  { $_->[0] }
        sort { $a->[1] <=> $b->[1] }
        map  { [ $_, $_->[Parse::Marpa::Internal::SDFA::TAG] ] } @$SDFA
        )
    {
        $text .= Parse::Marpa::show_SDFA_state( $state, $tags );
    }
    $text;
}

sub Parse::Marpa::get_symbol {
    my $grammar = shift;
    my $symbol_name = shift;
    Parse::Marpa::get_canonical_symbol(
        $grammar,
        Parse::Marpa::Source::canonical_symbol_name($symbol_name)
    );
}

sub Parse::Marpa::get_canonical_symbol {
    my $grammar = shift;
    my $name    = shift;
    my $symbol_hash =
        $grammar->[Parse::Marpa::Internal::Grammar::SYMBOL_HASH];
    defined $symbol_hash ? $symbol_hash->{$name} : undef;
}

=begin Implementation

Symbol keys are names, with internal symbols beginning with a
underscore.  For the most part we use the raw names, but we need
to avoid conflict between internal names and user defined names.
To do this, we prepend another underscore to the name of any user
symbol which begins with an underscore.

Therefore, all keys not ending in an underscore, or ending with "__"
are available for user-defined symbols.  All those symbols ending with
one underscore, but not two, are reserved for internal uses

=end Implementation

=cut

sub canonical_name {
    my $name = shift;
    $name =~ /]$/ ? $name . "_" : $name;
}

sub add_terminal {
    my $grammar  = shift;
    my $name     = shift;
    my $lexer    = shift;
    my $priority = shift;
    my ( $regex, $prefix, $suffix );
    my $action;

    given ( ref $lexer ) {
        when ("") { $action = $lexer; }
        when ("ARRAY") { ( $regex, $prefix, $suffix ) = @$lexer; }
        default { croak("Bad argument to add_terminal for $name"); };
    }

    $priority //= 0;

    my ( $symbol_hash, $symbols, $default_null_value ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::SYMBOL_HASH,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::DEFAULT_NULL_VALUE,
    ];

    # I allow redefinition of a LHS symbol as a terminal
    # I need to test that this works, or disallow it
    my $symbol = $symbol_hash->{$name};
    if ( defined $symbol ) {

        if ( $symbol->[Parse::Marpa::Internal::Symbol::TERMINAL] ) {
            croak("Attempt to add terminal twice: $name");
        }

        @{$symbol}[
            Parse::Marpa::Internal::Symbol::PRODUCTIVE,
            Parse::Marpa::Internal::Symbol::NULLING,
            Parse::Marpa::Internal::Symbol::REGEX,
            Parse::Marpa::Internal::Symbol::PREFIX,
            Parse::Marpa::Internal::Symbol::SUFFIX,
            Parse::Marpa::Internal::Symbol::ACTION,
            Parse::Marpa::Internal::Symbol::TERMINAL,
            Parse::Marpa::Internal::Symbol::PRIORITY,
            ]
            = ( 1, 0, $regex, $prefix, $suffix, $action, 1, $priority, );

        return;
    }

    my $symbol_count = @$symbols;
    my $new_symbol   = [];
    @{$new_symbol}[
        Parse::Marpa::Internal::Symbol::ID,
        Parse::Marpa::Internal::Symbol::NAME,
        Parse::Marpa::Internal::Symbol::LHS,
        Parse::Marpa::Internal::Symbol::RHS,
        Parse::Marpa::Internal::Symbol::NULLABLE,
        Parse::Marpa::Internal::Symbol::PRODUCTIVE,
        Parse::Marpa::Internal::Symbol::NULLING,
        Parse::Marpa::Internal::Symbol::NULL_VALUE,
        Parse::Marpa::Internal::Symbol::REGEX,
        Parse::Marpa::Internal::Symbol::ACTION,
        Parse::Marpa::Internal::Symbol::TERMINAL,
        Parse::Marpa::Internal::Symbol::PRIORITY,
        ]
        = (
        $symbol_count, $name, [], [], 0, 1, 0, $default_null_value, $regex,
        $action, 1, $priority,
        );

    push( @$symbols, $new_symbol );
    weaken( $symbol_hash->{$name} = $new_symbol );
}

sub assign_symbol {
    my $grammar = shift;
    my $name    = shift;
    my ( $symbol_hash, $symbols, $default_null_value, ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::SYMBOL_HASH,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::DEFAULT_NULL_VALUE,
    ];

    my $symbol_count = @$symbols;
    my $symbol       = $symbol_hash->{$name};
    if ( not defined $symbol ) {
        @{$symbol}[
            Parse::Marpa::Internal::Symbol::ID,
            Parse::Marpa::Internal::Symbol::NAME,
            Parse::Marpa::Internal::Symbol::LHS,
            Parse::Marpa::Internal::Symbol::RHS,
            Parse::Marpa::Internal::Symbol::NULL_VALUE,
            ]
            = ( $symbol_count, $name, [], [], $default_null_value, );
        push( @$symbols, $symbol );
        weaken( $symbol_hash->{$name} = $symbol );
    }
    $symbol;
}

sub assign_user_symbol {
    my $self = shift;
    my $name = shift;
    assign_symbol( $self, canonical_name($name) );
}

sub add_user_rule {
    my $grammar   = shift;
    my $lhs_name  = shift;
    my $rhs_names = shift;
    my $action    = shift;
    my $priority  = shift;

    my ($rule_hash) = @{$grammar}[Parse::Marpa::Internal::Grammar::RULE_HASH];

    my $lhs_symbol = assign_symbol( $grammar, canonical_name($lhs_name) );
    $rhs_names //= [];
    my $rhs_symbols =
        [ map { assign_symbol( $grammar, canonical_name($_) ); }
            @$rhs_names ];

    # Don't allow the user to duplicate a rule
    my $rule_key = join( ",",
        map { $_->[Parse::Marpa::Internal::Symbol::ID] }
            ( $lhs_symbol, @$rhs_symbols ) );
    croak( "Duplicate rule: ", $lhs_name, " -> ", join( " ", @$rhs_names ) )
        if exists $rule_hash->{$rule_key};

    $rule_hash->{$rule_key} = 1;

    add_rule( $grammar, $lhs_symbol, $rhs_symbols, $action, $priority );
}

sub add_rule {
    my $grammar  = shift;
    my $lhs      = shift;
    my $rhs      = shift;
    my $action   = shift;
    my $priority = shift;

    my ( $rules, $package, $trace_rules, $trace_fh, ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::RULES,
        Parse::Marpa::Internal::Grammar::NAME,
        Parse::Marpa::Internal::Grammar::TRACE_RULES,
        Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE,
    ];

    my $rule_count = @$rules;
    my $new_rule   = [];
    my $nulling    = @$rhs ? undef : 1;
    $priority //= 0;

    @{$new_rule}[
        Parse::Marpa::Internal::Rule::ID,
        Parse::Marpa::Internal::Rule::NAME,
        Parse::Marpa::Internal::Rule::LHS,
        Parse::Marpa::Internal::Rule::RHS,
        Parse::Marpa::Internal::Rule::NULLABLE,
        Parse::Marpa::Internal::Rule::PRODUCTIVE,
        Parse::Marpa::Internal::Rule::NULLING,
        Parse::Marpa::Internal::Rule::ACTION,

        # Parse::Marpa::Internal::Rule::ORDER,
        Parse::Marpa::Internal::Rule::PRIORITY,
        ]
        = (
        $rule_count, "rule $rule_count",
        $lhs,        $rhs,
        $nulling, $nulling, $nulling,
        $action,

        # $rule_count,
        $priority,
        );

    # if this is a nulling rule with an action,
    # we get the null_value of the lhs from that
    if ( $nulling and $action ) {
        local ($Parse::Marpa::This::v) = [];
        if ( defined $action ) {
            my @warnings;
            local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
            $lhs->[Parse::Marpa::Internal::Symbol::NULL_VALUE] =
                eval( "package " . $package . ";\n" . $action );
            my $fatal_error = $@;
            if ($fatal_error or @warnings) {
                die_on_problems($fatal_error, \@warnings,
                    "evaluating null value",
                    "evaluating null value for "
                        . $lhs->[Parse::Marpa::Internal::Symbol::NAME],
                    \$action
                );
            }
        }
    }

    push( @$rules, $new_rule );
    {
        my $lhs_rules = $lhs->[Parse::Marpa::Internal::Symbol::LHS];
        weaken( $lhs_rules->[ scalar @$lhs_rules ] = $new_rule );
    }
    if ($nulling) {
        @{$lhs}[
            Parse::Marpa::Internal::Symbol::NULLABLE,
            Parse::Marpa::Internal::Symbol::PRODUCTIVE
            ]
            = ( 1, 1 );
    }
    else {
        my $last_symbol = [];
        SYMBOL: for my $symbol ( sort @$rhs ) {
            next SYMBOL if $symbol == $last_symbol;
            my $rhs_rules = $symbol->[Parse::Marpa::Internal::Symbol::RHS];
            weaken( $rhs_rules->[ scalar @$rhs_rules ] = $new_rule );
            $last_symbol = $symbol;
        }
    }
    if ($trace_rules) {
        print $trace_fh "Added rule #", $#$rules, ": ",
            $lhs->[Parse::Marpa::Internal::Symbol::NAME], " -> ",
            join( " ",
            map { $_->[Parse::Marpa::Internal::Symbol::NAME] } @$rhs ),
            "\n";
    }
    $new_rule;
}

# add one or more rules
sub add_user_rules {
    my $grammar = shift;
    my $rules   = shift;

    RULE: for my $rule (@$rules) {

        given ( ref $rule ) {
            when ("ARRAY") {
                my $arg_count = @$rule;

                # This warning can be removed if this interface remains
                # internal
                if ( $arg_count > 4 or $arg_count < 1 ) {
                    croak(
                        "Rule has $arg_count arguments: "
                            . join( ", ",
                            map { defined $_ ? $_ : "undef" } @$rule )
                            . "\n"
                            . "Rule must have from 1 to 3 arguments"
                    );
                }
                my ( $lhs, $rhs, $action, $priority ) = @$rule;
                add_user_rule( $grammar, $lhs, $rhs, $action, $priority );

            }
            when ("HASH") {
                add_rules_from_hash( $grammar, $rule );
            }
            default {
                croak( "Invalid rule reftype ", ( $_ ? $_ : "undefined" ) );
            }
        }

    }    # RULE

}

sub add_rules_from_hash {
    my $grammar = shift;
    my $options = shift;

    my ( $lhs_name, $rhs_names, $action );
    my ( $min,      $max,       $separator_name );
    my $proper_separation = 0;
    my $keep_separation   = 0;
    my $left_associative  = 1;
    my $priority          = 0;

    while ( my ( $option, $value ) = each(%$options) ) {
        given ($option) {
            when ("rhs")               { $rhs_names         = $value }
            when ("lhs")               { $lhs_name          = $value }
            when ("action")            { $action            = $value }
            when ("min")               { $min               = $value }
            when ("max")               { $max               = $value }
            when ("separator")         { $separator_name    = $value }
            when ("proper_separation") { $proper_separation = $value }
            when ("keep_separation")   { $keep_separation   = $value }
            when ("left_associative")  { $left_associative  = $value }
            when ("right_associative") { $left_associative  = !$value }
            when ("priority")          { $priority          = $value }
            default { croak("Unknown option in counted rule: $option") };
        }
    }

    # Take care of nulling rules
    if ( scalar @$rhs_names == 0 ) {
        add_user_rule( $grammar, $lhs_name, $rhs_names, $action, $priority );
        return;
    }

    # Take of obviously bad min, max values
    if ( defined $max and $max <= 0 ) {
        croak("rule max count is $max, not greater than zero");
    }
    if ( defined $min and $min < 0 ) {
        croak("rule min count is $min, less than zero");
    }

    # Ensure min is correctly defined
    if ( not defined $min ) {
        given ($max) {
            when (undef) { $min = $max = 1; }
            default {
                croak("rule max count is defined ($max), but no rule minium");
            }
        }
    }

    # This is an ordinary, non-counted rule,
    # which we'll take care of first as a special case
    if ( defined $max and $max == 1 and $min == 1 ) {
        if ( $max <= 1 and defined $separator_name ) {
            croak("separator defined for rule without repetitions");
        }
        add_user_rule( $grammar, $lhs_name, $rhs_names, $action, $priority );
        return;
    }

    if ( defined $max ) {
        croak("rule max count ($max) count is less than minium ($min)")
            if $max < $min;
        croak("Too many symbols on rhs for counted rule")
            if scalar @$rhs_names != 1;
        my $rhs_name = pop @$rhs_names;

        # specifically counted rules
        my $new_rule;
        for my $count ( $min .. $max ) {
            my $proper_counted_rhs;
            my $separator_terminated_rhs;
            my @separated_rhs = ($rhs_name);
            push( @separated_rhs, $separator_name )
                if defined $separator_name;
            given ($count) {
                when (0) { $proper_counted_rhs = [] }
                default {
                    $proper_counted_rhs =
                        [ (@separated_rhs) x ( $count - 1 ), $rhs_name ];
                    if ( not $proper_separation and defined $separator_name )
                    {
                        $separator_terminated_rhs =
                            [ (@separated_rhs) x ($count) ];
                    }
                }
            }

            # no change to @Parse::Marpa::This::v needed for action
            if ( defined $separator_name and not $keep_separation ) {
                $action = q{ $Parse::Marpa::This::v = [
                        @{$Parse::Marpa::This:v}[
                           grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                        ]
                    }
                    . $action;
            }
            $new_rule =
                add_user_rule( $grammar, $lhs_name, $proper_counted_rhs,
                $action, $priority );
            if ($separator_terminated_rhs) {
                add_user_rule( $grammar, $lhs_name, $separator_terminated_rhs,
                    $action, $priority );
            }
        }

        # There will be at least one rhs symbol since we take the last rule created
        # and max >= 1
        my $rhs = $new_rule->[Parse::Marpa::Internal::Rule::RHS]->[0];
        $rhs->[Parse::Marpa::Internal::Symbol::COUNTED] = 1;
        if ( defined $separator_name ) {
            my $separator =
                $new_rule->[Parse::Marpa::Internal::Rule::RHS]->[1];
            $separator->[Parse::Marpa::Internal::Symbol::COUNTED] = 1;
        }

        return;

    }    # min and max both defined

    # At this point we know that max is undefined, and that min must be

    # Right now we're doing this right associative.  Add option later to be
    # left associative?

    # nulling rule is special case
    if ( $min == 0 ) {
        my $rule_action;
        given ($action) {
            when (undef) { $rule_action = undef }
            default {
                $rule_action = q{ $Parse::Marpa::This::v = []; } . $action;
            }
        }
        add_user_rule( $grammar, $lhs_name, [], $rule_action, $priority );
        $min = 1;
    }

    croak("Only one rhs symbol allowed for counted rule")
        if scalar @$rhs_names != 1;

    # create the rhs symbol
    my $rhs_name           = pop @$rhs_names;
    my $canonical_rhs_name = canonical_name($rhs_name);
    my $rhs                = assign_symbol( $grammar, $canonical_rhs_name );
    $rhs->[Parse::Marpa::Internal::Symbol::COUNTED] = 1;

    # create the separator symbol, if we're using one
    my $separator;
    my $canonical_separator_name;
    if ( defined $separator_name ) {
        $canonical_separator_name = canonical_name($separator_name);
        $separator = assign_symbol( $grammar, $canonical_separator_name );
        $separator->[Parse::Marpa::Internal::Symbol::COUNTED] = 1;
    }

    # create the sequence symbol
    my $sequence_name = $canonical_rhs_name . "[Seq][$min-*]";
    $sequence_name .= "[Sep][" . $canonical_separator_name . "]"
        if defined $separator_name;
    my $sequence = assign_symbol( $grammar, $sequence_name );

    my $lhs = assign_symbol( $grammar, canonical_name($lhs_name) );

    # Don't allow the user to duplicate a rule
    # I'm pretty general here -- I consider a sequence rule a duplicate is rhs, lhs
    # and separator are the same.  I may want to get more fancy, but save that
    # for later.
    {
        my $rule_hash =
            $grammar->[Parse::Marpa::Internal::Grammar::RULE_HASH];
        my @key_rhs =
            defined $separator ? ( $rhs, $separator, $rhs ) : ($rhs);
        my $rule_key = join( ",",
            map { $_->[Parse::Marpa::Internal::Symbol::ID] }
                ( $lhs, @key_rhs ) );
        croak( "Duplicate rule: ",
            $lhs_name, " -> ", join( ",", @$rhs_names ) )
            if exists $rule_hash->{$rule_key};
        $rule_hash->{$rule_key} = 1;
    }

    # The following rules make evaluations volatile
    $grammar->[Parse::Marpa::Internal::Grammar::VOLATILE] = 1;

    my $rule_action;
    given ($action) {
        when (undef) { $rule_action = undef; }
        default {
            if ($left_associative) {

                # more efficient way to do this?
                $rule_action = q{
                    HEAD: for (;;) {
                        my $head = shift @$Parse::Marpa::This::v;
                        last HEAD unless scalar @$head;
                        unshift(@$Parse::Marpa::This::v, @$head);
                    }
                }
            }
            else {
                $rule_action = q{
                    TAIL: for (;;) {
                        my $tail = pop @$Parse::Marpa::This::v;
                        last TAIL unless scalar @$tail;
                        push(@$Parse::Marpa::This::v, @$tail);
                    }
                }
            }
            $rule_action .= $action;
        }
    }
    add_rule( $grammar, $lhs, [$sequence], $rule_action, $priority, );
    if ( defined $separator and not $proper_separation ) {
        unless ($keep_separation) {
            $rule_action = q{ pop @$Parse::Marpa::This::v; } . ($rule_action // "") ;
        }
        add_rule( $grammar, $lhs, [ $sequence, $separator, ],
            $rule_action, $priority, );
    }

    my @separated_rhs = ($rhs);
    push( @separated_rhs, $separator ) if defined $separator;

    # minimal sequence rule
    my $counted_rhs = [ (@separated_rhs) x ( $min - 1 ), $rhs ];

    if ($left_associative) {
        if ( defined $separator and not $keep_separation ) {
            $rule_action = q{
                [
                    [],
                    @{$Parse::Marpa::This::v}[
                        grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                    ]
                ]
            }
        }
        else {
            $rule_action = q{
                unshift(@$Parse::Marpa::This::v, []);
                $Parse::Marpa::This::v 
            }
        }
    }
    else {
        if ( defined $separator and not $keep_separation ) {
            $rule_action = q{
                [
                    @{$Parse::Marpa::This::v}[
                        grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                    ],
                    []
                ]
            }
        }
        else {
            $rule_action = q{
                push(@$Parse::Marpa::This::v, []);
                $Parse::Marpa::This::v 
            }
        }
    }

    add_rule( $grammar, $sequence, $counted_rhs, $rule_action, $priority, );

    # iterating sequence rule
    $rule_action = ( defined $separator and not $keep_separation )
        ? q{
            [
                @{$Parse::Marpa::This::v}[
                   grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                ],
            ]
        }
        : q{
            $Parse::Marpa::This::v
        };
    my @iterating_rhs = ( @separated_rhs, $sequence );
    if ($left_associative) {
        @iterating_rhs = reverse @iterating_rhs;
    }
    add_rule( $grammar, $sequence, ( \@iterating_rhs ),
        $rule_action, $priority, );

}    # sub add_rules_from_hash

sub add_user_terminals {
    my $grammar   = shift;
    my $terminals = shift;

    TERMINAL: for my $terminal (@$terminals) {
        my $arg_count = @$terminal;
        if ( $arg_count > 2 or $arg_count < 1 ) {
            croak("terminal must have from 1 or 2 arguments");
        }
        my ( $lhs_name, $lexer ) = @$terminal;
        add_user_terminal( $grammar, $lhs_name, $lexer );
    }
}

sub add_user_terminal {
    my $grammar  = shift;
    my $lhs_name = shift;
    my $lexer    = shift;

    add_terminal( $grammar, canonical_name($lhs_name), $lexer );
}

sub set_start {
    my $grammar    = shift;
    my $start_name = shift;
    my $success = 1;

    # my $trace_fh =
        # $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
    my $symbol_hash =
        $grammar->[Parse::Marpa::Internal::Grammar::SYMBOL_HASH];
    my $problems =
        $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS];
    my $start = $symbol_hash->{$start_name};

    if ( not defined $start ) {
        my $problem = "Start symbol: " . $start_name . " not defined";
        push(
            @{ $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS] },
            $problem
        );
        $success = 0;
    }

    my ( $lhs, $rhs, $terminal, $productive ) = @{$start}[
        Parse::Marpa::Internal::Symbol::LHS,
        Parse::Marpa::Internal::Symbol::RHS,
        Parse::Marpa::Internal::Symbol::TERMINAL,
        Parse::Marpa::Internal::Symbol::PRODUCTIVE,
    ];

    if ( not scalar @$lhs and not $terminal ) {
        my $problem = "Start symbol " . $start_name . " not on LHS of any rule";
        push(
            @{ $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS] },
            $problem
        );
        $success = 0;
    }

    if ( not $productive ) {
        my $problem = "Unproductive start symbol: " . $start_name;
        push(
            @{ $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS] },
            $problem
        );
        $success = 0;
    }

    $grammar->[Parse::Marpa::Internal::Grammar::START] = $start;

    $success;
}

# return list of rules reachable from the start symbol;
sub accessible {
    my $grammar = shift;
    my $start   = $grammar->[Parse::Marpa::Internal::Grammar::START];

    $start->[Parse::Marpa::Internal::Symbol::ACCESSIBLE] = 1;
    my $symbol_work_set = [$start];
    my $rule_work_set   = [];

    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

        SYMBOL_PASS: while ( my $work_symbol = shift @$symbol_work_set ) {
            my $rules_produced =
                $work_symbol->[Parse::Marpa::Internal::Symbol::LHS];
            PRODUCED_RULE: for my $rule (@$rules_produced) {

                next PRODUCED_RULE
                    if defined $rule
                        ->[Parse::Marpa::Internal::Rule::ACCESSIBLE];

                # assume nullable until we hit an unmarked or unreachable symbol
                $rule->[Parse::Marpa::Internal::Rule::ACCESSIBLE] = 1;
                $work_to_do++;
                push( @$rule_work_set, $rule );

            }
        }    # SYMBOL_PASS

        RULE: while ( my $work_rule = shift @$rule_work_set ) {
            my $rhs_symbol = $work_rule->[Parse::Marpa::Internal::Rule::RHS];

            RHS: for my $symbol (@$rhs_symbol) {

                next RHS
                    if defined $symbol
                        ->[Parse::Marpa::Internal::Symbol::ACCESSIBLE];
                $symbol->[Parse::Marpa::Internal::Symbol::ACCESSIBLE] =
                    1;
                $work_to_do++;

                push( @$symbol_work_set, $symbol );
            }

        }    # RULE

    }    # work_to_do loop

}

sub productive {
    my $grammar = shift;

    my ( $rules, $symbols ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::RULES,
        Parse::Marpa::Internal::Grammar::SYMBOLS
    ];

    # if a symbol's nullability could not be determined, it was unreachable
    # all nullable symbols are reachable
    for my $symbol (@$symbols) {
        if ( not defined $_->[Parse::Marpa::Internal::Symbol::NULLABLE] ) {
            $_->[Parse::Marpa::Internal::Symbol::PRODUCTIVE] = 0;
        }
        if ( $_->[Parse::Marpa::Internal::Symbol::NULLABLE] ) {
            $_->[Parse::Marpa::Internal::Symbol::PRODUCTIVE] = 1;
        }
    }

    # if a rule's nullability could not be determined, it was unreachable
    # all nullable rules are reachable
    for my $rule (@$rules) {
        if ( not defined $rule->[Parse::Marpa::Internal::Rule::NULLABLE] ) {
            $_->[Parse::Marpa::Internal::Symbol::PRODUCTIVE] = 0;
        }
        if ( $rule->[Parse::Marpa::Internal::Rule::NULLABLE] ) {
            $_->[Parse::Marpa::Internal::Symbol::PRODUCTIVE] = 1;
        }
    }

    my $symbol_work_set = [];
    $#$symbol_work_set = $#$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = $#$rules;

    for my $symbol_id (
        grep {
            defined $symbols->[$_]
                ->[Parse::Marpa::Internal::Symbol::PRODUCTIVE]
        } ( 0 .. $#$symbols )
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    }
    for my $rule_id (
        grep {
            defined $rules->[$_]
                ->[Parse::Marpa::Internal::Rule::PRODUCTIVE]
        } ( 0 .. $#$rules )
        )
    {
        $rule_work_set->[$rule_id] = 1;
    }
    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

        SYMBOL_PASS:
        for my $symbol_id ( grep { $symbol_work_set->[$_] }
            ( 0 .. $#$symbol_work_set ) )
        {
            my $work_symbol = $symbols->[$symbol_id];
            $symbol_work_set->[$symbol_id] = 0;

            my $rules_producing =
                $work_symbol->[Parse::Marpa::Internal::Symbol::RHS];
            PRODUCING_RULE: for my $rule (@$rules_producing) {

                # no work to do -- this rule already has nullability marked
                next PRODUCING_RULE
                    if defined $rule
                        ->[Parse::Marpa::Internal::Rule::PRODUCTIVE];

                # assume nullable until we hit an unmarked or unreachable symbol
                my $rule_reachable = 1;

                # are all symbols on the RHS of this rule bottom marked?
                RHS_SYMBOL:
                for my $rhs_symbol (
                    @{ $rule->[Parse::Marpa::Internal::Rule::RHS] } )
                {
                    my $reachable = $rhs_symbol
                        ->[Parse::Marpa::Internal::Symbol::PRODUCTIVE];

                    # unmarked symbol, change the assumption for rule to undef,
                    # but keep scanning for unreachable
                    # symbol, which will override everything else
                    if ( not defined $reachable ) {
                        $rule_reachable = undef;
                        next RHS_SYMBOL;
                    }

                    # any unreachable RHS symbol means the rule is unreachable
                    if ( $reachable == 0 ) {
                        $rule_reachable = 0;
                        last RHS_SYMBOL;
                    }
                }

                # if this pass found the rule reachable or unreachable, mark the rule
                if ( defined $rule_reachable ) {
                    $rule->[Parse::Marpa::Internal::Rule::PRODUCTIVE] =
                        $rule_reachable;
                    $work_to_do++;
                    $rule_work_set
                        ->[ $rule->[Parse::Marpa::Internal::Rule::ID] ] = 1;
                }

            }
        }    # SYMBOL_PASS

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#$rule_work_set ) )
        {
            my $work_rule = $rules->[$rule_id];
            $rule_work_set->[$rule_id] = 0;
            my $lhs_symbol = $work_rule->[Parse::Marpa::Internal::Rule::LHS];

            # no work to do -- this symbol already has reachability marked
            next RULE
                if defined $lhs_symbol
                    ->[Parse::Marpa::Internal::Symbol::PRODUCTIVE];

            # assume unreachable until we hit an unmarked or non-nullable symbol
            my $symbol_reachable = 0;

            LHS_RULE:
            for my $rule (
                @{ $lhs_symbol->[Parse::Marpa::Internal::Symbol::LHS] } )
            {

                my $reachable =
                    $rule->[Parse::Marpa::Internal::Rule::PRODUCTIVE];

                # unmarked symbol, change the assumption for rule to undef, but keep scanning for nullable
                # rule, which will override everything else
                if ( not defined $reachable ) {
                    $symbol_reachable = undef;
                    next LHS_RULE;
                }

                # any reachable rule means the LHS is reachable
                if ( $reachable == 1 ) {
                    $symbol_reachable = 1;
                    last LHS_RULE;
                }
            }

            # if this pass found the symbol reachable or unreachable, mark the symbol
            if ( defined $symbol_reachable ) {
                $lhs_symbol->[Parse::Marpa::Internal::Symbol::PRODUCTIVE]
                    = $symbol_reachable;
                $work_to_do++;
                $symbol_work_set
                    ->[ $lhs_symbol->[Parse::Marpa::Internal::Symbol::ID] ] =
                    1;
            }

        }    # RULE

    }    # work_to_do loop

}

sub nulling {
    my $grammar = shift;

    my ( $rules, $symbols, $default_null_value ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::RULES,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::DEFAULT_NULL_VALUE,
    ];

    my $symbol_work_set = [];
    $#$symbol_work_set = $#$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = $#$rules;

    for my $rule_id (
        map  { $_->[Parse::Marpa::Internal::Rule::ID] }
        grep { $_->[Parse::Marpa::Internal::Rule::NULLING] } @$rules
        )
    {
        $rule_work_set->[$rule_id] = 1;
    }

    for my $symbol_id (
        map  { $_->[Parse::Marpa::Internal::Symbol::ID] }
        grep { $_->[Parse::Marpa::Internal::Symbol::NULLING] } @$symbols
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    }

    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#$rule_work_set ) )
        {
            my $work_rule = $rules->[$rule_id];
            $rule_work_set->[$rule_id] = 0;
            my $lhs_symbol = $work_rule->[Parse::Marpa::Internal::Rule::LHS];

            # no work to do -- this symbol already is marked one way or the other
            next RULE
                if defined
                    $lhs_symbol->[Parse::Marpa::Internal::Symbol::NULLING];

            # assume nulling until we hit an unmarked or non-nulling symbol
            my $symbol_nulling = \$default_null_value;

            # make sure that all rules for this lhs are nulling
            LHS_RULE:
            for my $rule (
                @{ $lhs_symbol->[Parse::Marpa::Internal::Symbol::LHS] } )
            {

                my $nulling = $rule->[Parse::Marpa::Internal::Rule::NULLING];

                # unmarked rule, change the assumption for the symbol to undef,
                # but keep scanning for rule marked non-nulling,
                # which will override everything else
                if ( not defined $nulling ) {
                    $symbol_nulling = undef;
                    next LHS_RULE;
                }

                # any non-nulling rule means the LHS is not nulling
                if ( $nulling == 0 ) {
                    $symbol_nulling = 0;
                    last LHS_RULE;
                }
            }

            # if this pass found the symbol nulling or non-nulling
            #  mark the symbol
            if ( defined $symbol_nulling ) {
                $lhs_symbol->[Parse::Marpa::Internal::Symbol::NULLING] =
                    $symbol_nulling;
                $work_to_do++;

                $symbol_work_set
                    ->[ $lhs_symbol->[Parse::Marpa::Internal::Symbol::ID] ] =
                    1;
            }

        }    # RULE

        SYMBOL_PASS:
        for my $symbol_id ( grep { $symbol_work_set->[$_] }
            ( 0 .. $#$symbol_work_set ) )
        {
            my $work_symbol = $symbols->[$symbol_id];
            $symbol_work_set->[$symbol_id] = 0;

            my $rules_producing =
                $work_symbol->[Parse::Marpa::Internal::Symbol::RHS];
            PRODUCING_RULE: for my $rule (@$rules_producing) {

                # no work to do -- this rule already has nulling marked
                next PRODUCING_RULE
                    if defined $rule->[Parse::Marpa::Internal::Rule::NULLING];

                # assume nulling until we hit an unmarked or unreachable symbol
                my $rule_nulling = 1;

                # are all symbols on the RHS of this rule marked?
                RHS_SYMBOL:
                for my $rhs_symbol (
                    @{ $rule->[Parse::Marpa::Internal::Rule::RHS] } )
                {
                    my $nulling = $rhs_symbol
                        ->[Parse::Marpa::Internal::Symbol::NULLING];

                    # unmarked rule, change the assumption for rule to undef,
                    # but keep scanning for non-nulling
                    # rule, which will override everything else
                    if ( not defined $nulling ) {
                        $rule_nulling = undef;
                        next RHS_SYMBOL;
                    }

                    # any non-nulling RHS symbol means the rule is non-nulling
                    if ( $nulling == 0 ) {
                        $rule_nulling = 0;
                        last RHS_SYMBOL;
                    }
                }

                # if this pass found the rule reachable or unreachable, mark the rule
                if ( defined $rule_nulling ) {
                    $rule->[Parse::Marpa::Internal::Rule::NULLING] =
                        $rule_nulling;
                    $work_to_do++;
                    $rule_work_set
                        ->[ $rule->[Parse::Marpa::Internal::Rule::ID] ] = 1;
                }

            }
        }    # SYMBOL_PASS

    }    # work_to_do loop

}

# returns undef if there was a problem
sub nullable {
    my $grammar = shift;
    my ( $rules, $symbols, $tracing ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::RULES,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::TRACING,
    ];

    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ];
    }

    # boolean to track if current pass has changed anything
    my $work_to_do = 1;

    my $symbol_work_set = [];
    $#$symbol_work_set = @$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = @$rules;

    for my $symbol_id (
        map { $_->[Parse::Marpa::Internal::Symbol::ID] }
        grep {
                   $_->[Parse::Marpa::Internal::Symbol::NULLABLE]
                or $_->[Parse::Marpa::Internal::Symbol::NULLING]
        } @$symbols
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    }
    for my $rule_id (
        map  { $_->[Parse::Marpa::Internal::Rule::ID] }
        grep { defined $_->[Parse::Marpa::Internal::Rule::NULLABLE] } @$rules
        )
    {
        $rule_work_set->[$rule_id] = 1;
    }

    while ($work_to_do) {
        $work_to_do = 0;

        SYMBOL_PASS:
        for my $symbol_id ( grep { $symbol_work_set->[$_] }
            ( 0 .. $#$symbol_work_set ) )
        {
            my $work_symbol = $symbols->[$symbol_id];
            $symbol_work_set->[$symbol_id] = 0;
            my $rules_producing =
                $work_symbol->[Parse::Marpa::Internal::Symbol::RHS];

            PRODUCING_RULE: for my $rule (@$rules_producing) {

                # assume nullable until we hit an unmarked or non-nullable symbol
                my $rule_nullable = 1;

                # no work to do -- this rule already has nullability marked
                next PRODUCING_RULE
                    if
                    defined $rule->[Parse::Marpa::Internal::Rule::NULLABLE];

                # are all symbols on the RHS of this rule bottom marked?
                RHS_SYMBOL:
                for my $rhs_symbol (
                    @{ $rule->[Parse::Marpa::Internal::Rule::RHS] } )
                {
                    my $nullable = $rhs_symbol
                        ->[Parse::Marpa::Internal::Symbol::NULLABLE];

                    # unmarked symbol, change the assumption for rule to undef, but keep scanning for non-nullable
                    # symbol, which will override everything else
                    if ( not defined $nullable ) {
                        $rule_nullable = undef;
                        next RHS_SYMBOL;
                    }

                    # any non-nullable RHS symbol means the rule is not nullable
                    if ( $nullable == 0 ) {
                        $rule_nullable = 0;
                        last RHS_SYMBOL;
                    }
                }

                # if this pass found the rule nullable or not, so mark the rule
                if ( defined $rule_nullable ) {
                    $rule->[Parse::Marpa::Internal::Rule::NULLABLE] =
                        $rule_nullable;
                    $work_to_do++;
                    $rule_work_set
                        ->[ $rule->[Parse::Marpa::Internal::Rule::ID] ] = 1;
                }

            }
        }    # SYMBOL_PASS

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#$rule_work_set ) )
        {
            my $work_rule  = $rules->[$rule_id];
            my $lhs_symbol = $work_rule->[Parse::Marpa::Internal::Rule::LHS];

            # no work to do -- this symbol already has nullability marked
            next RULE
                if defined
                    $lhs_symbol->[Parse::Marpa::Internal::Symbol::NULLABLE];

            # assume non-nullable until we hit an unmarked or non-nullable symbol
            my $symbol_nullable = 0;

            LHS_RULE:
            for my $rule (
                @{ $lhs_symbol->[Parse::Marpa::Internal::Symbol::LHS] } )
            {

                my $nullable =
                    $rule->[Parse::Marpa::Internal::Rule::NULLABLE];

                # unmarked symbol, change the assumption for rule to undef,
                # but keep scanning for nullable
                # rule, which will override everything else
                if ( not defined $nullable ) {
                    $symbol_nullable = undef;
                    next LHS_RULE;
                }

                # any nullable rule means the LHS is nullable
                if ( $nullable == 1 ) {
                    $symbol_nullable = 1;
                    last LHS_RULE;
                }
            }

            # if this pass found the symbol nullable or not, mark the symbol
            if ( defined $symbol_nullable ) {
                $lhs_symbol->[Parse::Marpa::Internal::Symbol::NULLABLE] =
                    $symbol_nullable;
                $work_to_do++;
                $symbol_work_set
                    ->[ $lhs_symbol->[Parse::Marpa::Internal::Symbol::ID] ] =
                    1;
            }

        }    # RULE

    }    # work_to_do loop

    my $counted_nullable_count;
    for my $symbol (@$symbols) {
        my ( $name, $nullable, $counted, ) = @{$symbol}[
            Parse::Marpa::Internal::Symbol::NAME,
            Parse::Marpa::Internal::Symbol::NULLABLE,
            Parse::Marpa::Internal::Symbol::COUNTED,
        ];
        if ( $nullable and $counted ) {
            my $problem = "Nullable symbol $name is on rhs of counted rule";
            push(
                @{ $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS] },
                $problem
            );
            $counted_nullable_count++;
        }
    }
    if ($counted_nullable_count) {
        my $problem =
            "Counted nullables confuse Marpa -- please rewrite the grammar";
        push(
            @{ $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS] },
            $problem
        );
        return;
    }

    return 1;

}

sub create_NFA {
    my $grammar = shift;
    my ( $rules, $symbols, $symbol_hash, $start, $academic ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::RULES,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::SYMBOL_HASH,
        Parse::Marpa::Internal::Grammar::START,
        Parse::Marpa::Internal::Grammar::ACADEMIC
    ];

    $grammar->[Parse::Marpa::Internal::Grammar::NULLABLE_SYMBOL] =
        [ grep { $_->[Parse::Marpa::Internal::Symbol::NULLABLE] } @$symbols ];

    my $NFA = [];
    $grammar->[Parse::Marpa::Internal::Grammar::NFA] = $NFA;

    my $state_id = 0;
    my @NFA_by_item;

    # create S0
    my $s0 = [];
    @{$s0}[
        Parse::Marpa::Internal::NFA::ID,
        Parse::Marpa::Internal::NFA::NAME,
        Parse::Marpa::Internal::NFA::TRANSITION
        ]
        = ( $state_id++, "S0", {} );
    push( @$NFA, $s0 );

    # create the other states
    RULE: for my $rule (@$rules) {
        my ( $rule_id, $rhs, $useful ) = @{$rule}[
            Parse::Marpa::Internal::Rule::ID,
            Parse::Marpa::Internal::Rule::RHS,
            Parse::Marpa::Internal::Rule::USEFUL
        ];
        next RULE unless $academic or $useful;
        for my $position ( 0 .. scalar @{$rhs} ) {
            my $new_state = [];
            @{$new_state}[
                Parse::Marpa::Internal::NFA::ID,
                Parse::Marpa::Internal::NFA::NAME,
                Parse::Marpa::Internal::NFA::ITEM,
                Parse::Marpa::Internal::NFA::TRANSITION
                ]
                = ( $state_id, "S" . $state_id, [ $rule, $position ], {} );
            $state_id++;
            push( @$NFA, $new_state );
            $NFA_by_item[$rule_id][$position] = $new_state;
        }    # position
    }    # rule

    # now add the transitions
    STATE: for my $state (@$NFA) {
        my ( $id, $name, $item, $transition ) = @$state;

        # transitions from state 0:
        # for every rule with the start symbol on its LHS, the item [ rule, 0 ]
        if ( not defined $item ) {
            my @start_rules =
                @{ $start->[Parse::Marpa::Internal::Symbol::LHS] };
            my $start_alias =
                $start->[Parse::Marpa::Internal::Symbol::NULL_ALIAS];
            if ( defined $start_alias ) {
                push(
                    @start_rules,
                    @{  $start_alias->[Parse::Marpa::Internal::Symbol::LHS]
                        }
                );
            }

            RULE: for my $start_rule (@start_rules) {
                my ( $start_rule_id, $useful ) = @{$start_rule}[
                    Parse::Marpa::Internal::Rule::ID,
                    Parse::Marpa::Internal::Rule::USEFUL
                ];
                next RULE unless $useful;
                push(
                    @{ $transition->{""} },
                    $NFA_by_item[$start_rule_id][0]
                );
            }
            next STATE;
        }

        # transitions from states other than state 0:

        my ( $rule, $position ) = @{$item}[
            Parse::Marpa::Internal::LR0_item::RULE,
            Parse::Marpa::Internal::LR0_item::POSITION
        ];
        my $rule_id = $rule->[Parse::Marpa::Internal::Rule::ID];
        my $next_symbol =
            $rule->[Parse::Marpa::Internal::Rule::RHS]->[$position];

        # no transitions if position is after the end of the RHS
        if ( not defined $next_symbol ) { next STATE; }

        # the scanning transition: the transition if the position is at symbol X
        # in the RHS, via symbol X, to the state corresponding to the same
        # rule with the position incremented by 1
        # should I use ID as the key for those hashes, or NAME?
        push(
            @{  $transition
                    ->{ $next_symbol->[Parse::Marpa::Internal::Symbol::NAME] }
                },
            $NFA_by_item[$rule_id][ $position + 1 ]
        );

        # the prediction transitions: transitions if the position is at symbol X
        # in the RHS, via the empty symbol, to all states with X on the LHS and
        # position 0
        RULE:
        for my $predicted_rule (
            @{ $next_symbol->[Parse::Marpa::Internal::Symbol::LHS] } )
        {
            my ( $predicted_rule_id, $useful ) = @{$predicted_rule}[
                Parse::Marpa::Internal::Rule::ID,
                Parse::Marpa::Internal::Rule::USEFUL
            ];
            next RULE unless $useful;
            push(
                @{ $transition->{""} },
                $NFA_by_item[$predicted_rule_id][0]
            );
        }
    }
}

# take a list of kernel NFA states, possibly with duplicates, and return the fully
# built kernel split DFA (SDFA) state.  It builds the kernel state and its associated prediction state,
# as necessary.  The build is complete, except for the non-empty transitions, which are
# left to be set elsewhere.
#

sub assign_SDFA_kernel_state {
    my $grammar       = shift;
    my $kernel_states = shift;
    my ( $NFA_states, $SDFA_by_name, $SDFA ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::NFA,
        Parse::Marpa::Internal::Grammar::SDFA_BY_NAME,
        Parse::Marpa::Internal::Grammar::SDFA
    ];

    my $kernel_NFA_state_seen     = [];
    my $prediction_NFA_state_seen = [];

    # the two split DFA states which we are to find or create.  The kernel SDFA state is the
    # return value.
    my $kernel_SDFA_state;
    my $prediction_SDFA_state;

    # pre-allocate the arrays that track whether we've already used an NFA state
    $#$kernel_NFA_state_seen = $#$prediction_NFA_state_seen = @$NFA_states;

    # lists of NFA states to followed up on for the closure
    my $kernel_work_list = [
        grep {
            not $kernel_NFA_state_seen
                ->[ $_->[Parse::Marpa::Internal::NFA::ID] ]++
            } @$kernel_states
    ];
    my $prediction_work_list = [];

    # create the kernel SDFA state
    WORK_LIST: while (@$kernel_work_list) {
        my $next_work_list = [];

        NFA_STATE: for my $NFA_state (@$kernel_work_list) {

            my $to_states =
                $NFA_state->[Parse::Marpa::Internal::NFA::TRANSITION]->{""};

            # First the empty transitions.  These will all be predictions,
            # and need to go into the
            # work list for the prediction SDFA state
            if ( defined $to_states ) {
                push(
                    @$prediction_work_list,
                    grep {
                        not $prediction_NFA_state_seen
                            ->[ $_->[Parse::Marpa::Internal::NFA::ID] ]++
                        } @$to_states
                );
            }

            SYMBOL:
            for my $nullable_symbol (
                @{  $grammar
                        ->[Parse::Marpa::Internal::Grammar::NULLABLE_SYMBOL]
                }
                )
            {
                $to_states =
                    $NFA_state->[Parse::Marpa::Internal::NFA::TRANSITION]
                    ->{ $nullable_symbol
                        ->[Parse::Marpa::Internal::Symbol::NAME] };
                next SYMBOL unless defined $to_states;
                push(
                    @$next_work_list,
                    grep {
                        not $kernel_NFA_state_seen
                            ->[ $_->[Parse::Marpa::Internal::NFA::ID] ]++
                        } @$to_states
                );
            }
        }

        $kernel_work_list = $next_work_list;
    }    # kernel WORK_LIST

    my $NFA_ids = [];
    NFA_ID: for ( my $NFA_id = 0; $NFA_id <= $#$NFA_states; $NFA_id++ ) {
        next NFA_ID unless $kernel_NFA_state_seen->[$NFA_id];
        my $LR0_item =
            $NFA_states->[$NFA_id]->[Parse::Marpa::Internal::NFA::ITEM];
        my ( $rule, $position ) = @{$LR0_item}[
            Parse::Marpa::Internal::LR0_item::RULE,
            Parse::Marpa::Internal::LR0_item::POSITION
        ];
        my $rhs = $rule->[Parse::Marpa::Internal::Rule::RHS];
        if ( $position < @$rhs ) {
            my $next_symbol = $rhs->[$position];
            next NFA_ID
                if $next_symbol->[Parse::Marpa::Internal::Symbol::NULLING];
        }
        push( @$NFA_ids, $NFA_id );
    }
    my $kernel_SDFA_name = join( ",", @$NFA_ids );

    $kernel_SDFA_state = $SDFA_by_name->{$kernel_SDFA_name};

    # if we already built the kernel SDFA state, we have also already built any necessary prediction SDFA
    # state and linked it, so we're done
    return $kernel_SDFA_state if defined $kernel_SDFA_state;

    # build the kernel state except for the transitions.
    @{$kernel_SDFA_state}[
        Parse::Marpa::Internal::SDFA::ID,
        Parse::Marpa::Internal::SDFA::NAME,
        Parse::Marpa::Internal::SDFA::NFA_STATES
        ]
        = ( scalar @$SDFA, $kernel_SDFA_name, [ @{$NFA_states}[@$NFA_ids] ],
        );
    push( @$SDFA, $kernel_SDFA_state );
    $SDFA_by_name->{$kernel_SDFA_name} = $kernel_SDFA_state;

    # if there is no prediction half of the split DFA state, we are done.
    return $kernel_SDFA_state unless @$prediction_work_list;

    # there is a prediction state, so find its canonical name
    WORK_LIST: while (@$prediction_work_list) {
        my $next_work_list = [];

        NFA_STATE: for my $NFA_state (@$prediction_work_list) {

            SYMBOL:
            for my $symbol_name (
                "",
                map { $_->[Parse::Marpa::Internal::Symbol::NAME] } @{
                    $grammar
                        ->[Parse::Marpa::Internal::Grammar::NULLABLE_SYMBOL]
                }
                )
            {
                my $to_states =
                    $NFA_state->[Parse::Marpa::Internal::NFA::TRANSITION]
                    ->{$symbol_name};
                next SYMBOL unless defined $to_states;
                push(
                    @$next_work_list,
                    grep {
                        not $prediction_NFA_state_seen
                            ->[ $_->[Parse::Marpa::Internal::NFA::ID] ]++
                        } @$to_states
                );
            }
        }

        $prediction_work_list = $next_work_list;
    }    # kernel WORK_LIST

    $NFA_ids = [];
    NFA_ID: for ( my $NFA_id = 0; $NFA_id <= $#$NFA_states; $NFA_id++ ) {
        next NFA_ID unless $prediction_NFA_state_seen->[$NFA_id];
        my $LR0_item =
            $NFA_states->[$NFA_id]->[Parse::Marpa::Internal::NFA::ITEM];
        my ( $rule, $position ) = @{$LR0_item}[
            Parse::Marpa::Internal::LR0_item::RULE,
            Parse::Marpa::Internal::LR0_item::POSITION
        ];
        my $rhs = $rule->[Parse::Marpa::Internal::Rule::RHS];
        if ( $position < @$rhs ) {
            my $next_symbol = $rhs->[$position];
            next NFA_ID
                if $next_symbol->[Parse::Marpa::Internal::Symbol::NULLING];
        }
        push( @$NFA_ids, $NFA_id );
    }
    my $prediction_SDFA_name = join( ",", @$NFA_ids );

    $prediction_SDFA_state = $SDFA_by_name->{$prediction_SDFA_name};

    # if we have not already built the prediction SDFA state, build it
    if ( not defined $prediction_SDFA_state ) {

        # build the prediction state except for the transitions.
        @{$prediction_SDFA_state}[
            Parse::Marpa::Internal::SDFA::ID,
            Parse::Marpa::Internal::SDFA::NAME,
            Parse::Marpa::Internal::SDFA::NFA_STATES
            ]
            = (
            scalar @$SDFA,
            $prediction_SDFA_name, [ @{$NFA_states}[@$NFA_ids] ],
            );
        push( @$SDFA, $prediction_SDFA_state );
        $SDFA_by_name->{$prediction_SDFA_name} = $prediction_SDFA_state;

    }

    # add the empty transition from kernel SDFA state to prediction SDFA state
    $kernel_SDFA_state->[Parse::Marpa::Internal::SDFA::TRANSITION]->{""} =
        $prediction_SDFA_state;

    # return the kernel SDFA state
    $kernel_SDFA_state;
}

sub create_SDFA {
    my $grammar = shift;
    my ( $symbols, $symbol_hash, $NFA, $start, $tracing ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::SYMBOL_HASH,
        Parse::Marpa::Internal::Grammar::NFA,
        Parse::Marpa::Internal::Grammar::START,
        Parse::Marpa::Internal::Grammar::TRACING,
    ];

    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
    }

    my $SDFA = $grammar->[Parse::Marpa::Internal::Grammar::SDFA] = [];
    my $NFA_s0 = $NFA->[0];

    # next SDFA state to compute transitions for
    my $next_state_id = 0;

    my $initial_NFA_states =
        $NFA_s0->[Parse::Marpa::Internal::NFA::TRANSITION]->{""};
    if ( not defined $initial_NFA_states ) {
        say $trace_fh "Empty NFA, cannot create SDFA";
        return;
    }
    assign_SDFA_kernel_state( $grammar, $initial_NFA_states );

    while ( $next_state_id < scalar @$SDFA ) {

        # compute the SDFA state transitions from the transitions
        # of the NFA states of which it is composed
        my $NFA_to_states_by_symbol = {};

        my $SDFA_state = $SDFA->[ $next_state_id++ ];

        # aggregrate the transitions, by symbol, for every NFA state in this SDFA
        # state
        for my $NFA_state (
            @{ $SDFA_state->[Parse::Marpa::Internal::SDFA::NFA_STATES] } )
        {
            my $transition =
                $NFA_state->[Parse::Marpa::Internal::NFA::TRANSITION];
            NFA_TRANSITION:
            while ( my ( $symbol, $to_states ) = each(%$transition) ) {
                next NFA_TRANSITION if $symbol eq "";
                push( @{ $NFA_to_states_by_symbol->{$symbol} }, @$to_states );
            }
        }    # $NFA_state

        # for each transition symbol, create the transition to the SDFA kernel state
        while ( my ( $symbol, $to_states ) = each(%$NFA_to_states_by_symbol) )
        {
            $SDFA_state->[Parse::Marpa::Internal::SDFA::TRANSITION]
                ->{$symbol} =
                assign_SDFA_kernel_state( $grammar, $to_states );
        }
    }

    # For the parse phase, pre-compute the list of names of the lhs's of
    # complete items, the list of complete items, and the start rule (should
    # be maximum one per state)
    STATE: for my $state (@$SDFA) {
        my $lhs_list       = [];
        my $complete_rules = [];
        my $start_rule     = undef;
        $#$lhs_list = @$symbols;
        my $NFA_states = $state->[Parse::Marpa::Internal::SDFA::NFA_STATES];
        for my $NFA_state (@$NFA_states) {
            my $item = $NFA_state->[Parse::Marpa::Internal::NFA::ITEM];
            my ( $rule, $position ) = @{$item}[
                Parse::Marpa::Internal::LR0_item::RULE,
                Parse::Marpa::Internal::LR0_item::POSITION
            ];
            my ( $lhs, $rhs ) = @{$rule}[
                Parse::Marpa::Internal::Rule::LHS,
                Parse::Marpa::Internal::Rule::RHS
            ];
            if ( $position >= @$rhs ) {
                my ( $lhs_id, $lhs_is_start ) = @{$lhs}[
                    Parse::Marpa::Internal::Symbol::ID,
                    Parse::Marpa::Internal::Symbol::START
                ];
                $lhs_list->[$lhs_id] = 1;
                push( @{ $complete_rules->[$lhs_id] }, $rule );
                $start_rule = $rule if $lhs_is_start;
            }
        }    # NFA_state
        $state->[Parse::Marpa::Internal::SDFA::START_RULE] = $start_rule;
        $state->[Parse::Marpa::Internal::SDFA::COMPLETE_RULES] =
            $complete_rules;
        $state->[Parse::Marpa::Internal::SDFA::COMPLETE_LHS] =
            [ map { $_->[Parse::Marpa::Internal::Symbol::NAME] }
                @{$symbols}[ grep { $lhs_list->[$_] } ( 0 .. $#$lhs_list ) ]
            ];

    }    # STATE
}

sub setup_academic_grammar {
    my $grammar = shift;
    my $rules   = $grammar->[Parse::Marpa::Internal::Grammar::RULES];

    # in an academic grammar, consider all rules useful
    for my $rule (@$rules) {
        $rule->[Parse::Marpa::Internal::Rule::USEFUL] = 1;
    }
}

# given a nullable symbol, create a nulling alias and make the first symbol non-nullable
sub alias_symbol {
    my $grammar         = shift;
    my $nullable_symbol = shift;
    my ( $symbol, $symbols, ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::SYMBOL_HASH,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
    ];
    my ( $accessible, $productive, $name, $null_value ) =
        @{$nullable_symbol}[
        Parse::Marpa::Internal::Symbol::ACCESSIBLE,
        Parse::Marpa::Internal::Symbol::PRODUCTIVE,
        Parse::Marpa::Internal::Symbol::NAME,
        Parse::Marpa::Internal::Symbol::NULL_VALUE,
        ];

    # create the new, nulling symbol
    my $symbol_count = @$symbols;
    my $alias_name =
        $nullable_symbol->[Parse::Marpa::Internal::Symbol::NAME] . "[]";
    my $alias = [];
    @{$alias}[
        Parse::Marpa::Internal::Symbol::ID,
        Parse::Marpa::Internal::Symbol::NAME,
        Parse::Marpa::Internal::Symbol::LHS,
        Parse::Marpa::Internal::Symbol::RHS,
        Parse::Marpa::Internal::Symbol::ACCESSIBLE,
        Parse::Marpa::Internal::Symbol::PRODUCTIVE,
        Parse::Marpa::Internal::Symbol::NULLABLE,
        Parse::Marpa::Internal::Symbol::NULLING,
        Parse::Marpa::Internal::Symbol::NULL_VALUE,
        ]
        = (
        $symbol_count, $alias_name, [], [], $accessible,
        $productive, 1, 1, $null_value
        );
    push( @$symbols, $alias );
    weaken( $symbol->{$alias_name} = $alias );

    # turn the original symbol into a non-nullable with a reference to the new alias
    @{$nullable_symbol}[
        Parse::Marpa::Internal::Symbol::NULLABLE,
        Parse::Marpa::Internal::Symbol::NULL_ALIAS
        ]
        = ( 0, $alias );
    $alias;
}

=begin Innovation:

Chomsky-Horspool-Aycock Form is one of my innovations.   Aycock & Horspool's NNF,
in the worst case, is exponential in the size of the base grammar, and not
exactly pretty in the example they give.  I think realistic grammars are likely
to have productions with many nullables on the right side -- for example, the
right hand side of a production might have several uses of optional whitespace.
Grammars with a lot of these production might seriously bloat in size in NNF.

CHAF breaks up productions with a more than a small number nullables on the RHS into
"subproductions".  These are "reassembled" invisibly in evaluating the parse, so
that the semantics of the original grammar are not affected.

=end Innovation:

=cut

# For efficiency, steps in the CHAF evaluation
# work on a last-is-rest principle -- productions
# with a CHAF head always return reference to an array
# of values, of which the last value is (in turn)
# a reference to an array with the "rest" of the values.
# An empty array signals that there are no more.

# rewrite as Chomsky-Horspool-Aycock Form
sub rewrite_as_CHAF {
    my $grammar = shift;
    my ( $rules, $symbols, $old_start_symbol ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::RULES,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::START,
    ];

    # add null aliases to symbols which need them
    my $symbol_count = @$symbols;
    SYMBOL: for ( my $ix = 0; $ix < $symbol_count; $ix++ ) {
        my $symbol = $symbols->[$ix];
        my ( $productive, $accessible, $nulling, $nullable,
            $null_alias )
            = @{$symbol}[
            Parse::Marpa::Internal::Symbol::PRODUCTIVE,
            Parse::Marpa::Internal::Symbol::ACCESSIBLE,
            Parse::Marpa::Internal::Symbol::NULLING,
            Parse::Marpa::Internal::Symbol::NULLABLE,
            Parse::Marpa::Internal::Symbol::NULL_ALIAS
            ];

        # not necessary is the symbol already has a null
        # alias
        next SYMBOL if $null_alias;

        #  we don't bother with unreachable symbols
        next SYMBOL unless $productive;
        next SYMBOL unless $accessible;

        # look for proper nullable symbols
        next SYMBOL if $nulling;
        next SYMBOL unless $nullable;

        alias_symbol( $grammar, $symbol );
    }

    # mark, or create as needed, the useful rules

    # get the initial rule count -- new rules will be added and we don't iterate
    # over them
    my $rule_count = @$rules;
    RULE: for ( my $rule_id = 0; $rule_id < $rule_count; $rule_id++ ) {
        my $rule = $rules->[$rule_id];
        my ( $lhs, $rhs, $productive, $accessible, $nulling,
            $nullable, $priority )
            = @{$rule}[
            Parse::Marpa::Internal::Rule::LHS,
            Parse::Marpa::Internal::Rule::RHS,
            Parse::Marpa::Internal::Rule::PRODUCTIVE,
            Parse::Marpa::Internal::Rule::ACCESSIBLE,
            Parse::Marpa::Internal::Rule::NULLING,
            Parse::Marpa::Internal::Rule::NULLABLE,
            Parse::Marpa::Internal::Rule::PRIORITY,
            ];

        # unreachable and nulling rules are useless
        next RULE unless $productive;
        next RULE unless $accessible;
        next RULE if $nulling;

        # Keep track of whether the lhs side of any new rules we create should
        # be nullable.  If any symbol is a production is not nullable, the lhs
        # is not nullable.  If the original production is nullable, all symbols
        # are nullable, all subproductions will be, and all new lhs's should be.
        # But even if the original production is not nullable, some of the
        # subproductions may be.  These will always be in a series starting from
        # the far right.  Once the first non-nullable symbol is encountered,
        # that subproduction is non-nullable, that lhs will be, and since that
        # new lhs is on the far rhs of subsequent (going left) subproductions,
        # all subsequent subproductions and their lhs's will be non-nullable.
        #
        # Finally, in one more complication, remember that the nullable flag
        # was unset if a nullable was aliased.  So we need to check both the
        # NULL_ALIAS (for proper nullables) and the NULLING flags to see if

        my $last_nonnullable = -1;
        my $proper_nullables = [];
        my $rhs_null_value   = [];
        $#$rhs_null_value = $#$rhs;
        RHS_SYMBOL: for ( my $ix = 0; $ix <= $#$rhs; $ix++ ) {
            my $symbol = $rhs->[$ix];
            my ( $null_alias, $nulling, $null_value ) = @{$symbol}[
                Parse::Marpa::Internal::Symbol::NULL_ALIAS,
                Parse::Marpa::Internal::Symbol::NULLING,
                Parse::Marpa::Internal::Symbol::NULL_VALUE,
            ];
            $rhs_null_value->[$ix] = $null_value;
            next RHS_SYMBOL if $nulling;
            if ($null_alias) {
                push( @$proper_nullables, $ix );
                next RHS_SYMBOL;
            }
            $last_nonnullable = $ix;
        }

        # we found no properly nullable symbols in the RHS, so this rule is useful without
        # any changes
        if ( @$proper_nullables == 0 ) {
            $rule->[Parse::Marpa::Internal::Rule::USEFUL] = 1;
            next RULE;
        }

        # The left hand side of the first subproduction is the lhs of the original rule
        my $subp_lhs   = $lhs;
        my $subp_start = 0;

        # break this production into subproductions with a fixed number of proper nullables,
        # then factor out the proper nullables into a set of productions
        # with only non-nullable and nulling symbols.
        SUBPRODUCTION: for ( ;; ) {

            my $subp_end;
            my $proper_nullable0      = $proper_nullables->[0];
            my $subp_proper_nullable0 = $proper_nullable0 - $subp_start;
            my $proper_nullable1;
            my $subp_proper_nullable1;
            my $subp_factor0_rhs;
            my $next_subp_lhs;

            SETUP_SUBPRODUCTION: {

                if ( @$proper_nullables == 1 ) {
                    $subp_end = $#$rhs;
                    $subp_factor0_rhs =
                        [ @{$rhs}[ $subp_start .. $subp_end ] ];
                    $proper_nullables = [];
                    last SETUP_SUBPRODUCTION;
                }

                $proper_nullable1      = $proper_nullables->[1];
                $subp_proper_nullable1 = $proper_nullable1 - $subp_start;

                if ( @$proper_nullables == 2 ) {
                    $subp_end = $#$rhs;
                    $subp_factor0_rhs =
                        [ @{$rhs}[ $subp_start .. $subp_end ] ];
                    $proper_nullables = [];
                    last SETUP_SUBPRODUCTION;
                }

                # the following subproduction is non-nullable
                # this code apparently not yet (1 Nov 2007) tried !!!
                if ( $proper_nullable1 < $last_nonnullable ) {
                    $subp_end = $proper_nullable1;
                    splice( @$proper_nullables, 0, 2 );
                    $next_subp_lhs = assign_symbol( $grammar,
                        $lhs->[Parse::Marpa::Internal::Symbol::NAME] . "["
                            . $rule_id . ":"
                            . ( $subp_end + 1 )
                            . "]" );
                    @{$next_subp_lhs}[
                        Parse::Marpa::Internal::Symbol::NULLABLE,
                        Parse::Marpa::Internal::Symbol::ACCESSIBLE,
                        Parse::Marpa::Internal::Symbol::PRODUCTIVE,
                        Parse::Marpa::Internal::Symbol::NULLING,
                        ]
                        = ( 0, 1, 1, 0 );
                    $subp_factor0_rhs = [
                        @{$rhs}[ $subp_start .. $subp_end ],
                        $next_subp_lhs
                    ];
                    last SETUP_SUBPRODUCTION;
                }

                # if we got this far we have 3 or more proper nullables, and the next
                # subproduction is nullable
                $subp_end = $proper_nullable1 - 1;
                shift @$proper_nullables;
                $next_subp_lhs = assign_symbol( $grammar,
                          $lhs->[Parse::Marpa::Internal::Symbol::NAME] . "["
                        . $rule_id . ":"
                        . ( $subp_end + 1 )
                        . "]" );
                @{$next_subp_lhs}[
                    Parse::Marpa::Internal::Symbol::NULLABLE,
                    Parse::Marpa::Internal::Symbol::ACCESSIBLE,
                    Parse::Marpa::Internal::Symbol::PRODUCTIVE,
                    Parse::Marpa::Internal::Symbol::NULLING,
                    Parse::Marpa::Internal::Symbol::NULL_VALUE,
                    ]
                    = (
                    1, 1, 1, 0,
                    [   @{$rhs_null_value}
                            [ ( $subp_end + 1 ) .. $#$rhs_null_value ],
                        []
                    ],
                    );
                alias_symbol( $grammar, $next_subp_lhs );
                $subp_factor0_rhs =
                    [ @{$rhs}[ $subp_start .. $subp_end ], $next_subp_lhs ];

            }    # SETUP_SUBPRODUCTION

            my $factored_rhs = [$subp_factor0_rhs];

            FACTOR: {

                # We have additional factored productions if
                # 1) there is more than one proper nullable;
                # 2) there's only one, but replacing it with a nulling symbol will
                #    not make the entire production nulling
                #
                # Here and below we use the nullable flag to establish whether a
                # factored subproduction rhs would be nulling, on this principle:
                #
                # If substituting nulling symbols for all proper nullables does not
                # make a production nulling, then it is not nullable, and vice versa.

                last FACTOR if $nullable and not defined $proper_nullable1;

                # The second factored production, with a nulling symbol substituted for
                # the first proper nullable.
                # and nulling it would make this factored subproduction nulling, don't
                # bother.
                $factored_rhs->[1] = [@$subp_factor0_rhs];
                $factored_rhs->[1]->[$subp_proper_nullable0] =
                    $subp_factor0_rhs->[$subp_proper_nullable0]
                    ->[Parse::Marpa::Internal::Symbol::NULL_ALIAS];

                # The third factored production, with a nulling symbol replacing the
                # second proper nullable.  Make sure there ARE two proper nullables.
                last FACTOR unless defined $proper_nullable1;
                $factored_rhs->[2] = [@$subp_factor0_rhs];
                $factored_rhs->[2]->[$subp_proper_nullable1] =
                    $subp_factor0_rhs->[$subp_proper_nullable1]
                    ->[Parse::Marpa::Internal::Symbol::NULL_ALIAS];

                # The fourth and last factored production, with a nulling symbol replacing
                # both proper nullables.  We don't include it if it results in a nulling
                # production.
                last FACTOR if $nullable;
                $factored_rhs->[3] = [ @{ $factored_rhs->[2] } ];
                $factored_rhs->[3]->[$subp_proper_nullable0] =
                    $subp_factor0_rhs->[$subp_proper_nullable0]
                    ->[Parse::Marpa::Internal::Symbol::NULL_ALIAS];

            }    # FACTOR

            for ( my $ix = 0; $ix <= $#$factored_rhs; $ix++ ) {
                my $factor_rhs = $factored_rhs->[$ix];

                # No need to bother putting together values
                # if the rule's closure is not defined
                # and the values would all be discarded

                # figure out which closure to use
                # if the LHS is the not LHS of the original rule, we have a
                # special CHAF header
                my $has_chaf_lhs = ( $subp_lhs != $lhs );

                # if a CHAF LHS was created for the next subproduction,
                # there is a CHAF continuation for this subproduction.
                # It applies to this factor if there is one of the first two
                # factors of more than two.
                my $has_chaf_rhs = $next_subp_lhs;

                my $new_rule =
                    add_rule( $grammar, $subp_lhs, $factor_rhs, $priority );
                @{$new_rule}[
                    Parse::Marpa::Internal::Rule::USEFUL,
                    Parse::Marpa::Internal::Rule::ACCESSIBLE,
                    Parse::Marpa::Internal::Rule::PRODUCTIVE,
                    Parse::Marpa::Internal::Rule::NULLABLE,
                    Parse::Marpa::Internal::Rule::NULLING,

                    # Parse::Marpa::Internal::Rule::ORDER,
                    Parse::Marpa::Internal::Rule::HAS_CHAF_LHS,
                    Parse::Marpa::Internal::Rule::HAS_CHAF_RHS,
                    ]
                    = (
                    1, 1, 1, 0, 0,

                    # $rule_id,
                    $has_chaf_lhs,
                    $has_chaf_rhs,
                    );

                $new_rule->[Parse::Marpa::Internal::Rule::ORIGINAL_RULE] =
                    $rule;
                $new_rule->[Parse::Marpa::Internal::Rule::ACTION] =
                    $rule->[Parse::Marpa::Internal::Rule::ACTION];

            }    # for each factored rhs

            # no more
            last SUBPRODUCTION unless $next_subp_lhs;
            $subp_lhs   = $next_subp_lhs;
            $subp_start = $subp_end + 1;
            $nullable   = $subp_start > $last_nonnullable;

        }    # SUBPRODUCTION

    }    # RULE

    # Create a new start symbol
    my ( $productive, $null_value ) = @{$old_start_symbol}[
        Parse::Marpa::Internal::Symbol::PRODUCTIVE,
        Parse::Marpa::Internal::Symbol::NULL_VALUE,
    ];
    my $new_start_symbol =
        assign_symbol( $grammar,
        $old_start_symbol->[Parse::Marpa::Internal::Symbol::NAME] . "[']" );
    @{$new_start_symbol}[
        Parse::Marpa::Internal::Symbol::PRODUCTIVE,
        Parse::Marpa::Internal::Symbol::ACCESSIBLE,
        Parse::Marpa::Internal::Symbol::START,
        Parse::Marpa::Internal::Symbol::NULL_VALUE,
        ]
        = ( $productive, 1, 1, $null_value );

    # Create a new start rule
    my $new_start_rule =
        add_rule( $grammar, $new_start_symbol, [$old_start_symbol], 0 );
    @{$new_start_rule}[
        Parse::Marpa::Internal::Rule::PRODUCTIVE,
        Parse::Marpa::Internal::Rule::ACCESSIBLE,
        Parse::Marpa::Internal::Rule::USEFUL,
        Parse::Marpa::Internal::Rule::ACTION,
        ]
        = ( $productive, 1, 1, q{ $Parse::Marpa::This::v->[0] } );

    # If we created a null alias for the original start symobl, we need
    # to create a nulling start rule
    my $old_start_alias =
        $old_start_symbol->[Parse::Marpa::Internal::Symbol::NULL_ALIAS];
    if ($old_start_alias) {
        my $new_start_alias = alias_symbol( $grammar, $new_start_symbol );
        @{$new_start_alias}[ Parse::Marpa::Internal::Symbol::START, ] = (1);
        my $new_start_rule = add_rule( $grammar, $new_start_alias, [], 0 );

        # Nulling rules are not considered useful, but the top-level one is an exception
        @{$new_start_rule}[
            Parse::Marpa::Internal::Rule::PRODUCTIVE,
            Parse::Marpa::Internal::Rule::ACCESSIBLE,
            Parse::Marpa::Internal::Rule::USEFUL,
            ]
            = ( $productive, 1, 1, );
    }
    $grammar->[Parse::Marpa::Internal::Grammar::START] = $new_start_symbol;
}

package Parse::Marpa::Internal::Earley_item;

# Elements of the EARLEY ITEM structure
# Note that these are Earley items as modified by Aycock & Horspool, with SDFA states instead of
# LR(0) items.
#
use constant STATE => 0;    # the SDFA state
use constant PARENT =>
    1;    # the number of the Earley set with the parent item(s)
use constant TOKENS => 2;    # a list of the links from token scanning
use constant LINKS  => 3;    # a list of the links from the completer step
use constant SET    => 4;    # the set this item is in, for debugging
     # these next elements are "notations" for iterating over the parses
use constant POINTER      => 5;     # symbol just before pointer
use constant RULES        => 6;     # current list of rules
use constant RULE_CHOICE  => 7;     # current choice of rule
use constant LINK_CHOICE  => 8;     # current choice of link
use constant TOKEN_CHOICE => 9;     # current choice of token
use constant VALUE        => 10;    # value of pointer symbol
use constant PREDECESSOR  => 11;    # the predecessor link, if we have a value
use constant SUCCESSOR    => 12;    # the predecessor link, in reverse
use constant EFFECT       => 13;    # the cause link, in reverse
                                    # or the "parent" item
use constant LHS          => 14;    # LHS symbol

# Note that (at least right now) items either have a SUCCESSOR
# or an EFFECT, never both.

package Parse::Marpa::Internal::Parse;

use Scalar::Util qw(weaken);
use Data::Dumper;
use Carp;

my $parse_number = 0;

# Elements of the PARSE structure
use constant GRAMMAR       => 0;    # the grammar used
use constant CURRENT_SET   => 1;    # index of the first incomplete Earley set
use constant EARLEY_SETS   => 2;    # the array of the Earley sets
use constant EARLEY_HASHES => 3;    # the array of hashes used
                                    # to build the Earley sets
use constant CURRENT_PARSE_SET => 4;   # the set being taken as the end of
                                       # parse for an evaluation
                                       # only undef if there are no evaluation
                                       # notations in the earley items
use constant START_ITEM => 5;    # the start item for the current evaluation
use constant FURTHEST_EARLEME         => 7;    # last earley set with a token
use constant EXHAUSTED                => 8;    # parse can't continue?
use constant DEFAULT_PARSE_SET        => 14;
use constant PACKAGE       => 17;              # special "safe" namespace
use constant AMBIGUOUS_LEX => 19;

# use constant DEFAULT_ACTION           => 20;
use constant VOLATILE          => 21;
use constant LEXERS            => 22;    # an array, indexed by symbol id,
                                         # of the lexer for each symbol
use constant LEXABLES_BY_STATE => 23;    # an array, indexed by SDFA state id,
                                         # of the lexables belonging in it
use constant PRIORITIES        => 24;    # an array, indexed by SDFA state id,
                                         # of its priority
use constant STREAM            => 25;    # streaming input?
use constant LAST_COMPLETED_SET => 26;   # last earley set completed

# Set rule actions
sub set_actions {
    my $grammar        = shift;
    my $package        = shift;
    my $default_action = shift;

    my ( $rules, $symbols, $symbol_hash, $SDFA, $tracing ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::RULES,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::SYMBOL_HASH,
        Parse::Marpa::Internal::Grammar::SDFA,
        Parse::Marpa::Internal::Grammar::TRACING,
    ];

    my $trace_fh;
    my $trace_actions;
    if ($tracing) {
        $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
        $trace_actions = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_ACTIONS ];
    }

    RULE: for my $rule (@$rules) {

        next RULE unless $rule->[Parse::Marpa::Internal::Rule::USEFUL];

        my $action = $rule->[Parse::Marpa::Internal::Rule::ACTION];

        ACTION: {

            $action //= $default_action;
            last ACTION unless defined $action;

            # HAS_CHAF_RHS and HAS_CHAF_LHS would work well as a bit
            # mask in a C implementation
            my $has_chaf_lhs =
                $rule->[Parse::Marpa::Internal::Rule::HAS_CHAF_LHS];
            my $has_chaf_rhs =
                $rule->[Parse::Marpa::Internal::Rule::HAS_CHAF_RHS];

            last ACTION unless $has_chaf_lhs or $has_chaf_rhs;

            if ( $has_chaf_rhs and $has_chaf_lhs ) {
                $action = q{ $Parse::Marpa::This::v };
                last ACTION;
            }

            # At this point has chaf rhs or lhs but not both
            if ($has_chaf_lhs) {

                $action = q{
                        push(@$Parse::Marpa::This::v, []);
                        $Parse::Marpa::This::v;
                    };
                last ACTION;

            }

            # at this point must have chaf rhs and not a chaf lhs

            my $original_rule = $Parse::Marpa::This::rule
                ->[Parse::Marpa::Internal::Rule::ORIGINAL_RULE];

            $action = q{
                TAIL: for (;;) {
                    my $tail = pop @$Parse::Marpa::This::v;
                    last TAIL unless scalar @$tail;
                    push(@$Parse::Marpa::This::v, @$tail);
                }
            }    # q string
                . $action;

        }    # ACTION

        next RULE unless defined $action;

        my $code =
            "sub {\n" . "    package " . $package . ";\n" . $action . "\n}";

        if ($trace_actions) {
            print $trace_fh "Setting action for rule ",
                Parse::Marpa::brief_rule($rule), " to\n", $code, "\n";
        }

        my $closure;
        {
            my @warnings;
            local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
            $closure = eval $code;
            my $fatal_error = $@;
            if ($fatal_error or @warnings) {
                Parse::Marpa::Internal::die_on_problems($fatal_error, \@warnings,
                    "compiling action",
                    "compiling action for "
                        . Parse::Marpa::brief_original_rule($rule),,
                    \$code
                );
            }
        }

        $rule->[Parse::Marpa::Internal::Rule::CLOSURE] = $closure;

    }    # RULE

    my @lexers;
    $#lexers = $#$symbols;

    SYMBOL: for ( my $ix = 0; $ix <= $#lexers; $ix++ ) {

        my $symbol = $symbols->[$ix];
        my ( $name, $regex, $action ) = @{$symbol}[
            Parse::Marpa::Internal::Symbol::NAME,
            Parse::Marpa::Internal::Symbol::REGEX,
            Parse::Marpa::Internal::Symbol::ACTION,
        ];

        if ( defined $regex ) {
            $lexers[$ix] = $regex;
            next SYMBOL;
        }

        given ($action) {
            when (undef) {;}    # do nothing
                                # Right now do nothing but find lex_q_quote
            when ("lex_q_quote") {
                $lexers[$ix] = \&Parse::Marpa::Lex::lex_q_quote;
            }
            when ("lex_regex") {
                $lexers[$ix] = \&Parse::Marpa::Lex::lex_regex;
            }
            default {
                my $code = q'
                        sub {
                            my $STRING = shift;
                            my $START = (pos $$STRING) // 0;
                     '
                    . "package " . $package . ";\n" . $action . "; return\n}";

                if ($trace_actions) {
                    print $trace_fh
                        "Setting action for terminal ", $name, " to\n", $code,
                        "\n";
                }

                my $closure;
                {
                    my @warnings;
                    local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
                    $closure = eval $code;
                    my $fatal_error = $@;
                    if ($fatal_error or @warnings) {
                        Parse::Marpa::Internal::die_on_problems($fatal_error, \@warnings,
                            "compiling action",
                            "compiling action for $name",
                            \$code
                        );
                    }
                }

                $lexers[$ix] = $closure;

            }
        }

    }    # SYMBOL

    my @lexables_by_state;
    $#lexables_by_state = $#$SDFA;

    for my $state (@$SDFA) {
        my ( $id, $transition ) = @{$state}[
            Parse::Marpa::Internal::SDFA::ID,
            Parse::Marpa::Internal::SDFA::TRANSITION,
        ];
        $lexables_by_state[$id] = [
            grep { $lexers[$_] }
                map {
                $symbol_hash->{$_}->[Parse::Marpa::Internal::Symbol::ID]
                }
                grep { $_ ne "" }
                keys %$transition
        ];
    }

    return ( \@lexers, \@lexables_by_state, );

}    # sub set_actions

sub compile_regexes {
    my $grammar = shift;
    my ( $symbols, $default_lex_prefix, $default_lex_suffix, ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX,
        Parse::Marpa::Internal::Grammar::DEFAULT_LEX_SUFFIX,
    ];

    SYMBOL: for my $symbol (@$symbols) {
        my $regex = $symbol->[Parse::Marpa::Internal::Symbol::REGEX];
        next SYMBOL unless defined $regex;
        if ( "" =~ $regex ) {
            my $name = $symbol->[Parse::Marpa::Internal::Symbol::NAME];
            croak( "Attempt to add nullable terminal: ", $name );
        }
        my $prefix = $symbol->[Parse::Marpa::Internal::Symbol::PREFIX]
            // $default_lex_prefix;
        my $suffix = $symbol->[Parse::Marpa::Internal::Symbol::SUFFIX]
            // $default_lex_suffix;
        my $compiled_regex = qr/
            \G
            (?<mArPa_prefix>$prefix)
            (?<mArPa_match>$regex)
            (?<mArPa_suffix>$suffix)
        /xms;
        $symbol->[Parse::Marpa::Internal::Symbol::REGEX] = $compiled_regex;
    }    # SYMBOL

}

sub set_priorities {
    my $grammar    = shift;
    my $priorities = [];
    my $problem    = 0;

    my ($trace_fh, $trace_priorities);
    if ($grammar->[ Parse::Marpa::Internal::Grammar::TRACING ]) {
        $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
        $trace_priorities = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_PRIORITIES ];
    }
    my $SDFA = $grammar->[Parse::Marpa::Internal::Grammar::SDFA];
    $#$priorities = $#$SDFA;

    for my $state (@$SDFA) {
        my $priority;
        my $priority_conflict = 0;
        my ( $id, $complete_rules_by_lhs ) = @{$state}[
            Parse::Marpa::Internal::SDFA::ID,
            Parse::Marpa::Internal::SDFA::COMPLETE_RULES,
        ];
        my @complete_rules;
        LHS: for my $lhs_id ( 0 .. $#$complete_rules_by_lhs ) {
            my $rules = $complete_rules_by_lhs->[$lhs_id];
            next LHS unless defined $rules;
            push( @complete_rules, @$rules );
        }
        COMPLETE_RULE: for my $complete_rule (@complete_rules) {
            my $rule_priority =
                $complete_rule->[Parse::Marpa::Internal::Rule::PRIORITY];
            given ($priority) {
                when (undef) { $priority = $rule_priority }
                when ( $rule_priority != $_ ) { $priority_conflict++; }
            }
        }
        if ($priority_conflict) {
            $problem++;
            carp( "Priority conflict in SDFA ", $id );
            COMPLETE_RULE: for my $complete_rule (@complete_rules) {
                my $rule_priority =
                    $complete_rule->[Parse::Marpa::Internal::Rule::PRIORITY];
                carp(
                    "SDFA ", $id, ": ",
                    Parse::Marpa::brief_rule($complete_rule),
                    "has priority ",
                    $rule_priority
                );
            }
        }
        $priorities->[$id] = $priority // 0;
        if ($trace_priorities) {
            say $trace_fh "Priority for state $id: ", $priorities->[$id];
        }
    }    # for each SDFA state
    if ($problem) {
        croak( "Marpa cannot continue: ", $problem, " priority conflicts" );
    }

    $priorities;

}    # sub set_priorities

sub eval_grammar {
    my $parse          = shift;
    my $grammar        = shift;
    my $preamble       = shift;
    my $default_action = shift;

    my $package = $parse->[Parse::Marpa::Internal::Parse::PACKAGE] =
        sprintf( "Parse::Marpa::P_%x", $parse_number++ );

    $preamble //= $grammar->[Parse::Marpa::Internal::Grammar::PREAMBLE];
    $default_action //=
        $grammar->[Parse::Marpa::Internal::Grammar::DEFAULT_ACTION];

    if ( defined $preamble ) {
        my @warnings;
        local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
        eval( "package " . $package . ";\n" . $preamble );
        my $fatal_error = $@;
        if ($fatal_error or @warnings) {
            Parse::Marpa::Internal::die_on_problems($fatal_error, \@warnings,
                "evaluating preamble",
                "evaluating preamble",
                \$preamble
            );
        }
    }

    compile_regexes($grammar);
    @{$parse}[ LEXERS, LEXABLES_BY_STATE ] =
        set_actions( $grammar, $package, $default_action );
    $parse->[PRIORITIES] = set_priorities($grammar);
    $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
        Parse::Marpa::Internal::Grammar::EVALED;

}

sub Parse::Marpa::Parse::new {
    my $class = shift;

    my $parse = [];

    my $grammar;
    my $default_action;
    my $ambiguous_lex;
    my $preamble;
    my $stream = 0;

    # default for parse is non-volatile, but grammar setting
    # and explicit setting both override
    my $volatile = 0;

    my @grammar_args;
    if (scalar @_ == 1) { $grammar = shift; }
    else {
        my $args = {@_};
        while (my ($arg, $value) = each %$args) {
            given($arg) {
                when ("grammar") { $grammar = $value }
                when ("default_action") { $default_action = $value }
                when ("ambiguous_lex") { $ambiguous_lex = $value }
                when ("volatile") { $volatile = $value }
                when ("preamble") { $preamble = $value }
                when ("stream") { $stream = $value }
                default {
                     push(@grammar_args, $arg, $value);
                }
            }
        }
    }

    croak("No grammar specified") unless defined $grammar;

    my $grammar_class = ref $grammar;
    croak(
        "Don't recognize parse() grammar arg has wrong class: $grammar_class")
        unless $grammar_class eq "Parse::Marpa";

    Parse::Marpa::set($grammar, @grammar_args);
    my $tracing = $grammar->[Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE]
    }

    my $problems = $grammar->[Parse::Marpa::Internal::Grammar::PROBLEMS];
    if ($problems) {
        croak(
            Parse::Marpa::show_problems($grammar),
            "Attempt to parse grammar with fatal problems\n",
            "Marpa cannot proceed",
        );
    }

    if ( $grammar->[Parse::Marpa::Internal::Grammar::ACADEMIC] ) {
        croak(
            "Attempt to parse grammar marked academic\n",
            "Marpa cannot proceed"
        );
    }

    # allow the user to use a grammar "in place"?
    STATE:
    while ( my $state = $grammar->[Parse::Marpa::Internal::Grammar::STATE] )
    {
        last STATE if $state eq Parse::Marpa::Internal::Grammar::EVALED;
        given ($state) {
            when (Parse::Marpa::Internal::Grammar::PERL_RULES) {
                my $compiled_grammar = Parse::Marpa::compile($grammar);
                $grammar = Parse::Marpa::decompile($compiled_grammar);
            }
            when (Parse::Marpa::Internal::Grammar::SOURCE_RULES) {
                my $compiled_grammar = Parse::Marpa::compile($grammar);
                $grammar = Parse::Marpa::decompile($compiled_grammar);
            }
            when (Parse::Marpa::Internal::Grammar::PRECOMPUTED) {
                my $compiled_grammar = Parse::Marpa::compile($grammar);
                $grammar = Parse::Marpa::decompile($compiled_grammar);
            }
            when (Parse::Marpa::Internal::Grammar::COMPILED) {
                eval_grammar( $parse, $grammar, $preamble, $default_action );
            }
            when (Parse::Marpa::Internal::Grammar::IN_USE) {
                croak("Attempt to parse grammar already in use");
            }
            when (Parse::Marpa::Internal::Grammar::NEW) {
                croak("Attempt to parse grammar without rules");
            }
            default {
                croak(
                    "Attempt to parse grammar in inappropriate state\nAttempt to parse ",
                    $state
                );
            }
        }
    }    # while ne EVALED

    $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
        Parse::Marpa::Internal::Grammar::IN_USE;

    $ambiguous_lex =
        $grammar->[Parse::Marpa::Internal::Grammar::AMBIGUOUS_LEX]
        unless defined $ambiguous_lex;

    # volatile can be set, but never unset
    $volatile = $grammar->[Parse::Marpa::Internal::Grammar::VOLATILE]
        unless $volatile;

    my $earley_hash;
    my $earley_set;
    my $item;

    my $SDFA = $grammar->[Parse::Marpa::Internal::Grammar::SDFA];

    # A bit of a cheat here: I rely on an assumption about the numbering
    # of the SDFA states -- specifically, that state 0 contains the
    # start productions.
    my $SDFA0 = $SDFA->[0];
    my $key = pack( "JJ", $SDFA0 + 0, 0 );
    @{$item}[
        Parse::Marpa::Internal::Earley_item::STATE,
        Parse::Marpa::Internal::Earley_item::PARENT,
        Parse::Marpa::Internal::Earley_item::TOKENS,
        Parse::Marpa::Internal::Earley_item::LINKS,
        Parse::Marpa::Internal::Earley_item::SET
        ]
        = ( $SDFA0, 0, [], [], 0 );
    push( @$earley_set, $item );
    $earley_hash->{$key} = $item;

    my $resetting_state =
        $SDFA0->[Parse::Marpa::Internal::SDFA::TRANSITION]->{""};
    if ( defined $resetting_state ) {
        $key = pack( "JJ", $resetting_state, 0 );
        undef $item;
        @{$item}[
            Parse::Marpa::Internal::Earley_item::STATE,
            Parse::Marpa::Internal::Earley_item::PARENT,
            Parse::Marpa::Internal::Earley_item::TOKENS,
            Parse::Marpa::Internal::Earley_item::LINKS,
            Parse::Marpa::Internal::Earley_item::SET
            ]
            = ( $resetting_state, 0, [], [], 0 );
        push( @$earley_set, $item );
        $earley_hash->{$key} = $item;
    }

    @{$parse}[
        DEFAULT_PARSE_SET, CURRENT_SET,       FURTHEST_EARLEME,
        EARLEY_HASHES,     GRAMMAR,           EARLEY_SETS,
        AMBIGUOUS_LEX,
        STREAM,
        LAST_COMPLETED_SET,
        ]
        = (
        0, 0, 0, [$earley_hash],
        $grammar, [$earley_set], $ambiguous_lex,
        $stream,
        -1,
        );

    bless $parse, $class;
}

# Viewing methods, for debugging

sub Parse::Marpa::brief_earley_item {
    my $item = shift;
    my $ii   = shift;
    my ( $state, $parent, $set ) = @{$item}[
        Parse::Marpa::Internal::Earley_item::STATE,
        Parse::Marpa::Internal::Earley_item::PARENT,
        Parse::Marpa::Internal::Earley_item::SET
    ];
    my ( $id, $tag ) = @{$state}[
        Parse::Marpa::Internal::SDFA::ID,
        Parse::Marpa::Internal::SDFA::TAG
    ];
    my $text = $set . ":";
    $text .= ( $ii and defined $tag ) ? ( "St" . $tag ) : ( "S" . $id );
    $text .= "," . $parent;
}

sub show_token_choice {
    my $token = shift;
    my $ii    = shift;
    "[p="
        . Parse::Marpa::brief_earley_item( $token->[0], $ii ) . "; t="
        . $token->[1] . "]";
}

sub show_link_choice {
    my $link = shift;
    my $ii   = shift;
    "[p="
        . Parse::Marpa::brief_earley_item( $link->[0], $ii ) . "; c="
        . Parse::Marpa::brief_earley_item( $link->[1], $ii ) . "]";
}

sub Parse::Marpa::show_earley_item {
    my $item = shift;
    my $ii   = shift;
    my ($tokens,      $links,        $rules,     $rule_choice,
        $link_choice, $token_choice, $value,     $pointer,
        $lhs,         $predecessor,  $successor, $effect,
        )
        = @{$item}[
        Parse::Marpa::Internal::Earley_item::TOKENS,
        Parse::Marpa::Internal::Earley_item::LINKS,
        Parse::Marpa::Internal::Earley_item::RULES,
        Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
        Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
        Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
        Parse::Marpa::Internal::Earley_item::VALUE,
        Parse::Marpa::Internal::Earley_item::POINTER,
        Parse::Marpa::Internal::Earley_item::LHS,
        Parse::Marpa::Internal::Earley_item::PREDECESSOR,
        Parse::Marpa::Internal::Earley_item::SUCCESSOR,
        Parse::Marpa::Internal::Earley_item::EFFECT,
        ];

    my $text = Parse::Marpa::brief_earley_item( $item, $ii );
    $text .= "  predecessor: " . Parse::Marpa::brief_earley_item($predecessor)
        if defined $predecessor;
    $text .= "  successor: " . Parse::Marpa::brief_earley_item($successor)
        if defined $successor;
    $text .= "  effect: " . Parse::Marpa::brief_earley_item($effect)
        if defined $effect;
    my @symbols;
    push( @symbols,
        "pointer: " . $pointer->[Parse::Marpa::Internal::Symbol::NAME] )
        if defined $pointer;
    push( @symbols, "lhs: " . $lhs->[Parse::Marpa::Internal::Symbol::NAME] )
        if defined $lhs;
    $text .= "\n  " . join( "; ", @symbols ) if @symbols;
    $text .= "\n  value: " . Parse::Marpa::show_value( $value, $ii )
        if defined $value;

    if ( defined $tokens and @$tokens ) {
        $text .= "\n  token choice " . $token_choice;
        for my $token (@$tokens) {
            $text .= " " . show_token_choice( $token, $ii );
        }
    }
    if ( defined $links and @$links ) {
        $text .= "\n  link choice " . $link_choice;
        for my $link (@$links) {
            $text .= " " . show_link_choice( $link, $ii );
        }
    }
    if ( defined $rules and @$rules ) {
        $text .= "\n  rule choice " . $rule_choice;
        for my $rule (@$rules) {
            $text .= " [ " . Parse::Marpa::brief_rule($rule) . " ]";
        }
    }
    $text;
}

sub Parse::Marpa::show_earley_set {
    my $earley_set = shift;
    my $ii         = shift;
    my $text       = "";
    for my $earley_item (@$earley_set) {
        $text .= Parse::Marpa::show_earley_item( $earley_item, $ii ) . "\n";
    }
    $text;
}

sub Parse::Marpa::show_earley_set_list {
    my $earley_set_list  = shift;
    my $ii               = shift;
    my $text             = "";
    my $earley_set_count = @$earley_set_list;
    LIST: for ( my $ix = 0; $ix < $earley_set_count; $ix++ ) {
        my $set = $earley_set_list->[$ix];
        next LIST unless defined $set;
        $text .= "Earley Set $ix\n"
            . Parse::Marpa::show_earley_set( $set, $ii );
    }
    $text;
}

sub Parse::Marpa::Parse::show_status {
    my $parse = shift;
    my $ii    = shift;
    my ( $current_set, $furthest_earleme, $earley_set_list ) =
        @{$parse}[ CURRENT_SET, FURTHEST_EARLEME, EARLEY_SETS ];
    my $text =
          "Current Earley Set: "
        . $current_set
        . "; Furthest: "
        . $furthest_earleme . "\n";
    $text .= Parse::Marpa::show_earley_set_list( $earley_set_list, $ii );
}

sub Parse::Marpa::Parse::clear_notations {
    my $parse = shift;
    my ($earley_set_list) = @{$parse}[EARLEY_SETS];
    for my $earley_set (@$earley_set_list) {
        for my $earley_item (@$earley_set) {
            @{$earley_item}[
                Parse::Marpa::Internal::Earley_item::POINTER,
                Parse::Marpa::Internal::Earley_item::RULES,
                Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
                Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                Parse::Marpa::Internal::Earley_item::VALUE,
                Parse::Marpa::Internal::Earley_item::PREDECESSOR,
                Parse::Marpa::Internal::Earley_item::SUCCESSOR,
                Parse::Marpa::Internal::Earley_item::EFFECT,
                Parse::Marpa::Internal::Earley_item::LHS,
                ]
                = ( undef, [], 0, 0, 0, undef, undef, undef, undef, undef, );
        }
    }
}

# check parse?
sub Parse::Marpa::Parse::earleme {
    my $parse = shift;

    my $grammar = $parse->[ Parse::Marpa::Internal::Parse::GRAMMAR ];
    local ($Parse::Marpa::Internal::This::grammar) = $grammar;

    # lexables not checked -- don't use prediction here
    # maybe add this as an option
    my $lexables = Parse::Marpa::Internal::Parse::complete_set($parse);
    return Parse::Marpa::Internal::Parse::scan_set( $parse, @_ );
}

# Returns the position where the parse was exhausted,
# or -1 if the parse is not exhausted

# First arg is the current parse object
# Second arg is ref to string
# Third arg is the length of the portion to be used
sub Parse::Marpa::Parse::text {
    my $parse     = shift;
    my $input_ref = shift;
    my $length    = shift;

    my ( $grammar, $earley_sets, $current_set, 
        $ambiguous_lex, $lexers, )
        = @{$parse}[
        Parse::Marpa::Internal::Parse::GRAMMAR,
        Parse::Marpa::Internal::Parse::EARLEY_SETS,
        Parse::Marpa::Internal::Parse::CURRENT_SET,
        Parse::Marpa::Internal::Parse::AMBIGUOUS_LEX,
        Parse::Marpa::Internal::Parse::LEXERS,
        ];

    local ($Parse::Marpa::Internal::This::grammar) = $grammar;
    my $tracing = $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    my $trace_lex_tries;
    my $trace_lex_matches;
    if ($tracing) {
         $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
         $trace_lex_tries = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_LEX_TRIES ];
         $trace_lex_matches = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_LEX_MATCHES ];
    }

    my ( $symbols, ) =
        @{$grammar}[ Parse::Marpa::Internal::Grammar::SYMBOLS, ];

    $length = length $$input_ref unless defined $length;

    POS: for ( my $pos = ( pos $$input_ref // 0 ); $pos < $length; $pos++ ) {
        my @alternatives;

        # NOTE: Often the number of the earley set, and the idea of
        # lexical position will correspond.  Be careful that Marpa
        # imposes no such requirement, however.

        my $lexables = complete_set($parse);

        if ( $trace_lex_tries and scalar @$lexables ) {
            my $string_to_match = substr( $$input_ref, $pos, 20 );
            $string_to_match
                =~ s/([\x00-\x1F\x7F-\xFF])/sprintf("{%#.2x}", ord($1))/ge;
            say $trace_fh "Match target at $pos: ",
                $string_to_match;
        }

        LEXABLE: for my $lexable (@$lexables) {
            my ($symbol_id) = @{$lexable}[Parse::Marpa::Internal::Symbol::ID];
            if ($trace_lex_tries) {
                print $trace_fh "Trying to match ",
                    $lexable->[Parse::Marpa::Internal::Symbol::NAME],
                    " at $pos\n";
            }

            my $lexer      = $lexers->[$symbol_id];
            my $lexer_type = ref $lexer;
            croak("Illegal type for lexer: undefined")
                unless defined $lexer_type;

            pos $$input_ref = $pos;
            if ( $lexer_type eq "Regexp" ) {
                if ( $$input_ref =~ /$lexer/g ) {
                    my $match = $+{mArPa_match};

                    # my $prefix = $+{mArPa_prefix};
                    # my $suffix = $+{mArPa_suffix};
                    # my $length = length(${^MATCH});
                    my $length = ( pos $$input_ref ) - $pos;
                    croak(
                        "Internal error, zero length token -- this is a Marpa bug"
                    ) unless $length;
                    push( @alternatives, [ $lexable, $match, $length ] );
                    if ($trace_lex_matches) {
                        print $trace_fh
                            "Matched regex for ",
                            $lexable->[Parse::Marpa::Internal::Symbol::NAME],
                            " at $pos: ", $match, "\n";
                    }
                    last LEXABLE unless $ambiguous_lex;
                }    # if match

                next LEXABLE;

            }    # if defined regex

            # If it's a lexable and a regex was not defined, there must be a
            # closure
            croak("Illegal type for lexer: $lexer_type")
                unless $lexer_type eq "CODE";

            my ( $match, $length );
            {
                my @warnings;
                local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
                eval { ($match, $length) = $lexer->($input_ref); };
                my $fatal_error = $@;
                if ($fatal_error or @warnings) {
                    Parse::Marpa::Internal::die_on_problems(
                        $fatal_error, \@warnings,
                        "user supplied lexer",
                        "user supplied lexer for "
                            . $lexable->[Parse::Marpa::Internal::Symbol::NAME]
                            .  " at $pos",
                        \($lexable->[Parse::Marpa::Internal::Symbol::ACTION])
                    );
                }
            }

            next LEXABLE if not defined $match;

            $length //= length $match;

            push( @alternatives, [ $lexable, $match, $length ] );
            if ($trace_lex_matches) {
                print $trace_fh
                    "Matched Closure for ",
                    $lexable->[Parse::Marpa::Internal::Symbol::NAME],
                    " at $pos: ", $match, "\n";
            }

            last LEXABLE unless $ambiguous_lex;

        }    # LEXABLE

        my $active = scan_set( $parse, @alternatives );

        return $pos unless $active;

    }    # POS

    return -1;

}    # sub text

sub Parse::Marpa::Parse::end_input {
    my $parse = shift;

    my (
        $grammar,
        $current_set,
        $last_completed_set,
        $furthest_earleme,
    ) = @{$parse}[
        Parse::Marpa::Internal::Parse::GRAMMAR,
        Parse::Marpa::Internal::Parse::CURRENT_SET,
        Parse::Marpa::Internal::Parse::LAST_COMPLETED_SET,
        Parse::Marpa::Internal::Parse::FURTHEST_EARLEME,
    ];
    local ($Parse::Marpa::Internal::This::grammar) = $grammar;

    return if $last_completed_set >= $furthest_earleme;

    EARLEY_SET: while ($current_set <= $furthest_earleme) {
        Parse::Marpa::Internal::Parse::complete_set($parse);
        $current_set++;
        $parse->[ Parse::Marpa::Internal::Parse::CURRENT_SET ] = $current_set;
    }
}

=begin Apolegetic:

It's bad style, but this routine is in a tight loop and for efficiency
I pull the token alternatives out of @_ one by one as I go in the code,
rather than at the beginning of the subroutine.

The remaining arguments should be a list of token alternatives, as
array references.  The array for each alternative is (token, value,
length), where token is a symbol reference, value can anything
meaningful to the user, and length is the length of this token in
earlemes.

=end Apolegetic:

=cut

# Given a parse object and a list of alternative tokens starting at
# the current earleme, compute the Earley set for that earleme
sub scan_set {
    my $parse = shift;

    my ( $earley_set_list, $earley_hash_list, $grammar, $current_set,
        $furthest_earleme, $exhausted, )
        = @{$parse}[
        EARLEY_SETS,      EARLEY_HASHES, GRAMMAR, CURRENT_SET,
        FURTHEST_EARLEME, EXHAUSTED
        ];
    croak("Attempt to scan tokens on an exhausted parse") if $exhausted;
    my $SDFA = $grammar->[Parse::Marpa::Internal::Grammar::SDFA];

    my $earley_set = $earley_set_list->[$current_set];

    if ( not defined $earley_set ) {
        $earley_set_list->[$current_set] = [];
        if ( $current_set >= $furthest_earleme ) {
            $parse->[Parse::Marpa::Internal::Parse::EXHAUSTED] = $exhausted =
                1;
        }
        else {
            $parse->[CURRENT_SET]++;
        }
        return !$exhausted;
    }

    EARLEY_ITEM: for ( my $ix = 0; $ix < @$earley_set; $ix++ ) {

        my $earley_item = $earley_set->[$ix];
        my ( $state, $parent ) = @{$earley_item}[
            Parse::Marpa::Internal::Earley_item::STATE,
            Parse::Marpa::Internal::Earley_item::PARENT
        ];

        # I allow ambigious tokenization.
        # Loop through the alternative tokens.
        ALTERNATIVE: for my $alternative (@_) {
            my ( $token, $value, $length ) = @$alternative;

            if ( $length <= 0 ) {
                croak(    "Token "
                        . $token->[Parse::Marpa::Internal::Symbol::NAME]
                        . " with bad length "
                        . $length );
            }

            # Make sure it's an allowed terminal symbol.
            # TODO: Must remember to be sure that
            # nulling symbols are never terminals
            unless ( $token->[Parse::Marpa::Internal::Symbol::TERMINAL] ) {
                my $name = $token->[Parse::Marpa::Internal::Symbol::NAME];
                croak(    "Non-terminal "
                        . ( defined $name ? "$name " : "" )
                        . "supplied as token" );
            }

            # compute goto(kernel_state, token_name)
            my $kernel_state =
                $SDFA->[ $state->[Parse::Marpa::Internal::SDFA::ID] ]
                ->[Parse::Marpa::Internal::SDFA::TRANSITION]
                ->{ $token->[Parse::Marpa::Internal::Symbol::NAME] };
            next ALTERNATIVE unless $kernel_state;

            # Create the kernel item and its link.
            my $target_ix = $current_set + $length;
            my $target_earley_hash =
                ( $earley_hash_list->[$target_ix] ||= {} );
            my $target_earley_set = ( $earley_set_list->[$target_ix] ||= [] );
            if ( $target_ix > $furthest_earleme ) {
                $parse->[Parse::Marpa::Internal::Parse::FURTHEST_EARLEME] =
                    $furthest_earleme = $target_ix;
            }
            my $key = pack( "JJ", $kernel_state, $parent );
            my $target_earley_item = $target_earley_hash->{$key};
            unless ( defined $target_earley_item ) {
                @{$target_earley_item}[
                    Parse::Marpa::Internal::Earley_item::STATE,
                    Parse::Marpa::Internal::Earley_item::PARENT,
                    Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Internal::Earley_item::LINKS,
                    Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                    Parse::Marpa::Internal::Earley_item::TOKENS,
                    Parse::Marpa::Internal::Earley_item::SET
                    ]
                    = ( $kernel_state, $parent, 0, [], 0, [], $target_ix );
                $target_earley_hash->{$key} = $target_earley_item;
                push( @$target_earley_set, $target_earley_item );
            }
            push(
                @{  $target_earley_item
                        ->[Parse::Marpa::Internal::Earley_item::TOKENS]
                    },
                [ $earley_item, $value ]
            );

            my $resetting_state =
                $kernel_state->[Parse::Marpa::Internal::SDFA::TRANSITION]
                ->{""};
            next ALTERNATIVE unless defined $resetting_state;
            $key = pack( "JJ", $resetting_state, $target_ix );
            unless ( exists $target_earley_hash->{$key} ) {
                my $new_earley_item;
                @{$new_earley_item}[
                    Parse::Marpa::Internal::Earley_item::STATE,
                    Parse::Marpa::Internal::Earley_item::PARENT,
                    Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Internal::Earley_item::LINKS,
                    Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                    Parse::Marpa::Internal::Earley_item::TOKENS,
                    Parse::Marpa::Internal::Earley_item::SET
                    ]
                    = ( $resetting_state, $target_ix, 0, [], 0, [],
                    $target_ix );
                $target_earley_hash->{$key} = $new_earley_item;
                push( @$target_earley_set, $new_earley_item );
            }

        }    # ALTERNATIVE

    }    # EARLEY_ITEM

    $parse->[CURRENT_SET]++;

    return 1;

}    # sub scan_set

sub complete_set {
    my $parse = shift;

    my ($earley_set_list,   $earley_hash_list,  $grammar,
        $current_set,       $furthest_earleme,  $exhausted,
        $lexables_by_state, $priorities,
        )
        = @{$parse}[
        EARLEY_SETS,       EARLEY_HASHES,
        GRAMMAR,           CURRENT_SET,
        FURTHEST_EARLEME,  EXHAUSTED,
        LEXABLES_BY_STATE,
        PRIORITIES,
        ];
    croak("Attempt to complete another earley set in an exhausted parse")
        if $exhausted;

    my $earley_set  = $earley_set_list->[$current_set];
    my $earley_hash = $earley_hash_list->[$current_set];

    $earley_set ||= [];

    my ( $SDFA, $symbols, $tracing ) = @{$grammar}[
        Parse::Marpa::Internal::Grammar::SDFA,
        Parse::Marpa::Internal::Grammar::SYMBOLS,
        Parse::Marpa::Internal::Grammar::TRACING,
    ];

    my ($trace_fh, $trace_completions);
    if ($tracing) {
        $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
        $trace_completions 
            = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_COMPLETIONS ];
    }

    my $lexable_seen = [];
    $#$lexable_seen = $#$symbols;

    EARLEY_ITEM: for ( my $ix = 0; $ix < @$earley_set; $ix++ ) {

        my $earley_item = $earley_set->[$ix];
        my ( $state, $parent ) = @{$earley_item}[
            Parse::Marpa::Internal::Earley_item::STATE,
            Parse::Marpa::Internal::Earley_item::PARENT
        ];
        my $state_id = $state->[Parse::Marpa::Internal::SDFA::ID];

        for my $lexable ( @{ $lexables_by_state->[$state_id] } ) {
            $lexable_seen->[$lexable] = 1;
        }

        next EARLEY_ITEM if $current_set == $parent;

        COMPLETE_RULE:
        for my $complete_symbol_name (
            @{ $state->[Parse::Marpa::Internal::SDFA::COMPLETE_LHS] } )
        {
            PARENT_ITEM:
            for my $parent_item ( @{ $earley_set_list->[$parent] } ) {
                my ( $parent_state, $grandparent ) = @{$parent_item}[
                    Parse::Marpa::Internal::Earley_item::STATE,
                    Parse::Marpa::Internal::Earley_item::PARENT
                ];
                my $kernel_state =
                    $SDFA->[ $parent_state->[Parse::Marpa::Internal::SDFA::ID]
                    ]->[Parse::Marpa::Internal::SDFA::TRANSITION]
                    ->{$complete_symbol_name};
                next PARENT_ITEM unless defined $kernel_state;

                my $key = pack( "JJ", $kernel_state, $grandparent );
                my $target_earley_item = $earley_hash->{$key};
                unless ( defined $target_earley_item ) {
                    @{$target_earley_item}[
                        Parse::Marpa::Internal::Earley_item::STATE,
                        Parse::Marpa::Internal::Earley_item::PARENT,
                        Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                        Parse::Marpa::Internal::Earley_item::LINKS,
                        Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                        Parse::Marpa::Internal::Earley_item::TOKENS,
                        Parse::Marpa::Internal::Earley_item::SET
                        ]
                        = (
                        $kernel_state, $grandparent, 0, [], 0, [],
                        $current_set
                        );
                    $earley_hash->{$key} = $target_earley_item;
                    push( @$earley_set, $target_earley_item );
                }
                push(
                    @{  $target_earley_item
                            ->[Parse::Marpa::Internal::Earley_item::LINKS]
                        },
                    [ $parent_item, $earley_item ]
                );

                my $resetting_state =
                    $kernel_state->[Parse::Marpa::Internal::SDFA::TRANSITION]
                    ->{""};
                next PARENT_ITEM unless defined $resetting_state;
                $key = pack( "JJ", $resetting_state, $current_set );
                unless ( defined $earley_hash->{$key} ) {
                    my $new_earley_item;
                    @{$new_earley_item}[
                        Parse::Marpa::Internal::Earley_item::STATE,
                        Parse::Marpa::Internal::Earley_item::PARENT,
                        Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                        Parse::Marpa::Internal::Earley_item::LINKS,
                        Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                        Parse::Marpa::Internal::Earley_item::TOKENS,
                        Parse::Marpa::Internal::Earley_item::SET
                        ]
                        = (
                        $resetting_state, $current_set, 0, [], 0, [],
                        $current_set
                        );
                    $earley_hash->{$key} = $new_earley_item;
                    push( @$earley_set, $new_earley_item );
                }

            }    # PARENT_ITEM

        }    # COMPLETE_RULE

    }    # EARLEY_ITEM

    EARLEY_ITEM: for my $earley_item (@$earley_set) {
        my $links =
            $earley_item->[Parse::Marpa::Internal::Earley_item::LINKS];
        my @sorted_links =
            map  { $_->[0] }
            sort { $b->[1] <=> $a->[1] }
            map {
            [   $_,
                $priorities->[
                    $_->[1]->[Parse::Marpa::Internal::Earley_item::STATE]
                    ->[Parse::Marpa::Internal::SDFA::ID]
                ]
            ]
            } @$links;
        $earley_item->[Parse::Marpa::Internal::Earley_item::LINKS] =
            \@sorted_links;
    }

    # TODO: Prove that the completion links are UNIQUE

    # Free memory for the hash
    $earley_hash_list->[$current_set] = undef;

    $parse->[Parse::Marpa::Internal::Parse::DEFAULT_PARSE_SET] = $current_set;
    $parse->[Parse::Marpa::Internal::Parse::LAST_COMPLETED_SET] = $current_set;

    if ($trace_completions) {
        print $trace_fh Parse::Marpa::show_earley_set($earley_set);
    }

    # Dream up some efficiency hack here.  Memoize sorted lexables by state?
    my $lexables = [
        sort {
            $a->[Parse::Marpa::Internal::Symbol::PRIORITY]
                <=> $b->[Parse::Marpa::Internal::Symbol::PRIORITY]
            }
            map { $symbols->[$_] }
            grep { $lexable_seen->[$_] } ( 0 .. $#$symbols )
    ];
    return $lexables;

}    # sub complete_set

sub Parse::Marpa::Parse::show {
    my $parse = shift;
    my $text  = "";

    croak("No parse supplied") unless defined $parse;

    my ( $start_item, $current_parse_set ) = @{$parse}[
        Parse::Marpa::Internal::Parse::START_ITEM,
        Parse::Marpa::Internal::Parse::CURRENT_PARSE_SET,
    ];

    local ($Data::Dumper::Terse)       = 1;

    my $value = $start_item->[Parse::Marpa::Internal::Earley_item::VALUE];
    croak("Parse not evaluated") unless defined $value;

    my ( $rules, $rule_choice ) = @{$start_item}[
        Parse::Marpa::Internal::Earley_item::RULES,
        Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
    ];

    $text .= Parse::Marpa::show_derivation($start_item);

}

sub Parse::Marpa::show_derivation {
    my $item = shift;
    my $text = "";

    RHS_SYMBOL: for ( ;; ) {

        my $data = 0;

        my ($rules,  $rule_choice,  $links,   $link_choice,
            $tokens, $token_choice, $pointer, $value,
            )
            = @{$item}[
            Parse::Marpa::Internal::Earley_item::RULES,
            Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
            Parse::Marpa::Internal::Earley_item::LINKS,
            Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
            Parse::Marpa::Internal::Earley_item::TOKENS,
            Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
            Parse::Marpa::Internal::Earley_item::POINTER,
            Parse::Marpa::Internal::Earley_item::VALUE,
            ];

        last RHS_SYMBOL unless defined $pointer;
        my $symbol_name = $pointer->[Parse::Marpa::Internal::Symbol::NAME];

        if ( defined $rules and $rule_choice <= $#$rules ) {
            my $rule = $rules->[$rule_choice];
            $text
                .= "[ "
                . Parse::Marpa::brief_earley_item($item) . "] "
                . Parse::Marpa::brief_rule($rule) . "\n";
            $data = 1;
        }

        if ( $token_choice <= $#$tokens ) {
            my ( $predecessor, $token ) = @{ $tokens->[$token_choice] };
            $text
                .= "[ "
                . Parse::Marpa::brief_earley_item($item) . "] "
                . "$symbol_name = "
                . Dumper($token);
            $item = $predecessor;
            next RHS_SYMBOL;
        }

        $text .= Parse::Marpa::brief_earley_item($item) . "No data\n"
            unless $data;

        if ( $link_choice <= $#$links ) {
            my ( $predecessor, $cause ) = @{ $links->[$link_choice] };
            $text .= Parse::Marpa::show_derivation($cause);
            $item = $predecessor;
        }

    }

    $text;

}

# returns 1 if it starts OK, undef otherwise
sub Parse::Marpa::Parse::initial {
    my $parse         = shift;
    my $parse_set_arg = shift;

    # TODO: At some point I may need to ensure that evaluation notations are
    # cleared, rather than just assume it.

    # Is the best way to do this?
    my $parse_class = ref $parse;
    my $right_class = "Parse::Marpa::Parse";
    croak(
        "Don't parse argument is class: $parse_class; should be: $right_class"
    ) unless $parse_class eq $right_class;

    my ($grammar,                 $earley_sets,
        $start_item,              $current_parse_set,
        $volatile,
        $stream,
        )
        = @{$parse}[
        Parse::Marpa::Internal::Parse::GRAMMAR,
        Parse::Marpa::Internal::Parse::EARLEY_SETS,
        Parse::Marpa::Internal::Parse::START_ITEM,
        Parse::Marpa::Internal::Parse::CURRENT_PARSE_SET,
        Parse::Marpa::Internal::Parse::VOLATILE,
        Parse::Marpa::Internal::Parse::STREAM,
        ];
    local ($Parse::Marpa::Internal::This::grammar) = $grammar;
    my $tracing = $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    my $trace_iteration_changes;
    if ($tracing) {
        $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
        $trace_iteration_changes = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_ITERATION_CHANGES ];
    }

    local ($Data::Dumper::Terse) = 1;

    if (not $stream) {
         Parse::Marpa::Parse::end_input($parse);
    }
    my $default_parse_set = $parse->[ Parse::Marpa::Internal::Parse::DEFAULT_PARSE_SET ];

    if ( defined $current_parse_set ) {
        my $need_to_clear = $volatile;
        if ( defined $parse_set_arg
            and $parse_set_arg != $current_parse_set )
        {
            $current_parse_set = $parse_set_arg;
            $start_item        = undef;
            $need_to_clear++;
        }
        Parse::Marpa::Parse::clear_notations($parse) if $need_to_clear;
    }

    if ( not defined $current_parse_set ) {
        $start_item = undef;
        $current_parse_set = $parse_set_arg // $default_parse_set;
    }

    # If we already have a start item, use it
    my $start_rule;
    if ( defined $start_item ) {
        my $state = $start_item->[Parse::Marpa::Internal::Earley_item::STATE];
        $start_rule = $state->[Parse::Marpa::Internal::SDFA::START_RULE];
    }

    # Otherwise, look for the start item and start rule
    if ( not defined $start_rule ) {
        my $earley_set = $earley_sets->[$current_parse_set];

        # The start rule, if not nulling, must be a pure links rule
        # (no tokens) because I don't allow tokens to be recognized
        # for the start symbol

        my $item;
        my $rule;

        # mark start items with LHS?
        EARLEY_ITEM: for ( my $ix = 0; $ix <= $#$earley_set; $ix++ ) {
            $item = $earley_set->[$ix];
            my $state = $item->[Parse::Marpa::Internal::Earley_item::STATE];
            $rule = $state->[Parse::Marpa::Internal::SDFA::START_RULE];
            last EARLEY_ITEM if $rule;
        }

        $start_item = $item;
        $start_rule = $rule;
    }

    return unless $start_rule;

    my $previous_value =
        $start_item->[Parse::Marpa::Internal::Earley_item::VALUE];
    return 1 if $previous_value;

    @{$parse}[
        Parse::Marpa::Internal::Parse::START_ITEM,
        Parse::Marpa::Internal::Parse::CURRENT_PARSE_SET,
        ]
        = ( $start_item, $current_parse_set );

    my ($lhs) = @$start_rule[Parse::Marpa::Internal::Rule::LHS];
    my ( $nulling, $null_value ) = @{$lhs}[
        Parse::Marpa::Internal::Symbol::NULLING,
        Parse::Marpa::Internal::Symbol::NULL_VALUE
    ];

    if ($nulling) {
        @{$start_item}[
            Parse::Marpa::Internal::Earley_item::VALUE,
            Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
            Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
            Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
            Parse::Marpa::Internal::Earley_item::RULES,
            Parse::Marpa::Internal::Earley_item::LHS,
            ]
            = ( \$null_value, 0, 0, 0, [$start_rule], $lhs, );
        if ($trace_iteration_changes) {
            print $trace_fh
                "Setting nulling start value of ",
                Parse::Marpa::brief_earley_item($start_item), ", ",
                $lhs->[Parse::Marpa::Internal::Symbol::NAME], " to ",
                Dumper($null_value);
        }
        return 1;
    }

    my $value = initialize_children( $start_item, $lhs );
    @{$start_item}[
        Parse::Marpa::Internal::Earley_item::VALUE,
        Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
        Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
        Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
        Parse::Marpa::Internal::Earley_item::RULES,
        Parse::Marpa::Internal::Earley_item::LHS,
        ]
        = ( \$value, 0, 0, 0, [$start_rule], $lhs, );
    if ($trace_iteration_changes) {
        print $trace_fh "Setting start value of ",
            Parse::Marpa::brief_earley_item($start_item), ", ",
            $lhs->[Parse::Marpa::Internal::Symbol::NAME], " to ",
            Dumper($value);
    }

    1;

}

sub Parse::Marpa::Parse::find_complete_rule {
    my $parse         = shift;
    my $start_earleme = shift;
    my $symbol        = shift;
    my $last_earleme  = shift;

    my ( $default_parse_set, $earley_sets, ) = @{$parse}[
        Parse::Marpa::Internal::Parse::DEFAULT_PARSE_SET,
        Parse::Marpa::Internal::Parse::EARLEY_SETS,
    ];

    # Set up the defaults for undefined arguments
    $start_earleme //= 0;
    $last_earleme  //= $default_parse_set;
    $last_earleme = $default_parse_set if $last_earleme > $default_parse_set;

    # We symbol from the user, so we need to canonicalize it.
    $symbol = canonical_name($symbol) if defined $symbol;

    EARLEME:
    for (
        my $earleme = $last_earleme;
        $earleme >= $start_earleme;
        $earleme--
        )
    {
        my $earley_set = $earley_sets->[$earleme];

        ITEM: for my $earley_item (@$earley_set) {
            my ( $state, $parent ) = @{$earley_item}[
                Parse::Marpa::Internal::Earley_item::STATE,
                Parse::Marpa::Internal::Earley_item::PARENT,
            ];
            next ITEM unless $parent == $start_earleme;
            if ( defined $symbol ) {
                my $complete_rules =
                    $state->[Parse::Marpa::Internal::SDFA::COMPLETE_RULES]
                    ->{$symbol};
                next ITEM unless $complete_rules;
            }
            my $complete_lhs =
                $state->[Parse::Marpa::Internal::SDFA::COMPLETE_LHS];
            next ITEM unless scalar @$complete_lhs;
            return ( $earleme, $complete_lhs );
        }    # ITEM
    }    # EARLEME
    return;
}

sub initialize_children {
    my $item       = shift;
    my $lhs_symbol = shift;

    my $grammar = $Parse::Marpa::Internal::This::grammar;
    my $tracing = $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    my $trace_evaluation_choices;
    my $trace_iteration_changes;
    my $trace_iteration_searches;
    my $trace_values;
 
    if ($tracing) {
        $trace_fh
            = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
        $trace_evaluation_choices
            = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_EVALUATION_CHOICES ];
        $trace_iteration_changes
            = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_ITERATION_CHANGES ];
        $trace_iteration_searches
            = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_ITERATION_SEARCHES ];
        $trace_values
            = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_VALUES ];
    }

    $item->[Parse::Marpa::Internal::Earley_item::LHS] = $lhs_symbol;
    my $lhs_symbol_id = $lhs_symbol->[Parse::Marpa::Internal::Symbol::ID];

    my ( $state, $child_rule_choice ) = @{$item}[
        Parse::Marpa::Internal::Earley_item::STATE,
        Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
    ];

    if ( not defined $child_rule_choice ) {
        $child_rule_choice = 0;
    }
    my $child_rules =
        $state->[Parse::Marpa::Internal::SDFA::COMPLETE_RULES]
        ->[$lhs_symbol_id];
    my $rule = $child_rules->[$child_rule_choice];
    if ( $trace_evaluation_choices and scalar @$child_rules > 1 )
    {
        my ( $set, $parent ) = @{$item}[
            Parse::Marpa::Internal::Earley_item::SET,
            Parse::Marpa::Internal::Earley_item::PARENT,
        ];
        say $trace_fh "Choose rule ", $child_rule_choice,
            " of ", ( scalar @$child_rules ), " at earlemes ", $parent, "-",
            $set, ": ", Parse::Marpa::brief_rule($rule);
        for ( my $ix = 0; $ix <= $#$child_rules; $ix++ ) {
            my $choice = $child_rules->[$ix];
            say $trace_fh
                "Rule choice $ix at $parent-$set: ",
                Parse::Marpa::brief_rule($choice);
        }
    }
    local ($Parse::Marpa::This::rule) = $rule;
    my ($rhs) = @{$rule}[Parse::Marpa::Internal::Rule::RHS];

    local ($Parse::Marpa::This::v) = [];    # to store values in

    my @work_entries;

    CHILD:
    for ( my $child_number = $#$rhs; $child_number >= 0; $child_number-- ) {

        my $child_symbol = $rhs->[$child_number];
        my $nulling =
            $child_symbol->[Parse::Marpa::Internal::Symbol::NULLING];

        if ($nulling) {
            $Parse::Marpa::This::v->[$child_number] =
                $child_symbol->[Parse::Marpa::Internal::Symbol::NULL_VALUE];
            next CHILD;
        }

        my ( $tokens, $links, $previous_value, $previous_predecessor,
            $item_set, )
            = @{$item}[
            Parse::Marpa::Internal::Earley_item::TOKENS,
            Parse::Marpa::Internal::Earley_item::LINKS,
            Parse::Marpa::Internal::Earley_item::VALUE,
            Parse::Marpa::Internal::Earley_item::PREDECESSOR,
            Parse::Marpa::Internal::Earley_item::SET,
            ];

        if ( defined $previous_value ) {
            $Parse::Marpa::This::v->[$child_number] = $$previous_value;
            $item = $previous_predecessor;
            next CHILD;
        }

        unless ( defined $child_rules ) {
            $child_rules       = [];
            $child_rule_choice = 0;
        }

        if (@$tokens) {
            my ( $predecessor, $value ) = @{ $tokens->[0] };

            if ( $trace_evaluation_choices
                and scalar @$tokens > 1 )
            {
                my ( $set, $parent ) = @{$item}[
                    Parse::Marpa::Internal::Earley_item::SET,
                    Parse::Marpa::Internal::Earley_item::PARENT,
                ];
                say $trace_fh "Choose token 0 of ",
                    ( scalar @$tokens ), " at earlemes ", $parent, "-", $set,
                    ": ", show_token_choice( $tokens->[0] );
                for ( my $ix = 1; $ix <= $#$tokens; $ix++ ) {
                    my $choice = $tokens->[$ix];
                    say $trace_fh
                        "Alternative token choice $ix at $parent-$set: ",
                        show_token_choice($choice);
                }
            }

            @{$item}[
                Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
                Parse::Marpa::Internal::Earley_item::RULES,
                Parse::Marpa::Internal::Earley_item::VALUE,
                Parse::Marpa::Internal::Earley_item::PREDECESSOR,
                Parse::Marpa::Internal::Earley_item::POINTER,
                ]
                = (
                0, 0, $child_rule_choice, $child_rules, \$value, $predecessor,
                $child_symbol,
                );
            if ($trace_iteration_changes) {
                my $predecessor_set =
                    $predecessor->[ Parse::Marpa::Internal::Earley_item::SET,
                    ];
                print $trace_fh
                    "Initializing token value of ",
                    Parse::Marpa::brief_earley_item($item), ", ",
                    $child_symbol->[Parse::Marpa::Internal::Symbol::NAME],
                    " at ", $predecessor_set, "-", $item_set, " to ",
                    Dumper($value);
            }
            $Parse::Marpa::This::v->[$child_number] = $value;
            weaken(
                $predecessor->[Parse::Marpa::Internal::Earley_item::SUCCESSOR]
                    = $item );
            $item = $predecessor;
            next CHILD;
        }

        # We've eliminated nulling symbols and symbols caused by tokens,
        # so we have to have a symbol caused by a completion

        my ( $predecessor, $cause ) = @{ $links->[0] };
        weaken( $cause->[Parse::Marpa::Internal::Earley_item::EFFECT] =
                $item );

        if ( $trace_evaluation_choices
            and scalar @$links > 1 )
        {
            my ( $set, $parent ) = @{$item}[
                Parse::Marpa::Internal::Earley_item::SET,
                Parse::Marpa::Internal::Earley_item::PARENT,
            ];
            say $trace_fh "Choose link 0 of ",
                ( scalar @$links ), " at earlemes ", $parent, "-", $set, ": ",
                show_link_choice( $links->[0] );
            for ( my $ix = 1; $ix <= $#$links; $ix++ ) {
                my $choice = $links->[$ix];
                say $trace_fh
                    "Alternative link choice $ix at $parent-$set: ",
                    show_link_choice( $links->[$ix] );
            }
        }

        # my $value = initialize_children($cause, $child_symbol);
        my $work_entry =
            [ $predecessor, $item, $child_number, $cause, $child_symbol, ];

        # for efficiency push (right-to-left evaluation) is the default
        push( @work_entries, $work_entry );

        @{$item}[
            Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
            Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
            Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
            Parse::Marpa::Internal::Earley_item::RULES,

            # Parse::Marpa::Internal::Earley_item::VALUE,
            Parse::Marpa::Internal::Earley_item::PREDECESSOR,
            Parse::Marpa::Internal::Earley_item::POINTER,
            ]
            = (
            0,                  0,
            $child_rule_choice, $child_rules,

            # \$value,
            $predecessor,
            $child_symbol,
            );

        # $Parse::Marpa::This::v->[ $child_number ] = $value;
        weaken(
            $predecessor->[Parse::Marpa::Internal::Earley_item::SUCCESSOR] =
                $item );
        $item = $predecessor;

    }    # CHILD

    for my $work_entry (@work_entries) {
        my ( $predecessor_item, $effect_item, $rhs_index, $cause,
            $child_symbol, )
            = @$work_entry;
        my $value = $Parse::Marpa::This::v->[$rhs_index] =
            initialize_children( $cause, $child_symbol );
        $effect_item->[Parse::Marpa::Internal::Earley_item::VALUE] = \$value;
        if ($trace_iteration_searches) {
            my $predecessor_set =
                $predecessor_item->[Parse::Marpa::Internal::Earley_item::SET];
            my $item_set =
                $effect_item->[Parse::Marpa::Internal::Earley_item::SET];
            print $trace_fh
                "Initializing caused value of ",
                Parse::Marpa::brief_earley_item($effect_item), ", ",
                $child_symbol->[Parse::Marpa::Internal::Symbol::NAME], " at ",
                $predecessor_set, "-", $item_set, " to ", Dumper($value);
        }
    }

    my $closure = $rule->[Parse::Marpa::Internal::Rule::CLOSURE];
    my @warnings;
    return $closure unless defined $closure;

    my $result;
    {
        my @warnings;
        local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
        $result = eval { $closure->() };
        my $fatal_error = $@;
        if ($fatal_error or @warnings) {
            Parse::Marpa::Internal::die_on_problems($fatal_error, \@warnings,
                "computing value",
                "computing value for rule: "
                    . Parse::Marpa::brief_original_rule($rule),
                \($rule->[Parse::Marpa::Internal::Rule::ACTION])
            );
        }
    }

    if ($trace_values) {
        say $trace_fh "Rule ", Parse::Marpa::brief_rule($rule), "; value:\n", $result;
    }

    $result;

}

sub Parse::Marpa::Parse::value {
    my $parse = shift;

    my $start_item = $parse->[Parse::Marpa::Internal::Parse::START_ITEM];
    return unless defined $start_item;
    my $value_ref = $start_item->[Parse::Marpa::Internal::Earley_item::VALUE];
    croak("No value defined") unless defined $value_ref;
    return $value_ref;
}

# TODO Add check to ensure that the argument is an evaluated parse.
sub Parse::Marpa::Parse::next {
    my $parse = shift;

    croak("No parse supplied") unless defined $parse;
    my $parse_class = ref $parse;
    my $right_class = "Parse::Marpa::Parse";
    croak(
        "Don't parse argument is class: $parse_class; should be: $right_class"
    ) unless $parse_class eq $right_class;

    my ( $grammar, $start_item, $current_parse_set, $volatile, )
        = @{$parse}[
        Parse::Marpa::Internal::Parse::GRAMMAR,
        Parse::Marpa::Internal::Parse::START_ITEM,
        Parse::Marpa::Internal::Parse::CURRENT_PARSE_SET,
        Parse::Marpa::Internal::Parse::VOLATILE,
        ];
    croak("Parse not initialized: no start item") unless defined $start_item;
    my $start_value =
        $start_item->[Parse::Marpa::Internal::Earley_item::VALUE];
    croak("Parse not initialized: no start value")
        unless defined $start_value;

    local ($Parse::Marpa::Internal::This::grammar) = $grammar;
    my $tracing = $grammar->[ Parse::Marpa::Internal::Grammar::TRACING ];
    my $trace_fh;
    my $trace_iteration_changes;
    my $trace_iteration_searches;
    if ($tracing) {
        $trace_fh = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_FILE_HANDLE ];
        $trace_iteration_changes = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_ITERATION_CHANGES ];
        $trace_iteration_searches = $grammar->[ Parse::Marpa::Internal::Grammar::TRACE_ITERATION_SEARCHES ];
    }

    local ($Data::Dumper::Terse) = 1;

    Parse::Marpa::Parse::clear_notations($parse) if $volatile;

    # find the "bottom left corner item", by following predecessors,
    # and causes when there is no predecessor

    EVALUATION: for ( ;; ) {
        my $item             = $start_item;
        my $find_left_corner = 1;

        # Look for an item we can (potentially) iterate.
        ITERATION_CANDIDATE: for ( ;; ) {

            # if we set the flag to find the item in the bottom
            # left hand corner, do so
            LEFT_CORNER_CANDIDATE: while ($find_left_corner) {

                # undefine the values as we go along
                $item->[Parse::Marpa::Internal::Earley_item::VALUE] = undef;

                my $predecessor =
                    $item->[Parse::Marpa::Internal::Earley_item::PREDECESSOR];

                # Follow the predecessors all the way until
                # just before the prediction.  The prediction
                # is the item whose "parent" is the same as its
                # Earley set.
                if (defined $predecessor
                    ->[Parse::Marpa::Internal::Earley_item::POINTER] )
                {
                    $item = $predecessor;
                    next LEFT_CORNER_CANDIDATE;
                }

                # At the far left end, see if we have a cause (or
                # child) item.  If so, descend.
                my ( $link_choice, $links ) = @{$item}[
                    Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Internal::Earley_item::LINKS,
                ];
                last LEFT_CORNER_CANDIDATE if $link_choice > $#$links;
                $item = $links->[$link_choice]->[1];
                if ($trace_iteration_searches) {
                    print $trace_fh
                        "Seeking left corner at ",
                        Parse::Marpa::brief_earley_item($item), "\n";
                }

            }    # LEFT_CORNER_CANDIDATE

            # We have our candidate, now try to iterate it,
            # exhausting the rule choice if necessary

            # TODO: is this block necessary ?

            my ($token_choice, $tokens,      $link_choice,
                $links,        $rule_choice, $rules,
                )
                = @{$item}[
                Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                Parse::Marpa::Internal::Earley_item::TOKENS,
                Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                Parse::Marpa::Internal::Earley_item::LINKS,
                Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
                Parse::Marpa::Internal::Earley_item::RULES,
                ];

            # If we can increment the token_choice, this is our
            # candidate
            if ( $token_choice < $#$tokens ) {
                $token_choice++;
                $item->[Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE] =
                    $token_choice;
                last ITERATION_CANDIDATE;
            }

            # If we can increment the link_choice, this is our
            # candidate
            if ( $link_choice < $#$links ) {
                $link_choice++;
                $item->[Parse::Marpa::Internal::Earley_item::LINK_CHOICE] =
                    $link_choice;
                last ITERATION_CANDIDATE;
            }

            # Iterate rule, if possible
            if ( $rule_choice < $#$rules ) {

                @{$item}[
                    Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
                    Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                    ]
                    = ( ++$rule_choice, 0, 0, );

                if ($trace_iteration_changes) {
                    print $trace_fh
                        "Incremented rule choice for ",
                        Parse::Marpa::brief_earley_item($item), ", ",
                        Parse::Marpa::brief_earley_item($item), " to ",
                        $rule_choice, "\n";
                }

                last ITERATION_CANDIDATE;

            }

            # This candidate could not be iterated.  Set up to look
            # for another.

            @{$item}[
                Parse::Marpa::Internal::Earley_item::VALUE,
                Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
                ]
                = ( undef, 0 );

            my ( $successor, $effect ) = @{$item}[
                Parse::Marpa::Internal::Earley_item::SUCCESSOR,
                Parse::Marpa::Internal::Earley_item::EFFECT,
            ];

            $find_left_corner = 0;

            if ( defined $successor ) {
                $item = $successor;
                if ($trace_iteration_changes) {
                    print $trace_fh
                        "Trying to iterate successor ",
                        Parse::Marpa::brief_earley_item($item), "\n";
                }

                # Did the successor have a cause?  If so iterate from
                # it or the "left corner" below it
                my ( $link_choice, $links ) = @{$item}[
                    Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Internal::Earley_item::LINKS,
                ];
                if ( $link_choice <= $#$links ) {
                    @{$item}[
                        Parse::Marpa::Internal::Earley_item::VALUE,
                        Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
                        ]
                        = ( undef, 0 );
                    $item             = $links->[$link_choice]->[1];
                    $find_left_corner = 1;
                }
                next ITERATION_CANDIDATE;

            }

            # If no more candidates, we are finished with all the
            # evaluations for this parse
            return unless defined $effect;

            $item = $effect;
            if ($trace_iteration_searches) {
                print $trace_fh
                    "Trying to iterate effect ",
                    Parse::Marpa::brief_earley_item($item), "\n";
            }

        }    # ITERATION_CANDIDATE

        # We've found and iterated an item.
        # Now try to evaluate it.
        # First, climb the tree, recalculating
        # all the successor and effect values.

        my $reason = "Iterating";

        STEP_UP_TREE: for ( ;; ) {

            RESET_VALUES: {

                my ($token_choice, $tokens,  $link_choice, $links,
                    $rule_choice,  $pointer, $item_set
                    )
                    = @{$item}[
                    Parse::Marpa::Internal::Earley_item::TOKEN_CHOICE,
                    Parse::Marpa::Internal::Earley_item::TOKENS,
                    Parse::Marpa::Internal::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Internal::Earley_item::LINKS,
                    Parse::Marpa::Internal::Earley_item::RULE_CHOICE,
                    Parse::Marpa::Internal::Earley_item::POINTER,
                    Parse::Marpa::Internal::Earley_item::SET,
                    ];

                if ( $token_choice <= $#$tokens ) {
                    my ( $predecessor, $value ) =
                        @{ $tokens->[$token_choice] };
                    @{$item}[
                        Parse::Marpa::Internal::Earley_item::VALUE,
                        Parse::Marpa::Internal::Earley_item::PREDECESSOR,
                        ]
                        = ( \$value, $predecessor, );
                    if ($trace_iteration_changes) {
                        my $predecessor_set = $predecessor
                            ->[ Parse::Marpa::Internal::Earley_item::SET, ];
                        print $trace_fh $reason,
                            " token choice for ",
                            Parse::Marpa::brief_earley_item($item), " to ",
                            $token_choice, ", ",
                            $pointer->[Parse::Marpa::Internal::Symbol::NAME],
                            " at ", $predecessor_set, "-", $item_set, " = ",
                            Dumper($value);
                    }
                    weaken( $predecessor
                            ->[Parse::Marpa::Internal::Earley_item::SUCCESSOR]
                            = $item );
                    last RESET_VALUES;
                }

                if ( $link_choice <= $#$links ) {
                    my ( $predecessor, $cause ) = @{ $links->[$link_choice] };
                    weaken(
                        $cause->[Parse::Marpa::Internal::Earley_item::EFFECT]
                            = $item );
                    my $value = initialize_children( $cause, $pointer );
                    @{$item}[
                        Parse::Marpa::Internal::Earley_item::VALUE,
                        Parse::Marpa::Internal::Earley_item::PREDECESSOR,
                        ]
                        = ( \$value, $predecessor, );
                    if ($trace_iteration_changes) {
                        my $predecessor_set = $predecessor
                            ->[ Parse::Marpa::Internal::Earley_item::SET, ];
                        print $trace_fh $reason,
                            " cause choice for ",
                            Parse::Marpa::brief_earley_item($item), " to ",
                            $link_choice, ", ",
                            $pointer->[Parse::Marpa::Internal::Symbol::NAME],
                            " at ", $predecessor_set, "-", $item_set, " = ",
                            Dumper($value);
                    }
                    weaken( $predecessor
                            ->[Parse::Marpa::Internal::Earley_item::SUCCESSOR]
                            = $item );
                    last RESET_VALUES;
                }

            }    # RESET_VALUES

            $reason = "Recalculating Parent";

            my ( $successor, $effect ) = @{$item}[
                Parse::Marpa::Internal::Earley_item::SUCCESSOR,
                Parse::Marpa::Internal::Earley_item::EFFECT,
            ];

            if ( defined $successor ) {
                $item = $successor;
                next STEP_UP_TREE;
            }

            # If no successor or effect, we're at the top of the tree
            last STEP_UP_TREE unless defined $effect;

            $item = $effect;

        }    # STEP_UP_TREE

        # Initialize everything else left unvalued.
        Parse::Marpa::Parse::initial( $parse, $current_parse_set );

        # Rejected evaluations are not yet implemented.
        # Therefore this evaluation pass succeeded.
        return 1;

    }    # EVALUATION

    return;

}

sub Parse::Marpa::show_value {
    my $value_ref = shift;
    my $ii        = shift;
    return "none" unless defined $value_ref;
    my $value = $$value_ref;
    return "undef" unless defined $value;
    if ($ii) {
        my $type = ref $value;
        return $type if $type;
    }
    return "$value";
}

package Parse::Marpa::Source;

sub gen_symbol_from_regex {
    my $regex = shift;
    state $uniq_number;
    state %regex;
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


=head1 NAME

Parse::Marpa - (pre-Alpha) Jay Earley's general parsing algorithm, with LR(0) precomputation

=head1 VERSION

Pre-alpha Version

This is strictly a developer's version.
Nothing useful will be found here,
and the documentation is also inchoate.
Those not developing this module will want to wait
for at least a released, beta version.

=cut

=head1 SYNOPSIS

    TO DO
    ...

=head1 DESCRIPTION

Notes toward the documentation: Point out that lexing honors the pos setting
and must not alter it, even on successful match.  Warn user of counter-intuitive
behavior.

Warn user that in the lex patterns all named captures beginning with any
"MARPA_" in any capitalization variant ("marpa_", "MaRpA_", etc.) is
reserved.

=head1 AUTHOR

Jeffrey Kegler

=head1 DEPENDENCIES

Requires Perl 5.8.

=head1 BUGS

Please report any bugs or feature requests to
C<bug-parse-marpa at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Parse-Marpa>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Parse::Marpa
    
You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Parse-Marpa>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Parse-Marpa>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Parse-Marpa>

=item * Search CPAN

L<http://search.cpan.org/dist/Parse-Marpa>

=back

=head1 ACKNOWLEDGMENTS

Marpa is
the parser described in John Aycock and R.
Nigel Horspool's "Practical Earley Parsing",
I<The Computer Journal>,
Vol. 45, No. 6, 2002, pp. 620-630.
I've made significant changes to it,
which are documented separately (L<Parse::Marpa::ALGORITHM>).
Aycock and Horspool, for their part,
built on the algorithm discovered by Jay Earley,
and described in his
"An efficient context-free parsing algorithm",
I<Communications of the
Association for Computing Machinery>,
13:2:94-102, 1970.

In writing the Pure Perl version of Marpa, I benefited from studying
the work of Francois Desarmenien (C<Parse::Yapp>), 
Damian Conway (C<Parse::RecDescent>) and
Graham Barr (C<Scalar::Util>).

=head1 COPYRIGHT & LICENSE

Copyright 2007 Jeffrey Kegler, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Parse::Marpa

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
