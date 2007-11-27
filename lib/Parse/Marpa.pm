package Parse::Marpa;

require 5.009005;

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

use Carp;
use Scalar::Util qw(weaken);
use Data::Dumper;

our $VERSION = '0.001_048';
$VERSION = eval $VERSION;

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

package Parse::Marpa::Symbol;

use constant ID              => 0;
use constant NAME            => 1;
use constant LHS             => 2;    # rules with this as the lhs,
                                      # as a ref to an array of rule refs
use constant RHS             => 3;    # rules with this in the rhs,
                                      # as a ref to an array of rule refs
use constant START_REACHABLE => 4;    # reachable from start symbol?
use constant INPUT_REACHABLE => 5;    # reachable from input symbol?
use constant START           => 6;    # is one of the start symbols?
use constant REGEX           => 7;    # regex, for terminals; undef otherwise
use constant NULLING         => 8;    # always is null?
use constant NULLABLE        => 9;    # can match null?
use constant NULL_VALUE      => 10;    # value when null
use constant NULL_ALIAS      => 11;   # for a non-nulling symbol,
                                      # ref of a its nulling alias,
                                      # if there is one
                                      # otherwise undef
use constant TERMINAL        => 12;   # terminal?
use constant CLOSURE         => 13;   # closure to do lexing
use constant ORDER           => 14;   # order, for lexing
use constant COUNTED         => 15;   # used on rhs of counted rule?

package Parse::Marpa::Rule;

use constant ID                       => 0;
use constant NAME                     => 1;
use constant LHS                      => 2;    # ref of the left hand symbol
use constant RHS                      => 3;    # array of symbol refs
use constant NULLABLE                 => 4;    # can match null?
use constant START_REACHABLE          => 5;    # reachable from start symbol?
use constant INPUT_REACHABLE          => 6;    # reachable from input symbol?
use constant NULLING                  => 7;    # always matches null?
use constant USEFUL                   => 8;    # use this rule in NFA?
use constant ACTION                   => 9;    # action for this rule
use constant CLOSURE                  => 10;   # closure for evaluating this rule
use constant ORIGINAL_RULE            => 11;   # for a rewritten rule, the original
use constant ORDER                    => 12;   # the order in which rules are to
                                               # be tried -- not necessarily unique
use constant HAS_CHAF_LHS             => 13;   # has CHAF internal symbol as lhs?
use constant HAS_CHAF_RHS             => 14;   # has CHAF internal symbol on rhs?

package Parse::Marpa::NFA;

use constant ID   => 0;
use constant NAME => 1;
use constant ITEM => 2;          # an LR(0) item
use constant TRANSITION => 3;    # the transitions, as a hash
                                 # from symbol name to NFA states

package Parse::Marpa::SDFA;

use constant ID             => 0;
use constant NAME           => 1;
use constant NFA_STATES     => 2;    # in an SDFA: an array of NFA states
use constant TRANSITION     => 3;    # the transitions, as a hash
                                     # from symbol name to SDFA states
use constant COMPLETE_LHS   => 4;    # an array of the lhs's of complete rules
use constant COMPLETE_RULES => 5;    # an array of lists of the complete rules,
                                     # indexed by lhs
use constant START_RULE     => 6;    # the start rule
use constant TAG            => 7;    # implementation-independant tag
use constant LEXABLES       => 8;    # lexable symbols for this state

package Parse::Marpa::LR0_item;

use constant RULE     => 0;
use constant POSITION => 1;

package Parse::Marpa::Grammar;

use constant ID              => 0; # number of this grammar
use constant NAME            => 1; # namespace special to this grammar
use constant RULES           => 2;     # array of rule refs
use constant SYMBOLS         => 3;     # array of symbol refs
use constant RULE_HASH       => 4;     # hash by name of symbol refs
use constant SYMBOL_HASH     => 5;     # hash by name of symbol refs
use constant START           => 6;     # ref to start symbol
use constant NFA             => 7;     # array of states
use constant SDFA            => 8;     # array of states
use constant SDFA_BY_NAME    => 9;     # hash from SDFA name to SDFA reference
use constant NULLABLE_SYMBOL => 10;    # array of refs of the nullable symbols
use constant ACADEMIC        => 11;    # true if this is a textbook grammar,
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
use constant VOLATILE           => 20; # default callback for showing location

package Parse::Marpa::This;

package Parse::Marpa;

my $grammar_number = 0;

# The lexables theoretically should be numbered within
# grammars, but I number them globally for efficiency.
# Other numbers (like the symbol ID #'s) will overflow
# first, so running out of numbers is not likely to be
# an issue.
my $terminal_number = 0;

my $default_default_lex_prefix = "";
my $default_default_lex_suffix = "";

# Constructor

sub new {
    my $class = shift;
    my %args  = @_;

    my $rules;
    my $terminals;
    my $start;

    # Academic grammar?  An "academic grammar" is one, usually from a textbook, which we are using
    # to debug the NFA and SDFA logic.  We leave it unchanged.  Since we don't augment it, we can't
    # parse with this grammar.  It's only useful to test the NFA and SDFA logic
    my $academic = 0;
    my $default_null_value;
    my $default_action;
    my $default_lex_prefix = $default_default_lex_prefix;
    my $default_lex_suffix = $default_default_lex_suffix;
    my $ambiguous_lex = 1;
    my $trace_fh = *STDERR;
    my $trace_rules = 0;
    my $location_callback = sub {
        my $earleme = shift;
        "Earleme " . $earleme;
    };
    my $volatile = 1;

    my %arg_logic = (
        "rules"              => sub { $rules     = $_[0] },
        "terminals"          => sub { $terminals = $_[0] },
        "start"              => sub { $start     = $_[0] },
        "academic"           => sub { $academic  = $_[0] },
        "default_null_value" => sub { $default_null_value = $_[0] },
        "default_action"    => sub { $default_action = $_[0] },
        "default_lex_prefix" => sub { $default_lex_prefix = $_[0] },
        "default_lex_suffix" => sub { $default_lex_suffix = $_[0] },
        "ambiguous_lex"      => sub { $ambiguous_lex = $_[0] },
        "trace_file_handle"  => sub { $trace_fh = $_[0] },
        "trace_rules"        => sub { $trace_rules = $_[0] },
        "location_callback"  => sub { $location_callback = $_[0] },
        "volatile"           => sub { $volatile = $_[0] },
    );

    while ( my ( $arg, $value ) = each %args ) {
        my $closure = $arg_logic{$arg};
        croak("Undefined argument to ", $class, "::new: ", $arg)
            unless defined $closure;
        $closure->($value);
    }

    croak("No rules specified")        unless defined $rules;
    croak("No terminals specified")    unless defined $terminals;
    croak("No start symbol specified") unless defined $start;

    my $grammar = [];
    # Note: this limits the number of grammar to the number of integers --
    # not likely to be a big problem.
    my $namespace = sprintf("Parse::Marpa::G_%x", $grammar_number);
    @{$grammar}[
        Parse::Marpa::Grammar::ID,
        Parse::Marpa::Grammar::NAME,
        Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::SYMBOL_HASH,
        Parse::Marpa::Grammar::RULES,
        Parse::Marpa::Grammar::RULE_HASH,
        Parse::Marpa::Grammar::SDFA_BY_NAME,
        Parse::Marpa::Grammar::ACADEMIC,
        Parse::Marpa::Grammar::DEFAULT_NULL_VALUE,
        Parse::Marpa::Grammar::DEFAULT_ACTION,
        Parse::Marpa::Grammar::DEFAULT_LEX_PREFIX,
        Parse::Marpa::Grammar::DEFAULT_LEX_SUFFIX,
        Parse::Marpa::Grammar::AMBIGUOUS_LEX,
        Parse::Marpa::Grammar::TRACE_FILE_HANDLE,
        Parse::Marpa::Grammar::TRACE_RULES,
        ] = (
            $grammar_number++,
            $namespace,
            [], {}, [], {}, {},
            $academic,
            $default_null_value,
            $default_action,
            $default_lex_prefix,
            $default_lex_suffix,
            $ambiguous_lex,
            $trace_fh,
            $trace_rules,
        );
    bless( $grammar, $class );

    add_user_rules( $grammar, $rules );
    add_user_terminals( $grammar, $terminals );
    compile( $grammar, $start )
}

sub compile {
    my $grammar = shift;
    my $start = shift;

    my $academic = $grammar->[ Parse::Marpa::Grammar::ACADEMIC ];

    nulling($grammar);
    nullable($grammar);
    input_reachable($grammar);
    set_start( $grammar, $start );
    start_reachable($grammar);
    if ($academic) {
        setup_academic_grammar($grammar);
    }
    else {
        rewrite_as_CHAF($grammar);
    }
    create_NFA($grammar);
    create_SDFA($grammar);

    $grammar;
}

# Viewing Methods (for debugging)

sub show_symbol {
    my $symbol = shift;
    my $text   = "";
    $text .= sprintf "%d: %s, lhs=[%s], rhs=[%s]",
        $symbol->[Parse::Marpa::Symbol::ID],
        $symbol->[Parse::Marpa::Symbol::NAME],
        join( " ",
        map { $_->[Parse::Marpa::Rule::ID] }
            @{ $symbol->[Parse::Marpa::Symbol::LHS] } ),
        join( " ",
        map { $_->[Parse::Marpa::Rule::ID] }
            @{ $symbol->[Parse::Marpa::Symbol::RHS] } );
    if ( not $symbol->[Parse::Marpa::Symbol::INPUT_REACHABLE] ) {
        $text .= " !upreach";
    }
    if ( not $symbol->[Parse::Marpa::Symbol::START_REACHABLE] ) {
        $text .= " !downreach";
    }
    if ( $symbol->[Parse::Marpa::Symbol::NULLABLE] ) { $text .= " nullable"; }
    if ( $symbol->[Parse::Marpa::Symbol::NULLING] )  { $text .= " nulling"; }
    $text .= "\n";
}

sub show_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Grammar::SYMBOLS];
    my $text    = "";
    for my $symbol_ref (@$symbols) {
        $text .= show_symbol($symbol_ref);
    }
    $text;
}

sub show_nulling_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Grammar::SYMBOLS];
    join( " ",
        sort map { $_->[Parse::Marpa::Symbol::NAME] }
            grep { $_->[Parse::Marpa::Symbol::NULLING] } @$symbols );
}

sub show_nullable_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Grammar::NULLABLE_SYMBOL];
    join( " ", sort map { $_->[Parse::Marpa::Symbol::NAME] } @$symbols );
}

sub show_input_reachable_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Grammar::SYMBOLS];
    join( " ",
        sort map { $_->[Parse::Marpa::Symbol::NAME] }
            grep { $_->[Parse::Marpa::Symbol::INPUT_REACHABLE] } @$symbols );
}

sub show_start_reachable_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Parse::Marpa::Grammar::SYMBOLS];
    join( " ",
        sort map { $_->[Parse::Marpa::Symbol::NAME] }
            grep { $_->[Parse::Marpa::Symbol::START_REACHABLE] } @$symbols );
}

sub brief_rule {
    my $rule = shift;
    my ( $lhs, $rhs, $rule_id )
        = @{$rule}[
            Parse::Marpa::Rule::LHS,
            Parse::Marpa::Rule::RHS,
            Parse::Marpa::Rule::ID
        ];
    my $text .= $rule_id . ": " . $lhs->[Parse::Marpa::Symbol::NAME] . " ->";
    if (@$rhs) {
        $text .= " "
            . join( " ", map { $_->[Parse::Marpa::Symbol::NAME] } @$rhs );
    }
    $text;
}

sub show_rule {
    my $rule = shift;

    my ( $rhs, $input_reachable, $start_reachable, $nullable,
        $nulling, $useful )
        = @{$rule}[
        Parse::Marpa::Rule::RHS,
        Parse::Marpa::Rule::INPUT_REACHABLE,
        Parse::Marpa::Rule::START_REACHABLE,
        Parse::Marpa::Rule::NULLABLE,
        Parse::Marpa::Rule::NULLING,
        Parse::Marpa::Rule::USEFUL
        ];
    my $text    = brief_rule($rule);
    my @comment = ();

    if ( not (@$rhs) )          { push( @comment, "empty" ); }
    if ( not $input_reachable ) { push( @comment, "!upreach" ); }
    if ( not $start_reachable ) { push( @comment, "!downreach" ); }
    if ($nullable)              { push( @comment, "nullable" ); }
    if ($nulling)               { push( @comment, "nulling" ); }
    if ( not $useful )          { push( @comment, "!useful" ); }
    if (@comment) {
        $text .= " " . join( " ", "/*", @comment, "*/" );
    }
    $text .= "\n";
}

sub show_rules {
    my $grammar = shift;
    my $rules   = $grammar->[Parse::Marpa::Grammar::RULES];
    my $text;

    for my $rule (@$rules) {
        $text .= show_rule($rule);
    }
    $text;
}

sub show_item {
    my $item = shift;
    my $text = "";
    if ( not defined $item ) {
        $text .= "/* empty */";
    }
    else {
        my ( $rule, $position ) =
            @{$item}[ Parse::Marpa::LR0_item::RULE,
            Parse::Marpa::LR0_item::POSITION ];
        my @names = (
            $rule->[Parse::Marpa::Rule::LHS]->[Parse::Marpa::Symbol::NAME] );
        push( @names,
            map { $_->[Parse::Marpa::Symbol::NAME] }
                @{ $rule->[Parse::Marpa::Rule::RHS] } );
        splice( @names, $position + 1, 0, "." );
        splice( @names, 1, 0, "::=" );
        $text .= join( " ", @names );
    }
    $text;
}

sub show_NFA_state {
    my $state = shift;
    my ( $name, $item, $transition ) = @{$state}[
        Parse::Marpa::NFA::NAME, Parse::Marpa::NFA::ITEM,
        Parse::Marpa::NFA::TRANSITION
    ];
    my $text .= $name . ": " . show_item($item) . "\n";
    for my $symbol_name ( sort keys %$transition ) {
        my $transition_states = $transition->{$symbol_name};
        $text
            .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => "
            . join( " ",
            map { $_->[Parse::Marpa::NFA::NAME] } @$transition_states )
            . "\n";
    }
    $text;
}

sub show_NFA {
    my $grammar = shift;
    my $text    = "";
    my $NFA     = $grammar->[Parse::Marpa::Grammar::NFA];
    for my $state (@$NFA) {
        $text .= show_NFA_state($state);
    }
    $text;
}

sub show_SDFA_state {
    my $state = shift;
    my $tags  = shift;

    my $text = "";
    my (
        $id, $name, $NFA_states, $transition, $tag, $lexables,
    ) = @{$state}[
        Parse::Marpa::SDFA::ID,         Parse::Marpa::SDFA::NAME,
        Parse::Marpa::SDFA::NFA_STATES, Parse::Marpa::SDFA::TRANSITION,
        Parse::Marpa::SDFA::TAG,
        Parse::Marpa::SDFA::LEXABLES,
    ];

    $text .= defined $tags ? "St" . $tag : "S" . $id;
    $text .= ": " . $name . "\n";
    for my $NFA_state (@$NFA_states) {
        my $item = $NFA_state->[Parse::Marpa::NFA::ITEM];
        $text .= show_item($item) . "\n";
    }
    if (@$lexables) {
        $text
            .= "lexables: "
            . join(" ", sort map {
                $_->[ Parse::Marpa::Symbol::NAME ]
            } @$lexables)
            . "\n"
    }
    for my $symbol_name ( sort keys %$transition ) {
        my ( $to_id, $to_name ) =
            @{ $transition->{$symbol_name} }[ Parse::Marpa::SDFA::ID,
            Parse::Marpa::SDFA::NAME ];
        $text
            .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => "
            . (defined $tags ? "St" . $tags->[ $to_id ] : "S" . $to_id)
            . " ("
            . $to_name . ")\n";
    }
    $text;
}

sub tag_SDFA {
    my $grammar = shift;
    my $SDFA = $grammar->[Parse::Marpa::Grammar::SDFA];
    return if defined $SDFA->[0]->[ Parse::Marpa::SDFA::TAG ];
    my $tag = 0;
    for my $state (
        sort {
            $a-> [ Parse::Marpa::SDFA::NAME ]
            cmp $b-> [ Parse::Marpa::SDFA::NAME ]
        }
        @$SDFA
    ) {
        $state->[ Parse::Marpa::SDFA::TAG ] = $tag++;
    }
}

sub show_SDFA {
    my $grammar = shift;
    my $text = "";
    my $SDFA = $grammar->[Parse::Marpa::Grammar::SDFA];
    for my $state (@$SDFA) { $text .= show_SDFA_state($state); }
    $text;
}

sub show_ii_SDFA {
    my $grammar = shift;
    my $text = "";
    my $SDFA = $grammar->[Parse::Marpa::Grammar::SDFA];
    my $tags;
    tag_SDFA($grammar);

    for my $state (@$SDFA) {
        $tags->[ $state->[ Parse::Marpa::SDFA::ID ] ]
             = $state->[ Parse::Marpa::SDFA::TAG ];
    }
    for my $state (
        map { $_->[0] } 
        sort { $a->[1] <=> $b->[1] } 
        map { [ $_, $_->[ Parse::Marpa::SDFA::TAG ] ] }
        @$SDFA
    ) {
        $text .= show_SDFA_state($state, $tags);
    }
    $text;
}

# Accessor Methods

sub get_symbol {
    my $grammar     = shift;
    my $name        = shift;
    my $symbol_hash = $grammar->[Parse::Marpa::Grammar::SYMBOL_HASH];
    defined $symbol_hash ? $symbol_hash->{$name} : undef;
}

# Mutator Methods

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
    my $grammar = shift;
    my $name    = shift;
    my $lexer = shift;
    my ($regex, $prefix, $suffix);
    my $closure;

    if (defined ref $lexer) {
        ($regex, $prefix, $suffix) = @$lexer;
    } else {
        $closure = $lexer;
    }

    my ( $symbol_hash, $symbols,
        $default_null_value,
        $default_lex_prefix,
        $default_lex_suffix,
    ) = @{$grammar}[
            Parse::Marpa::Grammar::SYMBOL_HASH,
            Parse::Marpa::Grammar::SYMBOLS,
            Parse::Marpa::Grammar::DEFAULT_NULL_VALUE,
            Parse::Marpa::Grammar::DEFAULT_LEX_PREFIX,
            Parse::Marpa::Grammar::DEFAULT_LEX_SUFFIX,
        ];

    my $compiled_regex;
    if ( defined $regex ) {
        if ( "" =~ $regex ) {
            croak("Attempt to add nullable terminal: $name");
        }
        $prefix //= $default_lex_prefix;
        $suffix //= $default_lex_suffix;
        $compiled_regex = qr/
            \G
            (?<mArPa_prefix>$prefix)
            (?<mArPa_match>$regex)
            (?<mArPa_suffix>$suffix)
        /xms;
    } elsif (defined $closure) {
       croak("Terminal closures not yet implemented");
    }

    # I allow redefinition of a LHS symbol as a terminal
    # I need to test that this works, or disallow it
    my $symbol = $symbol_hash->{$name};
    if ( defined $symbol) {

        if ($symbol->[ Parse::Marpa::Symbol::TERMINAL ]) {
            croak("Attempt to add terminal twice: $name");
        }

        @{$symbol}[
            Parse::Marpa::Symbol::INPUT_REACHABLE,
            Parse::Marpa::Symbol::NULLING,
            Parse::Marpa::Symbol::REGEX,
            Parse::Marpa::Symbol::CLOSURE,
            Parse::Marpa::Symbol::TERMINAL,
            Parse::Marpa::Symbol::ORDER,
        ] = (
            1, 0,
            $compiled_regex,
            $closure,
            1,
            $terminal_number++,
        );

        return;
    }

    my $symbol_count = @$symbols;
    my $new_symbol = [];
    @{$new_symbol}[
        Parse::Marpa::Symbol::ID,
        Parse::Marpa::Symbol::NAME,
        Parse::Marpa::Symbol::LHS,
        Parse::Marpa::Symbol::RHS,
        Parse::Marpa::Symbol::NULLABLE,
        Parse::Marpa::Symbol::INPUT_REACHABLE,
        Parse::Marpa::Symbol::NULLING,
        Parse::Marpa::Symbol::NULL_VALUE,
        Parse::Marpa::Symbol::REGEX,
        Parse::Marpa::Symbol::TERMINAL,
        Parse::Marpa::Symbol::ORDER,
    ] = (
        $symbol_count, $name, [], [],
        0, 1, 0,
        $default_null_value,
        $compiled_regex,
        1,
        $terminal_number++,
    );

    push( @$symbols, $new_symbol );
    weaken( $symbol_hash->{$name} = $new_symbol );
}

sub assign_symbol {
    my $grammar = shift;
    my $name    = shift;
    my (
        $symbol_hash, $symbols, $default_null_value,
    ) = @{$grammar}[
        Parse::Marpa::Grammar::SYMBOL_HASH,
        Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::DEFAULT_NULL_VALUE,
    ];

    my $symbol_count = @$symbols;
    my $symbol       = $symbol_hash->{$name};
    if ( not defined $symbol ) {
        @{$symbol}[
            Parse::Marpa::Symbol::ID,  Parse::Marpa::Symbol::NAME,
            Parse::Marpa::Symbol::LHS, Parse::Marpa::Symbol::RHS,
            Parse::Marpa::Symbol::NULL_VALUE,
        ] = (
            $symbol_count, $name, [], [],
            $default_null_value,
        );
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
    my $grammar      = shift;
    my $lhs_name  = shift;
    my $rhs_names = shift;
    my $action    = shift;
    my ( $rule_hash ) = @{$grammar}[ Parse::Marpa::Grammar::RULE_HASH ];

    my $lhs_symbol = assign_symbol( $grammar, canonical_name($lhs_name) );
    $rhs_names //= [];
    my $rhs_symbols = [ map { assign_symbol( $grammar, canonical_name($_) ); } @$rhs_names ];

    # Don't allow the user to duplicate a rule
    my $rule_key =
        join( ",", map { $_->[Parse::Marpa::Symbol::ID ] } ( $lhs_symbol, @$rhs_symbols ) );
    croak( "Duplicate rule: ", $lhs_name, " -> ", join(" ", @$rhs_names ) )
        if exists $rule_hash->{$rule_key};

    $rule_hash->{$rule_key} = 1;

    add_rule( $grammar, $lhs_symbol, $rhs_symbols, $action);
}

sub add_rule {
    my $grammar = shift;
    my $lhs     = shift;
    my $rhs     = shift;
    my $action  = shift;

    my (
        $rules,
        $package,
        $trace_rules,
        $trace_fh,
    ) = @{$grammar}[
        Parse::Marpa::Grammar::RULES,
        Parse::Marpa::Grammar::NAME,
        Parse::Marpa::Grammar::TRACE_RULES,
        Parse::Marpa::Grammar::TRACE_FILE_HANDLE,
    ];
    my $rule_count = @$rules;
    my $new_rule   = [];
    my $nulling = @$rhs ? undef : 1;
    @{$new_rule}[
        Parse::Marpa::Rule::ID,
        Parse::Marpa::Rule::NAME,
        Parse::Marpa::Rule::LHS,
        Parse::Marpa::Rule::RHS,
        Parse::Marpa::Rule::NULLABLE,
        Parse::Marpa::Rule::INPUT_REACHABLE,
        Parse::Marpa::Rule::NULLING,
        Parse::Marpa::Rule::ACTION,
        Parse::Marpa::Rule::ORDER,
        ]
        = (
        $rule_count, "rule $rule_count",
        $lhs, $rhs,
        $nulling, $nulling, $nulling,
        $action,
        $rule_count,
        );

    # if this is a nulling rule with an action,
    # we get the null_value of the lhs from that
    if ($nulling and $action) {
        local($Parse::Marpa::This::v) = [];
        if (defined $action) {
            {
                local $SIG{__WARN__} = sub {0};
                $lhs->[ Parse::Marpa::Symbol::NULL_VALUE ]
                    = eval (
                        "package " . $package . ";\n"
                        . $action
                    );
            }
            if ($@) {
                croak("Compile time error evaluating null value for ",
                    $lhs->[ Parse::Marpa::Symbol::NAME ],
                    ":\n", $@);
            }
        }
    }

    push( @$rules, $new_rule );
    {
        my $lhs_rules = $lhs->[Parse::Marpa::Symbol::LHS];
        weaken( $lhs_rules->[ scalar @$lhs_rules ] = $new_rule );
    }
    if ($nulling) {
        @{$lhs}[
            Parse::Marpa::Symbol::NULLABLE,
            Parse::Marpa::Symbol::INPUT_REACHABLE
            ]
            = ( 1, 1 );
    }
    else {
        my $last_symbol = [];
        SYMBOL: for my $symbol ( sort @$rhs ) {
            next SYMBOL if $symbol == $last_symbol;
            my $rhs_rules = $symbol->[Parse::Marpa::Symbol::RHS];
            weaken( $rhs_rules->[ scalar @$rhs_rules ] = $new_rule );
            $last_symbol = $symbol;
        }
    }
    if ($trace_rules) {
        print $trace_fh
            "Added rule #", $#$rules, ": ",
                 $lhs->[ Parse::Marpa::Symbol::NAME ], " -> ",
                 join(" ", map { $_->[ Parse::Marpa::Symbol::NAME ]} @$rhs),
                 "\n";
    }
    $new_rule;
}

# add one or more rules
sub add_user_rules {
    my $grammar = shift;
    my $rules = shift;

    RULE: for my $rule (@$rules) {

	given (ref $rule) {
	    when ("ARRAY") {
		my $arg_count = @$rule;
		# This warning can be removed if this interface remains
		# internal
		if ( $arg_count > 3 or $arg_count < 1) {
		    croak("Rule has $arg_count arguments: "
			. join(", ", map { defined $_ ? $_ : "undef" } @$rule) . "\n"
			. "Rule must have from 1 to 3 arguments"
		    );
		}
		my ($lhs, $rhs, $action) = @$rule;
                add_user_rule( $grammar, $lhs, $rhs, $action);

	    }
	    when ("HASH") {
	        add_rules_from_hash($grammar, $rule)
	    }
	    default { croak("Invalid rule reftype ", ($_ ? $_ : "undefined")) }
	}

    } # RULE

}

sub add_rules_from_hash {
    my $grammar = shift;
    my $options = shift;

    my ($lhs_name, $rhs_names, $action);
    my ($min, $max, $separator_name);
    my $proper_separation = 0;
    my $keep_separation = 0;
    my $left_associative = 1;

    while (my ($option, $value) = each(%$options)) {
	given ($option) {
	    when ("rhs") { $rhs_names = $value }
	    when ("lhs") { $lhs_name = $value }
	    when ("action") { $action = $value }
	    when ("min") { $min = $value }
	    when ("max") { $max = $value }
	    when ("separator") { $separator_name = $value }
	    when ("proper_separation") { $proper_separation = $value }
	    when ("keep_separation") { $keep_separation = $value }
	    when ("left_associative") { $left_associative = $value }
	    when ("right_associative") { $left_associative = !$value }
	    default { croak("Unknown option in counted rule: $option") }
	}
    }

    # Take care of nulling rules
    if (scalar @$rhs_names == 0) {
	add_user_rule($grammar, $lhs_name, $rhs_names, $action);
        return;
    }

    # Take of obviously bad min, max values
    if (defined $max and $max <= 0) {
       croak("rule max count is $max, not greater than zero");
    }
    if (defined $min and $min < 0) {
       croak("rule min count is $min, less than zero");
    }

    # Ensure min is correctly defined
    if (not defined $min) {
        given($max) {
	    when (undef) { $min = $max = 1; }
	    default {
	       croak("rule max count is defined ($max), but no rule minium");
	    }
	}
    }

    # This is an ordinary, non-counted rule,
    # which we'll take care of first as a special case
    if (defined $max and $max == 1 and $min == 1) {
	if ($max <= 1 and defined $separator_name) {
	    croak("separator defined for rule without repetitions");
	}
        add_user_rule( $grammar, $lhs_name, $rhs_names, $action );
        return;
    }

    if (defined $max) {
	croak("rule max count ($max) count is less than minium ($min)")
	   if $max < $min;
	croak("Too many symbols on rhs for counted rule") if scalar @$rhs_names != 1;
	my $rhs_name = pop @$rhs_names;

	# specifically counted rules
        my $new_rule;
	for my $count ( $min .. $max ) {
	    my $proper_counted_rhs;
	    my $separator_terminated_rhs;
	    my @separated_rhs = ($rhs_name);
	    push(@separated_rhs, $separator_name) if defined $separator_name;
	    given ($count) {
	        when (0) { $proper_counted_rhs = [ ] }
	        default {
		    $proper_counted_rhs = [ (@separated_rhs) x ($count - 1), $rhs_name ];
                    if (not $proper_separation and defined $separator_name) {
                        $separator_terminated_rhs = [ (@separated_rhs) x ($count) ];
                    }
		}
	    }
            # no change to @Parse::Marpa::This::v needed for action
            if (defined $separator_name and not $keep_separation) {
                $action =
                    q{ $Parse::Marpa::This::v = [
                        @{$Parse::Marpa::This:v}[
                           grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                        ]
                    }
                    . $action;
            }
	    $new_rule = add_user_rule($grammar, $lhs_name, $proper_counted_rhs, $action);
            if ($separator_terminated_rhs) {
                add_user_rule($grammar, $lhs_name, $separator_terminated_rhs, $action);
            }
	}

        # There will be at least one rhs symbol since we take the last rule created
        # and max >= 1
        my $rhs = $new_rule->[Parse::Marpa::Rule::RHS]->[0];
        $rhs->[Parse::Marpa::Symbol::COUNTED] = 1;
        if (defined $separator_name)
        {
            my $separator = $new_rule->[Parse::Marpa::Rule::RHS]->[1];
            $separator->[Parse::Marpa::Symbol::COUNTED] = 1;
        }

        return;

    } # min and max both defined

    # At this point we know that max is undefined, and that min must be

    # Right now we're doing this right associative.  Add option later to be
    # left associative?

    # nulling rule is special case
    if ($min == 0) {
        my $rule_action;
        given ($action) {
            when (undef) { $rule_action = undef }
            default {
               $rule_action = q{ $Parse::Marpa::This::v = []; } . $action
            }
        }
	add_user_rule($grammar, $lhs_name, [ ], $rule_action);
        $min = 1;
    }

    croak("Only one rhs symbol allowed for counted rule") if scalar @$rhs_names != 1;

    # create the rhs symbol
    my $rhs_name = pop @$rhs_names;
    my $canonical_rhs_name = canonical_name($rhs_name);
    my $rhs = assign_symbol($grammar, $canonical_rhs_name);
    $rhs->[ Parse::Marpa::Symbol::COUNTED ] = 1;

    # create the separator symbol, if we're using one
    my $separator;
    my $canonical_separator_name;
    if (defined $separator_name) {
        $canonical_separator_name = canonical_name($separator_name);
        $separator = assign_symbol($grammar, $canonical_separator_name);
        $separator->[ Parse::Marpa::Symbol::COUNTED ] = 1;
    }

    # create the sequence symbol
    my $sequence_name = $canonical_rhs_name . "[Seq][$min-*]";
    $sequence_name .= "[Sep][" . $canonical_separator_name . "]" if defined $separator_name;
    my $sequence = assign_symbol($grammar, $sequence_name);

    my $lhs = assign_symbol($grammar, canonical_name($lhs_name));

    # Don't allow the user to duplicate a rule
    # I'm pretty general here -- I consider a sequence rule a duplicate is rhs, lhs
    # and separator are the same.  I may want to get more fancy, but save that
    # for later.
    {
        my $rule_hash = $grammar->[ Parse::Marpa::Grammar::RULE_HASH ];
        my @key_rhs = defined $separator ? ($rhs, $separator, $rhs) : ($rhs);
        my $rule_key = join( ",", map { $_->[Parse::Marpa::Symbol::ID] } ($lhs, @key_rhs) );
        croak( "Duplicate rule: ", $lhs_name, " -> ", join(",", @$rhs_names) )
            if exists $rule_hash->{$rule_key};
        $rule_hash->{$rule_key} = 1;
    }

    # The following rules make evaluations volatile
    $grammar->[ Parse::Marpa::Grammar::VOLATILE ] = 1;

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
            } else {
                $rule_action = q{
                    TAIL: for (;;) {
                        my $tail = pop @$Parse::Marpa::This::v;
                        last TAIL unless scalar @$tail;
                        push(@$Parse::Marpa::This::v, @$tail);
                    }
                }
            }
            $rule_action .= $action
        }
    }
    add_rule(
        $grammar,
        $lhs,
        [ $sequence ],
        $rule_action,
    );
    if (defined $separator and not $proper_separation) {
        unless ($keep_separation) {
            $rule_action =
                q{ pop @$Parse::Marpa::This::v; } .
                $rule_action
        }
        add_rule(
            $grammar,
            $lhs,
            [ $sequence, $separator, ],
            $rule_action,
        );
    }

    my @separated_rhs = ($rhs);
    push(@separated_rhs, $separator) if defined $separator;

    # minimal sequence rule
    my $counted_rhs = [ (@separated_rhs) x ($min - 1), $rhs ];
 
    if ($left_associative) {
        if (defined $separator and not $keep_separation) {
            $rule_action = q{
                [
                    [],
                    @{$Parse::Marpa::This::v}[
                        grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                    ]
                ]
            }
        } else {
            $rule_action = q{
                unshift(@$Parse::Marpa::This::v, []);
                $Parse::Marpa::This::v 
            }
        }
    } else {
        if (defined $separator and not $keep_separation) {
            $rule_action = q{
                [
                    @{$Parse::Marpa::This::v}[
                        grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                    ],
                    []
                ]
            }
        } else {
            $rule_action = q{
                push(@$Parse::Marpa::This::v, []);
                $Parse::Marpa::This::v 
            }
        }
    }
 
    add_rule(
        $grammar,
        $sequence,
        $counted_rhs,
        $rule_action,
    );

    # iterating sequence rule
    $rule_action =
        (defined $separator and not $keep_separation)
        ?  q{
            [
                @{$Parse::Marpa::This::v}[
                   grep { !($_ % 2) } (0 .. $#$Parse::Marpa::This::v)
                ],
            ]
        }
        : q{
            $Parse::Marpa::This::v
        };
    my @iterating_rhs = (@separated_rhs, $sequence);
    if ($left_associative) {
        @iterating_rhs = reverse @iterating_rhs;
    }
    add_rule(
        $grammar,
        $sequence,
        (\@iterating_rhs),
        $rule_action,
    );

} # sub add_rules_from_hash

sub add_user_terminals {
    my $grammar  = shift;
    my $terminals = shift;

    TERMINAL: for my $terminal (@$terminals) {
        my $arg_count = @$terminal;
        if ( $arg_count > 2 or $arg_count < 1) {
            croak("terminal must have from 1 or 2 arguments");
        }
        my ($lhs_name, $lexer) = @$terminal;
        add_user_terminal( $grammar, $lhs_name, $lexer);
    }
}

sub add_user_terminal {
    my $grammar = shift;
    my $lhs_name = shift;
    my $lexer = shift;

    add_terminal( $grammar, canonical_name($lhs_name), $lexer );
}

sub set_start {
    my $grammar    = shift;
    my $start_name = shift;

    my $symbol_hash = $grammar->[Parse::Marpa::Grammar::SYMBOL_HASH];
    my $start  = $symbol_hash->{$start_name};

    if ( not defined $start ) {
        croak( "start symbol " . $start_name . " not defined\n" );
    }

    my ($lhs, $rhs, $terminal, $input_reachable) = @{$start}[
        Parse::Marpa::Symbol::LHS,
        Parse::Marpa::Symbol::RHS,
        Parse::Marpa::Symbol::TERMINAL,
        Parse::Marpa::Symbol::INPUT_REACHABLE,
    ];

    if ( not scalar @$lhs and not $terminal ) {
        croak( "start symbol " . $start_name . " not on LHS of any rule\n" );
    }

    if ( not $input_reachable ) {
        croak( "start symbol " . $start_name . " not input reachable\n" );
    }

    $grammar->[Parse::Marpa::Grammar::START] = $start;
}

# return list of rules reachable from the start symbol;
sub start_reachable {
    my $grammar = shift;
    my $start   = $grammar->[Parse::Marpa::Grammar::START];

    $start->[Parse::Marpa::Symbol::START_REACHABLE] = 1;
    my $symbol_work_set = [$start];
    my $rule_work_set   = [];

    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

        SYMBOL_PASS: while ( my $work_symbol = shift @$symbol_work_set ) {
            my $rules_produced = $work_symbol->[Parse::Marpa::Symbol::LHS];
            PRODUCED_RULE: for my $rule (@$rules_produced) {

                next PRODUCED_RULE
                    if defined $rule->[Parse::Marpa::Rule::START_REACHABLE];

                # assume nullable until we hit an unmarked or unreachable symbol
                $rule->[Parse::Marpa::Rule::START_REACHABLE] = 1;
                $work_to_do++;
                push( @$rule_work_set, $rule );

            }
        }    # SYMBOL_PASS

        RULE: while ( my $work_rule = shift @$rule_work_set ) {
            my $rhs_symbol = $work_rule->[Parse::Marpa::Rule::RHS];

            RHS: for my $symbol (@$rhs_symbol) {

                next RHS
                    if
                    defined $symbol->[Parse::Marpa::Symbol::START_REACHABLE];
                $symbol->[Parse::Marpa::Symbol::START_REACHABLE] = 1;
                $work_to_do++;

                push( @$symbol_work_set, $symbol );
            }

        }    # RULE

    }    # work_to_do loop

}

sub input_reachable {
    my $grammar = shift;

    my ( $rules, $symbols ) =
        @{$grammar}[ Parse::Marpa::Grammar::RULES,
        Parse::Marpa::Grammar::SYMBOLS ];

    # if a symbol's nullability could not be determined, it was unreachable
    # all nullable symbols are reachable
    for my $symbol (@$symbols) {
        if ( not defined $_->[Parse::Marpa::Symbol::NULLABLE] ) {
            $_->[Parse::Marpa::Symbol::INPUT_REACHABLE] = 0;
        }
        if ( $_->[Parse::Marpa::Symbol::NULLABLE] ) {
            $_->[Parse::Marpa::Symbol::INPUT_REACHABLE] = 1;
        }
    }

    # if a rule's nullability could not be determined, it was unreachable
    # all nullable rules are reachable
    for my $rule (@$rules) {
        if ( not defined $rule->[Parse::Marpa::Rule::NULLABLE] ) {
            $_->[Parse::Marpa::Symbol::INPUT_REACHABLE] = 0;
        }
        if ( $rule->[Parse::Marpa::Rule::NULLABLE] ) {
            $_->[Parse::Marpa::Symbol::INPUT_REACHABLE] = 1;
        }
    }

    my $symbol_work_set = [];
    $#$symbol_work_set = $#$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = $#$rules;

    for my $symbol_id (
        grep {
            defined $symbols->[$_]->[Parse::Marpa::Symbol::INPUT_REACHABLE]
        } ( 0 .. $#$symbols )
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    }
    for my $rule_id (
        grep {
            defined $rules->[$_]->[Parse::Marpa::Rule::INPUT_REACHABLE]
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

            my $rules_producing = $work_symbol->[Parse::Marpa::Symbol::RHS];
            PRODUCING_RULE: for my $rule (@$rules_producing) {

                # no work to do -- this rule already has nullability marked
                next PRODUCING_RULE
                    if defined $rule->[Parse::Marpa::Rule::INPUT_REACHABLE];

                # assume nullable until we hit an unmarked or unreachable symbol
                my $rule_reachable = 1;

                # are all symbols on the RHS of this rule bottom marked?
                RHS_SYMBOL:
                for my $rhs_symbol ( @{ $rule->[Parse::Marpa::Rule::RHS] } )
                {
                    my $reachable =
                        $rhs_symbol->[Parse::Marpa::Symbol::INPUT_REACHABLE];

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
                    $rule->[Parse::Marpa::Rule::INPUT_REACHABLE] =
                        $rule_reachable;
                    $work_to_do++;
                    $rule_work_set->[ $rule->[Parse::Marpa::Rule::ID] ] = 1;
                }

            }
        }    # SYMBOL_PASS

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#$rule_work_set ) )
        {
            my $work_rule = $rules->[$rule_id];
            $rule_work_set->[$rule_id] = 0;
            my $lhs_symbol = $work_rule->[Parse::Marpa::Rule::LHS];

            # no work to do -- this symbol already has reachability marked
            next RULE
                if
                defined $lhs_symbol->[Parse::Marpa::Symbol::INPUT_REACHABLE];

            # assume unreachable until we hit an unmarked or non-nullable symbol
            my $symbol_reachable = 0;

            LHS_RULE:
            for my $rule ( @{ $lhs_symbol->[Parse::Marpa::Symbol::LHS] } )
            {

                my $reachable = $rule->[Parse::Marpa::Rule::INPUT_REACHABLE];

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
                $lhs_symbol->[Parse::Marpa::Symbol::INPUT_REACHABLE] =
                    $symbol_reachable;
                $work_to_do++;
                $symbol_work_set->[ $lhs_symbol->[Parse::Marpa::Symbol::ID] ]
                    = 1;
            }

        }    # RULE

    }    # work_to_do loop

}

sub nulling {
    my $grammar = shift;

    my ( $rules, $symbols, $default_null_value ) =
        @{$grammar}[
            Parse::Marpa::Grammar::RULES,
            Parse::Marpa::Grammar::SYMBOLS,
            Parse::Marpa::Grammar::DEFAULT_NULL_VALUE,
    ];

    my $symbol_work_set = [];
    $#$symbol_work_set = $#$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = $#$rules;

    for my $rule_id (
        map  { $_->[Parse::Marpa::Rule::ID] }
        grep { $_->[Parse::Marpa::Rule::NULLING] } @$rules
    ) {
        $rule_work_set->[$rule_id] = 1;
    }

    for my $symbol_id (
        map  { $_->[ Parse::Marpa::Symbol::ID ] }
        grep { $_->[ Parse::Marpa::Symbol::NULLING ] } @$symbols
    ) {
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
            my $lhs_symbol = $work_rule->[Parse::Marpa::Rule::LHS];

            # no work to do -- this symbol already is marked one way or the other
            next RULE if defined $lhs_symbol->[Parse::Marpa::Symbol::NULLING];

            # assume nulling until we hit an unmarked or non-nulling symbol
            my $symbol_nulling = \$default_null_value;

            # make sure that all rules for this lhs are nulling
            LHS_RULE:
            for my $rule ( @{ $lhs_symbol->[Parse::Marpa::Symbol::LHS] } )
            {

                my $nulling = $rule->[Parse::Marpa::Rule::NULLING];

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
                $lhs_symbol->[Parse::Marpa::Symbol::NULLING] = $symbol_nulling;
                $work_to_do++;

                $symbol_work_set->[ $lhs_symbol->[Parse::Marpa::Symbol::ID] ]
                    = 1;
            }

        }    # RULE

        SYMBOL_PASS:
        for my $symbol_id ( grep { $symbol_work_set->[$_] }
            ( 0 .. $#$symbol_work_set ) )
        {
            my $work_symbol = $symbols->[$symbol_id];
            $symbol_work_set->[$symbol_id] = 0;

            my $rules_producing = $work_symbol->[Parse::Marpa::Symbol::RHS];
            PRODUCING_RULE: for my $rule (@$rules_producing) {

                # no work to do -- this rule already has nulling marked
                next PRODUCING_RULE
                    if defined $rule->[Parse::Marpa::Rule::NULLING];

                # assume nulling until we hit an unmarked or unreachable symbol
                my $rule_nulling = 1;

                # are all symbols on the RHS of this rule marked?
                RHS_SYMBOL:
                for my $rhs_symbol ( @{ $rule->[Parse::Marpa::Rule::RHS] } )
                {
                    my $nulling =
                        $rhs_symbol->[Parse::Marpa::Symbol::NULLING];

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
                    $rule->[Parse::Marpa::Rule::NULLING] = $rule_nulling;
                    $work_to_do++;
                    $rule_work_set->[ $rule->[Parse::Marpa::Rule::ID] ] = 1;
                }

            }
        }    # SYMBOL_PASS

    }    # work_to_do loop

}

sub nullable {
    my $grammar = shift;
    my ( $rules, $symbols ) =
        @{$grammar}[ Parse::Marpa::Grammar::RULES,
        Parse::Marpa::Grammar::SYMBOLS ];

    my $work_to_do =
        1;    # boolean to track if current pass has changed anything

    my $symbol_work_set = [];
    $#$symbol_work_set = @$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = @$rules;

    for my $symbol_id (
        map  { $_->[Parse::Marpa::Symbol::ID] }
        grep {
            $_->[Parse::Marpa::Symbol::NULLABLE]
            or $_->[Parse::Marpa::Symbol::NULLING]
        } @$symbols
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    }
    for my $rule_id (
        map  { $_->[Parse::Marpa::Rule::ID] }
        grep { defined $_->[Parse::Marpa::Rule::NULLABLE] } @$rules
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
            my $rules_producing = $work_symbol->[Parse::Marpa::Symbol::RHS];

            PRODUCING_RULE: for my $rule (@$rules_producing) {

                # assume nullable until we hit an unmarked or non-nullable symbol
                my $rule_nullable = 1;

                # no work to do -- this rule already has nullability marked
                next PRODUCING_RULE
                    if defined $rule->[Parse::Marpa::Rule::NULLABLE];

                # are all symbols on the RHS of this rule bottom marked?
                RHS_SYMBOL:
                for my $rhs_symbol ( @{ $rule->[Parse::Marpa::Rule::RHS] } )
                {
                    my $nullable =
                        $rhs_symbol->[Parse::Marpa::Symbol::NULLABLE];

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
                    $rule->[Parse::Marpa::Rule::NULLABLE] = $rule_nullable;
                    $work_to_do++;
                    $rule_work_set->[ $rule->[Parse::Marpa::Rule::ID] ] = 1;
                }

            }
        }    # SYMBOL_PASS

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#$rule_work_set ) )
        {
            my $work_rule  = $rules->[$rule_id];
            my $lhs_symbol = $work_rule->[Parse::Marpa::Rule::LHS];

            # no work to do -- this symbol already has nullability marked
            next RULE
                if defined $lhs_symbol->[Parse::Marpa::Symbol::NULLABLE];

            # assume non-nullable until we hit an unmarked or non-nullable symbol
            my $symbol_nullable = 0;

            LHS_RULE:
            for my $rule ( @{ $lhs_symbol->[Parse::Marpa::Symbol::LHS] } )
            {

                my $nullable = $rule->[Parse::Marpa::Rule::NULLABLE];

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
                $lhs_symbol->[Parse::Marpa::Symbol::NULLABLE] =
                    $symbol_nullable;
                $work_to_do++;
                $symbol_work_set->[ $lhs_symbol->[Parse::Marpa::Symbol::ID] ]
                    = 1;
            }

        }    # RULE

    }    # work_to_do loop

    my $counted_nullable_count;
    for my $symbol (@$symbols) {
        my (
            $name,
            $nullable,
            $counted,
        ) = @{$symbol}[
            Parse::Marpa::Symbol::NAME,
            Parse::Marpa::Symbol::NULLABLE,
            Parse::Marpa::Symbol::COUNTED,
        ];
        if ($nullable and $counted) {
            carp("Nullable symbol $name is on rhs of counted rule");
            $counted_nullable_count++;
        }
    }
    if ($counted_nullable_count) {
        croak("Counted nullable confuse Marpa -- please rewrite the grammar");
    }

}

sub create_NFA {
    my $grammar = shift;
    my ( $rules, $symbols, $symbol_hash, $start, $academic ) = @{$grammar}[
        Parse::Marpa::Grammar::RULES,       Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::SYMBOL_HASH, Parse::Marpa::Grammar::START,
        Parse::Marpa::Grammar::ACADEMIC
    ];

    $grammar->[Parse::Marpa::Grammar::NULLABLE_SYMBOL] =
        [ grep { $_->[Parse::Marpa::Symbol::NULLABLE] } @$symbols ];

    my $NFA = [];
    $grammar->[Parse::Marpa::Grammar::NFA] = $NFA;

    my $state_id = 0;
    my @NFA_by_item;

    # create S0
    my $s0 = [];
    @{$s0}[
        Parse::Marpa::NFA::ID, Parse::Marpa::NFA::NAME,
        Parse::Marpa::NFA::TRANSITION
        ]
        = ( $state_id++, "S0", {} );
    push( @$NFA, $s0 );

    # create the other states
    RULE: for my $rule (@$rules) {
        my ( $rule_id, $rhs, $useful ) = @{$rule}[
            Parse::Marpa::Rule::ID, Parse::Marpa::Rule::RHS,
            Parse::Marpa::Rule::USEFUL
        ];
        next RULE unless $academic or $useful;
        for my $position ( 0 .. scalar @{$rhs} ) {
            my $new_state = [];
            @{$new_state}[
                Parse::Marpa::NFA::ID,   Parse::Marpa::NFA::NAME,
                Parse::Marpa::NFA::ITEM, Parse::Marpa::NFA::TRANSITION
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
            my @start_rules = @{ $start->[Parse::Marpa::Symbol::LHS] };
            my $start_alias = $start->[Parse::Marpa::Symbol::NULL_ALIAS];
            if ( defined $start_alias ) {
                push( @start_rules,
                    @{ $start_alias->[Parse::Marpa::Symbol::LHS] } );
            }

            RULE: for my $start_rule (@start_rules) {
                my ( $start_rule_id, $useful ) =
                    @{$start_rule}[ Parse::Marpa::Rule::ID,
                    Parse::Marpa::Rule::USEFUL ];
                next RULE unless $useful;
                push(
                    @{ $transition->{""} },
                    $NFA_by_item[$start_rule_id][0]
                );
            }
            next STATE;
        }

        # transitions from states other than state 0:

        my ( $rule, $position ) =
            @{$item}[ Parse::Marpa::LR0_item::RULE,
            Parse::Marpa::LR0_item::POSITION ];
        my $rule_id     = $rule->[Parse::Marpa::Rule::ID];
        my $next_symbol = $rule->[Parse::Marpa::Rule::RHS]->[$position];

        # no transitions if position is after the end of the RHS
        if ( not defined $next_symbol ) { next STATE; }

        # the scanning transition: the transition if the position is at symbol X
        # in the RHS, via symbol X, to the state corresponding to the same
        # rule with the position incremented by 1
        # should I use ID as the key for those hashes, or NAME?
        push(
            @{  $transition->{ $next_symbol->[Parse::Marpa::Symbol::NAME] }
                },
            $NFA_by_item[$rule_id][ $position + 1 ]
        );

        # the prediction transitions: transitions if the position is at symbol X
        # in the RHS, via the empty symbol, to all states with X on the LHS and
        # position 0
        RULE:
        for my $predicted_rule (
            @{ $next_symbol->[Parse::Marpa::Symbol::LHS] } )
        {
            my ( $predicted_rule_id, $useful ) =
                @{$predicted_rule}[ Parse::Marpa::Rule::ID,
                Parse::Marpa::Rule::USEFUL ];
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
        Parse::Marpa::Grammar::NFA, Parse::Marpa::Grammar::SDFA_BY_NAME,
        Parse::Marpa::Grammar::SDFA
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
            not $kernel_NFA_state_seen->[ $_->[Parse::Marpa::NFA::ID] ]++
            } @$kernel_states
    ];
    my $prediction_work_list = [];

    # create the kernel SDFA state
    WORK_LIST: while (@$kernel_work_list) {
        my $next_work_list = [];

        NFA_STATE: for my $NFA_state (@$kernel_work_list) {

            my $to_states = $NFA_state->[Parse::Marpa::NFA::TRANSITION]->{""};

            # First the empty transitions.  These will all be predictions,
            # and need to go into the
            # work list for the prediction SDFA state
            if ( defined $to_states ) {
                push(
                    @$prediction_work_list,
                    grep {
                        not $prediction_NFA_state_seen
                            ->[ $_->[Parse::Marpa::NFA::ID] ]++
                        } @$to_states
                );
            }

            SYMBOL:
            for my $nullable_symbol (
                @{ $grammar->[Parse::Marpa::Grammar::NULLABLE_SYMBOL] } )
            {
                $to_states =
                    $NFA_state->[Parse::Marpa::NFA::TRANSITION]
                    ->{ $nullable_symbol->[Parse::Marpa::Symbol::NAME] };
                next SYMBOL unless defined $to_states;
                push(
                    @$next_work_list,
                    grep {
                        not $kernel_NFA_state_seen
                            ->[ $_->[Parse::Marpa::NFA::ID] ]++
                        } @$to_states
                );
            }
        }

        $kernel_work_list = $next_work_list;
    }    # kernel WORK_LIST

    my $NFA_ids = [];
    NFA_ID: for ( my $NFA_id = 0; $NFA_id <= $#$NFA_states; $NFA_id++ ) {
        next NFA_ID unless $kernel_NFA_state_seen->[$NFA_id];
        my $LR0_item = $NFA_states->[$NFA_id]->[Parse::Marpa::NFA::ITEM];
        my ( $rule, $position ) =
            @{$LR0_item}[ Parse::Marpa::LR0_item::RULE,
            Parse::Marpa::LR0_item::POSITION ];
        my $rhs = $rule->[Parse::Marpa::Rule::RHS];
        if ( $position < @$rhs ) {
            my $next_symbol = $rhs->[$position];
            next NFA_ID if $next_symbol->[Parse::Marpa::Symbol::NULLING];
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
        Parse::Marpa::SDFA::ID, Parse::Marpa::SDFA::NAME,
        Parse::Marpa::SDFA::NFA_STATES
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
            for my $symbol_name ( "",
                map { $_->[Parse::Marpa::Symbol::NAME] }
                @{ $grammar->[Parse::Marpa::Grammar::NULLABLE_SYMBOL] } )
            {
                my $to_states =
                    $NFA_state->[Parse::Marpa::NFA::TRANSITION]
                    ->{$symbol_name};
                next SYMBOL unless defined $to_states;
                push(
                    @$next_work_list,
                    grep {
                        not $prediction_NFA_state_seen
                            ->[ $_->[Parse::Marpa::NFA::ID] ]++
                        } @$to_states
                );
            }
        }

        $prediction_work_list = $next_work_list;
    }    # kernel WORK_LIST

    $NFA_ids = [];
    NFA_ID: for ( my $NFA_id = 0; $NFA_id <= $#$NFA_states; $NFA_id++ ) {
        next NFA_ID unless $prediction_NFA_state_seen->[$NFA_id];
        my $LR0_item = $NFA_states->[$NFA_id]->[Parse::Marpa::NFA::ITEM];
        my ( $rule, $position ) =
            @{$LR0_item}[ Parse::Marpa::LR0_item::RULE,
            Parse::Marpa::LR0_item::POSITION ];
        my $rhs = $rule->[Parse::Marpa::Rule::RHS];
        if ( $position < @$rhs ) {
            my $next_symbol = $rhs->[$position];
            next NFA_ID if $next_symbol->[Parse::Marpa::Symbol::NULLING];
        }
        push( @$NFA_ids, $NFA_id );
    }
    my $prediction_SDFA_name = join( ",", @$NFA_ids );

    $prediction_SDFA_state = $SDFA_by_name->{$prediction_SDFA_name};

    # if we have not already built the prediction SDFA state, build it
    if ( not defined $prediction_SDFA_state ) {

        # build the prediction state except for the transitions.
        @{$prediction_SDFA_state}[
            Parse::Marpa::SDFA::ID, Parse::Marpa::SDFA::NAME,
            Parse::Marpa::SDFA::NFA_STATES
            ]
            = (
            scalar @$SDFA,
            $prediction_SDFA_name, [ @{$NFA_states}[@$NFA_ids] ],
            );
        push( @$SDFA, $prediction_SDFA_state );
        $SDFA_by_name->{$prediction_SDFA_name} = $prediction_SDFA_state;

    }

    # add the empty transition from kernel SDFA state to prediction SDFA state
    $kernel_SDFA_state->[Parse::Marpa::SDFA::TRANSITION]->{""} =
        $prediction_SDFA_state;

    # return the kernel SDFA state
    $kernel_SDFA_state;
}

sub create_SDFA {
    my $grammar = shift;
    my ( $symbols, $symbol_hash, $NFA, $start ) = @{$grammar}[
        Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::SYMBOL_HASH,
        Parse::Marpa::Grammar::NFA,
        Parse::Marpa::Grammar::START
    ];
    my $SDFA = $grammar->[Parse::Marpa::Grammar::SDFA] = [];
    my $NFA_s0 = $NFA->[0];

    # next SDFA state to compute transitions for
    my $next_state_id = 0;

    my $initial_NFA_states = $NFA_s0->[Parse::Marpa::NFA::TRANSITION]->{""};
    if ( not defined $initial_NFA_states ) {
        carp("Empty NFA, cannot create SDFA");
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
            @{ $SDFA_state->[Parse::Marpa::SDFA::NFA_STATES] } )
        {
            my $transition = $NFA_state->[Parse::Marpa::NFA::TRANSITION];
            NFA_TRANSITION:
            while ( my ( $symbol, $to_states ) = each(%$transition) ) {
                next NFA_TRANSITION if $symbol eq "";
                push( @{ $NFA_to_states_by_symbol->{$symbol} }, @$to_states );
            }
        }    # $NFA_state

        # for each transition symbol, create the transition to the SDFA kernel state
        while ( my ( $symbol, $to_states ) = each(%$NFA_to_states_by_symbol) )
        {
            $SDFA_state->[Parse::Marpa::SDFA::TRANSITION]->{$symbol} =
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
        my $NFA_states = $state->[Parse::Marpa::SDFA::NFA_STATES];
        for my $NFA_state (@$NFA_states) {
            my $item = $NFA_state->[Parse::Marpa::NFA::ITEM];
            my ( $rule, $position ) = @{$item}[
                Parse::Marpa::LR0_item::RULE,
                Parse::Marpa::LR0_item::POSITION
            ];
            my ( $lhs, $rhs ) =
                @{$rule}[ Parse::Marpa::Rule::LHS, Parse::Marpa::Rule::RHS ];
            if ( $position >= @$rhs ) {
                my ($lhs_id, $lhs_is_start) = @{$lhs}[
                    Parse::Marpa::Symbol::ID,
                    Parse::Marpa::Symbol::START
                ];
                $lhs_list->[ $lhs_id ] = 1;
                push( @{$complete_rules->[$lhs_id]}, $rule );
                $start_rule = $rule if $lhs_is_start;
            }
        }    # NFA_state
        $state->[Parse::Marpa::SDFA::START_RULE]     = $start_rule;
        LHS: for my $lhs_id (0 .. $#$complete_rules) {
            my $rules = $complete_rules->[$lhs_id];
            next LHS unless defined $rules;
            $rules = [ sort {
               $a->[ Parse::Marpa::Rule::ORDER ] <=> $b->[ Parse::Marpa::Rule::ORDER ]
            } @$rules ];
        }
        $state->[Parse::Marpa::SDFA::COMPLETE_RULES] = $complete_rules;
        $state->[Parse::Marpa::SDFA::COMPLETE_LHS] =
            [ map { $_->[Parse::Marpa::Symbol::NAME] }
                @{$symbols}[ grep { $lhs_list->[$_] } ( 0 .. $#$lhs_list ) ] ];
        $state->[Parse::Marpa::SDFA::LEXABLES] = [
             grep { $_->[Parse::Marpa::Symbol::CLOSURE] // $_->[Parse::Marpa::Symbol::REGEX] }
             map { $symbol_hash->{$_} }
             grep { $_ ne "" }
             keys %{$state->[ Parse::Marpa::SDFA::TRANSITION ]}
        ]
    }    # STATE
}

sub setup_academic_grammar {
    my $grammar = shift;
    my $rules   = $grammar->[Parse::Marpa::Grammar::RULES];

    # in an academic grammar, consider all rules useful
    for my $rule (@$rules) {
        $rule->[Parse::Marpa::Rule::USEFUL] = 1;
    }
}

# given a nullable symbol, create a nulling alias and make the first symbol non-nullable
sub alias_symbol {
    my $grammar         = shift;
    my $nullable_symbol = shift;
    my ( $symbol, $symbols, ) =
        @{$grammar}[
            Parse::Marpa::Grammar::SYMBOL_HASH,
            Parse::Marpa::Grammar::SYMBOLS,
        ];
    my ( $start_reachable, $input_reachable, $name, $null_value ) = @{$nullable_symbol}[
        Parse::Marpa::Symbol::START_REACHABLE,
        Parse::Marpa::Symbol::INPUT_REACHABLE,
        Parse::Marpa::Symbol::NAME,
        Parse::Marpa::Symbol::NULL_VALUE,
    ];

    # create the new, nulling symbol
    my $symbol_count = @$symbols;
    my $alias_name   = $nullable_symbol->[Parse::Marpa::Symbol::NAME] . "[]";
    my $alias        = [];
    @{$alias}[
        Parse::Marpa::Symbol::ID,
        Parse::Marpa::Symbol::NAME,
        Parse::Marpa::Symbol::LHS,
        Parse::Marpa::Symbol::RHS,
        Parse::Marpa::Symbol::START_REACHABLE,
        Parse::Marpa::Symbol::INPUT_REACHABLE,
        Parse::Marpa::Symbol::NULLABLE,
        Parse::Marpa::Symbol::NULLING,
        Parse::Marpa::Symbol::NULL_VALUE,
        ]
        = (
        $symbol_count, $alias_name, [], [], $start_reachable,
        $input_reachable, 1, 1,
        $null_value
        );
    push( @$symbols, $alias );
    weaken( $symbol->{$alias_name} = $alias );

    # turn the original symbol into a non-nullable with a reference to the new alias
    @{$nullable_symbol}[ Parse::Marpa::Symbol::NULLABLE,
        Parse::Marpa::Symbol::NULL_ALIAS ] = ( 0, $alias );
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
        Parse::Marpa::Grammar::RULES,
        Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::START,
    ];

    # add null aliases to symbols which need them
    my $symbol_count = @$symbols;
    SYMBOL: for ( my $ix = 0; $ix < $symbol_count; $ix++ ) {
        my $symbol = $symbols->[$ix];
        my ( $input_reachable, $start_reachable, $nulling, $nullable,
            $null_alias )
            = @{$symbol}[
            Parse::Marpa::Symbol::INPUT_REACHABLE,
            Parse::Marpa::Symbol::START_REACHABLE,
            Parse::Marpa::Symbol::NULLING,
            Parse::Marpa::Symbol::NULLABLE,
            Parse::Marpa::Symbol::NULL_ALIAS
            ];

        # not necessary is the symbol already has a null
        # alias
        next SYMBOL if $null_alias;

        #  we don't bother with unreachable symbols
        next SYMBOL unless $input_reachable;
        next SYMBOL unless $start_reachable;

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
        my ( $lhs, $rhs, $input_reachable, $start_reachable, $nulling,
            $nullable )
            = @{$rule}[
            Parse::Marpa::Rule::LHS,
            Parse::Marpa::Rule::RHS,
            Parse::Marpa::Rule::INPUT_REACHABLE,
            Parse::Marpa::Rule::START_REACHABLE,
            Parse::Marpa::Rule::NULLING,
            Parse::Marpa::Rule::NULLABLE
            ];

        # unreachable and nulling rules are useless
        next RULE unless $input_reachable;
        next RULE unless $start_reachable;
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
        my $rhs_null_value = [];
        $#$rhs_null_value = $#$rhs;
        RHS_SYMBOL: for ( my $ix = 0; $ix <= $#$rhs; $ix++ ) {
            my $symbol = $rhs->[$ix];
            my ( $null_alias, $nulling, $null_value ) = @{$symbol}[
                Parse::Marpa::Symbol::NULL_ALIAS,
                Parse::Marpa::Symbol::NULLING,
                Parse::Marpa::Symbol::NULL_VALUE,
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
            $rule->[Parse::Marpa::Rule::USEFUL] = 1;
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
                              $lhs->[Parse::Marpa::Symbol::NAME] . "[" 
                            . $rule_id . ":"
                            . ( $subp_end + 1 )
                            . "]" );
                    @{$next_subp_lhs}[
                        Parse::Marpa::Symbol::NULLABLE,
                        Parse::Marpa::Symbol::START_REACHABLE,
                        Parse::Marpa::Symbol::INPUT_REACHABLE,
                        Parse::Marpa::Symbol::NULLING,
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
                          $lhs->[Parse::Marpa::Symbol::NAME] . "[" 
                        . $rule_id . ":"
                        . ( $subp_end + 1 )
                        . "]" );
                @{$next_subp_lhs}[
                    Parse::Marpa::Symbol::NULLABLE,
                    Parse::Marpa::Symbol::START_REACHABLE,
                    Parse::Marpa::Symbol::INPUT_REACHABLE,
                    Parse::Marpa::Symbol::NULLING,
                    Parse::Marpa::Symbol::NULL_VALUE,
                    ]
                    = (
                        1, 1, 1, 0,
                        [ @{$rhs_null_value}[ ($subp_end+1) .. $#$rhs_null_value], [] ],
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
                    ->[Parse::Marpa::Symbol::NULL_ALIAS];

                # The third factored production, with a nulling symbol replacing the
                # second proper nullable.  Make sure there ARE two proper nullables.
                last FACTOR unless defined $proper_nullable1;
                $factored_rhs->[2] = [@$subp_factor0_rhs];
                $factored_rhs->[2]->[$subp_proper_nullable1] =
                    $subp_factor0_rhs->[$subp_proper_nullable1]
                    ->[Parse::Marpa::Symbol::NULL_ALIAS];

                # The fourth and last factored production, with a nulling symbol replacing
                # both proper nullables.  We don't include it if it results in a nulling
                # production.
                last FACTOR if $nullable;
                $factored_rhs->[3] = [ @{ $factored_rhs->[2] } ];
                $factored_rhs->[3]->[$subp_proper_nullable0] =
                    $subp_factor0_rhs->[$subp_proper_nullable0]
                    ->[Parse::Marpa::Symbol::NULL_ALIAS];

            }    # FACTOR

            for (my $ix = 0; $ix <= $#$factored_rhs; $ix++) {
                my $factor_rhs = $factored_rhs->[$ix];

                # No need to bother putting together values
                # if the rule's closure is not defined
                # and the values would all be discarded

                # figure out which closure to use
                # if the LHS is the not LHS of the original rule, we have a
                # special CHAF header
                my $has_chaf_lhs = ($subp_lhs != $lhs);

                # if a CHAF LHS was created for the next subproduction,
                # there is a CHAF continuation for this subproduction.
                # It applies to this factor if there is one of the first two
                # factors of more than two.
                my $has_chaf_rhs = $next_subp_lhs;

                my $new_rule = add_rule( $grammar, $subp_lhs, $factor_rhs );
                @{$new_rule}[
                    Parse::Marpa::Rule::USEFUL,
                    Parse::Marpa::Rule::START_REACHABLE,
                    Parse::Marpa::Rule::INPUT_REACHABLE,
                    Parse::Marpa::Rule::NULLABLE,
                    Parse::Marpa::Rule::NULLING,
                    Parse::Marpa::Rule::ORDER,
                    Parse::Marpa::Rule::HAS_CHAF_LHS,
                    Parse::Marpa::Rule::HAS_CHAF_RHS,
                    ] = (
                        1, 1, 1, 0, 0,
                        $rule_id,
                        $has_chaf_lhs,
                        $has_chaf_rhs,
                    );

                $new_rule->[ Parse::Marpa::Rule::ORIGINAL_RULE ] = $rule;
                $new_rule->[ Parse::Marpa::Rule::ACTION ]
                    = $rule->[ Parse::Marpa::Rule::ACTION ];

            } # for each factored rhs

            # no more
            last SUBPRODUCTION unless $next_subp_lhs;
            $subp_lhs   = $next_subp_lhs;
            $subp_start = $subp_end + 1;
            $nullable   = $subp_start > $last_nonnullable;

        }    # SUBPRODUCTION

    }    # RULE

    # Create a new start symbol
    my ($input_reachable, $null_value) = @{$old_start_symbol}[
        Parse::Marpa::Symbol::INPUT_REACHABLE,
        Parse::Marpa::Symbol::NULL_VALUE,
    ];
    my $new_start_symbol =
        assign_symbol( $grammar,
        $old_start_symbol->[Parse::Marpa::Symbol::NAME] . "[']" );
    @{$new_start_symbol}[
        Parse::Marpa::Symbol::INPUT_REACHABLE,
        Parse::Marpa::Symbol::START_REACHABLE,
        Parse::Marpa::Symbol::START,
        Parse::Marpa::Symbol::NULL_VALUE,
        ]
        = ( $input_reachable, 1, 1, $null_value );

    # Create a new start rule
    my $new_start_rule = add_rule( $grammar, $new_start_symbol, [$old_start_symbol] );
    @{$new_start_rule}[
        Parse::Marpa::Rule::INPUT_REACHABLE,
        Parse::Marpa::Rule::START_REACHABLE,
        Parse::Marpa::Rule::USEFUL,
        Parse::Marpa::Rule::ACTION,
        ]
        = (
            $input_reachable, 1, 1,
            q{ $Parse::Marpa::This::v->[0] }
        );

    # If we created a null alias for the original start symobl, we need
    # to create a nulling start rule
    my $old_start_alias = $old_start_symbol->[Parse::Marpa::Symbol::NULL_ALIAS];
    if ( $old_start_alias ) {
        my $new_start_alias = alias_symbol( $grammar, $new_start_symbol );
        @{$new_start_alias}[
            Parse::Marpa::Symbol::START,
        ] = ( 1 );
        my $new_start_rule = add_rule( $grammar, $new_start_alias, [] );

        # Nulling rules are not considered useful, but the top-level one is an exception
        @{$new_start_rule}[
            Parse::Marpa::Rule::INPUT_REACHABLE,
            Parse::Marpa::Rule::START_REACHABLE,
            Parse::Marpa::Rule::USEFUL,
            ]
            = ( $input_reachable, 1, 1, );
    }
    $grammar->[Parse::Marpa::Grammar::START] = $new_start_symbol;
}

package Parse::Marpa::Earley_item;

# Elements of the EARLEY ITEM structure
# Note that these are Earley items as modified by Aycock & Horspool, with SDFA states instead of
# LR(0) items.
#
use constant STATE             => 0;   # the SDFA state
use constant PARENT            => 1;   # the number of the Earley set with the parent item(s)
use constant TOKENS            => 2;   # a list of the links from token scanning
use constant LINKS             => 3;   # a list of the links from the completer step
use constant SET               => 4;   # the set this item is in, for debugging
                       # these next elements are "notations" for iterating over the parses
use constant POINTER           => 5;   # symbol just before pointer
use constant RULES             => 6;   # current list of rules
use constant RULE_CHOICE       => 7;   # current choice of rule
use constant LINK_CHOICE       => 8;   # current choice of link
use constant TOKEN_CHOICE      => 9;   # current choice of token
use constant VALUE             => 10;  # value of pointer symbol
use constant PREDECESSOR       => 11;  # the predecessor link, if we have a value
use constant SUCCESSOR         => 12;  # the predecessor link, in reverse
use constant EFFECT            => 13;  # the cause link, in reverse
                                       # or the "parent" item
use constant LHS               => 14;  # LHS symbol

# Note that (at least right now) items either have a SUCCESSOR
# or an EFFECT, never both.

package Parse::Marpa::Parse;

use Scalar::Util qw(weaken);
use Data::Dumper;
use Carp;

my $parse_number = 0;

# Elements of the PARSE structure
use constant GRAMMAR     => 0;    # the grammar used
use constant CURRENT_SET => 1;    # index of the first incomplete Earley set
use constant EARLEY_SETS => 2;    # the array of the Earley sets
use constant EARLEY_HASHES => 3;    # the array of hashes used
                                  # to build the Earley sets
use constant CURRENT_PARSE_SET    => 4;    # the set being taken as the end of
                                  # parse for an evaluation
                                  # only undef if there are no evaluation
                                  # notations in the earley items
use constant START_ITEM               => 5;    # the start item for the current evaluation
use constant TRACE_FILE_HANDLE        => 6;    # trace level
use constant FURTHEST_EARLEME         => 7;    # last earley set with a token
use constant EXHAUSTED                => 8;    # parse can't continue?
use constant TRACE_LEX_TRIES          => 9; 
use constant TRACE_LEX_MATCHES        => 10; 
use constant TRACE_ITERATION_SEARCHES => 11; 
use constant TRACE_ITERATION_CHANGES  => 12; 
use constant DEFAULT_PARSE_SET        => 14;
use constant TRACE_COMPLETIONS        => 15; 
use constant LOCATION_CALLBACK        => 16;
use constant PACKAGE                  => 17; # special "safe" namespace
use constant TRACE_ACTIONS            => 18;
use constant AMBIGUOUS_LEX            => 19;
use constant DEFAULT_ACTION           => 20;
use constant VOLATILE                 => 21;

# implementation dependent constant, used below in unpack
use constant J_LENGTH => ( length pack( "J", 0, 0 ) );

# Constructor method

# Set rule actions
sub set_actions {
    my $grammar           = shift;
    my $package           = shift;
    my $default_action    = shift;

    my (
        $rules,
    ) = @{$grammar}[
        Parse::Marpa::Grammar::RULES,
    ];

    RULE: for my $rule (@$rules) {

        next RULE unless $rule->[ Parse::Marpa::Rule::USEFUL ];

        my $action = $rule->[ Parse::Marpa::Rule::ACTION ];

        ACTION: {

            $action //= $default_action;
            last ACTION unless defined $action;
        
            # HAS_CHAF_RHS and HAS_CHAF_LHS would work well as a bit
            # mask in a C implementation
            my $has_chaf_lhs = $rule->[ Parse::Marpa::Rule::HAS_CHAF_LHS ];
            my $has_chaf_rhs = $rule->[ Parse::Marpa::Rule::HAS_CHAF_RHS ];

            last ACTION unless $has_chaf_lhs or $has_chaf_rhs;

            if ($has_chaf_rhs and $has_chaf_lhs) {
                $action = q{ $Parse::Marpa::This::v };
                last ACTION;
            }

            # At this point has chaf rhs or lhs but not both
            if ($has_chaf_lhs) {

                $action =
                    q{
                        push(@$Parse::Marpa::This::v, []);
                        $Parse::Marpa::This::v;
                    };
                last ACTION;

            }

            # at this point must have chaf rhs and not a chaf lhs

            my $original_rule
                = $Parse::Marpa::This::rule->[ Parse::Marpa::Rule::ORIGINAL_RULE ];

            $action = q{
                TAIL: for (;;) {
                    my $tail = pop @$Parse::Marpa::This::v;
                    last TAIL unless scalar @$tail;
                    push(@$Parse::Marpa::This::v, @$tail);
                }
            } # q string
            . $action;

        } # ACTION

        next RULE unless defined $action;

        my $code = 
            "sub {\n"
            . "    package " . $package . ";\n"
            . $action
            . "\n}";

        if ($Parse::Marpa::This::trace_actions) {
            print $Parse::Marpa::This::trace_fh
                "Setting action for rule ",
                Parse::Marpa::brief_rule($rule), " to\n",
                $code,
                "\n"
        }

        my $closure;
        {
            local $SIG{__WARN__} = sub {0};
            $closure = eval $code;
        }
        if ($@) {
            croak("Problem compiling action:\n"
                , $code
                , "\nFailed to compile closure for ",
                , Parse::Marpa::brief_rule($rule),
                , "\n"
                , $@
            );
        }

        $rule->[ Parse::Marpa::Rule::CLOSURE ] = $closure;

    } # RULE
}

sub new {
    my $class   = shift;

    my $parse   = [];

    my $grammar;
    my $default_action;
    my $trace_actions = 0;
    my $trace_fh;
    my $ambiguous_lex;

    # default for parse is non-volatile, but grammar setting
    # and explicit setting both override
    my $volatile = 0;

    given (scalar @_) {
        when (1) {
            $grammar = shift;
        }
        default {
            my %args  = @_;

            my %arg_logic = (
                "grammar"              => sub { $grammar     = $_[0] },
                "default_action"    => sub { $default_action = $_[0] },
                "ambiguous_lex"      => sub { $ambiguous_lex = $_[0] },
                "trace_file_handle"  => sub { $trace_fh = $_[0] },
                "trace_actions"        => sub { $trace_actions = $_[0] },
                "volatile"        => sub { $volatile = $_[0] },
            );

            while ( my ( $arg, $value ) = each %args ) {
                my $closure = $arg_logic{$arg};
                croak("Undefined argument to ", $class, "::new: ", $arg)
                    unless defined $closure;
                $closure->($value);
            }

        }
    }

    croak("No grammar specified")        unless defined $grammar;

    my $grammar_class = ref $grammar;
    croak(
        "Don't recognize parse() grammar arg has wrong class: $grammar_class")
        unless $grammar_class eq "Parse::Marpa";

    # deep copy grammar
    # works, but strengthens weak refs

    # This could be made more efficient with a custom routine

    my $grammar_copy;
    my $d = Data::Dumper->new([$grammar], ["grammar_copy"]);
    $d->Purity(1);
    eval $d->Dump();
    $grammar = $grammar_copy;

    # Eliminate or weaken all circular references
    my $symbol_hash = $grammar->[ Parse::Marpa::Grammar::SYMBOL_HASH ];
    while (my ($name, $ref) = each %{$symbol_hash}) {
        weaken($symbol_hash->{$name} = $ref);
    }
    for my $symbol (@{$grammar->[ Parse::Marpa::Grammar::SYMBOLS]}) {
        $symbol->[Parse::Marpa::Symbol::LHS] = undef;
        $symbol->[Parse::Marpa::Symbol::RHS] = undef;
    }

    $ambiguous_lex = $grammar->[ Parse::Marpa::Grammar::AMBIGUOUS_LEX ]
        unless defined $ambiguous_lex;

    $trace_fh = $grammar->[ Parse::Marpa::Grammar::TRACE_FILE_HANDLE ]
        unless defined $trace_fh;
    local($Parse::Marpa::This::trace_fh) = $trace_fh;
    local($Parse::Marpa::This::trace_actions) = $trace_actions;

    $default_action = $grammar->[ Parse::Marpa::Grammar::DEFAULT_ACTION ]
        unless defined $default_action;

    # volatile can be set, but never unset
    $volatile = $grammar->[ Parse::Marpa::Grammar::VOLATILE ]
        unless $volatile;

    my (
        $SDFA,
        $location_callback,
    ) = @{$grammar}[
        Parse::Marpa::Grammar::SDFA,
        Parse::Marpa::Grammar::LOCATION_CALLBACK,
    ];

    croak("Attempt to parse grammar with empty SDFA")
        if not defined $SDFA
            or not scalar @$SDFA;

    my $package = sprintf("Parse::Marpa::P_%x", $parse_number++);

    # I should do (or at least allow) a deep copy of the grammar
    # rather than creating closures "in place"
    set_actions($grammar, $package, $default_action);

    my $earley_hash;
    my $earley_set;
    my $item;

    # A bit of a cheat here: I rely on an assumption about the numbering
    # of the SDFA states -- specifically, that state 0 contains the
    # start productions.
    my $SDFA0 = $SDFA->[0];
    my $key = pack( "JJ", $SDFA0 + 0, 0 );
    @{$item}[
        Parse::Marpa::Earley_item::STATE,
        Parse::Marpa::Earley_item::PARENT,
        Parse::Marpa::Earley_item::TOKENS,
        Parse::Marpa::Earley_item::LINKS,
        Parse::Marpa::Earley_item::SET
    ] = ( $SDFA0, 0, [], [], 0 );
    push( @$earley_set, $item );
    $earley_hash->{$key} = $item;

    my $resetting_state = $SDFA0->[Parse::Marpa::SDFA::TRANSITION]->{""};
    if ( defined $resetting_state ) {
        $key = pack( "JJ", $resetting_state, 0 );
        undef $item;
        @{$item}[
            Parse::Marpa::Earley_item::STATE,
            Parse::Marpa::Earley_item::PARENT,
            Parse::Marpa::Earley_item::TOKENS,
            Parse::Marpa::Earley_item::LINKS,
            Parse::Marpa::Earley_item::SET
        ] = ( $resetting_state, 0, [], [], 0 );
        push( @$earley_set, $item );
        $earley_hash->{$key} = $item;
    }

    @{$parse}[
        DEFAULT_PARSE_SET, CURRENT_SET, FURTHEST_EARLEME,
        EARLEY_HASHES,
        GRAMMAR,
        EARLEY_SETS,
        LOCATION_CALLBACK,
        PACKAGE,
        DEFAULT_ACTION,
        AMBIGUOUS_LEX,
        TRACE_FILE_HANDLE,
        TRACE_ACTIONS,
    ] = (
        0, 0, 0,
        [$earley_hash],
        $grammar,
        [$earley_set],
        $location_callback,
        $package,
        $default_action,
        $ambiguous_lex,
        $trace_fh,
        $trace_actions,
    );

    bless $parse, $class;
}

# Viewing methods, for debugging

sub brief_earley_item {
    my $item = shift;
    my $ii = shift;
    my ( $state, $parent, $set ) = @{$item}[
        Parse::Marpa::Earley_item::STATE,
        Parse::Marpa::Earley_item::PARENT,
        Parse::Marpa::Earley_item::SET
    ];
    my ($id, $tag) = @{$state}[
        Parse::Marpa::SDFA::ID,
        Parse::Marpa::SDFA::TAG
    ];
    my $text = $set . ":";
    $text .= ($ii and defined $tag) ? ("St" . $tag) : ("S" . $id);
    $text .= "," . $parent;
}

sub show_earley_item {
    my $item = shift;
    my $ii = shift;
    my ( $tokens, $links,
        $rules, $rule_choice, $link_choice, $token_choice,
        $value,
        $pointer, $lhs,
        $predecessor, $successor, $effect,
    ) = @{$item}[
        Parse::Marpa::Earley_item::TOKENS,
        Parse::Marpa::Earley_item::LINKS,
        Parse::Marpa::Earley_item::RULES,
        Parse::Marpa::Earley_item::RULE_CHOICE,
        Parse::Marpa::Earley_item::LINK_CHOICE,
        Parse::Marpa::Earley_item::TOKEN_CHOICE,
        Parse::Marpa::Earley_item::VALUE,
        Parse::Marpa::Earley_item::POINTER,
        Parse::Marpa::Earley_item::LHS,
        Parse::Marpa::Earley_item::PREDECESSOR,
        Parse::Marpa::Earley_item::SUCCESSOR,
        Parse::Marpa::Earley_item::EFFECT,
    ];

    my $text = brief_earley_item($item, $ii);
    $text .= "  predecessor: " . brief_earley_item( $predecessor )
        if defined $predecessor;
    $text .= "  successor: " . brief_earley_item( $successor )
        if defined $successor;
    $text .= "  effect: " . brief_earley_item( $effect )
        if defined $effect;
    my @symbols;
    push(@symbols, "pointer: " . $pointer->[ Parse::Marpa::Symbol::NAME ])
        if defined $pointer;
    push(@symbols, "lhs: " . $lhs->[ Parse::Marpa::Symbol::NAME ])
        if defined $lhs;
    $text .= "\n  " . join("; ", @symbols) if @symbols;
    $text .= "\n  value: " . show_value($value, $ii) if defined $value;
    if (defined $tokens and @$tokens) {
        $text .= "\n  token choice " . $token_choice;
        for my $token (@$tokens) {
            $text .= " [p="
                . brief_earley_item( $token->[0], $ii ) . "; t="
                . $token->[1] . "]";
        }
    }
    if (defined $links and @$links) {
        $text .= "\n  link choice " . $link_choice;
        for my $link (@$links) {
            $text .= " [p="
            . brief_earley_item( $link->[0], $ii ) . "; c="
            . brief_earley_item( $link->[1], $ii ) . "]";
        }
    }
    if (defined $rules and @$rules) {
        $text .= "\n  rule choice " . $rule_choice;
        for my $rule (@$rules) {
            $text .= " [ " . Parse::Marpa::brief_rule($rule) . " ]";
        }
    }
    $text;
}

sub show_earley_set {
    my $earley_set = shift;
    my $ii = shift;
    my $text       = "";
    for my $earley_item (@$earley_set) {
        $text .= show_earley_item($earley_item, $ii) . "\n";
    }
    $text;
}

sub show_earley_set_list {
    my $earley_set_list  = shift;
    my $ii = shift;
    my $text             = "";
    my $earley_set_count = @$earley_set_list;
    LIST: for ( my $ix = 0; $ix < $earley_set_count; $ix++ ) {
        my $set = $earley_set_list->[$ix];
        next LIST unless defined $set;
        $text .= "Earley Set $ix\n" . show_earley_set($set, $ii);
    }
    $text;
}

sub show_status {
    my $parse = shift;
    my $ii = shift;
    my ( $current_set, $furthest_earleme, $earley_set_list ) =
        @{$parse}[ CURRENT_SET, FURTHEST_EARLEME, EARLEY_SETS ];
    my $text
        = "Current Earley Set: " . $current_set
        . "; Furthest: " . $furthest_earleme. "\n";
    $text .= show_earley_set_list($earley_set_list, $ii);
}

sub clear_notations {
    my $parse = shift;
    my ( $earley_set_list ) =
        @{$parse}[ EARLEY_SETS ];
    for my $earley_set (@$earley_set_list) {
        for my $earley_item (@$earley_set) {
            @{$earley_item}[
                Parse::Marpa::Earley_item::POINTER,
                Parse::Marpa::Earley_item::RULES,
                Parse::Marpa::Earley_item::RULE_CHOICE,
                Parse::Marpa::Earley_item::LINK_CHOICE,
                Parse::Marpa::Earley_item::TOKEN_CHOICE,
                Parse::Marpa::Earley_item::VALUE,
                Parse::Marpa::Earley_item::PREDECESSOR,
                Parse::Marpa::Earley_item::SUCCESSOR,
                Parse::Marpa::Earley_item::EFFECT,
                Parse::Marpa::Earley_item::LHS,
            ] = (
                undef, [],
                0, 0, 0,
                undef, undef, undef, undef, undef,
            );
        }
    }
}

sub gen_bracket_regex {
    my ($left, $right) = @_;
    qr/\G[^\Q$left$right\E\\\\]*(\Q$left\E|\Q$right\E|[\\\\]\Q$left\E|[\\\\]\Q$right\E)/;
}

my %regex_data = (
    '{' => ['}', gen_bracket_regex('{', '}') ],
    '<' => ['>', gen_bracket_regex('<', '>') ],
    '[' => [']', gen_bracket_regex('[', ']') ],
    '(' => [')', gen_bracket_regex('(', ')') ],
);

# This is POSIX "punct" character class, except for backslash,
# and the right side bracketing symbols.
# \043 is single quote, \0133 is the left square bracket.
my $punct = qr'[!"#$%&\043(*+,-./:;<=?\0133^_`{|~@]';

sub lex_q_quote {
    my $string = shift;
    my $start = (pos $$string) // 0;
    say "lex_q_quote pos=", (pos $$string);
    $$string =~ m/\G\s*qq?([[:punct:]])/gc;
    my $left = $1;
    return unless defined $left;
    say "lex_q_quote pos=", (pos $$string);

    my $regex_data = $regex_data{$1};
    if (not defined $regex_data) {
	my $regex = qr/\G[^\Q$left\E\\\\]*(\Q$left\E|[\\\\]\Q$left\E)/;
	$regex_data{$left} = $regex_data = [undef, $regex];
    }
    my ($right, $regex) = @$regex_data;
    # unbracketed quote
    if (not defined $right) {
	MATCH: while ($$string =~ /\G$regex/gc) {
	    next MATCH unless defined $1;
	    if ($1 eq $left) {
		my $length = (pos $$string) - $start;
		return (substr($$string, $start, $length), $length);
	    }
	}
	return;
    }

    # bracketed quote
    my $depth=1;
    MATCH: while ($$string =~ /\G$regex/gc) {
	next MATCH unless defined $1;
	given ($1) {
	   when ($left) { $depth++; }
	   when ($right) { $depth--; }
	}
	if ($depth <= 0) {
	    my $length = (pos $$string) - $start;
	    return (substr($$string, $start, $length), $length);
	}
    }
    return;
}

# check parse? 
sub lex_earleme {
    my $parse = shift;

    # lexables not checked -- don't use prediction here
    # maybe add this as an option
    my $lexables = complete_set($parse);
    return scan_set($parse, @_);
}

# Returns the position where the parse was exhausted,
# or -1 if the parse is not exhausted
sub lex_string {
    my $parse = shift;
    my $input_ref = shift;
    my $length = shift;

    my (
        $grammar,
        $earley_sets,
        $current_set,
        $trace_fh,
        $trace_lex_tries,
        $trace_lex_matches,
        $ambiguous_lex,
    ) = @{$parse}[
        Parse::Marpa::Parse::GRAMMAR,
        Parse::Marpa::Parse::EARLEY_SETS,
        Parse::Marpa::Parse::CURRENT_SET,
        Parse::Marpa::Parse::TRACE_FILE_HANDLE,
        Parse::Marpa::Parse::TRACE_LEX_TRIES,
        Parse::Marpa::Parse::TRACE_LEX_MATCHES,
        Parse::Marpa::Parse::AMBIGUOUS_LEX,
    ];

    local($Parse::Marpa::This::trace_lex_tries) = $trace_lex_tries;
    local($Parse::Marpa::This::trace_lex_matches) = $trace_lex_matches;

    my (
        $symbols,
    ) = @{$grammar}[
        Parse::Marpa::Grammar::SYMBOLS,
    ];

    local($Parse::Marpa::This::trace_fh) = $trace_fh;

    $length = length $$input_ref unless defined $length;

    POS: for (my $pos = (pos $$input_ref // 0); $pos < $length; $pos++) {
        my @alternatives;

        # NOTE: Often the number of the earley set, and the idea of
        # lexical position will correspond.  Be careful that Marpa
        # imposes no such requirement, however.


        my $lexables = complete_set($parse);

        LEXABLE: for my $lexable (@$lexables) {
            my ($regex) = @{$lexable}[
                Parse::Marpa::Symbol::REGEX,
            ];
            if ($Parse::Marpa::This::trace_lex_tries) {
                print $Parse::Marpa::This::trace_fh
                    "Trying to match ",
                    $lexable->[ Parse::Marpa::Symbol::NAME ],
                    " at $pos\n";
            }

            pos $$input_ref = $pos;
            if (defined $regex) {
                if ($$input_ref =~ /$regex/g) {
                    my $match = $+{mArPa_match};
                    # my $prefix = $+{mArPa_prefix};
                    # my $suffix = $+{mArPa_suffix};
                    # my $length = length(${^MATCH});
                    my $length = (pos $$input_ref) - $pos;
                    croak("Internal error, zero length token -- this is a Marpa bug")
                        unless $length;
                    push(@alternatives, [ $lexable, $match, $length ]);
                    if ($Parse::Marpa::This::trace_lex_matches) {
                        print $Parse::Marpa::This::trace_fh
                            "Matched Regex for ", $lexable->[ Parse::Marpa::Symbol::NAME ],
                            " at $pos: ", $match, "\n";
                    }
                    last LEXABLE unless $ambiguous_lex;
                } # if match

                next LEXABLE;

            } # if defined regex

            # If it's a lexable and a regex was not defined, there must be a
            # closure

            if (my ($match, $length) = $lexable->[ Parse::Marpa::Symbol::CLOSURE ]->()) {
                $length //= length $match;

                push(@alternatives, [ $lexable, $match, $length ]);
                if ($Parse::Marpa::This::trace_lex_matches) {
                    print $Parse::Marpa::This::trace_fh
                        "Matched Closure for ", $lexable->[ Parse::Marpa::Symbol::NAME ],
                        " at $pos: ", $match, "\n";
                }
                last LEXABLE unless $ambiguous_lex;
            }

        } # LEXABLE

        my $active = scan_set($parse, @alternatives);

        return $pos unless $active;

    } # POS

    return -1;

} # sub lex_string

sub lex_end {
    my $parse = shift;
    complete_set($parse);
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

    my (
        $earley_set_list, $earley_hash_list, $grammar,
        $current_set, $furthest_earleme, $exhausted,
    ) = @{$parse}[
        EARLEY_SETS, EARLEY_HASHES, GRAMMAR,
        CURRENT_SET, FURTHEST_EARLEME, EXHAUSTED ];
    croak("Attempt to scan tokens on an exhausted parse") if $exhausted;
    my $SDFA = $grammar->[Parse::Marpa::Grammar::SDFA];

    my $earley_set  = $earley_set_list->[$current_set];

    if ( not defined $earley_set ) {
        $earley_set_list->[$current_set] = [];
        if ($current_set >= $furthest_earleme) {
            $parse->[ Parse::Marpa::Parse::EXHAUSTED ]
                = $exhausted
                =  1;
        } else {
            $parse->[ CURRENT_SET ] ++;
        }
        return !$exhausted;
    }

    EARLEY_ITEM: for ( my $ix = 0; $ix < @$earley_set; $ix++ ) {

        my $earley_item = $earley_set->[$ix];
        my ( $state, $parent ) = @{$earley_item}[
            Parse::Marpa::Earley_item::STATE,
            Parse::Marpa::Earley_item::PARENT
        ];

        # I allow ambigious tokenization.
        # Loop through the alternative tokens.
        ALTERNATIVE: for my $alternative (@_) {
            my ( $token, $value, $length ) = @$alternative;

            if ( $length <= 0 ) {
                croack(   "Token "
                        . $token->[Parse::Marpa::Symbol::NAME]
                        . " with bad length "
                        . $length );
            }

            # Make sure it's an allowed terminal symbol.
            # TODO: Must remember to be sure that
            # nulling symbols are never terminals
            unless ( $token->[ Parse::Marpa::Symbol::TERMINAL ] ) {
                my $name = $token->[Parse::Marpa::Symbol::NAME];
                croak(   "Non-terminal "
                        . (defined $name ? "$name " : "")
                        . "supplied as token");
            }

            # compute goto(kernel_state, token_name)
            my $kernel_state =
                $SDFA->[ $state->[Parse::Marpa::SDFA::ID] ]
                ->[Parse::Marpa::SDFA::TRANSITION]
                ->{ $token->[Parse::Marpa::Symbol::NAME] };
            next ALTERNATIVE unless $kernel_state;

            # Create the kernel item and its link.
            my $target_ix = $current_set + $length;
            my $target_earley_hash =
                ( $earley_hash_list->[$target_ix] ||= {} );
            my $target_earley_set = ( $earley_set_list->[$target_ix] ||= [] );
            if ($target_ix > $furthest_earleme) {
                $parse->[ Parse::Marpa::Parse::FURTHEST_EARLEME ]
                    = $furthest_earleme
                    = $target_ix;
            }
            my $key = pack( "JJ", $kernel_state, $parent );
            my $target_earley_item = $target_earley_hash->{$key};
            unless ( defined $target_earley_item ) {
                @{$target_earley_item}[
                    Parse::Marpa::Earley_item::STATE,
                    Parse::Marpa::Earley_item::PARENT,
                    Parse::Marpa::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Earley_item::LINKS,
                    Parse::Marpa::Earley_item::TOKEN_CHOICE,
                    Parse::Marpa::Earley_item::TOKENS,
                    Parse::Marpa::Earley_item::SET
                    ] = (
                        $kernel_state, $parent,
                        0, [],
                        0, [],
                        $target_ix
                    );
                $target_earley_hash->{$key} = $target_earley_item;
                push( @$target_earley_set, $target_earley_item );
            }
            push(
                @{  $target_earley_item->[Parse::Marpa::Earley_item::TOKENS]
                    },
                [ $earley_item, $value ]
            );

            my $resetting_state =
                $kernel_state->[Parse::Marpa::SDFA::TRANSITION]->{""};
            next ALTERNATIVE unless defined $resetting_state;
            $key = pack( "JJ", $resetting_state, $target_ix );
            unless ( exists $target_earley_hash->{$key} ) {
                my $new_earley_item;
                @{$new_earley_item}[
                    Parse::Marpa::Earley_item::STATE,
                    Parse::Marpa::Earley_item::PARENT,
                    Parse::Marpa::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Earley_item::LINKS,
                    Parse::Marpa::Earley_item::TOKEN_CHOICE,
                    Parse::Marpa::Earley_item::TOKENS,
                    Parse::Marpa::Earley_item::SET
                ] = (
                    $resetting_state, $target_ix,
                    0, [],
                    0, [],
                    $target_ix
                );
                $target_earley_hash->{$key} = $new_earley_item;
                push( @$target_earley_set, $new_earley_item );
            }

        }    # ALTERNATIVE

    } # EARLEY_ITEM

    $parse->[CURRENT_SET]++;

    return 1;

} # sub scan_set

sub complete_set {
    my $parse = shift;

    my (
        $earley_set_list, $earley_hash_list, $grammar,
        $current_set, $furthest_earleme, $exhausted,
        $trace_completions,
    ) = @{$parse}[
        EARLEY_SETS, EARLEY_HASHES, GRAMMAR,
        CURRENT_SET, FURTHEST_EARLEME, EXHAUSTED,
        TRACE_COMPLETIONS,
    ];
    croak("Attempt to complete another earley set in an exhausted parse") if $exhausted;

    my $earley_set  = $earley_set_list->[$current_set];
    my $earley_hash = $earley_hash_list->[$current_set];

    $earley_set ||= [];

    my (
        $SDFA, $symbols, $trace_fh
    ) = @{$grammar}[
        Parse::Marpa::Grammar::SDFA,
        Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::TRACE_FILE_HANDLE,
    ];

    my $lexable_seen = [];
    $#$lexable_seen = $#$symbols;

    EARLEY_ITEM: for ( my $ix = 0; $ix < @$earley_set; $ix++ ) {

        my $earley_item = $earley_set->[$ix];
        my ( $state, $parent ) = @{$earley_item}[
            Parse::Marpa::Earley_item::STATE,
            Parse::Marpa::Earley_item::PARENT
        ];

        for my $lexable (@{$state->[ Parse::Marpa::SDFA::LEXABLES ]}) {
            $lexable_seen->[ $lexable->[ Parse::Marpa::Symbol::ID ] ] = 1;
        }

        next EARLEY_ITEM if $current_set == $parent;

        COMPLETE_RULE:
        for my $complete_symbol_name (
            @{ $state->[Parse::Marpa::SDFA::COMPLETE_LHS] }
        ) {
            PARENT_ITEM:
            for my $parent_item ( @{ $earley_set_list->[$parent] } )
            {
                my ( $parent_state, $grandparent ) = @{$parent_item}[
                    Parse::Marpa::Earley_item::STATE,
                    Parse::Marpa::Earley_item::PARENT
                ];
                my $kernel_state =
                    $SDFA->[ $parent_state->[Parse::Marpa::SDFA::ID] ]
                    ->[Parse::Marpa::SDFA::TRANSITION]
                    ->{$complete_symbol_name};
                next PARENT_ITEM unless defined $kernel_state;

                my $key = pack( "JJ", $kernel_state, $grandparent );
                my $target_earley_item = $earley_hash->{$key};
                unless ( defined $target_earley_item ) {
                    @{$target_earley_item}[
                        Parse::Marpa::Earley_item::STATE,
                        Parse::Marpa::Earley_item::PARENT,
                        Parse::Marpa::Earley_item::LINK_CHOICE,
                        Parse::Marpa::Earley_item::LINKS,
                        Parse::Marpa::Earley_item::TOKEN_CHOICE,
                        Parse::Marpa::Earley_item::TOKENS,
                        Parse::Marpa::Earley_item::SET
                        ] = (
                            $kernel_state, $grandparent,
                            0, [],
                            0, [],
                            $current_set
                        );
                    $earley_hash->{$key} = $target_earley_item;
                    push( @$earley_set, $target_earley_item );
                }
                push(
                    @{  $target_earley_item
                            ->[Parse::Marpa::Earley_item::LINKS]
                        },
                    [ $parent_item, $earley_item ]
                );

                my $resetting_state =
                    $kernel_state->[Parse::Marpa::SDFA::TRANSITION]->{""};
                next PARENT_ITEM unless defined $resetting_state;
                $key = pack( "JJ", $resetting_state, $current_set );
                unless ( defined $earley_hash->{$key} ) {
                    my $new_earley_item;
                    @{$new_earley_item}[
                        Parse::Marpa::Earley_item::STATE,
                        Parse::Marpa::Earley_item::PARENT,
                        Parse::Marpa::Earley_item::LINK_CHOICE,
                        Parse::Marpa::Earley_item::LINKS,
                        Parse::Marpa::Earley_item::TOKEN_CHOICE,
                        Parse::Marpa::Earley_item::TOKENS,
                        Parse::Marpa::Earley_item::SET
                        ] = (
                            $resetting_state, $current_set,
                            0, [],
                            0, [],
                            $current_set
                        );
                    $earley_hash->{$key} = $new_earley_item;
                    push( @$earley_set, $new_earley_item );
                }

            }    # PARENT_ITEM

        }    # COMPLETE_RULE

    }    # EARLEY_ITEM

    # TODO: Prove that the completion links are UNIQUE

    # Free memory for the hash
    $earley_hash_list->[$current_set] = undef;

    $parse->[ Parse::Marpa::Parse::DEFAULT_PARSE_SET ] = $current_set;

    if ($trace_completions) {
        print $trace_fh show_earley_set($earley_set);
    }

    # Dream up some efficiency hack here.  Memoize sorted lexables by state?
    my $lexables = [
        sort { $a->[Parse::Marpa::Symbol::ORDER] <=> $b->[Parse::Marpa::Symbol::ORDER] }
        map { $symbols->[$_] }
        grep { $lexable_seen->[$_] }
        (0 .. $#$symbols)
    ];
    return $lexables;

} # sub complete_set

sub trace_file_handle {
    my $grammar = shift;
    my $fh = shift;
    $grammar->[ Parse::Marpa::Grammar::TRACE_FILE_HANDLE ] = $fh;
}

sub trace {
    my $parse = shift;
    my %parameter = (
        "lex (try|tries)?" => sub { $parse->[ Parse::Marpa::Parse::TRACE_LEX_TRIES ] = 1; },
        "lex match(es)?" => sub { $parse->[ Parse::Marpa::Parse::TRACE_LEX_MATCHES ] = 1; },
        "lex(es|ing)?" => sub {
            $parse->[ Parse::Marpa::Parse::TRACE_LEX_TRIES ] = 1;
            $parse->[ Parse::Marpa::Parse::TRACE_LEX_MATCHES ] = 1;
        },
        "iteration search(es)?" => sub { $parse->[ Parse::Marpa::Parse::TRACE_ITERATION_SEARCHES ] = 1; },
        "iteration change(s)?" => sub { $parse->[ Parse::Marpa::Parse::TRACE_ITERATION_CHANGES ] = 1; },
        "iteration(s)?" => sub {
            $parse->[ Parse::Marpa::Parse::TRACE_ITERATION_SEARCHES ] = 1;
            $parse->[ Parse::Marpa::Parse::TRACE_ITERATION_CHANGES ] = 1;
        },
        "completion(s)?" => sub {
            $parse->[ Parse::Marpa::Parse::TRACE_COMPLETIONS ] = 1;
        },
        "all" => sub {
            $parse->[ Parse::Marpa::Parse::TRACE_LEX_TRIES ] = 1;
            $parse->[ Parse::Marpa::Parse::TRACE_LEX_MATCHES ] = 1;
            $parse->[ Parse::Marpa::Parse::TRACE_ITERATION_SEARCHES ] = 1;
            $parse->[ Parse::Marpa::Parse::TRACE_ITERATION_CHANGES ] = 1;
            $parse->[ Parse::Marpa::Parse::TRACE_COMPLETIONS ] = 1;
        },
    );
    for my $argument (@_) {
        while (my ($key, $closure) = each %parameter) {
            if ($argument =~ /^$key$/) {
                $closure->();
            }
        }
    }
}

sub show {
    my $parse    = shift;
    my $text = "";

    croak("No parse supplied") unless defined $parse;

    my (
        $start_item, $current_parse_set
    ) = @{$parse}[
        Parse::Marpa::Parse::START_ITEM,
        Parse::Marpa::Parse::CURRENT_PARSE_SET,
    ];

    local($Parse::Marpa::This::parse) = $parse;
    local($Data::Dumper::Terse) = 1;

    my $value = $start_item->[ Parse::Marpa::Earley_item::VALUE ];
    croak("Parse not evaluated") unless defined $value;

    my ( $rules, $rule_choice)
        = @{$start_item}[
            Parse::Marpa::Earley_item::RULES,
            Parse::Marpa::Earley_item::RULE_CHOICE,
        ];
 
    $text .= show_derivation($start_item);

}

sub show_derivation {
    my $item = shift;
    my $text = "";

    RHS_SYMBOL: for (;;) {

        my $data = 0;

        my (
            $rules, $rule_choice,
            $links, $link_choice,
            $tokens, $token_choice,
            $pointer, $value,
        ) = @{$item}[
            Parse::Marpa::Earley_item::RULES, Parse::Marpa::Earley_item::RULE_CHOICE,
            Parse::Marpa::Earley_item::LINKS, Parse::Marpa::Earley_item::LINK_CHOICE,
            Parse::Marpa::Earley_item::TOKENS, Parse::Marpa::Earley_item::TOKEN_CHOICE,
            Parse::Marpa::Earley_item::POINTER, Parse::Marpa::Earley_item::VALUE,
        ];

        last RHS_SYMBOL unless defined $pointer;
        my $symbol_name = $pointer->[ Parse::Marpa::Symbol::NAME ];

        if (defined $rules and $rule_choice <= $#$rules) {
            my $rule = $rules->[$rule_choice];
            $text .= "[ "
                . brief_earley_item($item) . "] "
                . Parse::Marpa::brief_rule($rule) . "\n";
            $data = 1;
        }

        if ($token_choice <= $#$tokens) {
            my ($predecessor, $token) = @{$tokens->[$token_choice]};
            $text .= "[ "
                . brief_earley_item($item) . "] "
                . "$symbol_name = " . Dumper($token);
            $item = $predecessor;
            next RHS_SYMBOL;
        }

        $text .= brief_earley_item($item) . "No data\n"
             unless $data;

        if ($link_choice <= $#$links) {
            my ($predecessor, $cause) = @{$links->[$link_choice]};
            $text .= show_derivation($cause);
            $item = $predecessor;
        }


    }

    $text;

}

sub initial {
    my $parse    = shift;
    my $parse_set_arg  = shift;

    # TODO: At some point I may need to ensure that evaluation notations are
    # cleared, rather than just assume it.

    # Is the best way to do this?
    my $parse_class = ref $parse;
    my $right_class = "Parse::Marpa::Parse";
    croak("Don't parse argument is class: $parse_class; should be: $right_class")
        unless $parse_class eq $right_class;

    local($Parse::Marpa::This::parse) = $parse;
    my (
        $grammar, $earley_sets,
        $start_item,
        $current_parse_set, $default_parse_set,
        $trace_iteration_searches,
        $trace_iteration_changes,
        $volatile,
    ) = @{$parse}[
        Parse::Marpa::Parse::GRAMMAR,
        Parse::Marpa::Parse::EARLEY_SETS,
        Parse::Marpa::Parse::START_ITEM,
        Parse::Marpa::Parse::CURRENT_PARSE_SET,
        Parse::Marpa::Parse::DEFAULT_PARSE_SET,
        Parse::Marpa::Parse::TRACE_ITERATION_SEARCHES,
        Parse::Marpa::Parse::TRACE_ITERATION_CHANGES,
        Parse::Marpa::Parse::VOLATILE,
    ];
    local($Parse::Marpa::This::grammar) = $grammar;
    local($Parse::Marpa::This::trace_iteration_searches) = $trace_iteration_searches;
    local($Parse::Marpa::This::trace_iteration_changes) = $trace_iteration_changes;
    local($Data::Dumper::Terse) = 1;

    my $trace_fh = $grammar-> [ Parse::Marpa::Grammar::TRACE_FILE_HANDLE ];
    local($Parse::Marpa::This::trace_fh) = $trace_fh;

    if (defined $current_parse_set) {
        my $need_to_clear = $volatile;
        if (defined $parse_set_arg
            and $parse_set_arg != $current_parse_set
        ) {
            $current_parse_set = $parse_set_arg;
            $start_item = undef;
            $need_to_clear++;
        }
        clear_notations($parse) if $need_to_clear;
    }

    if (not defined $current_parse_set) {
        $start_item = undef;
        $current_parse_set = $parse_set_arg // $default_parse_set;
    }

    # If we already have a start item, use it
    my $start_rule;
    if (defined $start_item) {
        my $state = $start_item->[ Parse::Marpa::Earley_item::STATE ];
        $start_rule = $state->[Parse::Marpa::SDFA::START_RULE];
    }

    # Otherwise, look for the start item and start rule
    if (not defined $start_rule) {
        my $earley_set = $earley_sets->[$current_parse_set];

        # The start rule, if not nulling, must be a pure links rule
        # (no tokens) because I don't allow tokens to be recognized
        # for the start symbol

        my $item;
        my $rule;

        # mark start items with LHS?
        EARLEY_ITEM: for (my $ix = 0; $ix <= $#$earley_set; $ix++) {
            $item = $earley_set->[$ix];
            my $state = $item->[ Parse::Marpa::Earley_item::STATE ];
            $rule = $state->[Parse::Marpa::SDFA::START_RULE];
            last EARLEY_ITEM if $rule;
        }

        $start_item = $item;
        $start_rule = $rule;
    }

    return unless $start_rule;

    my $previous_value = $start_item->[ Parse::Marpa::Earley_item::VALUE ];
    return 1 if $previous_value;

    @{$parse}[
        Parse::Marpa::Parse::START_ITEM,
        Parse::Marpa::Parse::CURRENT_PARSE_SET,
    ] = ( $start_item, $current_parse_set );

    my  ($lhs) = @$start_rule[ Parse::Marpa::Rule::LHS ];
    my (
        $nulling, $null_value
    ) = @{$lhs}[
        Parse::Marpa::Symbol::NULLING,
        Parse::Marpa::Symbol::NULL_VALUE
    ];

    if ($nulling) {
         @{$start_item}[
             Parse::Marpa::Earley_item::VALUE,
             Parse::Marpa::Earley_item::TOKEN_CHOICE,
             Parse::Marpa::Earley_item::LINK_CHOICE,
             Parse::Marpa::Earley_item::RULE_CHOICE,
             Parse::Marpa::Earley_item::RULES,
             Parse::Marpa::Earley_item::LHS,
         ] = (
             \$null_value,
             0, 0, 0, [ $start_rule ],
             $lhs,
         );
         if ($Parse::Marpa::This::trace_iteration_changes) {
             print $Parse::Marpa::This::trace_fh "Setting nulling start value of ",
             brief_earley_item($start_item), ", ",
             $lhs->[ Parse::Marpa::Symbol::NAME ],
             " to ", Dumper($null_value);
         }
         return 1;
    }

    my $value = initialize_children($start_item, $lhs);
    @{$start_item}[
        Parse::Marpa::Earley_item::VALUE,
        Parse::Marpa::Earley_item::TOKEN_CHOICE,
        Parse::Marpa::Earley_item::LINK_CHOICE,
        Parse::Marpa::Earley_item::RULE_CHOICE,
        Parse::Marpa::Earley_item::RULES,
        Parse::Marpa::Earley_item::LHS,
    ] = (
        \$value,
         0, 0, 0, [ $start_rule ],
         $lhs,
    );
     if ($Parse::Marpa::This::trace_iteration_changes) {
         print $Parse::Marpa::This::trace_fh "Setting start value of ",
         brief_earley_item($start_item), ", ",
         $lhs->[ Parse::Marpa::Symbol::NAME ],
         " to ", Dumper($value);
     }

     1;

}

sub find_complete_rule {
    my $parse = shift;
    my $start_earleme = shift;
    my $symbol = shift;
    my $last_earleme = shift;

    my (
        $default_parse_set,
        $earley_sets,
    ) = @{$parse}[
        Parse::Marpa::Parse::DEFAULT_PARSE_SET,
        Parse::Marpa::Parse::EARLEY_SETS,
    ];

    # Set up the defaults for undefined arguments
    $start_earleme //= 0;
    $last_earleme //= $default_parse_set;
    $last_earleme = $default_parse_set if $last_earleme > $default_parse_set;

    # We symbol from the user, so we need to canonicalize it.
    $symbol = canonical_name($symbol) if defined $symbol;

    EARLEME: for (my $earleme = $last_earleme; $earleme >= $start_earleme; $earleme--) {
        my $earley_set = $earley_sets->[ $earleme ];

        ITEM: for my $earley_item (@$earley_set) {
            my ($state, $parent) = @{$earley_item}[
                Parse::Marpa::Earley_item::STATE,
                Parse::Marpa::Earley_item::PARENT,
            ];
            next ITEM unless $parent == $start_earleme;
            if (defined $symbol) {
                my $complete_rules = $state->[ Parse::Marpa::SDFA::COMPLETE_RULES ]->{$symbol};
                next ITEM unless $complete_rules;
            }
            my $complete_lhs = $state->[ Parse::Marpa::SDFA::COMPLETE_LHS ];
            next ITEM unless scalar @$complete_lhs;
            return ($earleme, $complete_lhs);
        } # ITEM
    } # EARLEME
    return;
}

sub initialize_children {
    my $item = shift;
    my $lhs_symbol = shift;

    $item->[ Parse::Marpa::Earley_item::LHS ] = $lhs_symbol;
    my $lhs_symbol_id = $lhs_symbol->[ Parse::Marpa::Symbol::ID ];

    my (
        $state, $child_rule_choice,
    ) = @{$item}[
        Parse::Marpa::Earley_item::STATE,
        Parse::Marpa::Earley_item::RULE_CHOICE,
    ];

    if (not defined $child_rule_choice) {
        $child_rule_choice = 0;
    }
    my $child_rules = $state->[ Parse::Marpa::SDFA::COMPLETE_RULES ] -> [ $lhs_symbol_id ];
    my $rule = $child_rules->[ $child_rule_choice ];
    local($Parse::Marpa::This::rule) = $rule;
    my ($rhs) = @{$rule}[ Parse::Marpa::Rule::RHS ];

    local($Parse::Marpa::This::v) = []; # to store values in

    CHILD: for (my $child_number = $#$rhs; $child_number >= 0; $child_number--) {

        my $child_symbol = $rhs->[ $child_number ];
        my $nulling = $child_symbol->[ Parse::Marpa::Symbol::NULLING ];

        if ($nulling)
        {
            $Parse::Marpa::This::v->[ $child_number ]
                = $child_symbol->[ Parse::Marpa::Symbol::NULL_VALUE ];
           next CHILD;
        }

        my (
            $tokens, $links,
            $previous_value, $previous_predecessor,
            $item_set,
        ) = @{$item}[
            Parse::Marpa::Earley_item::TOKENS,
            Parse::Marpa::Earley_item::LINKS,
            Parse::Marpa::Earley_item::VALUE,
            Parse::Marpa::Earley_item::PREDECESSOR,
            Parse::Marpa::Earley_item::SET,
        ];

        if (defined $previous_value) {
            $Parse::Marpa::This::v->[ $child_number ] = $$previous_value;
            $item = $previous_predecessor;
            next CHILD;
        }

        unless (defined $child_rules) {
            $child_rules = [];
            $child_rule_choice = 0;
        }

        if (@$tokens) {
            my ($predecessor, $value) = @{$tokens->[0]};
            @{$item}[
                Parse::Marpa::Earley_item::TOKEN_CHOICE,
                Parse::Marpa::Earley_item::LINK_CHOICE,
                Parse::Marpa::Earley_item::RULE_CHOICE,
                Parse::Marpa::Earley_item::RULES,
                Parse::Marpa::Earley_item::VALUE,
                Parse::Marpa::Earley_item::PREDECESSOR,
                Parse::Marpa::Earley_item::POINTER,
            ] = (
                0, 0,
                $child_rule_choice, $child_rules,
                \$value, $predecessor,
                $child_symbol,
            );
            if ($Parse::Marpa::This::trace_iteration_changes) {
                 my $predecessor_set = $predecessor->[
                    Parse::Marpa::Earley_item::SET,
                 ];
                 print $Parse::Marpa::This::trace_fh
                 "Initializing token value of ",
                 brief_earley_item($item), ", ",
                 $child_symbol->[ Parse::Marpa::Symbol::NAME ],
                 " at ", $predecessor_set, "-", $item_set,
                 " to ", Dumper($value);
            }
            $Parse::Marpa::This::v->[ $child_number ] = $value;
            weaken($predecessor->[Parse::Marpa::Earley_item::SUCCESSOR] = $item);
            $item = $predecessor;
            next CHILD;
        }

        # We've eliminated nulling symbols and symbols caused by tokens,
        # so we have to have a symbol caused by a completion

        my ($predecessor, $cause) = @{$links->[0]};
        weaken($cause->[ Parse::Marpa::Earley_item::EFFECT ] = $item);
        my $value = initialize_children($cause, $child_symbol);
        @{$item}[
            Parse::Marpa::Earley_item::TOKEN_CHOICE,
            Parse::Marpa::Earley_item::LINK_CHOICE,
            Parse::Marpa::Earley_item::RULE_CHOICE,
            Parse::Marpa::Earley_item::RULES,
            Parse::Marpa::Earley_item::VALUE,
            Parse::Marpa::Earley_item::PREDECESSOR,
            Parse::Marpa::Earley_item::POINTER,
        ] = (
            0, 0,
            $child_rule_choice, $child_rules,
            \$value, $predecessor,
            $child_symbol,
        );
        if ($Parse::Marpa::This::trace_iteration_searches) {
             my $predecessor_set = $predecessor->[
                Parse::Marpa::Earley_item::SET,
             ];
             print $Parse::Marpa::This::trace_fh
             "Initializing caused value of ",
             brief_earley_item($item), ", ",
             $child_symbol->[ Parse::Marpa::Symbol::NAME ],
             " at ", $predecessor_set, "-", $item_set,
             " to ", Dumper($value);
        }
        $Parse::Marpa::This::v->[ $child_number ] = $value;
        weaken($predecessor->[Parse::Marpa::Earley_item::SUCCESSOR] = $item);
        $item = $predecessor;

    }

    my $closure = $rule->[ Parse::Marpa::Rule::CLOSURE ];
    my @warnings;
    return $closure unless defined $closure;

    my $result = eval { $closure->() };
    if ($@) {
        croak("Problem computing value for rule ",
            Parse::Marpa::brief_rule($rule),
            "\n",
            $@,
        );
    }

    $result;

}

sub value {
    my $parse = shift;

    my $start_item
        = $parse->[ Parse::Marpa::Parse::START_ITEM ];
    return unless defined $start_item;
    my $value_ref = $start_item->[ Parse::Marpa::Earley_item::VALUE ];
    croak("No value defined") unless defined $value_ref;
    return $$value_ref
}

# TODO Add check to ensure that the argument is an evaluated parse.
sub next {
    my $parse = shift;

    croak("No parse supplied") unless defined $parse;
    my $parse_class = ref $parse;
    my $right_class = "Parse::Marpa::Parse";
    croak("Don't parse argument is class: $parse_class; should be: $right_class")
        unless $parse_class eq $right_class;

    local($Parse::Marpa::This::parse) = $parse;
    my (
        $grammar, $start_item,
        $current_parse_set,
        $trace_iteration_searches,
        $trace_iteration_changes,
        $volatile,
    ) = @{$parse}[
        Parse::Marpa::Parse::GRAMMAR,
        Parse::Marpa::Parse::START_ITEM,
        Parse::Marpa::Parse::CURRENT_PARSE_SET,
        Parse::Marpa::Parse::TRACE_ITERATION_SEARCHES,
        Parse::Marpa::Parse::TRACE_ITERATION_CHANGES,
        Parse::Marpa::Parse::VOLATILE,
    ];
    croak("Parse not initialized: no start item") unless defined $start_item;
    my $start_value = $start_item->[ Parse::Marpa::Earley_item::VALUE ];
    croak("Parse not initialized: no start value") unless defined $start_value;
    local($Parse::Marpa::This::grammar) = $grammar;
    local($Parse::Marpa::This::trace_iteration_searches) = $trace_iteration_searches;
    local($Parse::Marpa::This::trace_iteration_changes) = $trace_iteration_changes;
    local($Data::Dumper::Terse) = 1;

    my $trace_fh = $grammar-> [ Parse::Marpa::Grammar::TRACE_FILE_HANDLE ];
    local($Parse::Marpa::This::trace_fh) = $trace_fh;

    clear_notations($parse) if $volatile;

    # find the "bottom left corner item", by following predecessors,
    # and causes when there is no predecessor

    EVALUATION: for (;;) {
        my $item = $start_item;
        my $find_left_corner = 1;

        # Look for an item we can (potentially) iterate.
        ITERATION_CANDIDATE: for (;;) {

            # if we set the flag to find the item in the bottom
            # left hand corner, do so
            LEFT_CORNER_CANDIDATE: while ($find_left_corner) {

                # undefine the values as we go along
                $item->[ Parse::Marpa::Earley_item::VALUE ] = undef;

                my $predecessor = $item->[ Parse::Marpa::Earley_item::PREDECESSOR ];

                # Follow the predecessors all the way until
                # just before the prediction.  The prediction
                # is the item whose "parent" is the same as its
                # Earley set.
                if (defined $predecessor->[ Parse::Marpa::Earley_item::POINTER ]) {
                    $item = $predecessor;
                    next LEFT_CORNER_CANDIDATE;
                }

                # At the far left end, see if we have a cause (or
                # child) item.  If so, descend.
                my ($link_choice, $links)
                    = @{$item}[
                        Parse::Marpa::Earley_item::LINK_CHOICE,
                        Parse::Marpa::Earley_item::LINKS,
                    ];
                last LEFT_CORNER_CANDIDATE if $link_choice > $#$links;
                $item = $links->[$link_choice]->[1];
                if ($Parse::Marpa::This::trace_iteration_search) {
                     print $Parse::Marpa::This::trace_fh
                         "Seeking left corner at ", brief_earley_item($item), "\n";
                }

            } # LEFT_CORNER_CANDIDATE

            # We have our candidate, now try to iterate it,
            # exhausting the rule choice if necessary

            # TODO: if this block necessary ?

            my (
                $token_choice, $tokens,
                $link_choice, $links,
                $rule_choice, $rules,
            ) = @{$item}[
                Parse::Marpa::Earley_item::TOKEN_CHOICE,
                Parse::Marpa::Earley_item::TOKENS,
                Parse::Marpa::Earley_item::LINK_CHOICE,
                Parse::Marpa::Earley_item::LINKS,
                Parse::Marpa::Earley_item::RULE_CHOICE,
                Parse::Marpa::Earley_item::RULES,
            ];

            # If we can increment the token_choice, this is our
            # candidate
            if ($token_choice < $#$tokens) {
                $token_choice++;
                $item->[ Parse::Marpa::Earley_item::TOKEN_CHOICE ] = $token_choice;
                last ITERATION_CANDIDATE;
            }

            # If we can increment the link_choice, this is our
            # candidate
            if ($link_choice < $#$links) {
                $link_choice++;
                $item->[ Parse::Marpa::Earley_item::LINK_CHOICE ] = $link_choice;
                last ITERATION_CANDIDATE;
            }

            # Iterate rule, if possible
            if ($rule_choice < $#$rules) {

                @{$item}[
                    Parse::Marpa::Earley_item::RULE_CHOICE,
                    Parse::Marpa::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Earley_item::TOKEN_CHOICE,
                ] = (
                     ++$rule_choice, 0, 0,
                );

                if ($Parse::Marpa::This::trace_iteration_changes) {
                     print $Parse::Marpa::This::trace_fh
                         "Incremented rule choice for ",
                         brief_earley_item($item), ", ",
                         Parse::Marpa::Parse::brief_earley_item($item),
                         " to ", $rule_choice, "\n";
                }

                last ITERATION_CANDIDATE;

            }

            # This candidate could not be iterated.  Set up to look
            # for another.

            @{$item}[
                Parse::Marpa::Earley_item::VALUE,
                Parse::Marpa::Earley_item::RULE_CHOICE,
            ] = (undef, 0);

            my ($successor, $effect) = @{$item}[
                Parse::Marpa::Earley_item::SUCCESSOR,
                Parse::Marpa::Earley_item::EFFECT,
            ];

            $find_left_corner = 0;

            if (defined $successor) {
                $item = $successor;
                if ($Parse::Marpa::This::trace_iteration_changes) {
                     print $Parse::Marpa::This::trace_fh
                         "Trying to iterate successor ", brief_earley_item($item), "\n";
                }

                # Did the successor have a cause?  If so iterate from
                # it or the "left corner" below it
                my ($link_choice, $links)
                    = @{$item}[
                        Parse::Marpa::Earley_item::LINK_CHOICE,
                        Parse::Marpa::Earley_item::LINKS,
                    ];
                if ($link_choice <= $#$links) {
                    @{$item}[
                        Parse::Marpa::Earley_item::VALUE,
                        Parse::Marpa::Earley_item::RULE_CHOICE,
                    ] = (undef, 0);
                    $item = $links->[$link_choice]->[1];
                    $find_left_corner = 1;
                }
                next ITERATION_CANDIDATE;

            }

            # If no more candidates, we are finished with all the
            # evaluations for this parse
            return unless defined $effect;

            $item = $effect;
            if ($Parse::Marpa::This::trace_iteration_search) {
                 print $Parse::Marpa::This::trace_fh
                     "Trying to iterate effect ", brief_earley_item($item), "\n";
            }

        } # ITERATION_CANDIDATE

        # We've found and iterated an item.
        # Now try to evaluate it.
        # First, climb the tree, recalculating
        # all the successor and effect values.

        my $reason = "Iterating";

        STEP_UP_TREE: for (;;) {

            RESET_VALUES: {

                my (
                    $token_choice, $tokens,
                    $link_choice, $links,
                    $rule_choice,
                    $pointer, $item_set
                ) = @{$item}[
                    Parse::Marpa::Earley_item::TOKEN_CHOICE,
                    Parse::Marpa::Earley_item::TOKENS,
                    Parse::Marpa::Earley_item::LINK_CHOICE,
                    Parse::Marpa::Earley_item::LINKS,
                    Parse::Marpa::Earley_item::RULE_CHOICE,
                    Parse::Marpa::Earley_item::POINTER,
                    Parse::Marpa::Earley_item::SET,
                ];

                if ($token_choice <= $#$tokens) {
                    my ($predecessor, $value) = @{$tokens->[$token_choice]};
                    @{$item}[
                        Parse::Marpa::Earley_item::VALUE,
                        Parse::Marpa::Earley_item::PREDECESSOR,
                    ] = (
                        \$value,
                        $predecessor,
                    );
                    if ($Parse::Marpa::This::trace_iteration_changes) {
                         my $predecessor_set = $predecessor->[
                            Parse::Marpa::Earley_item::SET,
                         ];
                         print $Parse::Marpa::This::trace_fh
                             $reason, " token choice for ",
                             brief_earley_item($item),
                             " to ", $token_choice, ", ",
                             $pointer->[ Parse::Marpa::Symbol::NAME ],
                             " at ", $predecessor_set, "-", $item_set,
                             " = ", Dumper($value);
                    }
                    weaken($predecessor->[Parse::Marpa::Earley_item::SUCCESSOR] = $item);
                    last RESET_VALUES;
                }

                if ($link_choice <= $#$links) {
                    my ($predecessor, $cause) = @{$links->[$link_choice]};
                    weaken($cause->[ Parse::Marpa::Earley_item::EFFECT ] = $item);
                    my $value = initialize_children($cause, $pointer);
                    @{$item}[
                        Parse::Marpa::Earley_item::VALUE,
                        Parse::Marpa::Earley_item::PREDECESSOR,
                    ] = (
                        \$value,
                        $predecessor,
                    );
                    if ($Parse::Marpa::This::trace_iteration_changes) {
                         my $predecessor_set = $predecessor->[
                            Parse::Marpa::Earley_item::SET,
                         ];
                         print $Parse::Marpa::This::trace_fh
                         $reason , " cause choice for ",
                         brief_earley_item($item),
                         " to ", $link_choice, ", ",
                         $pointer->[ Parse::Marpa::Symbol::NAME ],
                         " at ", $predecessor_set, "-", $item_set,
                         " = ", Dumper($value);
                    }
                    weaken($predecessor->[Parse::Marpa::Earley_item::SUCCESSOR] = $item);
                    last RESET_VALUES;
                }

            } # RESET_VALUES

            $reason = "Recalculating Parent";

            my ($successor, $effect) = @{$item}[
                Parse::Marpa::Earley_item::SUCCESSOR,
                Parse::Marpa::Earley_item::EFFECT,
            ];

            if (defined $successor) {
                $item = $successor;
                next STEP_UP_TREE;
            }

            # If no successor or effect, we're at the top of the tree
            last STEP_UP_TREE unless defined $effect;

            $item = $effect;

        } # STEP_UP_TREE

        # Initialize everything else left unvalued.
        initial($parse, $current_parse_set);

        # Rejected evaluations are not yet implemented.
        # Therefore this evaluation pass succeeded.
        return 1;

    } # EVALUATION

    return;

}

sub show_value {
    my $value_ref = shift;
    my $ii = shift;
    return "none" unless defined $value_ref;
    my $value = $$value_ref;
    return "undef" unless defined $value;
    if ($ii) {
        my $type  = ref $value;
        return $type if $type;
    }
    return "$value";
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
