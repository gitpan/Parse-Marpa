package Parse::Marpa;

require 5.008000;

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

use warnings;
use strict;

use Carp;
use Scalar::Util qw(weaken);

our $VERSION = '0.001_027';
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

package Parse::Marpa::Symbol;

use constant ID              => 0;
use constant NAME            => 1;
use constant LHS             => 2;    # rules with this as the lhs,
                                      # as a ref to an array of rule refs
use constant RHS             => 3;    # rules with this in the rhs,
                                      # as a ref to an array of rule refs
use constant NULLABLE        => 4;    # can match null?
use constant START_REACHABLE => 5;    # reachable from start symbol?
use constant INPUT_REACHABLE => 6;    # reachable from input symbol?
use constant NULL_VALUE      => 7;    # ref to value of nulling symbol,
                                      # otherwise undef
use constant START           => 8;    # is one of the start symbols?
use constant REGEX           => 9;    # regex, for terminals; undef otherwise
use constant NULL_ALIAS      => 10;   # for a non-nulling symbol,
                                      # ref of a its nulling alias,
                                      # if there is one
                                      # otherwise undef
use constant TERMINAL        => 11;   # terminal?

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
use constant ORIGINAL_CLOSURE         => 9;    # closure assigned when rule created
use constant CLOSURE                  => 10;   # closure to use
use constant TYPE                     => 11;   # rule types

use constant CHAF_HEAD                => 1; # first segment of a CHAF rewrite
use constant CHAF_PIECE               => 2; # middle segment of a CHAF rewrite
use constant CHAF_TAIL                => 3; # last segment of a CHAF rewrite
use constant NORMAL                   => 4; # any other rule

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

package Parse::Marpa::LR0_item;

use constant RULE     => 0;
use constant POSITION => 1;

package Parse::Marpa::Grammar;

use constant RULES           => 0;     # array of rule refs
use constant SYMBOLS         => 1;     # array of symbol refs
use constant RULE_HASH       => 2;     # hash by name of symbol refs
use constant SYMBOL_HASH     => 3;     # hash by name of symbol refs
use constant START           => 4;     # ref to start symbol
use constant NFA             => 5;     # array of states
use constant SDFA            => 6;     # array of states
use constant SDFA_BY_NAME    => 7;     # hash from SDFA name to SDFA reference
use constant NULLABLE_SYMBOL => 8;     # array of refs of the nullable symbols
use constant ACADEMIC        => 9;     # true if this is a textbook grammar,
                                       # for checking the NFA and SDFA, and NOT
                                       # for actual Earley parsing
use constant DEFAULT_NULL_VALUE => 10; # default value for nulling symbols

package Parse::Marpa;

# Constructor

sub new {
    my $class = shift;
    my %args  = @_;

    my $rules;
    my $start;

    # Academic grammar?  An "academic grammar" is one, usually from a textbook, which we are using
    # to debug the NFA and SDFA logic.  We leave it unchanged.  Since we don't augment it, we can't
    # parse with this grammar.  It's only useful to test the NFA and SDFA logic
    my $academic = 0;
    my $default_null_value = "";

    my %arg_logic = (
        "rules"    => sub { $rules    = $_[0] },
        "start"    => sub { $start    = $_[0] },
        "academic" => sub { $academic = $_[0] },
        "default_null_value" => sub { $default_null_value = $_[0] },
    );

    while ( my ( $arg, $value ) = each %args ) {
        my $closure = $arg_logic{$arg};
        croak("Undefined argument to new $class: $arg")
            unless defined $closure;
        $closure->($value);
    }

    croak("No rules specified")        unless defined $rules;
    croak("No start symbol specified") unless defined $start;

    my $grammar = [];
    @{$grammar}[
        Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::SYMBOL_HASH,
        Parse::Marpa::Grammar::RULES,
        Parse::Marpa::Grammar::RULE_HASH,
        Parse::Marpa::Grammar::SDFA_BY_NAME,
        Parse::Marpa::Grammar::ACADEMIC,
        Parse::Marpa::Grammar::DEFAULT_NULL_VALUE,
        ]
        = ( [], {}, [], {}, {}, $academic, $default_null_value );
    bless( $grammar, $class );

    add_user_rules( $grammar, $rules );
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
    if ( $symbol->[Parse::Marpa::Symbol::NULL_VALUE] )  { $text .= " nulling"; }
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
            grep { $_->[Parse::Marpa::Symbol::NULL_VALUE] } @$symbols );
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
    my $ruleno  = -1;
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
    my ( $id, $name, $NFA_states, $transition, $tag) = @{$state}[
        Parse::Marpa::SDFA::ID,         Parse::Marpa::SDFA::NAME,
        Parse::Marpa::SDFA::NFA_STATES, Parse::Marpa::SDFA::TRANSITION,
        Parse::Marpa::SDFA::TAG
    ];

    $text .= defined $tags ? "St" . $tag : "S" . $id;
    $text .= ": " . $name . "\n";
    for my $NFA_state (@$NFA_states) {
        my $item = $NFA_state->[Parse::Marpa::NFA::ITEM];
        $text .= show_item($item) . "\n";
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
    my $regex   = shift;
    my ( $symbol_hash, $symbols )
        = @{$grammar}[
            Parse::Marpa::Grammar::SYMBOL_HASH,
            Parse::Marpa::Grammar::SYMBOLS
        ];

    if ( "" =~ $regex ) {
        croak("Attempt to add nullable terminal: $name");
    }

    # I allow redefinition of a LHS symbol as a terminal
    # I need to test that this works, or unallow it
    $name = canonical_name($name);
    my $symbol = $symbol_hash->{$name};
    if ( defined $symbol) {

        if ($symbol->[ Parse::Marpa::Symbol::TERMINAL ]) {
            croak("Attempt to add duplicate terminal: $name");
        }

        @{$symbol}[
            Parse::Marpa::Symbol::INPUT_REACHABLE,
            Parse::Marpa::Symbol::NULL_VALUE,
            Parse::Marpa::Symbol::REGEX,
            Parse::Marpa::Symbol::TERMINAL
        ] = (1, 0, $regex, 1);

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
        Parse::Marpa::Symbol::NULL_VALUE,
        Parse::Marpa::Symbol::REGEX,
        Parse::Marpa::Symbol::TERMINAL
        ] = (
            $symbol_count, $name, [], [],
            0, 1, 0, $regex, 1
        );

    push( @$symbols, $new_symbol );
    weaken( $symbol_hash->{$name} = $new_symbol );
}

sub assign_symbol {
    my $grammar = shift;
    my $name    = shift;
    my ( $symbol, $symbols ) =
        @{$grammar}[ Parse::Marpa::Grammar::SYMBOL_HASH,
        Parse::Marpa::Grammar::SYMBOLS ];

    my $symbol_count = @$symbols;
    my $ret          = $symbol->{$name};
    if ( not defined $ret ) {
        @{$ret}[
            Parse::Marpa::Rule::ID,  Parse::Marpa::Rule::NAME,
            Parse::Marpa::Rule::LHS, Parse::Marpa::Rule::RHS
            ]
            = ( $symbol_count, $name, [], [] );
        push( @$symbols, $ret );
        weaken( $symbol->{$name} = $ret );
    }
    $ret;
}

sub assign_user_symbol {
    my $self = shift;
    my $name = shift;
    assign_symbol( $self, canonical_name($name) );
}

sub add_user_rule {
    my $self      = shift;
    my $lhs_name  = shift;
    my $rhs_names = shift;
    my $closure   = shift;

    add_rule(
        $self,
        assign_symbol( $self, canonical_name($lhs_name) ),
        [ map { assign_symbol( $self, canonical_name($_) ); } @$rhs_names ],
        $closure
    );
}

sub add_rule {
    my $grammar = shift;
    my $lhs     = shift;
    my $rhs     = shift;
    my $closure = shift;

    my ( $rule, $rules, $default_null_value ) =
        @{$grammar}[
            Parse::Marpa::Grammar::RULE_HASH,
            Parse::Marpa::Grammar::RULES,
            Parse::Marpa::Grammar::DEFAULT_NULL_VALUE,
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
        Parse::Marpa::Rule::ORIGINAL_CLOSURE
        ]
        = (
        $rule_count, "rule $rule_count",
        $lhs, $rhs, $nulling, $nulling, $nulling,
        $closure
        );

    # Don't allow the same rule twice
    my $rule_key =
        join( ",", map { $_->[Parse::Marpa::Symbol::ID] } ( $lhs, @$rhs ) );
    croak( "Duplicate rule:" . show_rule($new_rule) ) if $rule->{$rule_key};

    # if this is a nulling rule with a closure,
    # we get the null_value of the lhs from that
    if ($nulling) {
        $lhs->[ Parse::Marpa::Symbol::NULL_VALUE ]
            = $closure ? \($closure->())
            : \$default_null_value;
    }

    $rule->{$rule_key} = $new_rule;

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
    $new_rule;
}

# add one or more rules
sub add_user_rules {
    my $grammar  = shift;
    my $rules = shift;

    rule: for my $rule (@$rules) {
        my $arg_count = @$rule;
        if ( $arg_count > 3 or $arg_count < 1) {
            croak("rule must have from 1 to 3 arguments");
        }
        my ($lhs, $rhs, $closure) = @$rule;
        $rhs = [] unless defined $rhs;
        if ( ref $rhs eq "Regexp" ) {
            add_terminal( $grammar, $lhs, $rhs );
            next rule;
        }

        add_user_rule( $grammar, $lhs, $rhs, $closure);
    }
}

sub set_start {
    my $grammar    = shift;
    my $start_name = shift;

    my $symbol = $grammar->[Parse::Marpa::Grammar::SYMBOL_HASH];
    my $start  = $symbol->{$start_name};
    if ( not defined $start ) {
        croak( "start symbol " . $start_name . " not defined\n" );
    }
    if ( not scalar @{ $start->[Parse::Marpa::Symbol::LHS] } ) {
        croak( "start symbol " . $start_name . " not on LHS of any rule\n" );
    }

    # I think I'll allow the start symbol to be on the RHS of a production.
    # After all, another start symbol will be created in the CHAF rewrite.
    # if ( scalar @{ $start->[Parse::Marpa::Symbol::RHS] } ) {
        # croak( "start symbol " . $start_name . " on RHS\n" );
    # }

    if ( not $start->[Parse::Marpa::Symbol::INPUT_REACHABLE] ) {
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
        grep { $_->[ Parse::Marpa::Symbol::NULL_VALUE ] } @$symbols
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
            next RULE if defined $lhs_symbol->[Parse::Marpa::Symbol::NULL_VALUE];

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
                $lhs_symbol->[Parse::Marpa::Symbol::NULL_VALUE] = $symbol_nulling;
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
                        $rhs_symbol->[Parse::Marpa::Symbol::NULL_VALUE];

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
            or $_->[Parse::Marpa::Symbol::NULL_VALUE]
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
            next NFA_ID if $next_symbol->[Parse::Marpa::Symbol::NULL_VALUE];
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
            next NFA_ID if $next_symbol->[Parse::Marpa::Symbol::NULL_VALUE];
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
    my ( $symbol, $NFA, $start ) = @{$grammar}[
        Parse::Marpa::Grammar::SYMBOLS, Parse::Marpa::Grammar::NFA,
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
        $#$lhs_list = @$symbol;
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
        $state->[Parse::Marpa::SDFA::COMPLETE_RULES] = $complete_rules;
        $state->[Parse::Marpa::SDFA::COMPLETE_LHS] =
            [ map { $_->[Parse::Marpa::Symbol::NAME] }
                @{$symbol}[ grep { $lhs_list->[$_] } ( 0 .. $#$lhs_list ) ] ];
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
    my ( $symbol, $symbols, $default_null_value ) =
        @{$grammar}[
            Parse::Marpa::Grammar::SYMBOL_HASH,
            Parse::Marpa::Grammar::SYMBOLS,
            Parse::Marpa::Grammar::DEFAULT_NULL_VALUE,
        ];
    my ( $start_reachable, $input_reachable, $name ) = @{$nullable_symbol}[
        Parse::Marpa::Symbol::START_REACHABLE,
        Parse::Marpa::Symbol::INPUT_REACHABLE,
        Parse::Marpa::Symbol::NAME
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
        Parse::Marpa::Symbol::NULL_VALUE,
        ]
        = (
        $symbol_count, $alias_name, [], [], $start_reachable,
        $input_reachable, 1, \$default_null_value
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

# rewrite as Chomsky-Horspool-Aycock Form
sub rewrite_as_CHAF {
    my $grammar = shift;
    my ( $rules, $symbols, $start ) = @{$grammar}[
        Parse::Marpa::Grammar::RULES, Parse::Marpa::Grammar::SYMBOLS,
        Parse::Marpa::Grammar::START
    ];

    # add null aliases to symbols which need them
    my $symbol_count = @$symbols;
    SYMBOL: for ( my $ix = 0; $ix < $symbol_count; $ix++ ) {
        my $symbol = $symbols->[$ix];
        my ( $input_reachable, $start_reachable, $null_value, $nullable,
            $null_alias )
            = @{$symbol}[
            Parse::Marpa::Symbol::INPUT_REACHABLE,
            Parse::Marpa::Symbol::START_REACHABLE,
            Parse::Marpa::Symbol::NULL_VALUE,
            Parse::Marpa::Symbol::NULLABLE,
            Parse::Marpa::Symbol::NULL_ALIAS
            ];

        # aliases are added at the end -- stop the iteration once we reach them
        last SYMBOL if $null_alias;

        #  we don't both with unreachable symbols
        next SYMBOL unless $input_reachable;
        next SYMBOL unless $start_reachable;

        # look for proper nullable symbols
        next SYMBOL if $null_value;
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
        RHS_SYMBOL: for ( my $ix = 0; $ix <= $#$rhs; $ix++ ) {
            my $symbol = $rhs->[$ix];
            my ( $null_alias, $null_value ) = @{$symbol}[
                Parse::Marpa::Symbol::NULL_ALIAS,
                Parse::Marpa::Symbol::NULL_VALUE,
            ];
            next RHS_SYMBOL if $null_value;
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
                if ( $proper_nullable1 < $last_nonnullable ) {
                    $subp_end = $proper_nullable1;
                    spice( @$proper_nullables, 0, 2 );
                    $next_subp_lhs = assign_symbol( $grammar,
                              $lhs->[Parse::Marpa::Symbol::NAME] . "[" 
                            . $rule_id . ":"
                            . ( $subp_end + 1 )
                            . "]" );
                    @{$next_subp_lhs}[
                        Parse::Marpa::Symbol::NULLABLE,
                        Parse::Marpa::Symbol::START_REACHABLE,
                        Parse::Marpa::Symbol::INPUT_REACHABLE,
                        Parse::Marpa::Symbol::NULL_VALUE,
                        ]
                        = ( 0, 1, 1, 0 );
                    $subp_factor0_rhs = [
                        @{$rhs}[ $subp_start .. $subp_end ],
                        $next_subp_lhs
                    ];
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
                    Parse::Marpa::Symbol::NULL_VALUE,
                    ]
                    = ( 1, 1, 1, 0 );
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

            for my $factor_rhs (@$factored_rhs) {
                my $new_rule = add_rule( $grammar, $subp_lhs, $factor_rhs );
                @{$new_rule}[
                    Parse::Marpa::Rule::USEFUL,
                    Parse::Marpa::Rule::START_REACHABLE,
                    Parse::Marpa::Rule::INPUT_REACHABLE,
                    Parse::Marpa::Rule::NULLABLE,
                    Parse::Marpa::Rule::NULLING
                    ]
                    = ( 1, 1, 1, 0, 0 );
            }

            # no more
            last SUBPRODUCTION unless $next_subp_lhs;
            $subp_lhs   = $next_subp_lhs;
            $subp_start = $subp_end + 1;
            $nullable   = $subp_start > $last_nonnullable;

        }    # SUBPRODUCTION

    }    # RULE

    # Create a new start symbol
    my $old_start       = $start;
    my $input_reachable = $old_start->[Parse::Marpa::Symbol::INPUT_REACHABLE];
    $start =
        assign_symbol( $grammar,
        $start->[Parse::Marpa::Symbol::NAME] . "[']" );
    @{$start}[
        Parse::Marpa::Symbol::INPUT_REACHABLE,
        Parse::Marpa::Symbol::START_REACHABLE,
        Parse::Marpa::Symbol::START
        ]
        = ( $input_reachable, 1, 1 );

    # Create a new start rule
    my $new_start_rule = add_rule( $grammar, $start, [$old_start] );
    @{$new_start_rule}[
        Parse::Marpa::Rule::INPUT_REACHABLE,
        Parse::Marpa::Rule::START_REACHABLE,
        Parse::Marpa::Rule::USEFUL
        ]
        = ( $input_reachable, 1, 1 );

    if ( $old_start->[Parse::Marpa::Symbol::NULL_ALIAS] ) {
        my $start_alias = alias_symbol( $grammar, $start );
        @{$start_alias}[Parse::Marpa::Symbol::START] = 1;
        my $new_start_rule = add_rule( $grammar, $start_alias, [] );

        # Nulling rules are not considered useful, but the top-level one is an exception
        @{$new_start_rule}[
            Parse::Marpa::Rule::INPUT_REACHABLE,
            Parse::Marpa::Rule::START_REACHABLE,
            Parse::Marpa::Rule::USEFUL,
            ]
            = ( $input_reachable, 1, 1, 1 );
    }
    $grammar->[Parse::Marpa::Grammar::START] = $start;
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
use constant SYMBOL            => 5;   # current symbol
use constant RULES             => 6;   # current list of rules
use constant RULE_CHOICE       => 7;   # current choice of rule
use constant LINK_CHOICE       => 8;   # current choice of link
use constant TOKEN_CHOICE      => 9;   # current choice of token
use constant VALUE             => 10;  # current value
use constant PREDECESSOR       => 11;  # the predecessor link, if we have a value
use constant SUCCESSOR         => 12;  # the predecessor link, in reverse
use constant EFFECT            => 13;  # the cause link, in reverse
                                       # or the "parent" item

use constant FIRST_NOTATION_FIELD   => SYMBOL;

package Parse::Marpa::Parse;

use Scalar::Util qw(weaken);

# Elements of the PARSE structure
use constant GRAMMAR     => 0;    # the grammar used
use constant CURRENT_SET => 1;    # index of the first incomplete Earley set
use constant EARLEY_SETS => 2;    # the array of the Earley sets
use constant EARLEY_HASH => 3;    # the array of hashes used
                                  # to build the Earley sets
use constant LAST_SET    => 4;    # the set being taken as the end of
                                  # parse for an evaluation
use constant START_ITEM  => 5;    # the start item for the current evaluation

# implementation dependent constant, used below in unpack
use constant J_LENGTH => ( length pack( "J", 0, 0 ) );

# Constructor method

sub new {
    my $class   = shift;
    my $grammar = shift;
    my $parse   = [];
    croak("No grammar supplied for new $class") unless defined $grammar;
    my $grammar_class = ref $grammar;
    croak(
        "Don't recognize parse() grammar arg has wrong class: $grammar_class")
        unless $grammar_class eq "Parse::Marpa";

    my $SDFA = $grammar->[Parse::Marpa::Grammar::SDFA];
    croak("Attempt to parse grammar with empty SDFA")
        if not defined $SDFA
            or not scalar @$SDFA;

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
    @{$parse}[ CURRENT_SET, EARLEY_HASH, GRAMMAR, EARLEY_SETS ] =
        ( 0, [$earley_hash], $grammar, [$earley_set] );

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
    $text .= ($ii and defined $tag) ? "St" . $tag : $id;
    $text .= "," . $parent;
}

sub show_earley_item {
    my $item = shift;
    my $ii = shift;
    my $text = brief_earley_item($item, $ii);
    my ( $tokens, $links,
        $rules, $rule_choice, $link_choice, $token_choice,
        $value, $symbol
    ) = @{$item}[
        Parse::Marpa::Earley_item::TOKENS,
        Parse::Marpa::Earley_item::LINKS,
        Parse::Marpa::Earley_item::RULES,
        Parse::Marpa::Earley_item::RULE_CHOICE,
        Parse::Marpa::Earley_item::LINK_CHOICE,
        Parse::Marpa::Earley_item::TOKEN_CHOICE,
        Parse::Marpa::Earley_item::VALUE,
        Parse::Marpa::Earley_item::SYMBOL,
    ];
    for my $token (@$tokens) {
        $text
            .= " [p="
            . brief_earley_item( $token->[0], $ii ) . "; t="
            . $token->[1] . "]";
    }
    for my $link (@$links) {
        $text
            .= " [p="
            . brief_earley_item( $link->[0], $ii ) . "; c="
            . brief_earley_item( $link->[1], $ii ) . "]";
    }
    my @choices;
    push(@choices, "rule choice: $rule_choice") if defined $rule_choice;
    push(@choices, "link choice: $link_choice") if defined $link_choice;
    push(@choices, "token choice: $token_choice") if defined $token_choice;
    push(@choices, "symbol: " . $symbol->[ Parse::Marpa::Symbol::NAME ])
        if defined $symbol;
    $text .= "\n  " . join("; ", @choices) if @choices;
    $text .= "\n  value: " . show_value($value, $ii) if defined $value;
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
    my ( $current_set, $earley_set_list ) =
        @{$parse}[ CURRENT_SET, EARLEY_SETS ];
    my $text = "Current Earley Set: " . $current_set . "\n";
    $text .= show_earley_set_list($earley_set_list, $ii);
}

sub clear_notations {
    my $parse = shift;
    my ( $current_set, $earley_set_list ) =
        @{$parse}[ CURRENT_SET, EARLEY_SETS ];
    for my $earley_set (@$earley_set_list) {
        for my $earley_item (@$earley_set) {
            splice(@$earley_item,
                Parse::Marpa::Earley_item::FIRST_NOTATION_FIELD);
        }
    }
}

# Mutator methods

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

sub token {
    my $parse = shift;

    my ( $earley_set_list, $earley_hash_list, $grammar, $current_set ) =
        @{$parse}[ EARLEY_SETS, EARLEY_HASH, GRAMMAR, CURRENT_SET ];
    my $SDFA = $grammar->[Parse::Marpa::Grammar::SDFA];

    my $earley_set  = $earley_set_list->[$current_set];
    my $earley_hash = $earley_hash_list->[$current_set];

    # It's helpful below to assume there's at least one item in the work list,
    # so for sanity's sake, I treat the empty work list
    # as a special case:
    #
    # If there's nothing in the work list, we're done.
    if ( not defined $earley_set ) {
        $earley_set_list->[$current_set] = [];
        $earley_hash->[$current_set]     = undef;
        $parse->[CURRENT_SET]++;
        return;
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
            # nulling symbols, and internal symbols
            # (including the start symbols) are never terminals
            unless ( $token->[ Parse::Marpa::Symbol::TERMINAL ] ) {
                croack(   "Non-terminal "
                        . $token->[Parse::Marpa::Symbol::NAME]
                        . " supplies as token");
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
            my $key = pack( "JJ", $kernel_state, $parent );
            my $target_earley_item = $target_earley_hash->{$key};
            unless ( defined $target_earley_item ) {
                @{$target_earley_item}[
                    Parse::Marpa::Earley_item::STATE,
                    Parse::Marpa::Earley_item::PARENT,
                    Parse::Marpa::Earley_item::LINKS,
                    Parse::Marpa::Earley_item::TOKENS,
                    Parse::Marpa::Earley_item::SET
                    ]
                    = ( $kernel_state, $parent, [], [], $target_ix );
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
                    Parse::Marpa::Earley_item::LINKS,
                    Parse::Marpa::Earley_item::TOKENS,
                    Parse::Marpa::Earley_item::SET
                ]
                    = ( $resetting_state, $target_ix, [], [], $target_ix );
                $target_earley_hash->{$key} = $new_earley_item;
                push( @$target_earley_set, $new_earley_item );
            }

        }    # ALTERNATIVE

        next EARLEY_ITEM if $current_set == $parent;

        COMPLETE_RULE:
        for my $complete_symbol_name (
            @{ $state->[Parse::Marpa::SDFA::COMPLETE_LHS] } )
        {
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
                        Parse::Marpa::Earley_item::LINKS,
                        Parse::Marpa::Earley_item::TOKENS,
                        Parse::Marpa::Earley_item::SET
                        ]
                        = ( $kernel_state, $grandparent, [], [], $current_set );
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
                        Parse::Marpa::Earley_item::LINKS,
                        Parse::Marpa::Earley_item::TOKENS,
                        Parse::Marpa::Earley_item::SET
                        ]
                        = ( $resetting_state, $current_set, [], [], $current_set);
                    $earley_hash->{$key} = $new_earley_item;
                    push( @$earley_set, $new_earley_item );
                }

            }    # PARENT_ITEM
        }    # COMPLETE_RULE
    }    # EARLEY_ITEM

    # TO DO: Prove that the completion links are UNIQUE

    $earley_set_list->[$current_set] = $earley_set;

    # Free memory for the hash
    $earley_hash_list->[$current_set] = undef;

    # Increment CURRENT_SET
    @{$parse}[CURRENT_SET]++;
}

sub initial {
    my $parse    = shift;
    my $end_set  = shift;

    # TODO: At some point I may need to ensure that evaluation notations are
    # cleared, rather than just assume it.

    croak("No parse supplied") unless defined $parse;
    my $parse_class = ref $parse;
    my $right_class = "Parse::Marpa::Parse";
    croak("Don't parse argument is class: $parse_class; should be: $right_class")
        unless $parse_class eq $right_class;

    my ( $grammar, $current_set, $earley_sets ) = @{$parse}[
        Parse::Marpa::Parse::GRAMMAR,
        Parse::Marpa::Parse::CURRENT_SET,
        Parse::Marpa::Parse::EARLEY_SETS
    ];

    # variables for the loop over the target earley set,
    # and to be set in it.
    unless (defined $end_set) {
        $end_set = $current_set - 1;
        if ($end_set < 0) {
            $end_set = 0;
        }
    }

    my $earley_set = $earley_sets->[$end_set];

    # The start rule, if not nulling, must be a pure links rule
    # (no tokens) because I don't allow tokens to be recognized
    # for the start symbol

    my $current_item;
    my $start_rule;

    # mark start items with LHS?
    EARLEY_ITEM: for (my $ix = 0; $ix <= $#$earley_set; $ix++) {
        $current_item = $earley_set->[$ix];
        my $state = $current_item->[ Parse::Marpa::Earley_item::STATE ];
        $start_rule = $state->[Parse::Marpa::SDFA::START_RULE];
        last EARLEY_ITEM if $start_rule;
    }

    return unless $start_rule;

    @{$parse}[
        Parse::Marpa::Parse::START_ITEM,
        Parse::Marpa::Parse::LAST_SET,
    ] = ( $current_item, $end_set );

    my  $lhs = $start_rule->[ Parse::Marpa::Rule::LHS ];
    my $null_value_ref = $lhs->[ Parse::Marpa::Symbol::NULL_VALUE ];

    if (defined $null_value_ref) {
         @{$current_item}[
             Parse::Marpa::Earley_item::VALUE,
             Parse::Marpa::Earley_item::TOKEN_CHOICE,
             Parse::Marpa::Earley_item::LINK_CHOICE,
             Parse::Marpa::Earley_item::RULE_CHOICE,
             Parse::Marpa::Earley_item::RULES,
         ] = ($null_value_ref, 0, 0, 0, []);
    }

    $current_item->[ Parse::Marpa::Earley_item::VALUE ]
        = \(initialize_children($current_item, $lhs));

}

sub initialize_children {
    my $item = shift;
    my $symbol = shift;

    my $symbol_id = $symbol->[ Parse::Marpa::Symbol::ID ];

    my ($state) = @{$item}[
        Parse::Marpa::Earley_item::STATE,
    ];

    my $child_rules = $state->[ Parse::Marpa::SDFA::COMPLETE_RULES ] -> [ $symbol_id ];
    my $child_rule_choice = 0;
    my $rule = $child_rules->[0];
    my ($rhs) = @{$rule}[ Parse::Marpa::Rule::RHS ];

    my @v; # to store values in

    CHILD: for (my $child_number = $#$rhs; $child_number >= 0; $child_number--) {

        my $child_symbol = $rhs->[ $child_number ];
        my $null_value_ref = $child_symbol->[ Parse::Marpa::Symbol::NULL_VALUE ];

        if ($null_value_ref)
        {
           $v[ $child_number ] = $$null_value_ref;
           next CHILD;
        }

        my ($tokens, $links, $previous_value, $previous_predecessor)
            = @{$item}[
                Parse::Marpa::Earley_item::TOKENS,
                Parse::Marpa::Earley_item::LINKS,
                Parse::Marpa::Earley_item::VALUE,
                Parse::Marpa::Earley_item::PREDECESSOR,
            ];

        if (defined $previous_value) {
            $v[ $child_number ] = $$previous_value;
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
            ] = (0, -1, $child_rule_choice, $child_rules, \$value, $predecessor);
            $v[ $child_number ] = $value;
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
        ] = (0, 0, $child_rule_choice, $child_rules, \$value, $predecessor);
        $v[ $child_number ] = $value;
        weaken($predecessor->[Parse::Marpa::Earley_item::SUCCESSOR] = $item);
        $item = $predecessor;

    }

    my $v_count = scalar @v;
    my $result =
        $v_count <= 0 ? "" :
        $v_count == 1 ? $v[0] :
        "(" . join(";", @v) . ")";

}

sub value {
    my $parse = shift;

    my $start_item
        = $parse->[ Parse::Marpa::Parse::START_ITEM ];
    return unless defined $start_item;
    return ${$start_item->[ Parse::Marpa::Earley_item::VALUE ]};
}

# TODO Add check to ensure that the argument is an evaluated parse.
sub next {
    my $parse = shift;

    my ( $start_item, $last_set )
        = @{$parse}[
            Parse::Marpa::Parse::START_ITEM,
            Parse::Marpa::Parse::LAST_SET,
        ];

    # find the "bottom left corner item", by following predecessors,
    # and causes when there is no predecessor

    my $item = $start_item;
    ITEM: for (;;) {
        my $predecessor = $item->[ Parse::Marpa::Earley_item::PREDECESSOR ];

        # undefine the values as we go along
        $item->[ Parse::Marpa::Earley_item::VALUE ] = undef;
        if (defined $predecessor) {
            $item = $predecessor;
            next ITEM;
        }
        my ($link_choice, $links)
            = @{$item}[
                Parse::Marpa::Earley_item::LINK_CHOICE,
                Parse::Marpa::Earley_item::LINKS
            ];
        last ITEM unless defined $link_choice;
        $item = $links->[$link_choice]->[1];
    }
}

sub show_value {
    my $value_ref = shift;
    my $ii = shift;
    return "none" unless defined $value_ref;
    my $value = $$value_ref;
    if ($ii) {
        my $type  = ref $value;
        return $type if $type;
    }
    return "$value";
}


=head1 NAME

Parse::Marpa - Earley's Algorithm, with improvements

=head1 VERSION

Pre-alpha Version

This is strictly a developer's version.
Nothing useful will be found here,
and the documentation is also inchoate.
Those not developing this module will want to wait
for at least a released, beta version.

=cut

=head1 SYNOPSIS

Earley's general parsing algorithm, with LR(0) precomputation

    TO DO
    ...

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

Marpa is, with minor improvements,
the parser described in John Aycock and R.
Nigel Horspool's "Practical Earley Parsing", I<The Computer Journal>,
Vol. 45, No. 6, 2002, pp. 620-630.  This combined LR(0) with Jay
Earley's parsing algorithm.

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
