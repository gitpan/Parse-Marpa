package Parse::Marpa;

use 5.006000;

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
use version; our $VERSION = qv('0.1_9');

use Carp;
use Scalar::Util qw(weaken);

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

# VERY COMMON STRUCTURE ELEMENTS
# Common elements in for many of the arrays
use constant ID   => 0;
use constant NAME => 1;

# ELEMENTS COMMON TO THE RULE and SYMBOL STRUCTURES
use constant LHS => 2;    # for rule, ref of the left hand symbol
   # for symbol, rules with this as the lhs, as a ref to an array of rule refs
use constant RHS => 3;    # array of symbol refs
   # for symbol, rules with this in the rhs, as a ref to an array of rule refs
use constant NULLABLE        => 4;    # can match null
use constant START_REACHABLE => 5;    # reachable from start symbol
use constant INPUT_REACHABLE => 6;    # reachable from input symbol
use constant NULLING         => 7;    # always matches null

# ADDITIONAL ELEMENTS OF THE SYMBOL STRUCTURE
use constant REGEX => 8;      # regex, for terminals; undef otherwise
use constant NULL_ALIAS => 9; # for a non-nulling symbol, ref of a its nulling alias, if there is one
                              # otherwise undef

# ADDITIONAL ELEMENTS of THE RULE STRUCTURE
use constant USEFUL          => 8; # boolean, true if rule is to be used in the NFA

# ELEMENTS of the NFA and SDFA STATE STRUCTURES
use constant ITEM       => 2; # in an NFA: an LR(0) item
use constant NFA_STATES => 2; # in an SDFA: an array of NFA states
use constant TRANSITION => 3; # the transitions, as a hash from symbol name to NFA (SDFA) states
use constant COMPLETE   => 4; # in an SDFA state, an array of the NFA states with
                              # complete LR(0) items

# ELEMENTS of the LR(0) ITEM STRUCTURE
use constant RULE     => 0;
use constant POSITION => 1;

# ELEMENTS of the GRAMMAR STRUCTURE
use constant RULES            => 0;    # array of rule refs
use constant SYMBOL          => 1;    # array of symbol refs
use constant RULE_HASH        => 2;    # hash by name of symbol refs
use constant SYMBOL_HASH      => 3;    # hash by name of symbol refs
use constant START            => 4;    # ref to start symbol
use constant NFA              => 5;    # array of states
use constant SDFA             => 6;    # array of states
use constant SDFA_BY_NAME     => 7;    # hash from SDFA name to SDFA reference
use constant NULLABLE_SYMBOL => 8;    # array of refs of the nullable symbols
use constant ACADEMIC         => 9;    # true if this is a textbook grammar,
                                       # for checking the NFA and SDFA, and NOT
                                       # for actual Earley parsing

# Constructor

sub new {
    my $class = shift;
    my %args = @_;

    my $rules;
    my $start;

    # Academic grammar?  An "academic grammar" is one, usually from a textbook, which we are using
    # to debug the NFA and SDFA logic.  We leave it unchanged.  Since we don't augment it, we can't
    # parse with this grammar.  It's only useful to test the NFA and SDFA logic
    my $academic = 0;

    my %arg_logic = (
       "rules" => sub { $rules = $_[0] },
       "start" => sub { $start = $_[0] },
       "academic" => sub { $academic = $_[0] },
    );

    while (my ($arg, $value) = each %args) {
        my $closure = $arg_logic{$arg};
        croak("Undefined argument to new $class: $arg") unless defined $closure;
        $closure->($value);
    }
    
    croak("No rules specified") unless defined $rules;
    croak("No start symbol specified") unless defined $start;

    my $self  = [];
    @{$self}[ SYMBOL, SYMBOL_HASH, RULES, RULE_HASH, SDFA_BY_NAME, ACADEMIC ] =
        ( [], {}, [], {}, {}, $academic );
    bless( $self, $class );

    _add_user_rules($self, $rules);
    _nullable($self);
    _nulling($self);
    _input_reachable($self);
    _set_start($self, $start);
    _start_reachable($self);
    if ($academic) {
        _setup_academic_grammar($self)
    } else {
        _rewrite_as_QNF($self);
    }
    _create_NFA($self);
    _create_SDFA($self);

    $self;
}

# Viewing Methods (for debugging)

sub show_symbol {
    my $symbol = shift;
    my $text   = "";
    $text .= sprintf "%d: %s, lhs=[%s], rhs=[%s]", $symbol->[ID],
        $symbol->[NAME], join( " ", map { $_->[ID] } @{ $symbol->[LHS] } ),
        join( " ", map { $_->[ID] } @{ $symbol->[RHS] } ),;
    if ( not $symbol->[INPUT_REACHABLE] ) { $text .= " !upreach"; }
    if ( not $symbol->[START_REACHABLE] ) { $text .= " !downreach"; }
    if ( $symbol->[NULLABLE] )            { $text .= " nullable"; }
    if ( $symbol->[NULLING] )            { $text .= " nulling"; }
    $text .= "\n";
}

sub show_symbols {
    my $grammar    = shift;
    my $symbols = $grammar->[SYMBOL];
    my $text    = "";
    for my $symbol_ref (@$symbols) {
        $text .= show_symbol($symbol_ref);
    }
    $text;
}

sub show_nulling_symbols {
    my $self    = shift;
    my $symbols = $self->[SYMBOL];
    join( " ", sort map { $_->[NAME] } grep { $_->[NULLING] } @$symbols );
}

sub show_nullable_symbols {
    my $self    = shift;
    my $symbols = $self->[NULLABLE_SYMBOL];
    join( " ", sort map { $_->[NAME] } @$symbols );
}

sub show_input_reachable_symbols {
    my $self    = shift;
    my $symbols = $self->[SYMBOL];
    join( " ",
        sort map { $_->[NAME] } grep { $_->[INPUT_REACHABLE] } @$symbols );
}

sub show_start_reachable_symbols {
    my $self    = shift;
    my $symbols = $self->[SYMBOL];
    join( " ",
        sort map { $_->[NAME] } grep { $_->[START_REACHABLE] } @$symbols );
}

sub show_rule {
    my $rule = shift;

    my ($lhs, $rhs, $rule_id, $input_reachable, $start_reachable, $nullable, $nulling, $useful)    
        = @{$rule}[LHS, RHS, ID, INPUT_REACHABLE, START_REACHABLE, NULLABLE, NULLING, USEFUL];
    my $text    = "";
    my @comment = ();

    $text .= $rule_id . ": " . $lhs->[NAME] . " ->";
    if (@$rhs) {
        $text .= " " . join( " ", map { $_->[NAME] } @$rhs );
    }
    else {
        push( @comment, "empty" );
    }
    if ( not $input_reachable ) { push( @comment, "!upreach" ); }
    if ( not $start_reachable ) { push( @comment, "!downreach" ); }
    if ( $nullable )            { push( @comment, "nullable" ); }
    if ( $nulling )             { push( @comment, "nulling" ); }
    if ( not $useful )          { push( @comment, "!useful" ); }
    if (@comment) {
        $text .= " " . join( " ", "/*", @comment, "*/" );
    }
    $text .= "\n";
}

sub show_rules {
    my $self   = shift;
    my $rules  = $self->[RULES];
    my $ruleno = -1;
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
        my ( $rule, $position ) = @{$item}[ RULE, POSITION ];
        my @names = ( $rule->[LHS]->[NAME] );
        push( @names, map { $_->[NAME] } @{ $rule->[RHS] } );
        splice( @names, $position + 1, 0, "." );
        splice( @names, 1, 0, "::=" );
        $text .= join( " ", @names );
    }
    $text;
}

sub show_NFA_state {
    my $state = shift;
    my ( $name, $item, $transition ) = @{$state}[ NAME, ITEM, TRANSITION ];
    my $text .= $name . ": " . show_item($item) . "\n";
    for my $symbol_name (sort keys %$transition ) {
        my $transition_states = $transition->{$symbol_name};
        $text .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => "
            . join( " ", map { $_->[NAME] } @$transition_states )
            . "\n";
    }
    $text;
}

sub show_NFA {
    my $self = shift;
    my $text = "";
    my $NFA  = $self->[NFA];
    for my $state (@$NFA) {
        $text .= show_NFA_state($state);
    }
    $text;
}

sub show_SDFA_state {
    my $state = shift;

    my $text = "";
    my ( $id, $name, $NFA_states, $transition ) =
        @{$state}[ ID, NAME, NFA_STATES, TRANSITION ];

    $text .= "S" . $id . ": " . $name . "\n";
    for my $NFA_state (@$NFA_states) {
        my $item = $NFA_state->[ITEM];
        $text .= show_item($item) . "\n";
    }
    for my $symbol_name ( sort keys %$transition ) {
        my ($to_id, $to_name) = @{$transition->{$symbol_name}}[ID, NAME];
        $text .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => S" . $to_id . " (" . $to_name . ")\n";
    }
    $text;
}

sub show_SDFA {
    my $self = shift;
    my $text = "";
    my $SDFA = $self->[SDFA];
    for my $state (@$SDFA) { $text .= show_SDFA_state($state); }
    $text;
}

# Accessor Methods

sub get_symbol {
    my $grammar = shift;
    my $name = shift;
    my $symbol_hash = $grammar->[SYMBOL_HASH];
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

sub _canonical_name {
    my $name = shift;
    $name =~ /]$/ ? $name . "_" : $name;
}

sub _add_terminal {
    my $self  = shift;
    my $name  = shift;
    my $regex = shift;
    my ( $symbol, $symbols ) = @$self[ SYMBOL_HASH, SYMBOL ];

    if ( $symbol->{$name} ) {
        croak("Attempt to add duplicate terminal: $name");
    }

    if ( "" =~ $regex ) {
        croak("Attempt to add nullable terminal: $name");
    }

    my $symbol_count = @$symbols;
    $name = _canonical_name($name);
    if ( exists $symbol->{$name} ) {
        croak("attempt to redefine symbol $name as a terminal");
    }
    my $new_symbol = [];
    @{$new_symbol}[ID, NAME, LHS, RHS, NULLABLE, START_REACHABLE, INPUT_REACHABLE, NULLING, REGEX] = (
        $symbol_count, $name, [], [], 0, undef, 1, 0, $regex
    );
    push( @$symbols, $new_symbol );
    weaken( $symbol->{$name} = $new_symbol );
}

sub _assign_symbol {
    my $self = shift;
    my $name = shift;
    my ( $symbol, $symbols ) = @{$self}[ SYMBOL_HASH, SYMBOL ];

    my $symbol_count = @$symbols;
    my $ret = $symbol->{$name};
    if ( not defined $ret ) {
        @{$ret}[ID, NAME, LHS, RHS] = (
            $symbol_count, $name, [], []
        );
        push( @$symbols, $ret );
        weaken( $symbol->{$name} = $ret );
    }
    $ret;
}

sub _assign_user_symbol {
    my $self = shift;
    my $name = shift;
    _assign_symbol($self, _canonical_name($name));
}

sub _add_user_rule {
    my $self      = shift;
    my $lhs_name  = shift;
    my $rhs_names = shift;

    _add_rule($self,
        _assign_symbol($self, _canonical_name($lhs_name)),
        [ map { _assign_symbol($self, _canonical_name($_)); } @$rhs_names ]
    );
}

sub _add_rule {
    my $self      = shift;
    my $lhs       = shift;
    my $rhs       = shift;

    my ($rule, $rules) = @{$self}[RULE_HASH, RULES];
    my $rule_count = @$rules;
    my $new_rule = [];
    my $nulling = !@$rhs ? 1 : undef;
    @{$new_rule}[ID, NAME, LHS, RHS, NULLABLE, START_REACHABLE, INPUT_REACHABLE, NULLING] = (
        $rule_count, "rule $rule_count", $lhs, $rhs,
            $nulling, undef,  $nulling, $nulling
    );

    # Don't allow the same rule twice
    my $rule_key = join(",", map { $_->[ID] } ($lhs, @$rhs));
    croak("Duplicate rule:" . show_rule($new_rule)) if $rule->{$rule_key};
    $rule->{$rule_key} = $new_rule;

    push( @$rules, $new_rule );
    {
        my $lhs_rules = $lhs->[LHS];
        weaken( $lhs_rules->[ scalar @$lhs_rules ] = $new_rule );
    }
    if ( $nulling ) {
        @{$lhs}[NULLABLE, INPUT_REACHABLE] = (1, 1);
    } else {
        my $last_ref = [];
        SYMBOL: for my $symbol_ref (sort @$rhs) {
            next SYMBOL if $symbol_ref == $last_ref;
            my $rhs_rules = $symbol_ref->[RHS];
            weaken( $rhs_rules->[ scalar @$rhs_rules ] = $new_rule );
            $last_ref = $symbol_ref;
        }
    }
    $new_rule;
}

# add one or more rules
sub _add_user_rules {
    my $self  = shift;
    my $rules = shift;

rule: for my $rule (@$rules) {
        if ( 0 == @$rule ) {
            croak("empty rule");
        }
        elsif ( 2 == @$rule ) {
            my ( $term, $regex ) = @$rule;
            if ( ref $regex eq "Regexp" ) {
                _add_terminal( $self, $term, $regex );
                next rule;
            }

            # fall through if not a terminal definition
        }

        _add_user_rule($self,
            $rule->[0],
            (   $#$rule > 0
                ? [ @{$rule}[ 1 .. $#$rule ] ]
                : []
            )
        );
    }
}

sub _set_start {
    my $self       = shift;
    my $start_name = shift;

    my $symbol = $self->[SYMBOL_HASH];
    my $start  = $symbol->{$start_name};
    if ( not defined $start ) {
        croak( "start symbol " . $start_name . " not defined\n" );
    }
    if ( not scalar @{ $start->[LHS] } ) {
        croak( "start symbol " . $start_name . " not on LHS of any rule\n" );
    }
    if ( scalar @{ $start->[RHS] } ) {
        croak( "start symbol " . $start_name . " on RHS\n" );
    }
    if ( not $start->[INPUT_REACHABLE] ) {
        croak( "start symbol " . $start_name . " not input reachable\n" );
    }
    $self->[START] = $start;
}

# return list of rules reachable from the start symbol;
sub _start_reachable {
    my $self  = shift;
    my $start = $self->[START];

    $start->[START_REACHABLE] = 1;
    my $symbol_work_set = [$start];
    my $rule_work_set   = [];

    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

    SYMBOL_PASS: while ( my $work_symbol = shift @$symbol_work_set ) {
            my $rules_produced = $work_symbol->[LHS];
        PRODUCED_RULE: for my $rule (@$rules_produced) {

                next PRODUCED_RULE if defined $rule->[START_REACHABLE];

              # assume nullable until we hit an unmarked or unreachable symbol
                $rule->[START_REACHABLE] = 1;
                $work_to_do++;
                push( @$rule_work_set, $rule );

            }
        }    # SYMBOL_PASS

    RULE: while ( my $work_rule = shift @$rule_work_set ) {
            my $rhs_symbol = $work_rule->[RHS];

        RHS: for my $symbol (@$rhs_symbol) {

                next RHS if defined $symbol->[START_REACHABLE];
                $symbol->[START_REACHABLE] = 1;
                $work_to_do++;

                push( @$symbol_work_set, $symbol );
            }

        }    # RULE

    }    # work_to_do loop

}

sub _input_reachable {
    my $self = shift;

    my ($rules, $symbols) = @{$self}[RULES, SYMBOL];

    # if a symbol's nullability could not be determined, it was unreachable
    # all nullable symbols are reachable
    for my $symbol (@$symbols) {
        if ( not defined $_->[NULLABLE] ) { $_->[INPUT_REACHABLE] = 0; }
        if ( $_->[NULLABLE] )             { $_->[INPUT_REACHABLE] = 1; }
    }

    # if a rule's nullability could not be determined, it was unreachable
    # all nullable rules are reachable
    for my $rule (@$rules) {
        if ( not defined $_->[NULLABLE] ) { $_->[INPUT_REACHABLE] = 0; }
        if ( $_->[NULLABLE] )             { $_->[INPUT_REACHABLE] = 1; }
    }

    my $symbol_work_set = [];
    $#$symbol_work_set = @$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = @$rules;

    for my $symbol_id (grep { defined $symbols ->  [ $_ ] -> [INPUT_REACHABLE] } ( 0 .. $#$symbols ) ) {
        $symbol_work_set -> [ $symbol_id ] = 1;
    }
    for my $rule_id (grep { defined $rules ->  [ $_ ] -> [INPUT_REACHABLE] } ( 0 .. $#$rules ) ) {
        $rule_work_set -> [ $rule_id ] = 1;
    }
    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

    SYMBOL_PASS: for my $symbol_id (grep { $symbol_work_set->[ $_ ] } (0 .. $#$symbol_work_set) ) {
            my $work_symbol = $symbols->[ $symbol_id ];
            $symbol_work_set->[ $symbol_id ] = 0;

            my $rules_producing = $work_symbol->[RHS];
        PRODUCING_RULE: for my $rule (@$rules_producing) {

                # no work to do -- this rule already has nullability marked
                next PRODUCING_RULE if defined $rule->[INPUT_REACHABLE];

              # assume nullable until we hit an unmarked or unreachable symbol
                my $rule_reachable = 1;

                # are all symbols on the RHS of this rule bottom marked?
            RHS_SYMBOL: for my $rhs_symbol ( @{ $rule->[RHS] } ) {
                    my $reachable = $rhs_symbol->[INPUT_REACHABLE];

# unmarked symbol, change the assumption for rule to undef, but keep scanning for unreachable
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
                    $rule->[INPUT_REACHABLE] = $rule_reachable;
                    $work_to_do++;
                    $rule_work_set -> [ $rule->[ ID ] ] = 1;
                }

            }
        }    # SYMBOL_PASS

    RULE: for my $rule_id (grep { $rule_work_set->[ $_ ] } (0 .. $#$rule_work_set) ) {
            my $work_rule = $rules->[ $rule_id ];
            $rule_work_set->[ $rule_id ] = 0;
            my $lhs_symbol = $work_rule->[LHS];

            # no work to do -- this symbol already has reachability marked
            next RULE if defined $lhs_symbol->[INPUT_REACHABLE];

          # assume unreachable until we hit an unmarked or non-nullable symbol
            my $symbol_reachable = 0;

        LHS_RULE: for my $rule ( @{ $lhs_symbol->[LHS] } ) {

                my $reachable = $rule->[INPUT_REACHABLE];

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
                $lhs_symbol->[INPUT_REACHABLE] = $symbol_reachable;
                $work_to_do++;
                $symbol_work_set->[ $lhs_symbol->[ ID ] ] = 1;
            }

        }    # RULE

    }    # work_to_do loop

}

sub _nulling {
    my $self = shift;

    my ($rules, $symbols) = @{$self}[RULES, SYMBOL];

    my $symbol_work_set = [];
    $#$symbol_work_set = @$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = @$rules;

    for my $rule_id (map { $_->[ ID ] } grep { $_->[NULLING] } @$rules )
    {
        $rule_work_set->[$rule_id] = 1;
    }
    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

    RULE: for my $rule_id (grep { $rule_work_set->[ $_ ] } (0 .. $#$rule_work_set) ) {
            my $work_rule = $rules->[$rule_id];
            $rule_work_set->[$rule_id] = 0;
            my $lhs_symbol = $work_rule->[LHS];

            # no work to do -- this symbol already is marked one way or the other
            next RULE if defined $lhs_symbol->[NULLING];

          # assume nulling until we hit an unmarked or non-nulling symbol
            my $symbol_nulling = 1;

        # make sure that all rules for this lhs are nulling
        LHS_RULE: for my $rule ( @{ $lhs_symbol->[LHS] } ) {

                my $nulling = $rule->[NULLING];

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
                $lhs_symbol->[NULLING] = $symbol_nulling;
                $work_to_do++;

                $symbol_work_set->[ $lhs_symbol->[ID] ] = 1;
            }

        }    # RULE

    SYMBOL_PASS: for my $symbol_id (grep { $symbol_work_set->[ $_ ] } (0 .. $#$symbol_work_set) ) {
            my $work_symbol = $symbols->[ $symbol_id ];
            $symbol_work_set->[ $symbol_id ] = 0;
            my $lhs_symbol = $work_symbol->[LHS];

            my $rules_producing = $work_symbol->[RHS];
        PRODUCING_RULE: for my $rule (@$rules_producing) {

                # no work to do -- this rule already has nulling marked
                next PRODUCING_RULE if defined $rule->[NULLING];

              # assume nulling until we hit an unmarked or unreachable symbol
                my $rule_nulling = 1;

                # are all symbols on the RHS of this rule bottom marked?
            RHS_SYMBOL: for my $rhs_symbol ( @{ $rule->[RHS] } ) {
                    my $nulling = $rhs_symbol->[ NULLING ];

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
                    $rule->[ NULLING ] = $rule_nulling;
                    $work_to_do++;
                    $rule_work_set -> [ $rule -> [ ID ] ] = 1;
                }

            }
        }    # SYMBOL_PASS

    }    # work_to_do loop

}

sub _nullable {
    my $self    = shift;
    my ($rules, $symbols)   = @{$self}[RULES, SYMBOL];

    my $work_to_do = 1;    # boolean to track if current pass has changed anything

    my $symbol_work_set = [];
    $#$symbol_work_set = @$symbols;
    my $rule_work_set = [];
    $#$rule_work_set = @$rules;

    for my $symbol_id (map { $_->[ ID ] }  grep { defined $_->[NULLABLE] } @$symbols ) {
        $symbol_work_set->[ $symbol_id ] = 1;
    }
    for my $rule_id (map { $_->[ ID ] }  grep { defined $_->[NULLABLE] } @$rules ) {
        $rule_work_set->[ $rule_id ] = 1;
    }

    while ($work_to_do) {
        $work_to_do = 0;

    SYMBOL_PASS: for my $symbol_id (grep { $symbol_work_set->[ $_ ] } (0 .. $#$symbol_work_set) ) {
        my $work_symbol = $symbols->[ $symbol_id ];
        $symbol_work_set->[ $symbol_id ] = 0;
        my $rules_producing = $work_symbol->[RHS];

        PRODUCING_RULE: for my $rule (@$rules_producing) {

             # assume nullable until we hit an unmarked or non-nullable symbol
                my $rule_nullable = 1;

                # no work to do -- this rule already has nullability marked
                next PRODUCING_RULE if defined $rule->[NULLABLE];

                # are all symbols on the RHS of this rule bottom marked?
            RHS_SYMBOL: for my $rhs_symbol ( @{ $rule->[RHS] } ) {
                    my $nullable = $rhs_symbol->[NULLABLE];

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
                    $rule->[NULLABLE] = $rule_nullable;
                    $work_to_do++;
                    $rule_work_set->[ $rule->[ ID ] ] = 1;
                }

            }
        }    # SYMBOL_PASS

    RULE: for my $rule_id (grep { $rule_work_set->[ $_ ] } (0 .. $#$rule_work_set) ) {
            my $work_rule = $rules->[ $rule_id ];
            my $lhs_symbol = $work_rule->[LHS];

            # no work to do -- this symbol already has nullability marked
            next RULE if defined $lhs_symbol->[NULLABLE];

         # assume non-nullable until we hit an unmarked or non-nullable symbol
            my $symbol_nullable = 0;

        LHS_RULE: for my $rule ( @{ $lhs_symbol->[LHS] } ) {

                my $nullable = $rule->[NULLABLE];

# unmarked symbol, change the assumption for rule to undef, but keep scanning for nullable
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
                $lhs_symbol->[NULLABLE] = $symbol_nullable;
                $work_to_do++;
                $symbol_work_set->[ $lhs_symbol->[ID] ] = 1;
            }

        }    # RULE

    }    # work_to_do loop

}

sub _create_NFA {
    my $self        = shift;
    my ($rules, $symbols, $symbol_hash, $start, $academic)
        = @{$self}[RULES, SYMBOL, SYMBOL_HASH, START, ACADEMIC];

    $self->[NULLABLE_SYMBOL] = [ grep { $_->[NULLABLE] } @$symbols ];

    my $NFA = [];
    $self->[NFA] = $NFA;

    my $state_id = 0;
    my @NFA_by_item;

    # create S0
    my $s0 = [];
    @{$s0}[ID, NAME, TRANSITION] = ($state_id++, "S0", {});
    push( @$NFA, $s0 );

    # create the other states
RULE: for my $rule (@$rules) {
        my ($rule_id, $rhs, $useful) = @{$rule}[ID, RHS, USEFUL];
        next RULE unless $academic or $useful;
        for my $position ( 0 .. scalar @{ $rhs } ) {
            my $new_state = [];
            @{$new_state}[ID, NAME, ITEM, TRANSITION]
                = ( $state_id, "S" . $state_id, [ $rule, $position ], {} );
            $state_id++;
            push( @$NFA, $new_state );
            $NFA_by_item[ $rule_id ][$position] = $new_state;
        }    # position
    }    # rule

    # now add the transitions
STATE: for my $state (@$NFA) {
        my ( $id, $name, $item, $transition ) = @$state;

       # transitions from state 0:
       # for every rule with the start symbol on its LHS, the item [ rule, 0 ]
        if ( not defined $item ) {
            my @start_rules = @{$start->[LHS]};
            my $start_alias = $start->[NULL_ALIAS];
            push(@start_rules, @{$start_alias->[LHS]}) if defined $start_alias;
            RULE: for my $start_rule (@start_rules) {
                my ($start_rule_id, $useful) = @{$start_rule}[ID, USEFUL];
                next RULE unless $useful;
                push(
                    @{ $transition->{""} },
                    $NFA_by_item[$start_rule_id][0]
                );
            }
            next STATE;
        }

        # transitions from states other than state 0:

        my ( $rule, $position ) = @{$item}[RULE, POSITION];
        my $rule_id     = $rule->[ID];
        my $next_symbol = $rule->[RHS]->[$position];

        # no transitions if position is after the end of the RHS
        if ( not defined $next_symbol ) { next STATE; }

      # the scanning transition: the transition if the position is at symbol X
      # in the RHS, via symbol X, to the state corresponding to the same
      # rule with the position incremented by 1
      # should I use ID as the key for those hashes, or NAME?
        push(
            @{ $transition->{ $next_symbol->[NAME] } },
            $NFA_by_item[$rule_id][ $position + 1 ]
        );

      # the prediction transitions: transitions if the position is at symbol X
      # in the RHS, via the empty symbol, to all states with X on the LHS and
      # position 0
        RULE: for my $predicted_rule ( @{ $next_symbol->[LHS] } ) {
            my ($predicted_rule_id, $useful) = @{$predicted_rule}[ID, USEFUL];
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

sub _assign_SDFA_kernel_state {
    my $self          = shift;
    my $kernel_states = shift;
    my ($NFA_states, $SDFA_by_name, $SDFA) = @{$self}[NFA, SDFA_BY_NAME, SDFA];

    my $kernel_NFA_state_seen     = [];
    my $prediction_NFA_state_seen = [];

# the two split DFA states which we are to find or create.  The kernel SDFA state is the
# return value.
    my $kernel_SDFA_state;
    my $prediction_SDFA_state;

  # pre-allocate the arrays that track whether we've already used an NFA state
    $#$kernel_NFA_state_seen = $#$prediction_NFA_state_seen =
        @$NFA_states;

    # lists of NFA states to followed up on for the closure
    my $kernel_work_list =
        [ grep { not $kernel_NFA_state_seen->[ $_->[ID] ]++ }
            @$kernel_states ];
    my $prediction_work_list = [];

    # create the kernel SDFA state
WORK_LIST: while (@$kernel_work_list) {
        my $next_work_list = [];

    NFA_STATE: for my $NFA_state (@$kernel_work_list) {

            my $to_states = $NFA_state->[TRANSITION]->{""};

            # First the empty transitions.  These will all be predictions,
            # and need to go into the
            # work list for the prediction SDFA state
            if ( defined $to_states ) {
                push( @$prediction_work_list,
                    grep { not $prediction_NFA_state_seen->[ $_->[ID] ]++ }
                        @$to_states );
            }

        SYMBOL:
            for my $nullable_symbol ( @{ $self->[NULLABLE_SYMBOL] } )
            {
                $to_states =
                    $NFA_state->[TRANSITION]->{ $nullable_symbol->[NAME] };
                next SYMBOL unless defined $to_states;
                push( @$next_work_list,
                    grep { not $kernel_NFA_state_seen->[ $_->[ID] ]++ }
                        @$to_states );
            }
        }

        $kernel_work_list = $next_work_list;
    }    # kernel WORK_LIST

    my $NFA_ids = [];
    NFA_ID: for (my $NFA_id = 0; $NFA_id <= $#$NFA_states; $NFA_id++) {
        next NFA_ID unless $kernel_NFA_state_seen->[$NFA_id];
        my $LR0_item = $NFA_states->[$NFA_id]->[ITEM];
        my ($rule, $position) = @{$LR0_item}[RULE, POSITION];
        my $rhs = $rule->[RHS];
        if ($position < @$rhs) 
        {
            my $next_symbol = $rhs->[$position];
            next NFA_ID if $next_symbol->[NULLING];
        }
        push(@$NFA_ids, $NFA_id);
    }
    my $kernel_SDFA_name = join( ",", @$NFA_ids );

    $kernel_SDFA_state = $SDFA_by_name->{$kernel_SDFA_name};

# if we already built the kernel SDFA state, we have also already built any necessary prediction SDFA
# state and linked it, so we're done
    return $kernel_SDFA_state if defined $kernel_SDFA_state;

    # build the kernel state except for the transitions.
    @{$kernel_SDFA_state}[ ID, NAME, NFA_STATES ] =
        ( scalar @$SDFA, $kernel_SDFA_name, [ @{$NFA_states}[@$NFA_ids] ], );
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
                map { $_->[NAME] } @{ $self->[NULLABLE_SYMBOL] } )
            {
                my $to_states = $NFA_state->[TRANSITION]->{$symbol_name};
                next SYMBOL unless defined $to_states;
                push( @$next_work_list,
                    grep { not $prediction_NFA_state_seen->[ $_->[ID] ]++ }
                        @$to_states );
            }
        }

        $prediction_work_list = $next_work_list;
    }    # kernel WORK_LIST

    $NFA_ids = [];
    NFA_ID: for (my $NFA_id = 0; $NFA_id <= $#$NFA_states; $NFA_id++) {
        next NFA_ID unless $prediction_NFA_state_seen->[$NFA_id];
        my $LR0_item = $NFA_states->[$NFA_id]->[ITEM];
        my ($rule, $position) = @{$LR0_item}[RULE, POSITION];
        my $rhs = $rule->[RHS];
        if ($position < @$rhs) 
        {
            my $next_symbol = $rhs->[$position];
            next NFA_ID if $next_symbol->[NULLING];
        }
        push(@$NFA_ids, $NFA_id);
    }
    my $prediction_SDFA_name = join( ",", @$NFA_ids );

    $prediction_SDFA_state = $SDFA_by_name->{$prediction_SDFA_name};

    # if we have not already built the prediction SDFA state, build it
    if ( not defined $prediction_SDFA_state ) {

        # build the prediction state except for the transitions.
        @{$prediction_SDFA_state}[ ID, NAME, NFA_STATES ] = (
            scalar @$SDFA,
            $prediction_SDFA_name, [ @{$NFA_states}[@$NFA_ids] ],
        );
        push( @$SDFA, $prediction_SDFA_state );
        $SDFA_by_name->{$prediction_SDFA_name} = $prediction_SDFA_state;

    }

    # add the empty transition from kernel SDFA state to prediction SDFA state
    $kernel_SDFA_state->[TRANSITION]->{""} = $prediction_SDFA_state;

    # return the kernel SDFA state
    $kernel_SDFA_state;
}

sub _create_SDFA {
    my $self   = shift;
    my ($symbol, $NFA)    = @{$self}[SYMBOL, NFA];
    my $SDFA   = $self->[SDFA] = [];
    my $NFA_s0 = $NFA->[0];

    # next SDFA state to compute transitions for
    my $next_state_id = 0;

    my $initial_NFA_states = $NFA_s0->[TRANSITION]->{""};
    if (not defined $initial_NFA_states) {
        carp("Empty NFA, cannot create SDFA");
        return;
    }
    _assign_SDFA_kernel_state( $self, $initial_NFA_states );

    while ( $next_state_id < scalar @$SDFA ) {

# compute the SDFA state transitions from the transtions of the NFA states of which it
# is composed
        my $NFA_to_states_by_symbol = {};

        my $SDFA_state = $SDFA->[ $next_state_id++ ];

        # aggregrate the transitions, by symbol, for every NFA state in this SDFA
        # state
        for my $NFA_state ( @{ $SDFA_state->[NFA_STATES] } ) {
            my $transition = $NFA_state->[TRANSITION];
        NFA_TRANSITION:
            while ( my ( $symbol, $to_states ) = each(%$transition) )
            {
                next NFA_TRANSITION if $symbol eq "";
                push( @{ $NFA_to_states_by_symbol->{$symbol} }, @$to_states );
            }
        } # $NFA_state

        # for each transition symbol, create the transition to the SDFA kernel state
        while ( my ( $symbol, $to_states ) =
            each(%$NFA_to_states_by_symbol) )
        {
            $SDFA_state->[TRANSITION]->{$symbol} =
                _assign_SDFA_kernel_state( $self, $to_states );
        }
    }

    # For the parse phase, pre-compute the list of names of the lhs's of
    # complete items
    STATE: for my $state (@$SDFA) {
        my $lhs_list = [];
        $#$lhs_list = @$symbol;
        my $NFA_states = $state->[NFA_STATES];
        for my $NFA_state (@$NFA_states) {
            my $item = $NFA_state->[ITEM];
            my ($rule, $position) = @{$item}[RULE, POSITION];
            $lhs_list->[ $rule->[LHS]->[ID] ] = 1 if $position >= @{$rule->[RHS]};
        } # NFA_state
        $state->[COMPLETE]
            = map { $_->[NAME] }
                @{$symbol}[
                    grep { $lhs_list->[ $_ ] } (0 .. $#$lhs_list)
                ];
    } # STATE
}

sub _setup_academic_grammar {
     my $self = shift;
     my $rules = $self->[RULES];
     # in an academic grammar, consider all rules useful
     for my $rule (@$rules) {
         $rule->[USEFUL] = 1;
     }
}

# given a nullable symbol, create a nulling alias and make the first symbol non-nullable
sub _alias_symbol {
    my $self = shift;
    my $nullable_symbol = shift;
    my ( $symbol, $symbols ) = @{$self}[ SYMBOL_HASH, SYMBOL ];
    my ($start_reachable, $input_reachable, $name)
        = @{$nullable_symbol}[START_REACHABLE, INPUT_REACHABLE, NAME];

    # create the new, nulling symbol
    my $symbol_count = @$symbols;
    my $alias_name = $nullable_symbol->[NAME] . "[]";
    my $alias = [];
    @{$alias}[ID, NAME, LHS, RHS, START_REACHABLE, INPUT_REACHABLE, NULLABLE, NULLING]
        = ($symbol_count, $alias_name, [], [], $start_reachable, $input_reachable, 1, 1);
    push( @$symbols, $alias );
    weaken( $symbol->{$alias_name} = $alias );

    # turn the original symbol into a non-nullable with a reference to the new alias
    @{$nullable_symbol}[NULLABLE, NULL_ALIAS] = (0, $alias);
    $alias;
}

=begin Innovation:

Factored Nihilist Normal Form is one of my innovations.   Aycock & Horspool's NNF,
in the worst case, is exponential in the size of the base grammar, and not
exactly pretty in the example they give.  I think realistic grammars are likely
to have productions with many nullables on the right side -- for example, the
right hand side of a production might have several uses of optional whitespace.
Grammars with a lot of these production might seriously bloat in size in NNF.

QNF breaks up productions with a more than a small number nullables on the RHS into
"subproductions".  These are "reassembled" invisibly in evaluating the parse, so
that the semantics of the original grammar are not affected.

=end Innovation:

=cut

# rewrite as Factored Nihilist Normal Form
sub _rewrite_as_QNF {
    my $self = shift;
    my ($rules, $symbols, $start) = @{$self}[RULES, SYMBOL, START];

    # add null aliases to symbols which need them
    my $symbol_count = @$symbols;
    SYMBOL: for (my $ix=0; $ix < $symbol_count; $ix++) {
        my $symbol = $symbols->[$ix];
        my ($input_reachable, $start_reachable, $nulling, $nullable, $null_alias)
            = @{$symbol}[INPUT_REACHABLE, START_REACHABLE, NULLING, NULLABLE, NULL_ALIAS];

        # aliases are added at the end -- stop the iteration once we reach them
        last SYMBOL if $null_alias;

        #  we don't both with unreachable symbols
        next SYMBOL unless $input_reachable;
        next SYMBOL unless $start_reachable;

        # look for proper nullable symbols
        next SYMBOL if $nulling;
        next SYMBOL unless $nullable;

        _alias_symbol( $self, $symbol );
    }

    # mark, or create as needed, the useful rules

    # get the initial rule count -- new rules will be added and we don't iterate
    # over them
    my $rule_count = @$rules;
    RULE: for (my $rule_id = 0; $rule_id < $rule_count; $rule_id++) {
        my $rule = $rules->[$rule_id];
        my ($lhs, $rhs, $input_reachable, $start_reachable, $nulling, $nullable)
            = @{$rule}[LHS, RHS, INPUT_REACHABLE, START_REACHABLE, NULLING, NULLABLE];

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
        RHS_SYMBOL: for (my $ix = 0; $ix <= $#$rhs; $ix++) {
            my $symbol = $rhs->[$ix];
            my ($null_alias, $nulling) = @{$symbol}[NULL_ALIAS, NULLING];
            next RHS_SYMBOL if $nulling;
            if ($null_alias) {
                push(@$proper_nullables, $ix);
                next RHS_SYMBOL;
            }
            $last_nonnullable = $ix;
        }

        # we found no properly nullable symbols in the RHS, so this rule is useful without
        # any changes
        if (@$proper_nullables == 0) {
            $rule->[USEFUL] = 1;
            next RULE;
        }

        # The left hand side of the first subproduction is the lhs of the original rule
        my $subp_lhs = $lhs;
        my $subp_start = 0;

        # break this production into subproductions with a fixed number of proper nullables,
        # then factor out the proper nullables into a set of productions
        # with only non-nullable and nulling symbols.
        SUBPRODUCTION: for (;;) {

            my $subp_end;
            my $proper_nullable0 = $proper_nullables->[0];
            my $subp_proper_nullable0 = $proper_nullable0 - $subp_start;
            my $proper_nullable1;
            my $subp_proper_nullable1;
            my $subp_factor0_rhs;
            my $next_subp_lhs;

            SETUP_SUBPRODUCTION: {

                if (@$proper_nullables == 1) {
                    $subp_end = $#$rhs;
                    $subp_factor0_rhs = [ @{$rhs}[$subp_start .. $subp_end] ];
                    $proper_nullables = [];
                    last SETUP_SUBPRODUCTION;
                }

                $proper_nullable1 = $proper_nullables->[1];
                $subp_proper_nullable1 = $proper_nullable1 - $subp_start;

                if (@$proper_nullables == 2) {
                    $subp_end = $#$rhs;
                    $subp_factor0_rhs = [ @{$rhs}[$subp_start .. $subp_end] ];
                    $proper_nullables = [];
                    last SETUP_SUBPRODUCTION;
                }

                # the following subproduction is non-nullable
                if ($proper_nullable1 < $last_nonnullable) {
                    $subp_end = $proper_nullable1;
                    spice(@$proper_nullables, 0, 2);
                    $next_subp_lhs = _assign_symbol($self,
                        $lhs->[NAME] .  "[" . $rule_id . ":" . ($subp_end + 1) . "]");
                    @{$next_subp_lhs}[NULLABLE, START_REACHABLE, INPUT_REACHABLE, NULLING] =
                        (0, 1, 1, 0);
                    $subp_factor0_rhs = [ @{$rhs}[$subp_start .. $subp_end], $next_subp_lhs ];
                }

                # if we got this far we have 3 or more proper nullables, and the next
                # subproduction is nullable
                $subp_end = $proper_nullable1 - 1;
                shift @$proper_nullables;
                $next_subp_lhs = _assign_symbol($self,
                    $lhs->[NAME] .  "[" . $rule_id . ":" . ($subp_end + 1) . "]");
                @{$next_subp_lhs}[NULLABLE, START_REACHABLE, INPUT_REACHABLE, NULLING] =
                    (1, 1, 1, 0);
                _alias_symbol( $self, $next_subp_lhs );
                $subp_factor0_rhs = [ @{$rhs}[$subp_start .. $subp_end], $next_subp_lhs ];

            } # SETUP_SUBPRODUCTION

            my $factored_rhs = [ $subp_factor0_rhs ];

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
                $factored_rhs->[1] = [ @$subp_factor0_rhs ];
                $factored_rhs->[1]->[ $subp_proper_nullable0 ]
                    = $subp_factor0_rhs->[ $subp_proper_nullable0 ]->[NULL_ALIAS];
                
                # The third factored production, with a nulling symbol replacing the
                # second proper nullable.  Make sure there ARE two proper nullables.
                last FACTOR unless defined $proper_nullable1;
                $factored_rhs->[2] = [ @$subp_factor0_rhs ];
                $factored_rhs->[2]->[ $subp_proper_nullable1 ]
                    = $subp_factor0_rhs->[ $subp_proper_nullable1 ]->[NULL_ALIAS];

                # The fourth and last factored production, with a nulling symbol replacing
                # both proper nullables.  We don't include it if it results in a nulling
                # production.
                last FACTOR if $nullable;
                $factored_rhs->[3] = [ @{$factored_rhs->[2]} ];
                $factored_rhs->[3]->[ $subp_proper_nullable0 ]
                    = $subp_factor0_rhs->[ $subp_proper_nullable0 ]->[NULL_ALIAS];

            } # FACTOR

            for my $factor_rhs (@$factored_rhs)
            {
                my $new_rule = _add_rule( $self, $subp_lhs, $factor_rhs);
                @{$new_rule}[USEFUL, START_REACHABLE, INPUT_REACHABLE, NULLABLE, NULLING]
                    = (1, 1, 1, 0, 0);
            }

            # no more 
            last SUBPRODUCTION unless $next_subp_lhs;
            $subp_lhs = $next_subp_lhs;
            $subp_start = $subp_end + 1;
            $nullable = $subp_start > $last_nonnullable;

        } # SUBPRODUCTION

    } # RULE

    my $old_start = $start;
    my $input_reachable = $old_start->[INPUT_REACHABLE];
    $start = _assign_symbol( $self, $start->[NAME] . "[']");
    my $new_start_rule = _add_rule( $self, $start, [ $old_start ]);
    @{$new_start_rule}[INPUT_REACHABLE, START_REACHABLE, USEFUL] = ($input_reachable, 1, 1);
    @{$start}[INPUT_REACHABLE, START_REACHABLE] = ($input_reachable, 1);
    if ($old_start->[NULL_ALIAS]) {
        _alias_symbol($self, $start);
        my $new_start_rule = _add_rule( $self, $start->[NULL_ALIAS], [ ]);
        # Nulling rules are not considered useful, but the top-level one is an exception
        @{$new_start_rule}[INPUT_REACHABLE, START_REACHABLE, USEFUL] = ($input_reachable, 1, 1);
    }
    $self->[START] = $start;
}

package Parse::Marpa::Parse;

# Elements of the PARSE structure
use constant GRAMMAR          => 0; # the grammar used
use constant CURRENT_SET      => 1; # index of the first incomplete Earley set
use constant EARLEY_SET       => 2; # the array of the Earley sets
use constant WORK_LIST        => 3; # the array of the Earley work lists
use constant NEXT_ITEM        => 4; # index of next Earley item

# Elements of the EARLEY ITEM structure
# Note that these are Earley items as modified by Aycock & Horspool, with SDFA states instead of 
# LR(0) items.
#
# the value of ID should be coordinated with the other constants of the same name in the namespace
# Parse::Marpa
#
use constant ID               => 0;
use constant STATE            => 1;    # the SDFA state
use constant PARENT           => 2;    # the number of the Earley set with the parent item
use constant TOKEN            => 3;    # a list of the links from token scanning
use constant LINK             => 4;    # a list of the links from the completer step

# Elements of the EARLEY WORK ENTRY structure
#
# STATE AND PARENT two same as for EARLEY ITEM
use constant DESTINATION        => 0;    # Earley item that is the destination for the data
# use constant STATE            => 1;    # the SDFA state
# use constant PARENT           => 2;    # the number of the Earley set with the parent item
use constant PREDECESSOR        => 3;    # reference to predecessor Earley item
use constant CAUSE              => 4;    # reference to causal Earley item
use constant VALUE              => 5;    # token value
                                         # in this work entry

# implementation dependent constant, used below in unpack
use constant J_LENGTH     => (length pack("J", 0, 0));

# Constructor method

sub new {
    my $class = shift;
    my $grammar = shift;
    my $self = [];
    croak("No grammar supplied for new $class") unless defined $grammar;
    my $grammar_class = ref $grammar;
    croak("Don't recognize parse() grammar arg has wrong class: $grammar_class")
        unless $grammar_class  eq "Parse::Marpa";

    my $SDFA = $grammar->[Parse::Marpa::SDFA];
    croak("Attempt to parse grammar with empty SDFA")
        if not defined $SDFA or not scalar @$SDFA;

    my $work_list0 = [];
    # A bit of a cheat here: I rely on an assumption about the numbering
    # of the SDFA states -- specifically, that state 0 contains the
    # start productions.
    my $SDFA0 = $SDFA->[0];
    @{$work_list0->[0]}[STATE, PARENT, PREDECESSOR, CAUSE]
        = ($SDFA0, 0, 0, 0);
    my $resetting_state = $SDFA0->[Parse::Marpa::TRANSITION]->{""};
    if (defined $resetting_state) {
        @{$work_list0->[1]}[STATE, PARENT, PREDECESSOR, CAUSE]
            = ($resetting_state, 0, 0, 0);
    }
    @{$self}[CURRENT_SET, WORK_LIST, GRAMMAR, NEXT_ITEM]
        = (0, [ $work_list0 ], $grammar, 1);

    bless $self, $class;
}

# Viewing methods, for debugging

sub show_work_entry {
    my $entry = shift;
    my ($state, $parent, $predecessor, $cause, $value, $destination) = @{$entry}
        [STATE,  PARENT,  PREDECESSOR,   CAUSE,  VALUE,  DESTINATION];
    my $text = $state->[ Parse::Marpa::ID ] . ", " . $parent;
    if ($cause) {
        $text .= "; " . brief_earley_item($predecessor) . ", " . brief_earley_item($cause);
    } elsif ($predecessor) {
        $text .= "; " . brief_earley_item($predecessor);
    }
    $text .= "; v=" . $value if defined $value;
    $text .= "; dest=" . $destination if defined $destination;
    $text .= "\n";
} # show_work_entry

sub show_work_list_list {
    my $parse = shift;
    my $work_lists = $parse->[WORK_LIST];
    my $text = "";
    my $work_list_count = @$work_lists;
    LIST: for (my $ix = 0; $ix < $work_list_count; $ix++) {
        my $list = $work_lists->[$ix];
        next LIST unless defined $list;
        $text .= "Earley Working List $ix\n" . show_work_list($list);
    }
    $text;
}

sub show_work_list {
    my $work_list = shift;
    my $text = "";
    for my $entry (@$work_list) {
        $text .= show_work_entry($entry);
    }
    $text;
}

sub brief_earley_item {
    my $item = shift;
    my ($state, $parent) = @{$item}[STATE, PARENT];
    my $text = $state->[ Parse::Marpa::ID ] . ", " . $parent . "\n";
}

sub show_earley_item {
    my $item = shift;
    my $text = brief_earley_item($item);
    my ($token, $link) = @{$item}[TOKEN, LINK];
    for my $value_entry (@$token) {
        $text .= " [p=" . $value_entry->[0]->[ Parse::Marpa::ID ]
            . "v=" . $value_entry->[1] . "]";
    }
    for my $link_entry (@$link) {
        $text .= " [p=" . $link_entry->[0]->[ Parse::Marpa::ID ]
            .  "c=" .  $link_entry->[1]->[ Parse::Marpa::ID ] . "]";
    }
    $text;
}

sub show_earley_set {
    my $earley_set = shift;
    my $text = "";
    for my $earley_item (@$earley_set) {
        $text .= show_earley_item($earley_item);
    }
    $text;
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

    my ($earley_set_list, $work_list_list, $grammar, $current_set, $next_item)
        = @{$parse}[EARLEY_SET, WORK_LIST, GRAMMAR, CURRENT_SET, NEXT_ITEM];
    my $SDFA = $grammar->[ Parse::Marpa::SDFA ];

    my $work_list = $work_list_list->[$current_set];
    my $record_number = 0;
    $work_list = [ @{$work_list}[
        # Perl complains unless the constant is sigiled as a subroutine
        map unpack("J", substr($_, -&J_LENGTH)),
        sort map pack("JJJ",
                $_->[ STATE ]->[ Parse::Marpa::ID ],
                $_->[ PARENT ],
                $record_number++
            ),
            @$work_list
        ]
    ];

    my $earley_item;
    my @earley_item_list;
    my $current_state = []; # dummy pointer so the first match fails
    my $current_parent = -1; # dummy Earley set so the first match fails
    my $completer_work_list = [];

    WORK_ENTRY: for my $work (@$work_list) {

        my ($state, $parent) = @{$work}[STATE, PARENT];

        # Once for each state, parent pair:
        #   Create the earley item
        if ($state == $current_state and $parent == $current_parent) {
            $earley_item = [];
            @{$earley_item}[ID, STATE, PARENT] = ($next_item++, $state, $parent);
            push(@earley_item_list, $earley_item);
            $current_state = $state;
            $current_parent = $parent;
        }

        $work->[DESTINATION] = $earley_item;

        # I allow ambigious tokenization.
        # Loop through the alternative tokens.
        ALTERNATIVE: for my $alternative (@_) {
            my ($token, $value, $length) = @$alternative;

            if ($length <= 0) {
                croack("Token " . $token->[ Parse::Marpa::NAME ]
                    . " with bad length " . $length);
            }

            # compute goto(kernel_state, token_name)
            my $kernel_state
                = $SDFA
                    -> [ $state -> [ Parse::Marpa::ID ] ]
                    -> [ Parse::Marpa::TRANSITION ]
                    -> { $token -> [ Parse::Marpa::NAME ] };
            next ALTERNATIVE unless $kernel_state;

            # Create the kernel item and its link.
            my $target_ix = $current_set  + $length;
            my $target_work_list = ($work_list_list->[ $target_ix ] ||= []);
            my @new_work_entry;
            @new_work_entry[STATE, PARENT, PREDECESSOR, CAUSE, VALUE]
                = ($kernel_state, $parent, $earley_item, 0, $value);
            push(@$target_work_list, \@new_work_entry);

            my $resetting_state
                = $kernel_state
                    -> [ Parse::Marpa::TRANSITION ]
                    -> { "" };
            next ALTERNATIVE unless defined $resetting_state;
            @new_work_entry[STATE, PARENT, PREDECESSOR, CAUSE, VALUE]
                = ($resetting_state, $parent, 0, 0, undef);
            push(@$target_work_list, \@new_work_entry);

        } # ALTERNATIVE

        next WORK_ENTRY if $current_set == $parent;

        COMPLETE_RULE: for my $complete_symbol_name (@{$state->[ Parse::Marpa::COMPLETE ] }) {
            PARENT_ITEM: for my $parent_item (@{$earley_set_list->[$parent]}) {
                my ($parent_state, $grandparent) = @{$parent_item}[STATE, PARENT];
                my $kernel_state
                    = $SDFA
                        -> [ $parent_state -> [ Parse::Marpa::ID ] ]
                        -> [ Parse::Marpa::TRANSITION ]
                        -> { $complete_symbol_name };
                next EARLEY_ITEM unless defined $kernel_state;
                my $new_work_entry;
                @{$new_work_entry}[STATE, PARENT, PREDECESSOR, CAUSE]
                    = ($kernel_state, $grandparent, $parent_item, $earley_item);
                push(@$completer_work_list, $new_work_entry);

                my $resetting_state
                    = $kernel_state
                        -> [ Parse::Marpa::TRANSITION ]
                        -> { "" };
                next PARENT_ITEM unless defined $resetting_state;
                @{$new_work_entry}[STATE, PARENT, PREDECESSOR, CAUSE]
                    = ($resetting_state, $current_set, 0, 0);
                push(@$completer_work_list, $new_work_entry);

            } # PARENT_ITEM
        } # COMPLETE_RULE
    } # EARLEY_ITEM

    # Go back over the earley set, and set up the links
    $record_number = 0;
    # TO HERE
    $work_list = [ @{$work_list}[
        # Perl complains unless the constant is sigiled as a subroutine
        map unpack("J", substr($_, -&J_LENGTH)),
        sort map {
                pack("JJBJJJ",
                    $_->[ STATE ]->[ Parse::Marpa::ID ],
                    $_->[ PARENT ],
                    (not defined $_->[DESTINATION]),
                    $_->[PREDECESSOR]+0,
                    $_->[CAUSE]+0,
                    $record_number++
                );
            }
            @$work_list, @$completer_work_list
        ]
    ];

    # Increment CURRENT_SET
    $parse->[CURRENT_SET]++;
}

=head1 NAME

Parse::Marpa - Earley's Algorithm, with improvements

=head1 VERSION

Version 0.1_2

=cut

=head1 SYNOPSIS

Earley's general parsing algorithm, with LR(0) precomputation

    use Parse::Marpa;

    my $foo = Parse::Marpa->new();
    ...

=head1 METHODS

=head1 AUTHOR

Jeffrey Kegler

=head1 DEPENDENCIES

According to C<perlvar>, Marpa is compatible with Perl 5.6.0.
As of this writing, it's only been tested on Perl 5.8.8.

=head1 BUGS

Please report any bugs or feature requests to
C<bug-parse-marpa at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Parse-Marpa>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Parse::Marpa

=head1 THE ALGORITHM

Marpa is essentially the parser described in John Aycock and R. Nigel Horspool's "Practical
Earley Parsing", I<The Computer Journal>, Vol. 45, No. 6, 2002, pp. 620-630.  This combined
LR(0) with Jay Earley's parsing algorithm.  I've made some improvements.

First, Aycock and Horspool's algorithm rewrites the original grammar into NNF (Nihilist Normal Form).
Earley's original algorithms had serious issues with nullable symbols and productions,
and NNF fixes most of them.
(A nullable symbol or production is one which could eventually parse out to the
empty string.)
Importantly, NNF also allows complete and easy mapping of the semantics
of the original grammar to its NNF rewrite,
so that NNF and the whole rewrite process can be made invisible to the user.

My problem with NNF grammar is that the rewritten grammar is exponentially larger
than the original in the theoretical worst case,
and I just don't like exponential explosion,
even as a theoretical possibility in pre-processing.
Furthermore, I think that in some cases likely to arise in practice
(Perl 6 "rules" with significant whitespace, for example),
the size explosion, while not exponential,
is linear with a very large multiplier.

My solution was is Quantum Nihilist Form (QNF).
This is NNF,
but with the further restriction that no more than two nullable symbols may appear
in any production.
The shortened QNF production maps back to the original grammar,
so that like NNF, the QNF rewrite can be made invisible to the user.
With QNF, the theoretical worst behavior is linear,
and in those difficult cases likely to arise in practice the multiplier is smaller.

Second, I've extended the scanning step of Earley's algorithm,
and introduced the "earleme" (named after Jay Earley).
Previous implementations
required the Earley grammar's input to be broken up into tokens, presumably by lexical analysis of
the input using DFA's (deterministic finite automata, which are the equivalent of regular expressions).
Requiring that the first level of analysis
be performed by a DFA hobbles a general parser like Earley's.

Marpa loosens the restriction, by allowing
the scanning phase of Earley's algorithm to add items not just
to the current Earley set and the next one, but to any later Earley set.
Since items can be scanned onto several different Earley sets,
so that the input to the Earley scanning step no longer has to be deterministic.
Several alternative scans of the input can be put into the Earley sets, and the power of
Earley's algorithm harnessed to deal with the indeterminism.

In the new Marpa scanner,
each scanned item has a length in "earlemes", call it C<l>.
If the current Earley set is C<i>, a newly scanned Earley item is added to Earley set C<l+i>.
The B<earleme> is the distance measured in Earley sets, and an implementation can sync earlemes up
with any measure that's convenient.
For example, the distance in earlemes may be the length of a string,
as measured either in ASCII characters,
or UNICODE graphemes.
Another implementation may define the earleme length as the distance in a token stream, measured in tokens.

=head1 WHY CALL IT MARPA? or BLATANT PLUG

This translator is named after the great Tibetan translator, Marpa.
At Marpa's time (the 11th century A.D.),
Indian Buddhism was at its height,
and a generation of Tibetans translators were devoting themselves to
obtaining its texts and translating them from Sanskrit.
Marpa was their major figure,
so much so that today he is known as Marpa Lotsawa,
or "Marpa the Translator".

In those days, the job of translator was not for the indoors type.
"Translation" required studying with the Buddhist teachers who had the
texts and could explain them.
That meant travel from Tibet to India.
From Marpa's home in the Lhotrak Valley, the easiest way to reach India was
15,000 foot Khala Chela Pass.
Even to reach Khala Chela's relatively easy, three-mile high summit,
Marpa had to cross two hundred
miles of Tibet, most of them difficult and all of them lawless.
From Khala Chela downhill to the great Buddhist
center of Nalanda University was four hundred miles, but Tibetans would stop
for years or months in Nepal, getting used to the low altitudes.

Tibetans had learned the hard way
not to go straight to Nalanda.
Almost no germs live in the cold, thin air of Tibet,
and Tibetans arriving directly in the lowlands had no immunities.
Whole expeditions had died from disease within weeks of arrival on the hot
plains.

Marpa plays a significant role in my novel,
B<The God Proof>, which centers around
Kurt GE<ouml>del's
proof of God's existence.
Yes, I<that> Kurt GE<ouml>del, and yes, he really did worked out a God Proof
(it's in his I<Collected Works>, Vol. 3, pp. 403-404).
B<The God Proof> is available at Amazon:
L<http://www.amazon.com/God-Proof-Jeffrey-Kegler/dp/1434807355>.

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
