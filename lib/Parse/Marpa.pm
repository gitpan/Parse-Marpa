package Parse::Marpa;

use warnings;
use strict;
use version; our $VERSION = qv('0.1_3');

use 5.006000;

use Carp;
use Scalar::Util qw(weaken);

=begin Implementation:

In style I try to follow Damian Conway's suggestions, but with many
exceptions.  In particular, in this module, there's a very specific
goal: it has to be easily translated into time-efficient C code.
That means lots of things which are suboptimal for Perl readability
-- avoidance of OO, heavy use of references, a strong preference
for arrays over hashes, etc.  Sometimes, when my first goals force
me into some really ugly style choice, I point that out in a comment.

So don't conclude that I think the below is what Perl should look
like -- it's not.

The reason for targeting time-efficiency and a C reimplementation:
the rap against Earley's has always been speed.  If this implementation
isn't a means to that end, it will be of no interest, and nobody
should care how beautifully it's written.  Don't get me wrong, I
think readability and maintainability are important -- I'm the one
most likely to be maintaining this code, and I'd like to be able
to read it when I come back to it some months hence.  But readability
is a lousy reason to write a uselessly slow module.

C conversion is important because one of two things are going to
happen: the module turns out to be so slow it's difficult to use,
or not.  If it's slow, the next thing to try is conversion to C.
If it's fast, that's an important discovery, and there will probably
be demand for an even faster version -- in C.  My current guess is
that Marpa is doomed to a C implementation, or failure.

=end Implementation:

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
use constant SYMBOLS          => 1;    # array of symbol refs
use constant RULE_HASH        => 2;    # hash by name of symbol refs
use constant SYMBOL_HASH      => 3;    # hash by name of symbol refs
use constant START            => 4;    # ref to start symbol
use constant NFA              => 5;    # array of states
use constant SDFA             => 6;    # array of states
use constant SDFA_BY_NAME     => 7;    # hash from SDFA name to SDFA reference
use constant NULLABLE_SYMBOLS => 8;    # array of refs of the nullable symbols
use constant ACADEMIC         => 9;    # true if this is a textbook grammar,
                                       # for checking the NFA and SDFA, and NOT
                                       # for actual Earley parsing

###############
# Constructor #
###############

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
    @{$self}[ SYMBOLS, SYMBOL_HASH, RULES, RULE_HASH, SDFA_BY_NAME, ACADEMIC ] =
        ( [], {}, [], {}, {}, $academic );
    bless( $self, $class );

    $self->_add_user_rules($rules);
    $self->_nullable();
    $self->_nulling();
    $self->_input_reachable();
    $self->_set_start($start);
    $self->_start_reachable();
    if ($academic) {
        $self->_setup_academic_grammar()
    } else {
        $self->_rewrite_as_QNF();
    }
    $self->_create_NFA;
    $self->_create_SDFA;

    $self;
}

#
# Viewing Methods
#

sub _show_symbol {
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

sub _show_symbols {
    my $grammar    = shift;
    my $symbols = $grammar->[SYMBOLS];
    my $text    = "";
    for my $symbol_ref (@$symbols) {
        $text .= _show_symbol($symbol_ref);
    }
    $text;
}

sub _show_nulling_symbols {
    my $self    = shift;
    my $symbols = $self->[SYMBOLS];
    join( " ", sort map { $_->[NAME] } grep { $_->[NULLING] } @$symbols );
}

sub _show_nullable_symbols {
    my $self    = shift;
    my $symbols = $self->[NULLABLE_SYMBOLS];
    join( " ", sort map { $_->[NAME] } @$symbols );
}

sub _show_input_reachable_symbols {
    my $self    = shift;
    my $symbols = $self->[SYMBOLS];
    join( " ",
        sort map { $_->[NAME] } grep { $_->[INPUT_REACHABLE] } @$symbols );
}

sub _show_start_reachable_symbols {
    my $self    = shift;
    my $symbols = $self->[SYMBOLS];
    join( " ",
        sort map { $_->[NAME] } grep { $_->[START_REACHABLE] } @$symbols );
}

sub _show_rule {
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

sub _show_rules {
    my $self   = shift;
    my $rules  = $self->[RULES];
    my $ruleno = -1;
    my $text;

    for my $rule (@$rules) {
        $text .= _show_rule($rule);
    }
    $text;
}

sub _show_item {
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

sub _show_NFA_state {
    my $state = shift;
    my ( $name, $item, $transition ) = @{$state}[ NAME, ITEM, TRANSITION ];
    my $text .= $name . ": " . _show_item($item) . "\n";
    while (my ($symbol_name, $transitions) = each %$transition ) {
        $text .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => "
            . join( " ", map { $_->[NAME] } @$transitions )
            . "\n";
    }
    $text;
}

sub _show_NFA {
    my $self = shift;
    my $text = "";
    my $NFA  = $self->[NFA];
    for my $state (@$NFA) {
        $text .= _show_NFA_state($state);
    }
    $text;
}

sub _show_SDFA_state {
    my $state = shift;

    my $text = "";
    my ( $id, $name, $NFA_states, $transition ) =
        @{$state}[ ID, NAME, NFA_STATES, TRANSITION ];

    $text .= "S" . $id . ": " . $name . "\n";
    for my $NFA_state (@$NFA_states) {
        my $item = $NFA_state->[ITEM];
        $text .= _show_item($item) . "\n";
    }
    for my $symbol_name ( keys %$transition ) {
        my ($to_id, $to_name) = @{$transition->{$symbol_name}}[ID, NAME];
        $text .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => S" . $to_id . " (" . $to_name . ")\n";
    }
    $text;
}

sub _show_SDFA {
    my $self = shift;
    my $text = "";
    my $SDFA = $self->[SDFA];
    for my $state (@$SDFA) { $text .= _show_SDFA_state($state); }
    $text;
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

sub _canonical_name {
    my $name = shift;
    $name =~ /]$/ ? $name . "_" : $name;
}

sub _add_terminal {
    my $self  = shift;
    my $name  = shift;
    my $regex = shift;
    my ( $symbol, $symbols ) = @$self[ SYMBOL_HASH, SYMBOLS ];

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
    my ( $symbol, $symbols ) = @{$self}[ SYMBOL_HASH, SYMBOLS ];

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
    $self->_assign_symbol(_canonical_name($name));
}

sub _add_user_rule {
    my $self      = shift;
    my $lhs_name  = shift;
    my $rhs_names = shift;

    $self->_add_rule(
        $self->_assign_symbol(_canonical_name($lhs_name)),
        [ map { $self->_assign_symbol(_canonical_name($_)); } @$rhs_names ]
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
                $self->_add_terminal( $term, $regex );
                next rule;
            }

            # fall through if not a terminal definition
        }

        $self->_add_user_rule(
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

#################
# Private Stuff #
#################

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

    my ($rules, $symbols) = @{$self}[RULES, SYMBOLS];

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

    my ($rules, $symbols) = @{$self}[RULES, SYMBOLS];

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
    my ($rules, $symbols)   = @{$self}[RULES, SYMBOLS];

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
        = @{$self}[RULES, SYMBOLS, SYMBOL_HASH, START, ACADEMIC];

    $self->[NULLABLE_SYMBOLS] = [ grep { $_->[NULLABLE] } @$symbols ];

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
            for my $nullable_symbol ( @{ $self->[NULLABLE_SYMBOLS] } )
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
                map { $_->[NAME] } @{ $self->[NULLABLE_SYMBOLS] } )
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
    my $NFA    = $self->[NFA];
    my $SDFA   = $self->[SDFA] = [];
    my $NFA_s0 = $NFA->[0];

    # next SDFA state to compute transitions for
    my $next_state_id = 0;

    my $initial_NFA_states = $NFA_s0->[TRANSITION]->{""};
    if (not defined $initial_NFA_states) {
        carp("Empty NFA, cannot create SDFA");
        return;
    }
    $self->_assign_SDFA_kernel_state( $initial_NFA_states );

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
                $self->_assign_SDFA_kernel_state($to_states);
        }
    }

    # For the parse phase, pre-compute the list of NFA states with complete
    # items
    STATE: for my $state (@$SDFA) {
        my $complete = [];
        my $NFA_states = $state->[NFA_STATES];
        for my $NFA_state (@$NFA_states) {
            my $item = $NFA_state->[ITEM];
            my ($rule, $position) = @{$item}[RULE, POSITION];
            push(@$complete, $item) if $position >= @{$rule->[RHS]};
        } # NFA_state
        $state->[COMPLETE] = $complete;
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
    my ( $symbol, $symbols ) = @{$self}[ SYMBOL_HASH, SYMBOLS ];
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
    my ($rules, $symbols, $start) = @{$self}[RULES, SYMBOLS, START];

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

        $self->_alias_symbol($symbol);
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
                    $next_subp_lhs = $self->_assign_symbol(
                        $lhs->[NAME] .  "[" . $rule_id . ":" . ($subp_end + 1) . "]");
                    @{$next_subp_lhs}[NULLABLE, START_REACHABLE, INPUT_REACHABLE, NULLING] =
                        (0, 1, 1, 0);
                    $subp_factor0_rhs = [ @{$rhs}[$subp_start .. $subp_end], $next_subp_lhs ];
                }

                # if we got this far we have 3 or more proper nullables, and the next
                # subproduction is nullable
                $subp_end = $proper_nullable1 - 1;
                shift @$proper_nullables;
                $next_subp_lhs = $self->_assign_symbol(
                    $lhs->[NAME] .  "[" . $rule_id . ":" . ($subp_end + 1) . "]");
                @{$next_subp_lhs}[NULLABLE, START_REACHABLE, INPUT_REACHABLE, NULLING] =
                    (1, 1, 1, 0);
                $self->_alias_symbol($next_subp_lhs);
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
                my $new_rule = $self->_add_rule($subp_lhs, $factor_rhs);
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
    $start = $self->_assign_symbol($start->[NAME] . "[']");
    my $new_start_rule = $self->_add_rule($start, [ $old_start ]);
    @{$new_start_rule}[INPUT_REACHABLE, START_REACHABLE, USEFUL] = ($input_reachable, 1, 1);
    @{$start}[INPUT_REACHABLE, START_REACHABLE] = ($input_reachable, 1);
    if ($old_start->[NULL_ALIAS]) {
        $self->_alias_symbol($start);
        my $new_start_rule = $self->_add_rule($start->[NULL_ALIAS], [ ]);
        # Nulling rules are not considered useful, but the top-level one is an exception
        @{$new_start_rule}[INPUT_REACHABLE, START_REACHABLE, USEFUL] = ($input_reachable, 1, 1);
    }
    $self->[START] = $start;
}

package Parse::Marpa::Parse;

# ELEMENTS of the PARSE STRUCTURE
use constant CURRENT_EARLEME  => 0;    # number of the last completed earleme
use constant EARLEY_SET       => 1;    # the array of the Earley sets

# ELEMENTS of the EARLEY ITEM STRUCTURE
# Note that these are Earley items as modified by Aycock & Horspool, with SDFA states instead of 
# LR(0) items.
use constant STATE            => 0;    # the SDFA state
use constant PARENT           => 1;    # the number of the Earley set with the parent item

sub new {
    my $class = shift;
    my $grammar = shift;
    my $self = [];
    croak("No grammar supplied for new $class") unless defined $grammar;

    my $SDFA = $grammar->[Parse::Marpa::SDFA];
    my $set0 = {};
    # A bit of a cheat here: I rely on an assumption about the numbering
    # of the SDFA states -- specifically, that state 0 contains the
    # start productions.
    my $SDFA0 = $SDFA->[0];
    @{$set0->{"0,0"}}[STATE, PARENT] = ($SDFA0, 0);
    my $resetting_state = $SDFA0->[Parse::Marpa::TRANSITION]->{""};
    if (defined $resetting_state) {
        @{$set0->{$resetting_state->[ Parse::Marpa::ID ] . ",0"}}[STATE, PARENT]
            = ($resetting_state, 0);
    }
    @{$self}[CURRENT_EARLEME, EARLEY_SET] = (0, [$set0]);

    bless $self, $class;
}

sub _show_earley_item {
    my $earley_item = shift;
    my $text = $earley_item->[STATE]->[ Parse::Marpa::ID ] . ", " . $earley_item->[PARENT] . ":";
    for (my $ix = 2; $ix <= $#$earley_item; $ix++) {
        $text .= " [ " . join(", ", $earley_item->[$ix]) . " ]"
    }
    $text .= "\n";
}

sub _show_earley_set {
    my $earley_set = shift;
    my $text = "";
    for my $earley_item (sort {
            $a->[STATE] <=> $b->[STATE] or $a->[PARENT] <=> $b->[PARENT]
        } values %$earley_set) {
        $text .= _show_earley_item($earley_item);
    }
    $text;
}

sub _show_earley_sets {
    my $parse = shift;
    my $earley_set = $parse->[EARLEY_SET];
    my $text;
    for (my $earley_set_ix = 0; $earley_set_ix <= $#$earley_set; $earley_set_ix++) {
        $text .= "Earley Set " . $earley_set_ix . "\n";
        $text .= _show_earley_set($earley_set->[$earley_set_ix]);
    }
    $text;
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
