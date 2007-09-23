package Parse::Marpa;

use warnings;
use strict;
use version; our $VERSION = qv('0.1_2');

=for TODO:
=cut

use Carp;
use Scalar::Util qw(weaken);

# common elements in for many of the arrays
use constant ID   => 0;
use constant NAME => 1;

# common elements in rule and symbol arrays
use constant LHS => 2;    # for rule, ref of the left hand symbol
   # for symbol, rules with this as the lhs, as a ref to an array of rule refs
use constant RHS => 3;    # array of symbol refs
   # for symbol, rules with this in the rhs, as a ref to an array of rule refs
use constant NULLABLE        => 4;    # can match null
use constant START_REACHABLE => 5;    # reachable from start symbol
use constant INPUT_REACHABLE => 6;    # reachable from input symbol

# additional elements of symbol array
use constant REGEX => 7;              # regex, for terminals; undef otherwise

# elements of NFA (SDFA) state
# elements of SDFA state
use constant ITEM       => 2;         # NFA: an LR(0) item
use constant NFA_STATES => 2;         # SDFA: an array of NFA states
use constant TRANSITION =>
    3;    # the transitions, as a hash from symbol name to NFA (SDFA) states

# elements of LR(0) item
use constant RULE     => 0;
use constant POSITION => 1;

# elements of grammar
use constant RULES            => 0;    # array of rule refs
use constant SYMBOLS          => 1;    # array of symbol refs
use constant SYMBOL_HASH      => 2;    # hash by name of symbol refs
use constant START            => 3;    # ref to start symbol
use constant NFA              => 4;    # array of states
use constant SDFA             => 5;    # array of states
use constant SDFA_BY_NAME     => 6;    # hash from SDFA name to SDFA reference
use constant NULLABLE_SYMBOLS => 7;    # array of refs of the nullable symbols

###############
# Constructor #
###############

sub new {
    my $class = shift;
    my %args = @_;

    my $rules = $args{rules};
    croak("No rules specified") unless defined $rules;
    my $start = $args{start};
    croak("No start symbol specified") unless defined $start;

    # augmented grammar?
    my $augment = $args{augment};
    # default is yes
    $augment = 1 unless defined $augment;

    my $self  = [];
    @{$self}[ SYMBOLS, SYMBOL_HASH, RULES, SDFA_BY_NAME ] =
        ( [], {}, [], {} );
    bless( $self, $class );

    $self->_add_rules($rules);
    $self->_nullable();
    $self->_input_reachable();
    $self->_set_start($start);
    $self->_start_reachable();
    $self->_create_NFA;
    $self->_create_SDFA;

    $self;
}

# Utilities

# push, but without duplication
sub _no_dup_push {
    my $array = shift;
ELEMENT_TO_ADD: for my $element_to_add (@_) {
        for my $current_element (@$array) {
            next ELEMENT_TO_ADD if $current_element == $element_to_add;
        }
        push( @$array, $element_to_add );
    }
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
    $text .= "\n";
}

sub _show_symbols {
    my $self    = shift;
    my $symbols = $self->[SYMBOLS];
    my $text    = "";
    for my $symbol_ref (@$symbols) {
        $text .= _show_symbol($symbol_ref);
    }
    $text;
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

    my $lhs     = $rule->[LHS];
    my $rhs     = $rule->[RHS];
    my $text    = "";
    my @comment = ();

    $text .= $rule->[ID] . ": " . $lhs->[NAME] . " ->";
    if (@$rhs) {
        $text .= " " . join( " ", map { $_->[NAME] } @$rhs );
    }
    else {
        push( @comment, "empty" );
    }
    if ( not $rule->[INPUT_REACHABLE] ) { push( @comment, "!upreach" ); }
    if ( not $rule->[START_REACHABLE] ) { push( @comment, "!downreach" ); }
    if ( $rule->[NULLABLE] )            { push( @comment, "nullable" ); }
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
    for my $symbol_name ( keys %$transition ) {
        $text .= " "
            . ( $symbol_name eq "" ? "empty" : "<" . $symbol_name . ">" )
            . " => "
            . join( " ", map { $_->[NAME] } @{ $transition->{$symbol_name} } )
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

=for Implementation:

Symbol keys are names, with internal symbols beginning with a
underscore.  For the most part we use the raw names, but we need
to avoid conflict between internal names and user defined names.
To do this, we prepend another underscore to the name of any user
symbol which begins with an underscore.

=cut

# Thus all keys not starting with an underscore, or starting with "__"
# are for user-defined symbols.  All others are internal.
sub _canonical_name {
    my $name = shift;
    $name =~ /^_/ ? "_" . $name : $name;
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
    @{$new_symbol}[ID, NAME, LHS, RHS, NULLABLE, START_REACHABLE, INPUT_REACHABLE, REGEX] = (
        $symbol_count, $name, [], [], 0, undef, 1, $regex
    );
    push( @$symbols, $new_symbol );
    weaken( $symbol->{$name} = $new_symbol );
}

sub _assign_symbol {
    my $self = shift;
    my $name = shift;
    my ( $symbol, $symbols ) = @{$self}[ SYMBOL_HASH, SYMBOLS ];

    my $symbol_count = @$symbols;
    $name = _canonical_name($name);
    my $ret = $symbol->{$name};
    if ( not defined $ret ) {
        @{$ret}[ID, NAME, LHS, RHS, NULLABLE, START_REACHABLE, INPUT_REACHABLE, REGEX] = (
            $symbol_count, $name, [], [], undef, undef, undef, undef
        );
        push( @$symbols, $ret );
        weaken( $symbol->{$name} = $ret );
    }
    $ret;
}

sub _add_rule {
    my $self      = shift;
    my $lhs_name  = shift;
    my $rhs_names = shift;

    my $rules      = $self->[RULES];
    my $rule_count = @$rules;
    my $lhs        = $self->_assign_symbol($lhs_name);
    my @rhs        = ();
    for my $name (@$rhs_names) {
        push( @rhs, $self->_assign_symbol($name) );
    }
    my $new_rule = [];
    @{$new_rule}[ID, NAME, LHS, RHS, NULLABLE, START_REACHABLE, INPUT_REACHABLE] = (
        $rule_count, "rule $rule_count", $lhs, [@rhs],
            ( ( 0 == scalar @rhs ) ? 1 : undef ), undef,  undef
    );
    push( @$rules, $new_rule );
    {
        my $lhs_rules = $lhs->[LHS];
        weaken( $lhs_rules->[ scalar @$lhs_rules ] = $new_rule );
    }
    if ( 0 == scalar @rhs ) {
        @{$lhs}[NULLABLE, INPUT_REACHABLE] = (1, 1);
        @{$new_rule}[NULLABLE, INPUT_REACHABLE] = (1, 1);
    }
    else {
        my $rhs_symbols = [ $rhs[0] ];
        _no_dup_push( $rhs_symbols, @rhs[ 1 .. $#rhs ] ) if $#rhs > 0;
        for my $symbol_ref (@$rhs_symbols) {
            my $rhs_rules = $symbol_ref->[RHS];
            weaken( $rhs_rules->[ scalar @$rhs_rules ] = $new_rule );
        }
    }
}

# add one or more rules
sub _add_rules {
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

        $self->_add_rule(
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

    my $change = 1;

    while ($change) {
        $change = 0;

    SYMBOL_PASS: while ( my $work_symbol = shift @$symbol_work_set ) {
            my $rules_produced = $work_symbol->[LHS];
        PRODUCED_RULE: for my $rule (@$rules_produced) {

                next PRODUCED_RULE if defined $rule->[START_REACHABLE];

              # assume nullable until we hit an unmarked or unreachable symbol
                $rule->[START_REACHABLE] = 1;
                $change++;
                push( @$rule_work_set, $rule );

            }
        }    # SYMBOL_PASS

    RULE: while ( my $work_rule = shift @$rule_work_set ) {
            my $rhs_symbol = $work_rule->[RHS];

        RHS: for my $symbol (@$rhs_symbol) {

                next RHS if defined $symbol->[START_REACHABLE];
                $symbol->[START_REACHABLE] = 1;
                $change++;

   # warn "Rule pass: adding symbol to work set: " . _show_symbol($lhs_symbol);
                push( @$symbol_work_set, $symbol );
            }

        }    # RULE

    }    # change loop

}

# needs to be run after nullable
sub _input_reachable {
    my $self = shift;

    my $rules   = $self->[RULES];
    my $symbols = $self->[SYMBOLS];

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

    my $symbol_work_set =
        [ grep { defined $_->[INPUT_REACHABLE] } @$symbols ];
    my $rule_work_set = [ grep { defined $_->[INPUT_REACHABLE] } @$rules ];
    my $change = 1;

    while ($change) {
        $change = 0;

    SYMBOL_PASS: while ( my $work_symbol = shift @$symbol_work_set ) {

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
                    $change++;
                    _no_dup_push( $rule_work_set, $rule );
                }

            }
        }    # SYMBOL_PASS

    RULE: while ( my $work_rule = shift @$rule_work_set ) {
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
                $change++;

                _no_dup_push( $symbol_work_set, $lhs_symbol );
            }

        }    # RULE

    }    # change loop

}

sub _nullable {
    my $self    = shift;
    my $rules   = $self->[RULES];
    my $symbols = $self->[SYMBOLS];

    my $change = 1;    # boolean to track if current pass has changed anything
    my $symbol_work_set = [ grep { defined $_->[NULLABLE] } @$symbols ];
    my $rule_work_set = [ grep { defined $_->[NULLABLE] } @{$rules} ];

    while ($change) {
        $change = 0;

    SYMBOL_PASS: while ( my $work_symbol = shift @$symbol_work_set ) {

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
                    $change++;
                    _no_dup_push( $rule_work_set, $rule );
                }

            }
        }    # SYMBOL_PASS

    RULE: while ( my $work_rule = shift @$rule_work_set ) {
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
                $change++;

                _no_dup_push( $symbol_work_set, $lhs_symbol );
            }

        }    # RULE

    }    # change loop

    $self->[NULLABLE_SYMBOLS] = [ grep { $_->[NULLABLE] } @$symbols ];
}

sub _create_NFA {
    my $self        = shift;
    my $rules       = $self->[RULES];
    my $symbol_hash = $self->[SYMBOL_HASH];
    my $start       = $self->[START];

    my $NFA = [];
    $self->[NFA] = $NFA;

    my $state_id = 0;
    my @NFA_by_item;

    # create S0
    push( @$NFA, [ $state_id++, "S0", undef, {} ] );

    # create the other states
RULE: for my $rule (@$rules) {
        my ($start_reachable, $input_reachable) = @{$rule}[START_REACHABLE, INPUT_REACHABLE];
        next RULE unless $start_reachable;
        next RULE unless $input_reachable;
        for my $position ( 0 .. scalar @{ $rule->[RHS] } ) {
            my $new_state =
                [ $state_id, "S" . $state_id, [ $rule, $position ], {} ];
            $state_id++;
            push( @$NFA, $new_state );
            $NFA_by_item[ $rule->[ID] ][$position] = $new_state;
        }    # position
    }    # rule

    # now add the transitions
STATE: for my $state (@$NFA) {
        my ( $id, $name, $item, $transition ) = @$state;

       # transitions from state 0:
       # for every rule with the start symbol on its LHS, the item [ rule, 0 ]
        if ( not defined $item ) {
            my $rules = $start->[LHS];
            RULE: for my $start_rule (@$rules) {
                my ($start_rule_id, $input_reachable) = @{$start_rule}[ID, INPUT_REACHABLE];
                next RULE unless $input_reachable;
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
            my ($predicted_rule_id, $input_reachable) = @{$predicted_rule}[ID, INPUT_REACHABLE];
            next RULE unless $input_reachable;
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
    my $NFA_states    = $self->[NFA];
    my $SDFA_by_name  = $self->[SDFA_BY_NAME];
    my $SDFA          = $self->[SDFA];

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

# First the empty transitions.  These will all be predictions, and need to go into the
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

    my $NFA_ids =
        [ grep { $kernel_NFA_state_seen->[$_] } ( 0 .. $#$NFA_states ) ];
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

    $NFA_ids =
        [ grep { $prediction_NFA_state_seen->[$_] }
            ( 0 .. $#$NFA_states ) ];
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

    $self->_assign_SDFA_kernel_state( $NFA_s0->[TRANSITION]->{""} );

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
}

=head1 NAME

Parse::Marpa - Earley's Algorithm, with improvements

=head1 VERSION

Version 0.1.1

=cut

=head1 SYNOPSIS

Earley's general parsing algorithm, with LR(0) precomputation

    use Parse::Marpa;

    my $foo = Parse::Marpa->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 FUNCTIONS

=head1 AUTHOR

Jeffrey Kegler

=head1 BUGS

Please report any bugs or feature requests to
C<bug-parse-marpa at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Parse-Marpa>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Parse::Marpa

=head1 ACKNOWLEDGEMENTS

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
