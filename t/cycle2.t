#!perl

# A grammars with cycles
use 5.010;
use strict;
use warnings;
use lib 'lib';
use lib 't/lib';
use English qw( -no_match_vars );
use Fatal qw(open close chdir);
use Test::More tests => 24;
use Carp;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Parse::Marpa');
}

my $example_dir = 'example';
chdir $example_dir;

my @expected_values = split /\n/xms, <<'EOS';
a
A(B(a))
A(B(A(B(a))))
A(B(A(B(A(B(a))))))
A(B(A(B(A(B(A(B(a))))))))
A(B(A(B(A(B(A(B(A(B(a))))))))))
A(B(A(B(A(B(A(B(A(B(A(B(a))))))))))))
A(B(A(B(A(B(A(B(A(B(A(B(A(B(a))))))))))))))
A(B(A(B(A(B(A(B(A(B(A(B(A(B(A(B(a))))))))))))))))
A(B(A(B(A(B(A(B(A(B(A(B(A(B(A(B(A(B(a))))))))))))))))))
A(B(A(B(A(B(A(B(A(B(A(B(A(B(A(B(A(B(A(B(a))))))))))))))))))))
EOS

my $mdl = <<'EOF';
semantics are perl5.  version is 1.008000.
start symbol is S.
default action is q{join(q{ }, @_)}.

S: A.

A: B. q{ 'A(' . $_[0] . ')' }.

A: /a/.

B: A. q{ 'B(' . $_[0] . ')' }.
EOF

my $trace;
open my $MEMORY, '>', \$trace;
my $grammar = Parse::Marpa::Grammar->new(
    {   mdl_source        => \$mdl,
        trace_file_handle => $MEMORY,
    }
);
$grammar->precompute();
close $MEMORY;

Marpa::Test::is( $trace, <<'EOS', 'cycle detection' );
Cycle found involving rule: 3: b -> a
Cycle found involving rule: 1: a -> b
EOS

my $recce = Parse::Marpa::Recognizer->new(
    {   grammar           => $grammar,
        trace_file_handle => *STDERR,
    }
);

my $text          = 'a';
my $fail_location = $recce->text( \$text );
if ( $fail_location >= 0 ) {
    Carp::croak(
        Parse::Marpa::show_location(
            'Parsing failed',
            \$text, $fail_location
        )
    );
}
$recce->end_input();

for my $depth ( 1, 2, 5, 10 ) {

    my $evaler = Parse::Marpa::Evaluator->new(
        { recce => $recce, cycle_depth => $depth } );
    my $parse_count = 0;
    while ( my $value = $evaler->value() ) {
        Marpa::Test::is(
            ${$value},
            $expected_values[ $parse_count++ ],
            'cycle depth test'
        );
    }

}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
