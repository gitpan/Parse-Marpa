#!perl

use 5.010;
use strict;
use warnings;
use lib 'lib';
use lib 't/lib';

use Carp;
use Scalar::Util qw(refaddr reftype isweak weaken);
use Test::More tests => 2;
use Test::Weaken;

BEGIN { Test::More::use_ok('Parse::Marpa'); }

my $test = sub {
    my $g = Parse::Marpa::Grammar->new(
        {   start => 'S',
            rules => [
                [ 'S', [qw/A A A A/] ],
                [ 'A', [qw/a/] ],
                [ 'A', [qw/E/] ],
                ['E'],
            ],
            terminals => ['a'],
        }
    );
    my $a = $g->get_symbol('a');
    my $recce = Parse::Marpa::Recognizer->new( { grammar => $g } );
    $recce->earleme( [ $a, 'a', 1 ] );
    $recce->earleme( [ $a, 'a', 1 ] );
    $recce->earleme( [ $a, 'a', 1 ] );
    $recce->earleme( [ $a, 'a', 1 ] );
    $recce->end_input();
    my $evaler = Parse::Marpa::Evaluator->new( { recce => $recce } );
    Carp::croak('No parse found') unless $evaler;
    $evaler->value();
    [ $g, $recce, $evaler ];
};

my $tester            = Test::Weaken->new($test);
my $unfreed_count     = $tester->test();
my $unfreed_proberefs = $tester->unfreed_proberefs();
my $total             = $tester->probe_count();
my $freed_count       = $total - $unfreed_count;

# The evaluator (for And_Node::PERL_CLOSURE) assigns a \undef, and this creates
# an undef "global".  No harm done if there's only one.

my $ignored_count = 0;
DELETE_UNDEF_CONSTANT: for my $ix ( 0 .. $#{$unfreed_proberefs} ) {
    if ( ref $unfreed_proberefs->[$ix] eq 'SCALAR'
        and not defined ${ $unfreed_proberefs->[$ix] } )
    {
        delete $unfreed_proberefs->[$ix];
        $ignored_count++;
        last DELETE_UNDEF_CONSTANT;
    }
}
$unfreed_count = @{$unfreed_proberefs};

Test::More::diag(
    "Freed=$freed_count, ignored=$ignored_count, unfreed=$unfreed_count, total=$total"
);

Test::More::cmp_ok( $unfreed_count, q{==}, 0, 'All refs freed' )
    or Test::More::diag("Unfreed refs: $unfreed_count");

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
