#!perl

# the example grammar in Aycock/Horspool paper

use strict;
use warnings;

use Test::More tests => 5;
use Test::Weaken;
use Scalar::Util qw(refaddr reftype isweak weaken);
use Data::Dumper qw(Dumper);

BEGIN {
	use_ok( 'Parse::Marpa' );
}

SKIP: {
    eval { require Test::Weaken };
    skip "Test::Weaken not available, skipping tests", 4 if $@;

    my ($weak_count, $strong_count, $unfreed_weak, $unfreed_strong)
        = Test::Weaken::poof(
            sub {
                my $g = new Parse::Marpa(
                    start => "S",
                    rules => [
                        [ "a" => qr/a/ ],
                        [ qw/S A A A A/ ],
                        [ qw/A a/ ],
                        [ qw/A E/ ],
                        [ qw/E/ ],
                    ],
                );
                my $a = $g->get_symbol("a");
                my $parse = new Parse::Marpa::Parse($g);
                $parse->token([$a, "a", 1]);
                $parse->token([$a, "a", 1]);
                $parse->token([$a, "a", 1]);
                $parse->token([$a, "a", 1]);
                $parse->token();
                [ $g, $parse ];
            }
        );

    cmp_ok($weak_count, "!=", 0, "Found $weak_count weak refs");
    cmp_ok($strong_count, "!=", 0, "Found $strong_count strong refs");

    cmp_ok(scalar @$unfreed_strong, "==", 0, "All strong refs freed")
        or diag("Unfreed strong refs: ", scalar @$unfreed_strong);

    cmp_ok(scalar @$unfreed_weak, "==", 0, "All weak refs freed")
        or diag("Unfreed weak refs: ", scalar @$unfreed_weak);

} # SKIP

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
