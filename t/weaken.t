use 5.009005;
use strict;
use warnings;

use Scalar::Util qw(refaddr reftype isweak weaken);
use Test::More;

eval "use Test::Weaken 0.002002";
if ($@) {
    plan skip_all
        => "Test::Weaken 0.002002 required for testing of memory cycles";
    exit 0;
}
plan tests => 5;
use_ok( 'Parse::Marpa' );

my $test = sub {
    my $g = new Parse::Marpa(
        start => "S",
        rules => [
            [ "S", [qw/A A A A/] ],
            [ "A", [qw/a/] ],
            [ "A", [qw/E/] ],
            [ "E" ],
        ],
        terminals => [
            [ "a" => qr/a/ ],
        ],
    );
    my $a = $g->get_symbol("a");
    my $parse = new Parse::Marpa::Parse($g);
    $parse->lex_earleme([$a, "a", 1]);
    $parse->lex_earleme([$a, "a", 1]);
    $parse->lex_earleme([$a, "a", 1]);
    $parse->lex_earleme([$a, "a", 1]);
    $parse->lex_end();
    $parse->initial();
    [ $g, $parse ];
};

my ($weak_count, $strong_count, $unfreed_weak, $unfreed_strong)
    = Test::Weaken::poof($test);

cmp_ok($weak_count, "!=", 0, "Found $weak_count weak refs");
cmp_ok($strong_count, "!=", 0, "Found $strong_count strong refs");

cmp_ok(scalar @$unfreed_strong, "==", 0, "All strong refs freed")
    or diag("Unfreed strong refs: ", scalar @$unfreed_strong);

my %weak_ok;
for my $fn (
    \(&Parse::Marpa::chaf_head_only),
    \(&Parse::Marpa::chaf_head_and_tail),
    \(&Parse::Marpa::chaf_tail_only),
) {
   $weak_ok{"$fn"} = 1;
}

my $unexpected_weak = [ grep { ! $weak_ok{$_ . ""}++ } @$unfreed_weak ];
    
cmp_ok(scalar @$unexpected_weak, "==", 0, "All weak refs freed")
    or diag("Unexpected unfreed weak refs: ", scalar @$unexpected_weak);

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
