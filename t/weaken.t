#!perl -T

# the example grammar in Aycock/Horspool paper

use strict;
use warnings;

use Test::More tests => 5;
use Scalar::Util qw(refaddr reftype isweak weaken);
use Data::Dumper qw(Dumper);

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $g = new Parse::Marpa(
    start => "S",
    rules => [
        [ "a" => qr/a/ ],
        [ qw/S A A A A/ ],
        [ qw/A a/ ],
        [ qw/A E/ ],
        [ qw/E/ ],
    ],
    augment => 0,
);

# assume g is a strong reference
my $reverse = {};
my $workset = [ \$g ];
my $weak = [];
$Data::Dumper::Maxdepth = 3;
# need to work with refs to a weak ref, because copying a weak ref (sigh) strengthens it
WORKSET: while (@$workset) {
    my $follow = [];
    REF: for my $rr (@$workset)
    {
         my $type = reftype $$rr;
         next REF unless defined $type;
         if (isweak $$rr)
         {
              push(@$weak, $rr);
              next REF;
         }
         if ($type eq "ARRAY" or $type eq "HASH" or $type eq "REF") {
              if (defined $reverse->{refaddr $$rr}) {
                   next REF;
              }
              $reverse->{refaddr $$rr} = $rr;
              if ($type eq "ARRAY") {
                  for my $ix (0 .. $#$$rr) {
                    push(@$follow, \ ($$rr->[$ix]) );
                  }
                  next REF;
              }
              if ($type eq "HASH") {
                  for my $ix (keys %$$rr) {
                    push(@$follow, \ ($$rr->{$ix}) );
                  }
                  next REF;
              }
              # not tested !!!
              if ($type eq "REF") {
                  push(@$follow, \$$$rr );
              }
         }
    }
    $workset = $follow;
}

my $strong = [];
my $ix = 0;
for my $strong_ref (values %$reverse) {
  weaken($strong->[$ix++] = $strong_ref);
}

my $weak_count = @$weak;
cmp_ok($weak_count, "!=", 0, "Found $weak_count weak refs");
my $strong_count = @$strong;
cmp_ok($strong_count, "!=", 0, "Found $strong_count strong refs");

undef $g;
undef $reverse;
undef $workset;

my @unfreed_strong = grep { defined $$_ } @$strong;
for my $rr (@unfreed_strong) {
    diag("Unfreed strong ref ", (refaddr $$rr), ": ", Dumper($rr));
}

cmp_ok(scalar @unfreed_strong, "==", 0, "All strong refs freed")
    or diag("Unfreed strong refs: ", scalar @unfreed_strong);

my @unfreed_weak = grep { defined $$_ } @$weak;
for my $rr (@unfreed_weak) {
    diag("Unfreed weak ref: ", Dumper($rr));
}

cmp_ok(scalar @unfreed_weak, "==", 0, "All weak refs freed")
    or diag("Unfreed weak refs: ", scalar @unfreed_weak);

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
