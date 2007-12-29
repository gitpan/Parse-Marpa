
# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in source form

use 5.009005;
use strict;
use feature ":5.10";
use warnings;
use lib "../lib";
use English;

use Test::More tests => 2;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $source; { local($RS) = undef; $source = <DATA> };

my $g = new Parse::Marpa(
    warnings => 1,
    code_lines => -1,
);

$g->set( source => \$source);

$g->precompute();

my $parse = new Parse::Marpa::Parse($g);

my $lc_a = $g->get_symbol("lowercase a");
$parse->earleme([$lc_a, "lowercase a", 1]);
$parse->earleme([$lc_a, "lowercase a", 1]);
$parse->earleme([$lc_a, "lowercase a", 1]);
$parse->earleme([$lc_a, "lowercase a", 1]);
$parse->end_input();

# from Tye McQueen's Algorithm::Loops
sub NextPermute(\@)
{
    my( $vals )= @_;
    my $last= $#{$vals};
    return !1   if  $last < 1;
    # Find last item not in reverse-sorted order:
    my $i= $last-1;
    $i--   while  0 <= $i  &&  $vals->[$i] ge $vals->[$i+1];
    # If complete reverse sort, we are done!
    if(  -1 == $i  ) {
        # Reset to starting/sorted order:
        @$vals= reverse @$vals;
        return !1;
    }
    # Re-sort the reversely-sorted tail of the list:
    @{$vals}[$i+1..$last]= reverse @{$vals}[$i+1..$last]
        if  $vals->[$i+1] gt $vals->[$last];
    # Find next item that will make us "greater":
    my $j= $i+1;
    $j++  while  $vals->[$i] ge $vals->[$j];
    # Swap:
    @{$vals}[$i,$j]= @{$vals}[$j,$i];
    return 1;
}

my $failure_count = 0;
my $total_count = 0;
my @a = sort (0, 1, 2, 3, 4);
my @answer = (
    "",
    "(lowercase a;;;)",
    "(lowercase a;lowercase a;;)",
    "(;lowercase a;lowercase a;lowercase a)",
    "(lowercase a;lowercase a;lowercase a;lowercase a)",
);

PERMUTATION: for (;;) {
    for my $i (@a) {
        $parse->initial($i);
        my $result = $parse->value();
        $total_count++;
        if ($answer[$i] ne $$result) {
            diag( "got "
		. $$result
		. " expected "
                . $answer[$i]
                . " for $i in ("
                . join(",", @a)
                . ")\n"
            );
            $failure_count++;
        }
    }
    if (not NextPermute(@a)) {
        last PERMUTATION;
    }
}
ok(!$failure_count, ($total_count-$failure_count) . " of $total_count parse permutations succeeded");

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__
semantics are perl5.  version is 0.1.65.  the start symbol is
S.  the default null value is q{}.  the default action is q{
     my $v_count = scalar @$Parse::Marpa::This::v;
     return "" if $v_count <= 0;
     return $Parse::Marpa::This::v->[0] if $v_count == 1;
     "(" . join(";", @$Parse::Marpa::This::v) . ")";
}.

S: A, A, A, A.

A: lowercase a.

lowercase a matches /a/.

E: . # empty

A: E.
