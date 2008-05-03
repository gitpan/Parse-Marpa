# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in source form

use 5.010_000;
use strict;
use warnings;
use lib "../lib";
use English qw( -no_match_vars ) ;

use Test::More tests => 2;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $source; { local($RS) = undef; $source = <DATA> };

my $g = new Parse::Marpa::Grammar({
    warnings => 1,
    code_lines => -1,
});

$g->set({ mdl_source => \$source});

$g->precompute();

my $recce = new Parse::Marpa::Recognizer({grammar => $g});

my $lc_a = Parse::Marpa::MDL::get_symbol($g, "lowercase a");
$recce->earleme([$lc_a, "lowercase a", 1]);
$recce->earleme([$lc_a, "lowercase a", 1]);
$recce->earleme([$lc_a, "lowercase a", 1]);
$recce->earleme([$lc_a, "lowercase a", 1]);
$recce->end_input();

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
    "(lowercase a;lowercase a;lowercase a;)",
    "(lowercase a;lowercase a;lowercase a;lowercase a)",
);

PERMUTATION: for (;;) {
    for my $i (@a) {
        my $evaler = new Parse::Marpa::Evaluator($recce, $i);
        my $result = $evaler->next();
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
semantics are perl5.  version is 0.211.3.  the start symbol is
S.  the default null value is q{}.  the default action is q{
     my $v_count = scalar @$_;
     return "" if $v_count <= 0;
     return $_->[0] if $v_count == 1;
     "(" . join(";", @$_) . ")";
}.

S: A, A, A, A.

A: lowercase a.

lowercase a matches /a/.

E: . # empty

A: E.
