# PLEASE DO NOT USE THIS SCRIPT AS AN EXAMPLE

# If you're a savvy Perl programmer, you look in the tests directory
# for examples.  In general, that's a good idea, but not with this
# test script.  Why?
#
# Marpa parses itself, which is A Very Good Thing.  That means
# Marpa's user interface relies on itself in circular fashion,
# and therefore cannot be safely used to test its own
# basic functionality.
#
# This script uses a special, undocumented "internal" interface,
# which at this point I ask users to consider off-limits.

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use 5.009005;
use strict;
use feature ":5.10";
use warnings;
use lib "../lib";

use Test::More tests => 16;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $g = new Parse::Marpa(
    start => "S",
    rules => [
        [ "S", [qw/A A A A/] ],
        [ "A", [qw/a/] ],
        [ "A", [qw/E/] ],
        [ "E" ],
    ],
    default_null_value => "",
    default_action =>
<<'EOCODE'
     my $v_count = scalar @$Parse::Marpa::This::v;
     return "" if $v_count <= 0;
     return $Parse::Marpa::This::v->[0] if $v_count == 1;
     "(" . join(";", @$Parse::Marpa::This::v) . ")";
EOCODE
);

$g->set(
    terminals => [
        [ "a" => ["a"] ],
    ],
);

$g->precompute();

is( $g->show_rules(), <<'EOS', "Aycock/Horspool Rules" );
0: S -> A A A A /* nullable !useful */
1: A -> a
2: A -> E /* nullable nulling !useful */
3: E -> /* empty nullable nulling !useful */
4: S -> A S[0:1]
5: S -> A[] S[0:1]
6: S -> A S[0:1][]
7: S[0:1] -> A S[0:2]
8: S[0:1] -> A[] S[0:2]
9: S[0:1] -> A S[0:2][]
10: S[0:2] -> A A
11: S[0:2] -> A[] A
12: S[0:2] -> A A[]
13: S['] -> S
14: S['][] -> /* empty nullable nulling */
EOS

is( $g->show_symbols(), <<'EOS', "Aycock/Horspool Symbols" );
0: S, lhs=[0 4 5 6], rhs=[13]
1: A, lhs=[1 2], rhs=[0 4 6 7 9 10 11 12]
2: a, lhs=[], rhs=[1] terminal
3: E, lhs=[3], rhs=[2] nullable nulling
4: S[], lhs=[], rhs=[] nullable nulling
5: A[], lhs=[], rhs=[5 8 11 12] nullable nulling
6: S[0:1], lhs=[7 8 9], rhs=[4 5]
7: S[0:1][], lhs=[], rhs=[6] nullable nulling
8: S[0:2], lhs=[10 11 12], rhs=[7 8]
9: S[0:2][], lhs=[], rhs=[9] nullable nulling
10: S['], lhs=[13], rhs=[]
11: S['][], lhs=[14], rhs=[] nullable nulling
EOS

is( $g->show_nullable_symbols(),
    "A[] E S['][] S[0:1][] S[0:2][] S[]",
    "Aycock/Horspool Nullable Symbols");
is( $g->show_nulling_symbols(),
    "A[] E S['][] S[0:1][] S[0:2][] S[]",
    "Aycock/Horspool Nulling Symbols");
is( $g->show_productive_symbols(),
    "A A[] E S S['] S['][] S[0:1] S[0:1][] S[0:2] S[0:2][] S[] a",
    "Aycock/Horspool Productive Symbols" );
is( $g->show_accessible_symbols(),
    "A A[] E S S['] S['][] S[0:1] S[0:1][] S[0:2] S[0:2][] S[] a",
    "Aycock/Horspool Accessible Symbols" );

is( $g->show_NFA(), <<'EOS', "Aycock/Horspool NFA" );
S0: /* empty */
 empty => S30 S32
S1: A ::= . a
 <a> => S2
S2: A ::= a .
S3: S ::= . A S[0:1]
 empty => S1
 <A> => S4
S4: S ::= A . S[0:1]
 empty => S12 S15 S18
 <S[0:1]> => S5
S5: S ::= A S[0:1] .
S6: S ::= . A[] S[0:1]
 <A[]> => S7
S7: S ::= A[] . S[0:1]
 empty => S12 S15 S18
 <S[0:1]> => S8
S8: S ::= A[] S[0:1] .
S9: S ::= . A S[0:1][]
 empty => S1
 <A> => S10
S10: S ::= A . S[0:1][]
 <S[0:1][]> => S11
S11: S ::= A S[0:1][] .
S12: S[0:1] ::= . A S[0:2]
 empty => S1
 <A> => S13
S13: S[0:1] ::= A . S[0:2]
 empty => S21 S24 S27
 <S[0:2]> => S14
S14: S[0:1] ::= A S[0:2] .
S15: S[0:1] ::= . A[] S[0:2]
 <A[]> => S16
S16: S[0:1] ::= A[] . S[0:2]
 empty => S21 S24 S27
 <S[0:2]> => S17
S17: S[0:1] ::= A[] S[0:2] .
S18: S[0:1] ::= . A S[0:2][]
 empty => S1
 <A> => S19
S19: S[0:1] ::= A . S[0:2][]
 <S[0:2][]> => S20
S20: S[0:1] ::= A S[0:2][] .
S21: S[0:2] ::= . A A
 empty => S1
 <A> => S22
S22: S[0:2] ::= A . A
 empty => S1
 <A> => S23
S23: S[0:2] ::= A A .
S24: S[0:2] ::= . A[] A
 <A[]> => S25
S25: S[0:2] ::= A[] . A
 empty => S1
 <A> => S26
S26: S[0:2] ::= A[] A .
S27: S[0:2] ::= . A A[]
 empty => S1
 <A> => S28
S28: S[0:2] ::= A . A[]
 <A[]> => S29
S29: S[0:2] ::= A A[] .
S30: S['] ::= . S
 empty => S3 S6 S9
 <S> => S31
S31: S['] ::= S .
S32: S['][] ::= .
EOS

is( $g->show_ii_SDFA(), <<'EOS', "Aycock/Horspool SDFA" );
St0: 1
A ::= . a
 <a> => St7 (2)
St1: 1,12,16,18,21,25,27
A ::= . a
S[0:1] ::= . A S[0:2]
S[0:1] ::= A[] . S[0:2]
S[0:1] ::= . A S[0:2][]
S[0:2] ::= . A A
S[0:2] ::= A[] . A
S[0:2] ::= . A A[]
 <A> => St4 (13,20,22,26,29)
 <S[0:2]> => St6 (17)
 <a> => St7 (2)
St2: 1,21,25,27
A ::= . a
S[0:2] ::= . A A
S[0:2] ::= A[] . A
S[0:2] ::= . A A[]
 <A> => St8 (22,26,29)
 <a> => St7 (2)
St3: 1,3,7,9,12,16,18,21,25,27
A ::= . a
S ::= . A S[0:1]
S ::= A[] . S[0:1]
S ::= . A S[0:1][]
S[0:1] ::= . A S[0:2]
S[0:1] ::= A[] . S[0:2]
S[0:1] ::= . A S[0:2][]
S[0:2] ::= . A A
S[0:2] ::= A[] . A
S[0:2] ::= . A A[]
 <A> => St12 (4,11,13,20,22,26,29)
 <S[0:1]> => St14 (8)
 <S[0:2]> => St6 (17)
 <a> => St7 (2)
St4: 13,20,22,26,29
S[0:1] ::= A . S[0:2]
S[0:1] ::= A S[0:2][] .
S[0:2] ::= A . A
S[0:2] ::= A[] A .
S[0:2] ::= A A[] .
 empty => St2 (1,21,25,27)
 <A> => St9 (23)
 <S[0:2]> => St5 (14)
St5: 14
S[0:1] ::= A S[0:2] .
St6: 17
S[0:1] ::= A[] S[0:2] .
St7: 2
A ::= a .
St8: 22,26,29
S[0:2] ::= A . A
S[0:2] ::= A[] A .
S[0:2] ::= A A[] .
 empty => St0 (1)
 <A> => St9 (23)
St9: 23
S[0:2] ::= A A .
St10: 30,32
S['] ::= . S
S['][] ::= .
 empty => St3 (1,3,7,9,12,16,18,21,25,27)
 <S> => St11 (31)
St11: 31
S['] ::= S .
St12: 4,11,13,20,22,26,29
S ::= A . S[0:1]
S ::= A S[0:1][] .
S[0:1] ::= A . S[0:2]
S[0:1] ::= A S[0:2][] .
S[0:2] ::= A . A
S[0:2] ::= A[] A .
S[0:2] ::= A A[] .
 empty => St1 (1,12,16,18,21,25,27)
 <A> => St9 (23)
 <S[0:1]> => St13 (5)
 <S[0:2]> => St5 (14)
St13: 5
S ::= A S[0:1] .
St14: 8
S ::= A[] S[0:1] .
EOS

my $parse = new Parse::Marpa::Parse($g);

my $set0_new = <<'EOS';
Earley Set 0
0:St10,0
0:St3,0
EOS

my $set1_at_0 = <<'EOS';
Earley Set 1
1:St7,0
  token choice 0 [p=0:St3,0; t=a]
EOS

my $set1_at_1 = <<'EOS';
1:St12,0
  link choice 0 [p=0:St3,0; c=1:St7,0]
1:St1,1
1:St11,0
  link choice 0 [p=0:St10,0; c=1:St12,0] [p=0:St10,0; c=1:St14,0]
1:St14,0
  link choice 0 [p=0:St3,0; c=1:St12,0] [p=0:St3,0; c=1:St6,0]
1:St6,0
  link choice 0 [p=0:St3,0; c=1:St12,0]
EOS

my $set2_at_1 = <<'EOS';
Earley Set 2
2:St7,1
  token choice 0 [p=1:St1,1; t=a]
EOS

my $set2_at_2 = <<'EOS';
2:St9,0
  link choice 0 [p=1:St12,0; c=2:St7,1]
2:St4,1
  link choice 0 [p=1:St1,1; c=2:St7,1]
2:St2,2
2:St6,0
  link choice 0 [p=0:St3,0; c=2:St9,0]
2:St13,0
  link choice 0 [p=1:St12,0; c=2:St4,1] [p=1:St12,0; c=2:St6,1]
2:St5,0
  link choice 0 [p=1:St12,0; c=2:St4,1]
2:St6,1
  link choice 0 [p=1:St1,1; c=2:St4,1]
2:St14,0
  link choice 0 [p=0:St3,0; c=2:St6,0] [p=0:St3,0; c=2:St5,0]
2:St11,0
  link choice 0 [p=0:St10,0; c=2:St13,0] [p=0:St10,0; c=2:St14,0]
EOS

my $set3_at_2 = <<'EOS';
Earley Set 3
3:St7,2
  token choice 0 [p=2:St2,2; t=a]
EOS

my $set3_at_3 = <<'EOS';
3:St9,1
  link choice 0 [p=2:St4,1; c=3:St7,2]
3:St8,2
  link choice 0 [p=2:St2,2; c=3:St7,2]
3:St0,3
3:St5,0
  link choice 0 [p=1:St12,0; c=3:St9,1]
3:St6,1
  link choice 0 [p=1:St1,1; c=3:St9,1]
3:St5,1
  link choice 0 [p=2:St4,1; c=3:St8,2]
3:St14,0
  link choice 0 [p=0:St3,0; c=3:St5,0]
3:St13,0
  link choice 0 [p=1:St12,0; c=3:St6,1] [p=1:St12,0; c=3:St5,1]
3:St11,0
  link choice 0 [p=0:St10,0; c=3:St14,0] [p=0:St10,0; c=3:St13,0]
EOS

my $set4_at_3 = <<'EOS';
Earley Set 4
4:St7,3
  token choice 0 [p=3:St0,3; t=a]
EOS

my $set4_at_4 = <<'EOS';
4:St9,2
  link choice 0 [p=3:St8,2; c=4:St7,3]
4:St5,1
  link choice 0 [p=2:St4,1; c=4:St9,2]
4:St13,0
  link choice 0 [p=1:St12,0; c=4:St5,1]
4:St11,0
  link choice 0 [p=0:St10,0; c=4:St13,0]
EOS

my $sets_new = $set0_new;
my $sets_at_0 = $sets_new . $set1_at_0;
my $sets_at_1 = $sets_at_0 . $set1_at_1 . $set2_at_1;
my $sets_at_2 = $sets_at_1 . $set2_at_2 . $set3_at_2;
my $sets_at_3 = $sets_at_2 . $set3_at_3 . $set4_at_3;
my $sets_at_4 = $sets_at_3 . $set4_at_4;

is( $parse->show_status(1),
    "Current Earley Set: 0; Furthest: 0\n" .  $sets_new,
    "Aycock/Horspool Parse Status before parse" );

my $a = $g->get_canonical_symbol("a");
$parse->earleme([$a, "a", 1]);

is( $parse->show_status(1),
    "Current Earley Set: 1; Furthest: 1\n" .  $sets_at_0,
    "Aycock/Horspool Parse Status at 0" );

$parse->earleme([$a, "a", 1]);

is( $parse->show_status(1),
    "Current Earley Set: 2; Furthest: 2\n" .  $sets_at_1,
    "Aycock/Horspool Parse Status at 1" );

$parse->earleme([$a, "a", 1]);

is( $parse->show_status(1),
    "Current Earley Set: 3; Furthest: 3\n" .  $sets_at_2,
    "Aycock/Horspool Parse Status at 2" );

$parse->earleme([$a, "a", 1]);

is( $parse->show_status(1),
    "Current Earley Set: 4; Furthest: 4\n" .  $sets_at_3,
    "Aycock/Horspool Parse Status at 3" );

$parse->end_input();

is( $parse->show_status(1),
    "Current Earley Set: 5; Furthest: 4\n" .  $sets_at_4,
    "Aycock/Horspool Parse Status at 4" );

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
my @answer = ("", qw[(a;;;) (a;a;;) (;a;a;a) (a;a;a;a)]);

PERMUTATION: for (;;) {
    for my $i (@a) {
        $parse->initial($i);
        my $result = $parse->value();
        $total_count++;
        if ($answer[$i] ne $$result) {
            diag(
                "got "
                . $$result
                . ", expected "
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
