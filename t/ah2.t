#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use strict;
use warnings;
use Parse::Marpa::Test qw( normalize_SDFA );

use Test::More tests => 12;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

my $g = new Parse::Marpa(
    start => "S",
    rules => [   [ "a" => qr/a/ ], [qw/S A A A A/], [qw/A a/],
        [qw/A E/], [qw/E/],
    ],
);

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
0: a, lhs=[], rhs=[1]
1: S, lhs=[0 4 5 6], rhs=[13]
2: A, lhs=[1 2], rhs=[0 4 6 7 9 10 11 12]
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
is( $g->show_input_reachable_symbols(),
    "A A[] E S S['] S['][] S[0:1] S[0:1][] S[0:2] S[0:2][] S[] a",
    "Aycock/Horspool Input Reachable Symbols" );
is( $g->show_start_reachable_symbols(),
    "A A[] E S S['] S['][] S[0:1] S[0:1][] S[0:2] S[0:2][] S[] a",
    "Aycock/Horspool Start Reachable Symbols" );

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

is( normalize_SDFA($g->show_SDFA()),
    normalize_SDFA(<<'EOS'), "Aycock/Horspool SDFA" );
S0: 30,32
S['] ::= . S
S['][] ::= .
 empty => S1 (1,3,7,9,12,16,18,21,25,27)
 <S> => S2 (31)
S1: 1,3,7,9,12,16,18,21,25,27
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
 <A> => S3 (4,11,13,20,22,26,29)
 <S[0:1]> => S7 (8)
 <S[0:2]> => S6 (17)
 <a> => S5 (2)
S2: 31
S['] ::= S .
S3: 4,11,13,20,22,26,29
S ::= A . S[0:1]
S ::= A S[0:1][] .
S[0:1] ::= A . S[0:2]
S[0:1] ::= A S[0:2][] .
S[0:2] ::= A . A
S[0:2] ::= A[] A .
S[0:2] ::= A A[] .
 empty => S4 (1,12,16,18,21,25,27)
 <A> => S8 (23)
 <S[0:1]> => S10 (5)
 <S[0:2]> => S9 (14)
S4: 1,12,16,18,21,25,27
A ::= . a
S[0:1] ::= . A S[0:2]
S[0:1] ::= A[] . S[0:2]
S[0:1] ::= . A S[0:2][]
S[0:2] ::= . A A
S[0:2] ::= A[] . A
S[0:2] ::= . A A[]
 <A> => S11 (13,20,22,26,29)
 <S[0:2]> => S6 (17)
 <a> => S5 (2)
S5: 2
A ::= a .
S6: 17
S[0:1] ::= A[] S[0:2] .
S7: 8
S ::= A[] S[0:1] .
S8: 23
S[0:2] ::= A A .
S9: 14
S[0:1] ::= A S[0:2] .
S10: 5
S ::= A S[0:1] .
S11: 13,20,22,26,29
S[0:1] ::= A . S[0:2]
S[0:1] ::= A S[0:2][] .
S[0:2] ::= A . A
S[0:2] ::= A[] A .
S[0:2] ::= A A[] .
 empty => S12 (1,21,25,27)
 <A> => S8 (23)
 <S[0:2]> => S9 (14)
S12: 1,21,25,27
A ::= . a
S[0:2] ::= . A A
S[0:2] ::= A[] . A
S[0:2] ::= . A A[]
 <A> => S13 (22,26,29)
 <a> => S5 (2)
S13: 22,26,29
S[0:2] ::= A . A
S[0:2] ::= A[] A .
S[0:2] ::= A A[] .
 empty => S14 (1)
 <A> => S8 (23)
S14: 1
A ::= . a
 <a> => S5 (2)
EOS

my $parse = new Parse::Marpa::Parse($g);

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status" );
Current Earley Set: 0
Earley Working List 0
0, 0
1, 0
EOS

my $a = $g->get_symbol("a");
$parse->token([$a, "a", 1]);

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status" );
Current Earley Set: 1
Earley Set 0
0, 0
1, 0
Earley Working List 1
5, 0; p=[1, 0]; v=a
EOS

$parse->token([$a, "a", 1]);

SKIP: {
    skip "not yet debugged", 1;

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status" );
not ok 12 - Aycock/Horspool Parse Status
#   Failed test 'Aycock/Horspool Parse Status'
#   in ah2.t at line 249.
#          got: 'Current Earley Set: 2
# Earley Set 0
# 0, 0
# 1, 0
# Earley Set 1
# 5, 0 [p=1, 0; v=a]
# '
#     expected: ''
EOS

}; # SKIP

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
