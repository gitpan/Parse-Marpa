#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use strict;
use warnings;
use Parse::Marpa::Test qw( normalize_SDFA );

use Test::More tests => 15;

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

#is( normalize_SDFA($g->show_SDFA()),
    #normalize_SDFA(<<'EOS'), "Aycock/Horspool SDFA" );

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

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status before parse" );
Current Earley Set: 0
Earley Set 0
0,0
1,0
EOS

my $a = $g->get_symbol("a");
$parse->token([$a, "a", 1]);

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status at 0" );
Current Earley Set: 1
Earley Set 0
0,0
1,0
Earley Set 1
5,0 [p=1,0; v=a]
EOS

$parse->token([$a, "a", 1]);

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status at 1" );
Current Earley Set: 2
Earley Set 0
0,0
1,0
Earley Set 1
5,0 [p=1,0; v=a]
3,0 [p=1,0; c=5,0]
4,1
2,0 [p=0,0; c=3,0] [p=0,0; c=7,0]
7,0 [p=1,0; c=3,0] [p=1,0; c=6,0]
6,0 [p=1,0; c=3,0]
Earley Set 2
5,1 [p=4,1; v=a]
EOS

$parse->token([$a, "a", 1]);

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status at 2" );
Current Earley Set: 3
Earley Set 0
0,0
1,0
Earley Set 1
5,0 [p=1,0; v=a]
3,0 [p=1,0; c=5,0]
4,1
2,0 [p=0,0; c=3,0] [p=0,0; c=7,0]
7,0 [p=1,0; c=3,0] [p=1,0; c=6,0]
6,0 [p=1,0; c=3,0]
Earley Set 2
5,1 [p=4,1; v=a]
8,0 [p=3,0; c=5,1]
11,1 [p=4,1; c=5,1]
12,2
6,0 [p=1,0; c=8,0]
10,0 [p=3,0; c=11,1] [p=3,0; c=6,1]
9,0 [p=3,0; c=11,1]
6,1 [p=4,1; c=11,1]
7,0 [p=1,0; c=6,0] [p=1,0; c=9,0]
2,0 [p=0,0; c=10,0] [p=0,0; c=7,0]
Earley Set 3
5,2 [p=12,2; v=a]
EOS

$parse->token([$a, "a", 1]);

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status at 3" );
Current Earley Set: 4
Earley Set 0
0,0
1,0
Earley Set 1
5,0 [p=1,0; v=a]
3,0 [p=1,0; c=5,0]
4,1
2,0 [p=0,0; c=3,0] [p=0,0; c=7,0]
7,0 [p=1,0; c=3,0] [p=1,0; c=6,0]
6,0 [p=1,0; c=3,0]
Earley Set 2
5,1 [p=4,1; v=a]
8,0 [p=3,0; c=5,1]
11,1 [p=4,1; c=5,1]
12,2
6,0 [p=1,0; c=8,0]
10,0 [p=3,0; c=11,1] [p=3,0; c=6,1]
9,0 [p=3,0; c=11,1]
6,1 [p=4,1; c=11,1]
7,0 [p=1,0; c=6,0] [p=1,0; c=9,0]
2,0 [p=0,0; c=10,0] [p=0,0; c=7,0]
Earley Set 3
5,2 [p=12,2; v=a]
8,1 [p=11,1; c=5,2]
13,2 [p=12,2; c=5,2]
14,3
9,0 [p=3,0; c=8,1]
6,1 [p=4,1; c=8,1]
9,1 [p=11,1; c=13,2]
7,0 [p=1,0; c=9,0]
10,0 [p=3,0; c=6,1] [p=3,0; c=9,1]
2,0 [p=0,0; c=7,0] [p=0,0; c=10,0]
Earley Set 4
5,3 [p=14,3; v=a]
EOS

$parse->token();

is( $parse->show_status(), <<'EOS', "Aycock/Horspool Parse Status at 4" );
Current Earley Set: 5
Earley Set 0
0,0
1,0
Earley Set 1
5,0 [p=1,0; v=a]
3,0 [p=1,0; c=5,0]
4,1
2,0 [p=0,0; c=3,0] [p=0,0; c=7,0]
7,0 [p=1,0; c=3,0] [p=1,0; c=6,0]
6,0 [p=1,0; c=3,0]
Earley Set 2
5,1 [p=4,1; v=a]
8,0 [p=3,0; c=5,1]
11,1 [p=4,1; c=5,1]
12,2
6,0 [p=1,0; c=8,0]
10,0 [p=3,0; c=11,1] [p=3,0; c=6,1]
9,0 [p=3,0; c=11,1]
6,1 [p=4,1; c=11,1]
7,0 [p=1,0; c=6,0] [p=1,0; c=9,0]
2,0 [p=0,0; c=10,0] [p=0,0; c=7,0]
Earley Set 3
5,2 [p=12,2; v=a]
8,1 [p=11,1; c=5,2]
13,2 [p=12,2; c=5,2]
14,3
9,0 [p=3,0; c=8,1]
6,1 [p=4,1; c=8,1]
9,1 [p=11,1; c=13,2]
7,0 [p=1,0; c=9,0]
10,0 [p=3,0; c=6,1] [p=3,0; c=9,1]
2,0 [p=0,0; c=7,0] [p=0,0; c=10,0]
Earley Set 4
5,3 [p=14,3; v=a]
8,2 [p=13,2; c=5,3]
9,1 [p=11,1; c=8,2]
10,0 [p=3,0; c=9,1]
2,0 [p=0,0; c=10,0]
EOS

my $evaluator = new Parse::Marpa::Evaluator($parse);

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
