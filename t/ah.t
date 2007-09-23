#!perl -T

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630

use strict;
use warnings;

use Test::More tests => 8;

BEGIN {
    use_ok('Parse::Marpa');
}

my $g = new Parse::Marpa(
    start => "S'",
    rules => [   [ "a" => qr/a/ ], [qw/S' S/], [qw/S A A A A/], [qw/A a/],
        [qw/A E/], [qw/E/],
    ],
    augment => 0,
);

is( $g->_show_rules(), <<'EOS', "Aycock/Horspool Rules" );
0: S' -> S /* nullable */
1: S -> A A A A /* nullable */
2: A -> a
3: A -> E /* nullable */
4: E -> /* empty nullable */
EOS

is( $g->_show_symbols(), <<'EOS', "Aycock/Horspool Symbols" );
0: a, lhs=[], rhs=[2]
1: S', lhs=[0], rhs=[] nullable
2: S, lhs=[1], rhs=[0] nullable
3: A, lhs=[2 3], rhs=[1] nullable
4: E, lhs=[4], rhs=[3] nullable
EOS

is( $g->_show_nullable_symbols(),
    "A E S S'", "Aycock/Horspool Nullable Symbols" );
is( $g->_show_input_reachable_symbols(),
    "A E S S' a", "Aycock/Horspool Input Reachable Symbols" );
is( $g->_show_start_reachable_symbols(),
    "A E S S' a", "Aycock/Horspool Start Reachable Symbols" );
is( $g->_show_NFA(), <<'EOS', "Aycock/Horspool NFA" );
S0: /* empty */
 empty => S1
S1: S' ::= . S
 empty => S3
 <S> => S2
S2: S' ::= S .
S3: S ::= . A A A A
 empty => S8 S10
 <A> => S4
S4: S ::= A . A A A
 empty => S8 S10
 <A> => S5
S5: S ::= A A . A A
 empty => S8 S10
 <A> => S6
S6: S ::= A A A . A
 empty => S8 S10
 <A> => S7
S7: S ::= A A A A .
S8: A ::= . a
 <a> => S9
S9: A ::= a .
S10: A ::= . E
 empty => S12
 <E> => S11
S11: A ::= E .
S12: E ::= .
EOS

is( $g->_show_SDFA(), <<'EOS', "Aycock/Horspool SDFA" );
S0: 1,2
S' ::= . S
S' ::= S .
 <S> => S2 (2)
 empty => S1 (3,4,5,6,7,8,10,11,12)
S1: 3,4,5,6,7,8,10,11,12
S ::= . A A A A
S ::= A . A A A
S ::= A A . A A
S ::= A A A . A
S ::= A A A A .
A ::= . a
A ::= . E
A ::= E .
E ::= .
 <A> => S3 (4,5,6,7)
 <a> => S5 (9)
 <E> => S6 (11)
S2: 2
S' ::= S .
S3: 4,5,6,7
S ::= A . A A A
S ::= A A . A A
S ::= A A A . A
S ::= A A A A .
 <A> => S7 (5,6,7)
 empty => S4 (8,10,11,12)
S4: 8,10,11,12
A ::= . a
A ::= . E
A ::= E .
E ::= .
 <a> => S5 (9)
 <E> => S6 (11)
S5: 9
A ::= a .
S6: 11
A ::= E .
S7: 5,6,7
S ::= A A . A A
S ::= A A A . A
S ::= A A A A .
 <A> => S8 (6,7)
 empty => S4 (8,10,11,12)
S8: 6,7
S ::= A A A . A
S ::= A A A A .
 <A> => S9 (7)
 empty => S4 (8,10,11,12)
S9: 7
S ::= A A A A .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
