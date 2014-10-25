#!perl

# The example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630

use 5.009005;
use strict;
use warnings;

use Test::More tests => 9;

BEGIN {
    use_ok('Parse::Marpa');
}

my $g = new Parse::Marpa(
    start => "S'",
    rules => [  
        [ "S'", [qw/S/] ],
        [ "S",  [qw/A A A A/] ],
        [ "A",  [qw/a/] ],
        [ "A",  [qw/E/] ],
        [ "E" ],
    ],
    terminals => [
        [ "a" => ["a"] ],
    ],
    academic => 1,
);

is( $g->show_rules(), <<'EOS', "Aycock/Horspool Rules" );
0: S' -> S /* nullable */
1: S -> A A A A /* nullable */
2: A -> a
3: A -> E /* nullable nulling */
4: E -> /* empty nullable nulling */
EOS

is( $g->show_symbols(), <<'EOS', "Aycock/Horspool Symbols" );
0: S', lhs=[0], rhs=[] nullable
1: S, lhs=[1], rhs=[0] nullable
2: A, lhs=[2 3], rhs=[1] nullable
3: a, lhs=[], rhs=[2]
4: E, lhs=[4], rhs=[3] nullable nulling
EOS

is( $g->show_nullable_symbols(),
    "A E S S'", "Aycock/Horspool Nullable Symbols" );
is( $g->show_nulling_symbols(),
    "E", "Aycock/Horspool Nulling Symbols" );
is( $g->show_input_reachable_symbols(),
    "A E S S' a", "Aycock/Horspool Input Reachable Symbols" );
is( $g->show_start_reachable_symbols(),
    "A E S S' a", "Aycock/Horspool Start Reachable Symbols" );
is( $g->show_NFA(), <<'EOS', "Aycock/Horspool NFA" );
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

is( $g->show_ii_SDFA(), <<'EOS', , "Aycock/Horspool SDFA" );
St0: 1,2
S' ::= . S
S' ::= S .
 empty => St2 (3,4,5,6,7,8,11,12)
 <S> => St1 (2)
St1: 2
S' ::= S .
St2: 3,4,5,6,7,8,11,12
S ::= . A A A A
S ::= A . A A A
S ::= A A . A A
S ::= A A A . A
S ::= A A A A .
A ::= . a
A ::= E .
E ::= .
lexables: a
 <A> => St3 (4,5,6,7)
 <a> => St8 (9)
St3: 4,5,6,7
S ::= A . A A A
S ::= A A . A A
S ::= A A A . A
S ::= A A A A .
 empty => St7 (8,11,12)
 <A> => St4 (5,6,7)
St4: 5,6,7
S ::= A A . A A
S ::= A A A . A
S ::= A A A A .
 empty => St7 (8,11,12)
 <A> => St5 (6,7)
St5: 6,7
S ::= A A A . A
S ::= A A A A .
 empty => St7 (8,11,12)
 <A> => St6 (7)
St6: 7
S ::= A A A A .
St7: 8,11,12
A ::= . a
A ::= E .
E ::= .
lexables: a
 <a> => St8 (9)
St8: 9
A ::= a .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
