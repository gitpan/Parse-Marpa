#!perl

use strict;
use warnings;

use Test::More tests => 9;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

# A grammar from Hopcroft & Ullman, pp. 248, 250.

my $g = new Parse::Marpa(
    start => "S'",
    rules => [
        [ "S'", [qw/S c/] ],
        [ "S",  [qw/S A/] ],
        [ "S",  [qw/A/] ],
        [ "A",  [qw/a S b/] ],
        [ "A",  [qw/a b/] ],
    ],
    terminals => [
        [ "a" => qr/a/ ],
        [ "b" => qr/b/ ],
        [ "c" => qr/c/ ],
    ],
    academic => 1,
);

is($g->show_rules(), <<'EOS', "Hopcroft/Ullman Rules");
0: S' -> S c
1: S -> S A
2: S -> A
3: A -> a S b
4: A -> a b
EOS

is($g->show_symbols(), <<'EOS', "Hopcroft/Ullman Symbols");
0: S', lhs=[0], rhs=[]
1: S, lhs=[1 2], rhs=[0 1 3]
2: c, lhs=[], rhs=[0]
3: A, lhs=[3 4], rhs=[1 2]
4: a, lhs=[], rhs=[3 4]
5: b, lhs=[], rhs=[3 4]
EOS

is($g->show_nullable_symbols(), "", "Hopcroft/Ullman Nullable Symbols");
is($g->show_nulling_symbols(), "", "Hopcroft/Ullman Nulling Symbols");
is($g->show_input_reachable_symbols(), 'A S S\' a b c', "Hopcroft/Ullman Input Reachable Symbols");
is($g->show_start_reachable_symbols(), 'A S S\' a b c', "Hopcroft/Ullman Start Reachable Symbols");
is($g->show_NFA(), <<'EOS', "Hopcroft/Ullman NFA");
S0: /* empty */
 empty => S1
S1: S' ::= . S c
 empty => S4 S7
 <S> => S2
S2: S' ::= S . c
 <c> => S3
S3: S' ::= S c .
S4: S ::= . S A
 empty => S4 S7
 <S> => S5
S5: S ::= S . A
 empty => S9 S13
 <A> => S6
S6: S ::= S A .
S7: S ::= . A
 empty => S9 S13
 <A> => S8
S8: S ::= A .
S9: A ::= . a S b
 <a> => S10
S10: A ::= a . S b
 empty => S4 S7
 <S> => S11
S11: A ::= a S . b
 <b> => S12
S12: A ::= a S b .
S13: A ::= . a b
 <a> => S14
S14: A ::= a . b
 <b> => S15
S15: A ::= a b .
EOS

is( $g->show_ii_SDFA(), <<'EOS', "Hopcroft/Ullman SDFA");
St0: 1
S' ::= . S c
 empty => St7 (4,7,9,13)
 <S> => St5 (2)
St1: 10,14
A ::= a . S b
A ::= a . b
 empty => St7 (4,7,9,13)
 <S> => St2 (11)
 <b> => St4 (15)
St2: 11
A ::= a S . b
 <b> => St3 (12)
St3: 12
A ::= a S b .
St4: 15
A ::= a b .
St5: 2
S' ::= S . c
 <c> => St6 (3)
St6: 3
S' ::= S c .
St7: 4,7,9,13
S ::= . S A
S ::= . A
A ::= . a S b
A ::= . a b
 <A> => St10 (8)
 <S> => St8 (5)
 <a> => St1 (10,14)
St8: 5
S ::= S . A
 empty => St11 (9,13)
 <A> => St9 (6)
St9: 6
S ::= S A .
St10: 8
S ::= A .
St11: 9,13
A ::= . a S b
A ::= . a b
 <a> => St1 (10,14)
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
