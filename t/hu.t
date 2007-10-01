#!perl

use strict;
use warnings;

use Test::More tests => 9;

BEGIN {
	use_ok( 'Parse::Marpa' );
}

# Grammar from Hopcroft & Ullman, pp. 248, 250
my $g = new Parse::Marpa(
    start => "S'",
    rules => [
        [ "a" => qr/a/ ],
        [ "b" => qr/b/ ],
        [ "c" => qr/c/ ],
        [ qw/S' S c/ ],
        [ qw/S S A/ ],
        [ qw/S A/ ],
        [ qw/A a S b/ ],
        [ qw/A a b/ ],
    ],
    academic => 1,
);

is($g->_show_rules(), <<'EOS', "Hopcroft/Ullman Rules");
0: S' -> S c
1: S -> S A
2: S -> A
3: A -> a S b
4: A -> a b
EOS

is($g->_show_symbols(), <<'EOS', "Hopcroft/Ullman Symbols");
0: a, lhs=[], rhs=[3 4]
1: b, lhs=[], rhs=[3 4]
2: c, lhs=[], rhs=[0]
3: S', lhs=[0], rhs=[]
4: S, lhs=[1 2], rhs=[0 1 3]
5: A, lhs=[3 4], rhs=[1 2]
EOS

is($g->_show_nullable_symbols(), "", "Hopcroft/Ullman Nullable Symbols");
is($g->_show_nulling_symbols(), "", "Hopcroft/Ullman Nulling Symbols");
is($g->_show_input_reachable_symbols(), 'A S S\' a b c', "Hopcroft/Ullman Input Reachable Symbols");
is($g->_show_start_reachable_symbols(), 'A S S\' a b c', "Hopcroft/Ullman Start Reachable Symbols");
is($g->_show_NFA(), <<'EOS', "Hopcroft/Ullman NFA");
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

is($g->_show_SDFA(), <<'EOS', "Hopcroft/Ullman SDFA");
S0: 1
S' ::= . S c
 <S> => S2 (2)
 empty => S1 (4,7,9,13)
S1: 4,7,9,13
S ::= . S A
S ::= . A
A ::= . a S b
A ::= . a b
 <S> => S4 (5)
 <A> => S3 (8)
 <a> => S6 (10,14)
S2: 2
S' ::= S . c
 <c> => S7 (3)
S3: 8
S ::= A .
S4: 5
S ::= S . A
 <A> => S8 (6)
 empty => S5 (9,13)
S5: 9,13
A ::= . a S b
A ::= . a b
 <a> => S6 (10,14)
S6: 10,14
A ::= a . S b
A ::= a . b
 <S> => S9 (11)
 empty => S1 (4,7,9,13)
 <b> => S10 (15)
S7: 3
S' ::= S c .
S8: 6
S ::= S A .
S9: 11
A ::= a S . b
 <b> => S11 (12)
S10: 15
A ::= a b .
S11: 12
A ::= a S b .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
