#!perl

use strict;
use warnings;

use Test::More tests => 9;

BEGIN {
	use_ok( 'Parse::Marpa' );
}


# Grammar from Grune & Jacobs, Parsing Techniques: A Practical Guide, pp 206-207
# the book is available on the web
my $g = new Parse::Marpa(
    start => "S'",
    rules => [
        [ 'n' => qr/n/ ],
        [ '$' => qr/\$/ ],
        [ ')' => qr/\)/ ],
        [ '(' => qr/\(/ ],
        [ '-' => qr/\-/ ],
        [ qw/S' S $/ ],
        [ qw/S E/ ],
        [ qw/E E - T/ ],
        [ qw/E T/ ],
        [ qw/T n/ ],
        [ qw/T ( E )/ ],
    ],
    academic => 1,
);

is($g->_show_rules(), <<'EOS', "Grune/Jacobs Rules");
0: S' -> S $
1: S -> E
2: E -> E - T
3: E -> T
4: T -> n
5: T -> ( E )
EOS

is($g->_show_symbols(), <<'EOS', "Grune/Jacobs Symbols");
0: n, lhs=[], rhs=[4]
1: $, lhs=[], rhs=[0]
2: ), lhs=[], rhs=[5]
3: (, lhs=[], rhs=[5]
4: -, lhs=[], rhs=[2]
5: S', lhs=[0], rhs=[]
6: S, lhs=[1], rhs=[0]
7: E, lhs=[2 3], rhs=[1 2 5]
8: T, lhs=[4 5], rhs=[2 3]
EOS

is($g->_show_nullable_symbols(), "", "Grune/Jacobs Nullable Symbols");
is($g->_show_nulling_symbols(), "", "Grune/Jacobs Nulling Symbols");
is($g->_show_input_reachable_symbols(), '$ ( ) - E S S\' T n', "Grune/Jacobs Input Reachable Symbols");
is($g->_show_start_reachable_symbols(), '$ ( ) - E S S\' T n', "Grune/Jacobs Start Reachable Symbols");
is($g->_show_NFA(), <<'EOS', "Grune/Jacobs NFA");
S0: /* empty */
 empty => S1
S1: S' ::= . S $
 empty => S4
 <S> => S2
S2: S' ::= S . $
 <$> => S3
S3: S' ::= S $ .
S4: S ::= . E
 empty => S6 S10
 <E> => S5
S5: S ::= E .
S6: E ::= . E - T
 empty => S6 S10
 <E> => S7
S7: E ::= E . - T
 <-> => S8
S8: E ::= E - . T
 empty => S12 S14
 <T> => S9
S9: E ::= E - T .
S10: E ::= . T
 empty => S12 S14
 <T> => S11
S11: E ::= T .
S12: T ::= . n
 <n> => S13
S13: T ::= n .
S14: T ::= . ( E )
 <(> => S15
S15: T ::= ( . E )
 empty => S6 S10
 <E> => S16
S16: T ::= ( E . )
 <)> => S17
S17: T ::= ( E ) .
EOS

is($g->_show_SDFA(), <<'EOS', "Grune/Jacobs SDFA");
S0: 1
S' ::= . S $
 empty => S1 (4,6,10,12,14)
 <S> => S2 (2)
S1: 4,6,10,12,14
S ::= . E
E ::= . E - T
E ::= . T
T ::= . n
T ::= . ( E )
 <(> => S5 (15)
 <E> => S7 (5,7)
 <T> => S4 (11)
 <n> => S3 (13)
S2: 2
S' ::= S . $
 <$> => S8 (3)
S3: 13
T ::= n .
S4: 11
E ::= T .
S5: 15
T ::= ( . E )
 empty => S6 (6,10,12,14)
 <E> => S9 (16)
S6: 6,10,12,14
E ::= . E - T
E ::= . T
T ::= . n
T ::= . ( E )
 <(> => S5 (15)
 <E> => S10 (7)
 <T> => S4 (11)
 <n> => S3 (13)
S7: 5,7
S ::= E .
E ::= E . - T
 <-> => S11 (8)
S8: 3
S' ::= S $ .
S9: 16
T ::= ( E . )
 <)> => S13 (17)
S10: 7
E ::= E . - T
 <-> => S11 (8)
S11: 8
E ::= E - . T
 empty => S12 (12,14)
 <T> => S14 (9)
S12: 12,14
T ::= . n
T ::= . ( E )
 <(> => S5 (15)
 <n> => S3 (13)
S13: 17
T ::= ( E ) .
S14: 9
E ::= E - T .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
