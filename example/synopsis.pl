#!perl

use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use Parse::Marpa;

# remember to use refs to strings
my $value = Parse::Marpa::mdl(
    (   do { local ($RS) = undef; my $source = <DATA>; \$source; }
    ),
    \('2+2*3')
);
say ${$value};

__DATA__
semantics are perl5.  version is 1.008000.  start symbol is Expression.

Expression: Expression, /[*]/, Expression.  priority 200.  q{
    $_[0] * $_[2]
}.

Expression: Expression, /[+]/, Expression.  priority 100.  q{
    $_[0] + $_[2]
}.

Expression: /\d+/.  q{ $_[0] }.
