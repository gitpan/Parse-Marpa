use 5.009005;
use strict;
use feature ":5.10";
use warnings;
use lib "../lib";
use English;

# use Test::More tests => 2;

# BEGIN {
	# use_ok( 'Parse::Marpa' );
# }
use Parse::Marpa;

my @tests = split(/\n/, <<'EO_TESTS');
time  / 25 ; # / ; die "this dies!";
sin  / 25 ; # / ; die "this dies!";
caller  / 25 ; # / ; die "this dies!";
eof  / 25 ; # / ; die "this dies!";
localtime  / 25 ; # / ; die "this dies!";
EO_TESTS

my $source; { local($RS) = undef; $source = <DATA> };

my $g = new Parse::Marpa(
    warnings => 1,
    code_lines => -1,
);

$g->set( source => \$source);

$g->precompute();

TEST: while (my $test = pop @tests) {
    say "Here's what I'm parsing: ", $test;
    my $parse = new Parse::Marpa::Parse($g);
    $parse->text(\$test);
    $parse->initial();
    my @parses;
    push(@parses, $parse->value);
    while ($parse->next) {
        push(@parses, $parse->value);
    }
    if (scalar @parses == 1) {
       say "Things look good, I've got just one parse:";
       say ${$parses[0]};
       print "\n";
       next TEST;
    }
    say "Things look complicated here, I've got ", scalar @parses, " parses:";
    for (my $i = 0; $i < @parses; $i++) {
        say "Parse $i: ", ${$parses[$i]};
    }
    print "\n";
}

__DATA__
semantics are perl5.  version is 0.1.56.  the start symbol is perl line.
the default lex prefix is qr/\s*/.

perl line: perl statements, optional comment.
q{
    my $result = $Parse::Marpa::This::v->[0];
    $result .= ", comment"
	if defined $Parse::Marpa::This::v->[1];
    $result
}.

perl statements: semicolon separated perl statement sequence.
q{ join(", ", @{$Parse::Marpa::This::v}) }.

perl statement: division. q{ "division" }.

perl statement: function call.
q{ $Parse::Marpa::This::v->[0] }.

perl statement: empty statement.  q{ "empty statement" }.

perl statement: /die/, string literal.  q{ "die statement" }.

division: expr, division sign, expr.

expr: function call.

expr: number.

function call: unary function name, argument.
q{ $Parse::Marpa::This::v->[0] . " function call" }.

function call: nullary function name.
q{ $Parse::Marpa::This::v->[0] . " function call" }.

argument: pattern match.

empty statement: horizontal whitespace.

horizontal whitespace matches qr/ \t/.

unary function name matches /(caller|eof|sin|localtime)/.

nullary function name matches /(caller|eof|sin|time|localtime)/.

number matches /\d+/.

semicolon matches /;/.

division sign matches qr{/}.

pattern match matches qr{/[^/]*/}.

comment matches /#.*/.

string literal matches qr{"[^"]*"}.
