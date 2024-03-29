#!perl

use 5.010;
use strict;
use warnings;
use Test::More tests => 2;
use lib 'lib';
use Parse::Marpa;

# This is code to test examples, in order to prove that they do actually
# compile and execute.  No checking other than for compilation errors
# or fatal exceptions is done.  This code DOES NOT do anything sensible.

Test::More::pass('misc.t compiled');

my $mdl_source = <<'END';
semantics are perl5.
version is 1.008000.
start symbol is S.

S: Document.

Document: /.+/ .

Document: .

END

my $trace_fh;
my $location;
my $first_result;
my @all_results;

my $grammar = Parse::Marpa::Grammar->new();

$grammar->set( { mdl_source => \$mdl_source } );

my $cloned_grammar = $grammar->clone();

my $stringified_grammar = $grammar->stringify();

## use Marpa::Test::Display unstringify snippet

$grammar =
    Parse::Marpa::Grammar::unstringify( $stringified_grammar, $trace_fh );

$grammar = Parse::Marpa::Grammar::unstringify($stringified_grammar);

## no Marpa::Test::Display

my $new_lex_preamble = q{1};

## use Marpa::Test::Display new Recognizer snippet

my $recce = Parse::Marpa::Recognizer->new(
    {   grammar      => $grammar,
        lex_preamble => $new_lex_preamble,
    }
);

## no Marpa::Test::Display

$recce->end_input();

my $stringified_recce = $recce->stringify();

## use Marpa::Test::Display unstringify Recognizer snippet

$recce =
    Parse::Marpa::Recognizer::unstringify( $stringified_recce, $trace_fh );

$recce = Parse::Marpa::Recognizer::unstringify($stringified_recce);

## no Marpa::Test::Display

my $cloned_recce = $recce->clone();

my $evaler = Parse::Marpa::Evaluator->new(
    {   recce => $recce,
        end   => $location,
        clone => 0,
    }
);

my $depth = 1;

$evaler->set( { trace_values => 1 } );

my $input_string = q{};
my $lexeme_start = 0;

{
## use Marpa::Test::Display lex_regex snippet

    my ( $regex, $token_length ) =
        Parse::Marpa::Lex::lex_regex( \$input_string, $lexeme_start );

## no Marpa::Test::Display
}

{
## use Marpa::Test::Display lex_q_quote snippet

    my ( $string, $token_length ) =
        Parse::Marpa::Lex::lex_q_quote( \$input_string, $lexeme_start );

## no Marpa::Test::Display
}

my $g = Parse::Marpa::Grammar->new();

$g->set( { start => Parse::Marpa::MDL::canonical_symbol_name('Document') } );

## use Marpa::Test::Display get_symbol snippet

my $op = Parse::Marpa::MDL::get_symbol( $grammar, 'Op' );

## no Marpa::Test::Display

my $grammar_description = $mdl_source;
my $string_to_parse     = q{};

## use Marpa::Test::Display mdl scalar snippet

$first_result = Parse::Marpa::mdl( \$grammar_description, \$string_to_parse );

## no Marpa::Test::Display

## use Marpa::Test::Display mdl array snippet

@all_results = Parse::Marpa::mdl( \$grammar_description, \$string_to_parse );

## no Marpa::Test::Display

## use Marpa::Test::Display mdl scalar hash args snippet

$first_result =
    Parse::Marpa::mdl( \$grammar_description, \$string_to_parse,
    { warnings => 0 } );

## no Marpa::Test::Display

Test::More::pass('misc.t ran to end');
