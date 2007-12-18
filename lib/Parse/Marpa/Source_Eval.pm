require 5.009005;

use feature ":5.10";
use warnings;
use strict;

# I use a separate file for this function, just to keep the
# namespace issues to a minimum.  I think this would be fine
# in the main .pm file, but I'd just like to keep things very
# simple with the lexicals variables on this one.

# It's all integers, except for the version number
use integer;

package Parse::Marpa::Internal::Grammar;

our $STATE;
our $PERL_RULES;

# package for various lexing utilities
package Parse::Marpa::Internal::Source_Eval;

sub Parse::Marpa::Internal::raw_grammar_eval {
     my $grammar = shift;
     my $raw_grammar = shift;

     my $start_symbol;
     my $semantics;
     my $version;
     my $preamble;
     my $default_lex_prefix;
     my $default_action;
     my $rules;
     my $terminals;
     my %strings;

     {
         my @warnings;
         local $SIG{__WARN__} = sub { push(@warnings, $_[0]) };
         eval $$raw_grammar;
         my $fatal_error = $@;
         if ($fatal_error or @warnings) {
              Parse::Marpa::Internal::die_on_problems(
                  $fatal_error,
                  \@warnings,
                  "evaluating gramar",
                  "evaluating gramar",
                  $raw_grammar,
              );
         }
     }

     # $grammar->[ Parse::Marpa::Internal::Grammar::START ] = $start_symbol;
     # $grammar->[ Parse::Marpa::Internal::Grammar::SEMANTICS ] = $semantics;
     # $grammar->[ Parse::Marpa::Internal::Grammar::VERSION ] = $version;
     # $grammar->[ Parse::Marpa::Internal::Grammar::PREAMBLE ] = $preamble;
     # $grammar->[ Parse::Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX ] = $default_lex_prefix;
     # $grammar->[ Parse::Marpa::Internal::Grammar::DEFAULT_ACTION ] = $default_action;
     Parse::Marpa::Internal::add_user_rules($grammar, $rules);
     Parse::Marpa::Internal::add_user_terminals($grammar, $terminals);
     $grammar->[Parse::Marpa::Internal::Grammar::STATE] =
         Parse::Marpa::Internal::Grammar::PERL_RULES;

}

1;    # End of Parse::Marpa

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
