require 5.009005;

# TO THE (POTENTIAL) READER:

# At this point it's my suggestion that
# reading the code and documentation below will serve no purpose.
# This is a developer-only version,
# not useful except for my own testing.
# What applies to the code applies also to the documentation
# -- it's an early draft.  In particular, the draft
# acknowledgments might be incomplete or just plain inaccurate.

# The above is simply a humble and non-binding personal request.
# Where there seems to be a conflict between the above suggestion
# and the standard Perl license, the standard Perl license
# prevails.

# thanks, Jeffrey Kegler

use feature ":5.10";
use warnings;
use strict;

# It's all integers, except for the version number
use integer;

# package for various lexing utilities
package Parse::Marpa::Internal::Source;


use English;
{
    local($RS) = undef;
    $Parse::Marpa::Internal::compiled_source_grammar = \(<DATA>);
}

1;    # End of Parse::Marpa

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
__DATA__
