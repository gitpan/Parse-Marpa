package Parse::Marpa;

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
package Parse::Marpa::Lex;

# \x{5c} is backslash
sub gen_bracket_regex {
    my ($left, $right) = @_;
    qr/
        \G
        [^\Q$left$right\E\x{5c}]*
        (
              \Q$left\E
            | \Q$right\E
            | \x{5c}\Q$left\E
            | \x{5c}\Q$right\E
            | \x{5c}
        )
    /xms;
}

my %regex_data = (
    '{' => ['}', gen_bracket_regex('{', '}') ],
    '<' => ['>', gen_bracket_regex('<', '>') ],
    '[' => [']', gen_bracket_regex('[', ']') ],
    '(' => [')', gen_bracket_regex('(', ')') ],
);

# This is POSIX "punct" character class, except for backslash,
# and the right side bracketing symbols.
# hex 27 is single quote, hex 5b is the left square bracket.
my $punct = qr'[!"#$%&\x{27}(*+,-./:;<=?\x{5b}^_`{|~@]';

sub lex_q_quote {
    my $string = shift;
    my $start = (pos $$string) // 0;
    $$string =~ m/\G\s*qq?($punct)/ogc;
    my $left = $1;
    return unless defined $left;

    my $regex_data = $regex_data{$1};
    if (not defined $regex_data) {
        # \x{5c} is backslash
	my $regex
            = qr/
                \G
                [^\Q$left\E\x{5c}]*
                (
                     \Q$left\E
                    |\x{5c}\Q$left\E
                    |\x{5c}
                )
            /xms;
	$regex_data{$left} = $regex_data = [undef, $regex];
    }
    my ($right, $regex) = @$regex_data;
    # unbracketed quote
    if (not defined $right) {
	MATCH: while ($$string =~ /$regex/gc) {
	    next MATCH unless defined $1;
	    if ($1 eq $left) {
		my $length = (pos $$string) - $start;
		return (substr($$string, $start, $length), $length);
	    }
	}
	return;
    }

    # bracketed quote
    my $depth=1;
    MATCH: while ($$string =~ /$regex/g) {
	given ($1) {
           when (undef) { return }
	   when ($left) { $depth++; }
	   when ($right) { $depth--; }
	}
	if ($depth <= 0) {
	    my $length = (pos $$string) - $start;
	    return (substr($$string, $start, $length), $length);
	}
    }
    return;
}

sub lex_regex {
    my $string = shift;
    my $lexeme_start = (pos $$string) // 0;
    $$string =~ m{\G\s*}ogc;
    my $value_start = pos $$string;
    $$string =~ m{\G(qr$punct|/)}ogc;
    my $left_side = $1;
    return unless defined $left_side;
    my $left = substr($left_side, -1);
    my $prefix = ($left_side =~ /^qr/) ? "" : "qr";

    my $regex_data = $regex_data{$left};
    if (not defined $regex_data) {
        # \x{5c} is backslash
	my $regex
            = qr/
                \G
                [^\Q$left\E\x{5c}]*
                (
                     \Q$left\E
                    |\x{5c}\Q$left\E
                    |\x{5c}
                )
            /xms;
	$regex_data{$left} = $regex_data = [undef, $regex];
    }
    my ($right, $regex) = @$regex_data;
    # unbracketed quote
    if (not defined $right) {
	MATCH: while ($$string =~ /$regex/gc) {
	    next MATCH unless defined $1;
	    if ($1 eq $left) {
                # also take in trailing options
                $$string =~ /\G[msixpo]*/g;
                my $pos = pos $$string;
                return (
                    $prefix . substr($$string, $value_start, $pos - $value_start),
                    $pos - $lexeme_start
                );
	    }
	}
	return;
    }

    # bracketed quote
    my $depth=1;
    MATCH: while ($$string =~ /$regex/g) {
	given ($1) {
           when (undef) { return }
	   when ($left) { $depth++; }
	   when ($right) { $depth--; }
	}
	if ($depth <= 0) {
            # also take in trailing options
            $$string =~ /\G[msixpo]*/g;
            my $pos = pos $$string;
            return (
                $prefix . substr($$string, $value_start, $pos - $value_start),
                $pos - $lexeme_start
            );
	}
    }
    return;
}

1;    # End of Parse::Marpa

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
