package Parse::Marpa;

use 5.010_000;

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
    my $start = shift;
    $$string =~ m/\Gqq?($punct)/ogc;
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
    my $lexeme_start = shift;
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

=head1 NAME

Parse::Marpa::Lex.pm -- Utility Methods for Lexing

=head1 OVERVIEW

These routines are used internally by MDL to implement lexing of regexes
and C<q-> and C<qq->quoted strings.
They are documented here to make them available for general use.

=head1 SYNOPSIS

    my ($regex, $token_length)
        = Parse::Marpa::Lex::lex_regex(\$string, $lexeme_start)

    my ($string, $token_length)
        = Parse::Marpa::Lex::lex_q_quote(\$string, $lexeme_start)

=head1 DESCRIPTION

=over 4

=item Parse::Marpa::Lex::lex_regex(C<$string_ref>, I<start_earleme>)

Takes as its first argument a string reference of the string containing a regex.
The regex must start in the position pointed to by C<pos $$string>.

I<start_earleme> must contain the start earleme of the regex for lexing purposes.
In many cases (such as the removal of leading whitespace), it's useful to discard
prefixes of the actual lexeme.
If prior to the call to C<lex_regex()> a prefix was removed, I<start_earleme>
should be the location where the prefix started.
If no prefix was removed, I<start_earleme> should be the same as C<pos $$string>.

How C<lex_regex()> delimits a regex is described in L<the MDL document|Parse::Marpa::LANGUAGE>.
C<lex_regex()> returns the null array on failure.
On success, returns an array of two elements.
The first element is a string containing the regex, including the C<qr-> operator if there
was one, the delimiters, and any postfix modifiers.
The second is its length for lexing purposes, including the length of
any discarded prefix.

=item Parse::Marpa::Lex::lex_q_quote(C<$string_ref>, I<start_earleme>)

Takes as its first argument a string reference of the string containing a C<q-> or C<qq->quoted string.
The C<q-> or C<qq->quoted string must start at the position pointed to by C<pos $$string>.

I<start_earleme> must contain the start earleme of the quoted string for lexing purposes.
In many cases (such as the removal of leading whitespace), it's useful to discard
prefixes of the actual lexeme.
If prior to the call to C<lex_q_quote()> a prefix was removed, I<start_earleme>
should be the location where the prefix started.
If no prefix was removed, I<start_earleme> should be the same as C<pos $$string>.

How C<lex_q_quote()> delimits a C<q-> or C<qq->quoted string is described in L<the MDL document|Parse::Marpa::LANGUAGE>.
C<lex_q_quote()> returns the null array on failure.
On success, returns an array of two elements.
The first element is a string containing the C<q-> or C<qq->quoted string,
including the C<q-> or C<qq-> "operator" and the delimiters.
The second is its length for lexing purposes, including the length of
any discarded prefix.

=back

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
