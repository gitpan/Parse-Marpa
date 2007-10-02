package Parse::Marpa::Test;

use base qw( Exporter );

our @EXPORT_OK = qw( normalize_SDFA );

# Take an SDFA's description and normalize out the state numbers,
# which can vary from implementation to implementation.  This is
# so that testing routines check that the SDFA's not necessarily
# produced in the same setup, and therefore not necessarily having
# the same state numbering, are equivalent.
sub normalize_SDFA {
    my $SDFA_description = shift;
    my $ret;
    SDFA_LINE: for my $SDFA_line (split(/\n/, $SDFA_description)) {
        if ($SDFA_line =~ s/( => S)\d+ \(([\d,]+)\)$/$1$2/) {
            $ret .= $SDFA_line . "\n";
	    next SDFA_LINE;
        }
        if ($SDFA_line =~ / ::= /) {
            $ret .= $SDFA_line . "\n";
	    next SDFA_LINE;
        }
	$SDFA_line =~ s/^(S)\d+: ([\d,]+)$/$1$2/;
	$ret .= $SDFA_line . "\n";
    }
    $ret;
}
