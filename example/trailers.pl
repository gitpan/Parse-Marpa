$start_symbol //= "(undefined start symbol)";
$semantics //= "not defined";
$version //= -1;

croak("Version requested is ", $version, "\nVersion must match ", $Parse::Marpa::VERSION, " exactly.")
   unless $version == $Parse::Marpa::VERSION;

croak("Semantics are ", $semantics, "\nThe only semantics currently available are perl5.")
   unless $semantics eq "perl5";

my $g = new Parse::Marpa(
    start => $start_symbol,
    rules => $rules,
    terminals => $terminals,
    default_lex_prefix => $default_lex_prefix,
    warnings => 1,
);

my $parse = new Parse::Marpa::Parse(
   grammar=> $g,
);

sub locator {
    my $earleme = shift;
    my $string = shift;

    state $lines;
    $lines = [0];
    my $pos = pos $$string = 0;
    NL: while ($$string =~ /\n/g) {
	$pos = pos $$string;
	push(@$lines, $pos);
	last NL if $pos > $earleme;
    }
    my $line = (@$lines) - ($pos > $earleme ? 2 : 1);
    my $line_start = $lines->[$line];
    return ($line, $line_start);
}

my $spec;

{
    local($RS) = undef;
    $spec = <GRAMMAR>;
    if ((my $earleme = $parse->lex_string(\$spec)) >= 0) {
	# print $parse->show_status();
	# for the editors, line numbering starts at 1
	# do something about this?
	my ($line, $line_start) = locator($earleme, \$spec);
	say STDERR "Parse exhausted at line ", $line+1, ", earleme $earleme";
	given (index($spec, "\n", $line_start)) {
	    when (undef) { say STDERR substr($spec, $line_start) }
	    default { say STDERR substr($spec, $line_start, $_-$line_start) }
	}
	say STDERR +(" " x ($earleme-$line_start)), "^";
	exit 1;
    }
    $parse->lex_end();
}

unless ($parse->initial()) {
    say STDERR "No parse";
    my ($earleme, $lhs) = $parse->find_complete_rule();
    unless (defined $earleme) {
	say STDERR "No rules completed";
	exit 1;
    }
    my ($line, $line_start) = locator($earleme, \$spec);
    say STDERR "Parses completed at line $line, earleme $earleme for symbols:";
    say STDERR join(", ", @$lhs);
    given (index($spec, "\n", $line_start)) {
	when (undef) { say STDERR substr($spec, $line_start) }
	default { say STDERR substr($spec, $line_start, $_-$line_start) }
    }
    say STDERR +(" " x ($earleme-$line_start)), "^";
    exit 1;
}

my $headers;
{ local($RS) = undef; open(HEADERS, "<", "headers.pl"); $headers = <HEADERS>; }

my $trailers;
{ local($RS) = undef; open(TRAILERS, "<", "trailers.pl"); $trailers = <TRAILERS>; }

my $value = $parse->value();
print $headers, $value, "\n", $trailers;

