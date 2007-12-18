$new_start_symbol //= "(undefined start symbol)";
$new_semantics //= "not defined";
$new_version //= -1;

my $g = new Parse::Marpa(
    start => $new_start_symbol,
    rules => $new_rules,
    terminals => $new_terminals,
    default_lex_prefix => $new_default_lex_prefix,
    default_action => $new_default_action,
    default_null_value => $new_default_null_value,
    warnings => 1,
    semantics => $new_semantics,
    version => $new_version,
    preamble => $new_preamble,
);

use Data::Dumper;
my $d = Data::Dumper->new( [$g], ["grammar"] );
$d->Purity(1);
$d->Indent(1);
print $d->Dump();
