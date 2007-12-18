$new_start_symbol //= "(undefined start symbol)";
$new_semantics //= "not defined";
$new_version //= -1;

croak("Version requested is ", $new_version, "\nVersion must match ", $Parse::Marpa::VERSION, " exactly.")
   unless $new_version == $Parse::Marpa::VERSION;

croak("Semantics are ", $new_semantics, "\nThe only semantics currently available are perl5.")
   unless $new_semantics eq "perl5";

my $g = new Parse::Marpa(
    start => $new_start_symbol,
    rules => $new_rules,
    terminals => $new_terminals,
    default_lex_prefix => $new_default_lex_prefix,
    default_null_value => $new_default_null_value,
    default_action => $new_default_action,
    warnings => 1,
);

print $g->compile();
