
sub Parse::Marpa::Internal::raw_source_grammar {

    my $raw_source_grammar = new Parse::Marpa(
        start => $new_start_symbol,
        rules => $new_rules,
        terminals => $new_terminals,
        version => $new_version,
        warnings => 1,
    );  
        
    $raw_source_grammar->set(
        default_lex_prefix => $new_default_lex_prefix
    ) if defined $new_default_lex_prefix;

    $raw_source_grammar->set(
        default_action => $new_default_action
    ) if defined $new_default_action;

    $raw_source_grammar->set(
        default_null_value => $new_default_null_value
    ) if defined $new_default_null_value;

    $raw_source_grammar;

}
        
1;

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
