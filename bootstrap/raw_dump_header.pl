use 5.009_005;
use strict;
use warnings;
use Parse::Marpa;
use Carp;
use English;

my %regex;

my $new_terminals = [];
my $new_rules = [];
my $new_preamble = "";
my $new_start_symbol;
my $new_semantics;
my $new_version;
my $new_default_action;
my $new_default_null_value;
my $new_default_lex_prefix;
our %strings;

sub usage {
   die("usage: $0 grammar-file\n");
}

my $argc = @ARGV;
usage() unless $argc == 0;

