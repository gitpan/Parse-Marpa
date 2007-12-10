use 5.9.5;
use strict;
use warnings;
use Parse::Marpa;
use Carp;
use English;

sub canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-_\s]+/-/g;
    $symbol;
}

sub canonical_version {
    my $version = shift;
    my @version = split(/\./, $version);
    my $result = sprintf("%d.", (shift @version));
    for my $subversion (@version) {
       $result .= sprintf("%03d", $subversion);
    }
    $result;
}

my %regex;

# URL escaping
sub gen_symbol_from_regex {
    my $regex = shift;
    state $uniq_number;
    $uniq_number //= 0;
    given ($regex) {
	when (/^qr/) { $regex = substr($regex, 3, -1); }
	default { $regex = substr($regex, 1, -1); }
    }
    my $symbol = $regex{$regex};
    return $symbol if defined $symbol;
    $symbol = substr($regex, 0, 20);
    $symbol =~ s/%/%%/g;
    $symbol =~ s/([^[:alnum:]_-])/sprintf("%%%.2x", ord($1))/ge;
    $symbol .= sprintf(":k%x", $uniq_number++);
    $regex{$regex} = $symbol;
    ($symbol, 1);
}

my $terminals = [];
my $rules = [];
my $preamble = "";
my %strings = ();


sub usage {
   die("usage: $0 grammar-file\n");
}

usage() unless scalar @ARGV == 1;

my $grammar_file_name = shift @ARGV;

open(GRAMMAR, "<", $grammar_file_name) or die("Cannot open $grammar_file_name: $!");

