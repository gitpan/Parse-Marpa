use 5.009005;
use strict;
use warnings;
use Test::More tests => 5;
use Fatal qw(open close chdir);

BEGIN {
	use_ok( 'Parse::Marpa' );
}

# program needs to be run either by Makefile or from t directory
my $example_dir = $0 =~ m{t/} ? "example" : "../example";
chdir($example_dir);

my $not_quite = "not_quite.marpa";

my $bootstrap;
open my $oldout, ">&STDOUT";
close STDOUT;
open STDOUT, ">", \$bootstrap;
{
    local(@ARGV)=($not_quite);
    do "bootstrap.pl" or die($!);
    die($@) if $@;
}
open STDOUT, ">&", $oldout;
ok($bootstrap, "bootstrapped version");

undef &canonical_symbol_name;
undef &canonical_version;
undef &gen_symbol_from_regex;
undef &usage;
undef &locator;

my $compile1;
close STDOUT;
open STDOUT, ">", \$compile1;
{
    local(@ARGV)=($not_quite);
    eval $bootstrap;
    die($@) if $@;
}
open STDOUT, ">&", $oldout;
ok($compile1, "first compiled version");

undef &canonical_symbol_name;
undef &canonical_version;
undef &gen_symbol_from_regex;
undef &usage;
undef &locator;

my $compile2;
close STDOUT;
open STDOUT, ">", \$compile2;
{
    local(@ARGV)=($not_quite);
    eval $compile1;
    die($@) if $@;
}
open STDOUT, ">&", $oldout;
ok($compile2, "second compiled version");
is($compile1, $compile2, "compiled versions are identical");
