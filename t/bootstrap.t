use 5.009005;
use strict;
use warnings;
use Test::More tests => 5;
use Fatal qw(open close chdir);
use Carp;

# probably won't work on windows

BEGIN {
	use_ok( 'Parse::Marpa' );
}

# program needs to be run either by Makefile or from t directory
my $bootstrap_dir = $0 =~ m{t/} ? "bootstrap" : "../bootstrap";
chdir($bootstrap_dir);

my $exit_code;

$exit_code = system("touch not_quite.marpa");
is($exit_code, 0, "touch not_quite.marpa");

$exit_code = system("make bootcopy1.pl");
is($exit_code, 0, "make bootcopy1.pl");

$exit_code = system("make bootcopy2.pl");
is($exit_code, 0, "make bootcopy2.pl");

$exit_code = system("cmp bootcopy1.pl bootcopy2.pl");
is($exit_code, 0, "bootstraped copies identical");
