#!perl -T

use Test::More skip_all => "I will, I promise, I promise!";
eval "use Test::Pod::Coverage 1.04";
plan skip_all => "Test::Pod::Coverage 1.04 required for testing POD coverage" if $@;

{
    all_pod_coverage_ok();
}
