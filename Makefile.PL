use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Parse::Marpa',
    AUTHOR              => 'Jeffrey Kegler',
    VERSION_FROM        => 'lib/Parse/Marpa.pm',
    ABSTRACT_FROM       => 'lib/Parse/Marpa.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
