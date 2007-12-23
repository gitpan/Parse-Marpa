require 5.009005;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Parse::Marpa',
    AUTHOR              => 'Jeffrey Kegler',
    VERSION_FROM        => 'lib/Parse/Marpa.pm',
    ABSTRACT_FROM       => 'lib/Parse/Marpa.pm',
    PL_FILES => {
        "lib/Parse/Marpa/Source.pm.PL" => "lib/Parse/Marpa/Source.pm"
    },
    PM => {
	'lib/Parse/Marpa/Raw_Source.pm'
	    => '$(INST_LIB)/Parse/Marpa/Raw_Source.pm',
	'lib/Parse/Marpa/LANGUAGE.pod'
	    => '$(INST_LIB)/Parse/Marpa/LANGUAGE.pod',
	'lib/Parse/Marpa/ALGORITHM.pod'
	    => '$(INST_LIB)/Parse/Marpa/ALGORITHM.pod',
	'lib/Parse/Marpa/preamble_Source.pm'
	    => '$(INST_LIB)/Parse/Marpa/preamble_Source.pm',
	'lib/Parse/Marpa/Lex.pm'
	    => '$(INST_LIB)/Parse/Marpa/Lex.pm',
	'lib/Parse/Marpa.pm'
	    => '$(INST_LIB)/Parse/Marpa.pm',
	'lib/Parse/Marpa/trailer_Raw_Source.pm'
	    => '$(INST_LIB)/Parse/Marpa/trailer_Raw_Source.pm',
	'lib/Parse/Marpa/Source.pm'
	    => '$(INST_LIB)/Parse/Marpa/Source.pm',
	'lib/Parse/Marpa/Source.pm.PL'
	    => '$(INST_LIB)/Parse/Marpa/Source.pm.PL',
	'lib/Parse/Marpa/WHY_CALL_IT_MARPA.pod'
	    => '$(INST_LIB)/Parse/Marpa/WHY_CALL_IT_MARPA.pod',
	'lib/Parse/Marpa/header_Raw_Source.pm'
	    => '$(INST_LIB)/Parse/Marpa/header_Raw_Source.pm',
    },
    EXE_FILES => [
                           'bootstrap/test.sh',
    ],
    PREREQ_PM => {
        'Test::More' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
