
.PHONY: all install install_raw install_stringified

all: test source_raw.pl

bootcopy1.pl: bootcopy0.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootcopy0.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

bootcopy0.pl: bootstrap.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootstrap.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

source_raw.pl: bootcopy1.pl self.marpa 
	perl -I../lib bootcopy1.pl self.marpa /dev/null /dev/null > $@

test: bootcopy0.pl bootcopy1.pl
	diff bootcopy0.pl bootcopy1.pl

install: test install_stringified install_raw

install_stringified: ../lib/Parse/Marpa/Source.pm

install_raw: ../lib/Parse/Marpa/Raw_Source.pm

../lib/Parse/Marpa/Source.pm: \
    ../lib/Parse/Marpa/Raw_Source.pm \
    ../lib/Parse/Marpa/Source.pm.PL \
    ../lib/Parse/Marpa/header_Raw_Source.pm
	(cd ..; perl -Ilib lib/Parse/Marpa/Source.pm.PL lib/Parse/Marpa/Source.pm )

../lib/Parse/Marpa/Raw_Source.pm: source_raw.pl ../lib/Parse/Marpa/header_Raw_Source.pm \
    ../lib/Parse/Marpa/trailer_Raw_Source.pm
	cat ../lib/Parse/Marpa/header_Raw_Source.pm source_raw.pl ../lib/Parse/Marpa/trailer_Raw_Source.pm \
	    > ../lib/Parse/Marpa/Raw_Source.pm
