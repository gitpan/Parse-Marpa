
.PHONY: all install install_raw install_compiled

all: test source_compiled.pl

source_compiled.pl: source_raw_dump.pl raw_to_compiled.pl
	perl -I../lib ./raw_to_compiled.pl <source_raw_dump.pl >$@
	echo "1;" >>$@

bootcopy2.pl: bootcopy1.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootcopy1.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

bootcopy1.pl: bootcopy0.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootcopy0.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

bootcopy0.pl: bootstrap.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootstrap.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

source_raw.pl: bootcopy2.pl self.marpa 
	perl -I../lib bootcopy2.pl self.marpa /dev/null /dev/null > $@

make_source_raw_dump.pl: bootcopy2.pl self.marpa bootstrap_header.pl raw_dump_trailer.pl
	perl -I../lib bootcopy2.pl self.marpa raw_dump_header.pl raw_dump_trailer.pl > $@

source_raw_dump.pl: make_source_raw_dump.pl
	perl -I../lib make_source_raw_dump.pl > $@

test: bootcopy1.pl bootcopy2.pl
	diff bootcopy1.pl bootcopy2.pl

install: install_compiled install_raw

install_compiled: ../lib/Parse/Marpa/Source.pm

install_raw: ../lib/Parse/Marpa/Raw_Source.pm

../lib/Parse/Marpa/Source.pm: source_compiled.pl ../lib/Parse/Marpa/preamble_Source.pm
	cat ../lib/Parse/Marpa/preamble_Source.pm source_compiled.pl > \
	    ../lib/Parse/Marpa/Source.pm

../lib/Parse/Marpa/Raw_Source.pm: source_raw.pl ../lib/Parse/Marpa/header_Raw_Source.pm \
    ../lib/Parse/Marpa/trailer_Raw_Source.pm
	cat ../lib/Parse/Marpa/header_Raw_Source.pm source_raw.pl ../lib/Parse/Marpa/trailer_Raw_Source.pm \
	    > ../lib/Parse/Marpa/Raw_Source.pm
