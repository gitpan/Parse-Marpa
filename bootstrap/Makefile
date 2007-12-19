
all: test source_compiled.pl

source_compiled.pl: source_raw.pl raw_to_compiled.pl
	perl -I../lib ./raw_to_compiled.pl <source_raw.pl >$@
	echo "1;" >>$@

bootcopy2.pl: bootcopy1.pl not_quite.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootcopy1.pl not_quite.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

bootcopy1.pl: bootcopy0.pl not_quite.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootcopy0.pl not_quite.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

bootcopy0.pl: bootstrap.pl not_quite.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootstrap.pl not_quite.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

make_source_raw.pl: bootcopy2.pl not_quite.marpa bootstrap_header.pl raw_dump_trailer.pl
	perl -I../lib bootcopy2.pl not_quite.marpa raw_dump_header.pl raw_dump_trailer.pl > $@

source_raw.pl: make_source_raw.pl
	perl -I../lib make_source_raw.pl > $@

test: bootcopy1.pl bootcopy2.pl
	diff bootcopy1.pl bootcopy2.pl

install: source_compiled.pl ../lib/Parse/Marpa/preamble_Source.pm
	cat ../lib/Parse/Marpa/preamble_Source.pm source_compiled.pl > \
	    ../lib/Parse/Marpa/Source.pm
