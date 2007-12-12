set -x
perl -I../lib bootstrap.pl not_quite.marpa > bootstrapped.pl &&
perl -I../lib bootstrapped.pl not_quite.marpa > compiled1.pl &&
perl -I../lib compiled1.pl not_quite.marpa > compiled2.pl &&
diff compiled1.pl compiled2.pl
