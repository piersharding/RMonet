#!/bin/sh

R CMD INSTALL --preclean .
R CMD INSTALL --clean .

rm src/RMonet.i*
rm src/RMonet.o
rm src/RMonet.so
rm rfc*.trc
rm *.Rout
rm RMonet_*.tar.gz
rm -rf autom4te.cache

