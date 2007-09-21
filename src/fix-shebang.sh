#!/bin/sh
GOSH="$1"
LIB="$2"
EXE="$3"
SCRIPT="$4"
sed "1s@#!/usr/bin/env gosh@#!$GOSH@" $SCRIPT.in > $SCRIPT.tmp && \
mv $SCRIPT.tmp $SCRIPT && \
chmod +x $SCRIPT && \
echo '#!/bin/sh' > $SCRIPT.sh && \
echo "$GOSH -I$LIB $EXE/$SCRIPT.scm" ' $@' >> $SCRIPT.sh && \
chmod +x $SCRIPT.sh


