#!/bin/sh
GOSH="$1"
LIB="$2"
SOCKBASE="$3"
LOGPATH="$4"
SCRIPT="$5"
sed "1s@#!/usr/bin/env gosh@#!$GOSH@" $SCRIPT.in > $SCRIPT.tmp && \
mv $SCRIPT.tmp $SCRIPT.pre0 && \
sed "s@##libdir##@$LIB@" $SCRIPT.pre0 > $SCRIPT.pre1 && \
sed "s@##KAHUA_SOCKET_BASE##@$SOCKBASE@" $SCRIPT.pre1 > $SCRIPT.pre2 && \
sed "s@##KAHUA_CGI_LOGPATH##@$LOGPATH@" $SCRIPT.pre2 > $SCRIPT.tmp && \
mv -f $SCRIPT.tmp $SCRIPT && \
rm -f $SCRIPT.pre0 $SCRIPT.pre1 $SCRIPT.pre2 && \
chmod +x $SCRIPT


