#!/bin/sh
GOSH="$1"
LIB="$2"
SOCKBASE="$3"
SCRIPT="$4"
sed "1s@#!/usr/bin/env gosh@#!$GOSH@" $SCRIPT.in > $SCRIPT.tmp && \
mv $SCRIPT.tmp $SCRIPT.pre0 && \
sed "s@##libdir##@$LIB@" $SCRIPT.pre0 > $SCRIPT.pre1 && \
sed "s@##KAHUA_SOCKET_BASE##@$SOCKBASE@" $SCRIPT.pre1 > $SCRIPT.tmp
mv $SCRIPT.tmp $SCRIPT && \
rm -f $SCRIPT.pre0 $SCRIPT.pre1 && \
chmod +x $SCRIPT


