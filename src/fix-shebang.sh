#!/bin/sh
GOSH="$1"
SCRIPT="$2"
sed "1s@#!/usr/bin/env gosh@#!$GOSH@" $SCRIPT.in > $SCRIPT.tmp && \
mv $SCRIPT.tmp $SCRIPT && \
chmod +x $SCRIPT

