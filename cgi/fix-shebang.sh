#!/bin/sh
GOSH="$1"
LIB="$2"
SOCKBASE="$3"
LOGPATH="$4"
FASTCGIP="$5"
SCRIPT_IN="$6"
SCRIPT_OUT="$7"

sed "1s@#!/usr/bin/env gosh@#!$GOSH@" $SCRIPT_IN > $SCRIPT.tmp && \
mv $SCRIPT.tmp $SCRIPT.pre0 && \
sed "s@##libdir##@$LIB@" $SCRIPT.pre0 > $SCRIPT.pre1 && \
sed "s@##KAHUA_SOCKET_BASE##@$SOCKBASE@" $SCRIPT.pre1 > $SCRIPT.pre2 && \
sed "s@##KAHUA_CGI_LOGPATH##@$LOGPATH@" $SCRIPT.pre2 > $SCRIPT.pre3 && \

case $FASTCGIP in
    yes|y)
	sed "s@##CGIBEGIN##@(with-fastcgi (lambda ()@" $SCRIPT.pre3 > $SCRIPT.pre4
	sed "s@##CGIEND##@))@" $SCRIPT.pre4 > $SCRIPT.pre5
	sed "s@##ADDITIONAL_LIBS##@(use www.fastcgi)@" $SCRIPT.pre5 > $SCRIPT.tmp
	;;
    *)
	sed "s@##CGIBEGIN##@@" $SCRIPT.pre3 > $SCRIPT.pre4
	sed "s@##CGIEND##@@"   $SCRIPT.pre4 > $SCRIPT.pre5
	sed "s@##ADDITIONAL_LIBS##@@" $SCRIPT.pre5 > $SCRIPT.tmp
	;;
esac && \

mv -f $SCRIPT.tmp $SCRIPT_OUT && \
chmod +x $SCRIPT_OUT && \

rm -f $SCRIPT.pre0 $SCRIPT.pre1 $SCRIPT.pre2 $SCRIPT.pre3 $SCRIPT.pre4 $SCRIPT.pre5



