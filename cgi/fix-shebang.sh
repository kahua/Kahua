#!/bin/sh
GOSH="$1"
LIB="$2"
WORKDIR="$3"
SOCKBASE="$4"
LOGPATH="$5"
FASTCGIP="$6"
SCRIPT_IN="$7"
SCRIPT_OUT="$8"

sed "1s@#!/usr/bin/env gosh@#!$GOSH@" $SCRIPT_IN > $SCRIPT.tmp && \
mv $SCRIPT.tmp $SCRIPT.pre0 && \
sed -e "s@##libdir##@$LIB@" \
    -e "s@##KAHUA_SOCKET_BASE##@$SOCKBASE@" \
    -e "s@##localstatedir##@$WORKDIR@" \
    -e "s@##KAHUA_CGI_LOGPATH##@$LOGPATH@" $SCRIPT.pre0 > $SCRIPT.pre1 && \

case $FASTCGIP in
    yes|y)
	sed -e "s@##CGIBEGIN##@(with-fastcgi (lambda ()@" \
	    -e "s@##CGIEND##@))@" \
	    -e "s@##ADDITIONAL_LIBS##@(use www.fastcgi)@" $SCRIPT.pre1 > $SCRIPT.tmp
	;;
    *)
	sed -e "s@##CGIBEGIN##@@" \
	    -e "s@##CGIEND##@@" \
	    -e "s@##ADDITIONAL_LIBS##@@" $SCRIPT.pre1 > $SCRIPT.tmp
	;;
esac && \

mv -f $SCRIPT.tmp $SCRIPT_OUT && \
chmod +x $SCRIPT_OUT && \

rm -f $SCRIPT.pre0 $SCRIPT.pre1 $SCRIPT.pre2 $SCRIPT.pre3 $SCRIPT.pre4 $SCRIPT.pre5



