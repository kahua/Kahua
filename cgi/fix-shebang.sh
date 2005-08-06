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
sed "s@##KAHUA_CGI_LOGPATH##@$LOGPATH@" $SCRIPT.pre2 > $SCRIPT.pre3 && \
cp $SCRIPT.pre3 $SCRIPT.pre3.fcgi
# original CGI
sed "s@##CGIBEGIN##@@" $SCRIPT.pre3 > $SCRIPT.pre4 && \
sed "s@##CGIEND##@@"   $SCRIPT.pre4 > $SCRIPT.pre5 && \
sed "s@##ADDITIONAL_LIBS##@@" $SCRIPT.pre5 > $SCRIPT.tmp && \
mv -f $SCRIPT.tmp $SCRIPT && \
chmod +x $SCRIPT && \
# fastcgi
sed "s@##CGIBEGIN##@(with-fastcgi (lambda ()@" $SCRIPT.pre3.fcgi > $SCRIPT.pre4.fcgi && \
sed "s@##CGIEND##@))@" $SCRIPT.pre4.fcgi > $SCRIPT.pre5.fcgi  && \
sed "s@##ADDITIONAL_LIBS##@(use www.fastcgi)@" $SCRIPT.pre5.fcgi > $SCRIPT.tmp.fcgi && \
mv -f $SCRIPT.tmp.fcgi $SCRIPT.fcgi && \
rm -f $SCRIPT.pre0 $SCRIPT.pre1 $SCRIPT.pre2 $SCRIPT.pre3 $SCRIPT.pre4 $SCRIPT.pre5 && \
rm -f $SCRIPT.pre3.fcgi $SCRIPT.pre4.fcgi $SCRIPT.pre5.fcgi && \
chmod +x $SCRIPT $SCRIPT.fcgi



