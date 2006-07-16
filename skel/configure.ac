dnl
dnl Configuring hello
dnl  process this file with "autoconf" to generate 'configure'.
dnl $Id: configure.ac,v 1.3 2005/09/21 11:10:37 shibata Exp $
AC_PREREQ(2.59)
AC_INIT(hello, 0.0.3, nobsun@kahua.org)

dnl Check for programs.  Set up PATH according to prefix.
if test "$prefix" != NONE; then
  tpath=$prefix/bin/kahua:$PATH
else
  tpath=$PATH
fi

AC_PATH_PROGS([GOSH], gosh)
AC_PATH_PROGS([KAHUA_INSTALL], kahua-install, [], $tpath)

dnl Check for other program
AC_PROG_INSTALL

if test X$GOSH = X; then 
  AC_MSG_ERROR([Cannot find Gauche installed.  Please install Gauche-0.8.4 or later, and make sure to set PATH properly.])
fi
if test X$KAHUA_INSTALL = X; then 
  AC_MSG_ERROR([Cannot find Kahua installed.  Please install Kahua-0.3.2 or later, and make sure to set PATH properly.])
fi

dnl Add more test

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile)
