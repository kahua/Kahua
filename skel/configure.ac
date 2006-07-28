dnl
dnl Configuring %%_PROJECT_NAME_%%
dnl  process this file with "autoconf" to generate 'configure'.
dnl $Id: configure.ac,v 1.1.2.3 2006/07/27 11:29:38 cut-sea Exp $
AC_PREREQ(2.59)
AC_INIT(%%_PROJECT_NAME_%%, 0.0.0, nobsun@kahua.org)

dnl Check for programs.  Set up PATH according to prefix.
if test "$prefix" != NONE; then
  tpath=$prefix/bin:$PATH
else
  tpath=$PATH
fi

AC_PATH_PROGS([GOSH], gosh)
AC_PATH_PROGS([KAHUA_INSTALL], kahua-install, [], $tpath)

dnl Check for other program
AC_PROG_INSTALL

if test X$GOSH = X; then 
  AC_MSG_ERROR([Cannot find Gauche installed.  Please install Gauche-0.8.7 or later, and make sure to set PATH properly.])
fi
if test X$KAHUA_INSTALL = X; then 
  AC_MSG_ERROR([Cannot find Kahua installed.  Please install Kahua-0.4.1 or later, and make sure to set PATH properly.])
fi

dnl Add more test

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile)
