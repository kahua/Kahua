dnl
dnl Configuring Kahua
dnl  process this file with "autoconf -I `gauche-config --ac`" to
dnl  generate 'configure'.
dnl $Id: configure.ac,v 1.15 2004/09/02 04:29:12 nobsun Exp $
AC_PREREQ(2.54)
AC_INIT(kahua, 0.2.8, shiro@acm.org)
AC_GAUCHE_INIT_EXT

dnl Sets the default value of INSTALL_TYPE macro.
AC_GAUCHE_INSTALL_TYPE(site)

dnl Get compiler parameters which Gauche has been compiled with.
AC_GAUCHE_CC
AC_GAUCHE_FLAGS

dnl Check for other programs.
AC_PROG_INSTALL

dnl Sets some additional installation destination
KAHUA_CGIDIR=$libexecdir/kahua
AC_ARG_WITH(cgidir,
  AC_HELP_STRING([--with-cgidir=PATH],
                 [Specifies the directory where Kahua cgi scripts should be
installed.  If disabled (--with-cgidir=no or --without-cgidir), cgi scripts
are not installed automatically.  Default: $libexecdir/kahua]),
  [
    case "$with_cgidir" in
      yes|no) KAHUA_CGIDIR= ;;
      *)      KAHUA_CGIDIR=$with_cgidir ;;
    esac
  ])
AC_SUBST(KAHUA_CGIDIR)

dnl Sets cgi log directory
KAHUA_CGILOGDIR=$localstatedir/kahua/cgilog
AC_ARG_WITH(cgilogdir,
  AC_HELP_STRING([--with-cgilogdir=PATH],
                 [Specifies the directory where Kahua cgi script log should be
output to.  If disabled (--with-cgilogdir=no or --without-cgilogdir), cgi script log directory is not specified automatically.  Default: $localstatedir/kahua/cgilog]),
  [
    case "$with_cgilogdir" in
      yes|no) KAHUA_CGILOGDIR= ;;
      *)      KAHUA_CGILOGDIR=$with_cgilogdir ;;
    esac
  ])
AC_SUBST(KAHUA_CGILOGDIR)

dnl Add more test

dnl Set LDFLAGS to generate shared library.
AC_GAUCHE_FIX_LIBS

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile 
          src/Makefile 
          cgi/Makefile 
          examples/Makefile
          emacs/Makefile
          plugins/Makefile
          test/Makefile)
