dnl
dnl Configuring kahua-elua
dnl  process this file with "autoconf -I `gauche-config --ac`" to
dnl  generate 'configure'.
dnl $Id: configure.ac,v 1.4 2004/03/05 03:55:09 yasuyuki Exp $
AC_PREREQ(2.54)
AC_INIT(kahua, 0.2, shiro@acm.org)
AC_GAUCHE_INIT_EXT

dnl Sets the default value of INSTALL_TYPE macro.
AC_GAUCHE_INSTALL_TYPE(site)

dnl Get compiler parameters which Gauche has been compiled with.
AC_GAUCHE_CC
AC_GAUCHE_FLAGS

dnl Check for other programs.
AC_PROG_INSTALL

dnl Sets some additional installation destination
KAHUA_CGIDIR=/usr/lib/cgi-bin
AC_ARG_WITH(cgidir,
  AC_HELP_STRING([--with-cgidir=PATH],
                 [Specifies the directory where Kahua cgi scripts should be
installed.  If disabled (--with-cgidir=no or --without-cgidir), cgi scripts
are not installed automatically.  Default: /usr/lib/cgi-bin/]),
  [
    case "$with_cgidir" in
      yes|no) KAHUA_CGIDIR= ;;
      *)      KAHUA_CGIDIR=$with_cgidir ;;
    esac
  ])
AC_SUBST(KAHUA_CGIDIR)

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
          plugins/Makefile)
