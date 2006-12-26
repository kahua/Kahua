dnl
dnl Configuring Kahua Examples
dnl  process this file with "autoconf -I `gauche-config --ac`" to
dnl  generate 'configure'.
dnl
dnl $Id: configure.ac,v 1.3 2006/12/26 23:05:16 bizenn Exp $
dnl
AC_PREREQ(2.54)
AC_INIT(examples, 1.0, info@kahua.org)
AC_GAUCHE_INIT_EXT

dnl Sets the default value of INSTALL_TYPE macro.
AC_GAUCHE_INSTALL_TYPE(site)

dnl Get compiler parameters which Gauche has been compiled with.
AC_GAUCHE_CC
AC_GAUCHE_FLAGS

dnl Check for other programs.
AC_PROG_INSTALL
AC_CHECK_PROGS(MAKEINFO, makeinfo)
AC_CHECK_PROGS(GZIP_PROGRAM, gzip)

KAHUA_STATICDIR=$localstatedir/kahua/static
AC_ARG_WITH(staticdir,
  AC_HELP_STRING([--with-staticdir=PATH],
                 [Specifies the directory where Kahua static documents should be
installed.  Default: $KAHUA_STATICDIR]),
  [
    case "$with_staticdir" in
      yes|no) KAHUA_STATICDIR= ;;
      *)      KAHUA_STATICDIR=$with_staticdir ;;
    esac
  ])
AC_SUBST(KAHUA_STATICDIR)

KAHUA_USER_OPTION=
AC_ARG_WITH(kahua-user,
  AC_HELP_STRING([--with-kahua-user=USER],
                 [User name execute kahua servers. Default: none]),
  [
    case "$with_kahua_user" in
      yes|no)  KAHUA_USER_OPTION= ;;
      *)       KAHUA_USER_OPTION="-o $with_kahua_user" ;;
    esac
  ])
AC_SUBST(KAHUA_USER_OPTION)

KAHUA_GROUP_OPTION=
AC_ARG_WITH(kahua-group,
  AC_HELP_STRING([--with-kahua-group=GROUP],
                 [Group name execute kahua servers. Default: none]),
  [
    case "$with_kahua_group" in
      yes|no) KAHUA_GROUP_OPTION= ;;
      *)      KAHUA_GROUP_OPTION="-g $with_kahua_group" ;;
    esac
  ])
AC_SUBST(KAHUA_GROUP_OPTION)

dnl Set LDFLAGS to generate shared library.
AC_GAUCHE_FIX_LIBS

dnl Output
AC_OUTPUT(Makefile)
