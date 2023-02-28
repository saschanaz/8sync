AC_INIT([8sync], [0.4.2], [cwebber@dustycloud.org])

PKG_CHECK_MODULES([GUILE], [guile-3.0])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

GUILE_PROGS

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_OUTPUT
