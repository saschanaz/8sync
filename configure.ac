AC_INIT([8sync], [0.1.0], [cwebber@dustycloud.org])

PKG_CHECK_MODULES([GUILE], [guile-2.0])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

m4_ifdef([GUILE_PROGS],
         [],
         [AC_ERROR(could not locate guile aclocal macros)])

GUILE_PROGS

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_OUTPUT
