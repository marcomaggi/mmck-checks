# @configure_input@
#

AC_PREREQ([2.69])
MMUX_PKG_VERSIONS([0],[2],[0],[devel.1])
AC_INIT([MMCK Checks],[MMUX_PACKAGE_VERSION],
  [marco.maggi-ipsu@poste.it],
  [mmck-checks],[http://github.com/marcomaggi/mmck-checks])
MMUX_INIT
AC_REVISION([0.1])
AC_COPYRIGHT([Copyright (c) 2019 Marco Maggi <marco.maggi-ipsu@poste.it>

Permission is hereby granted, free  of charge, to any person obtaining
a  copy  of this  software  and  associated documentation  files  (the
``Software''), to deal in  the Software without restriction, including
without limitation  the rights to  use, copy, modify,  merge, publish,
distribute, sublicense,  and/or sell  copies of  the Software,  and to
permit persons to whom the Software  is furnished to do so, subject to
the following conditions:

The  above  copyright  notice  and this  permission  notice  shall  be
included in all copies or substantial portions of the Software.

THE  SOFTWARE IS  PROVIDED ``AS  IS'', WITHOUT  WARRANTY OF  ANY KIND,
EXPRESS OR  IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE  WARRANTIES OF
MERCHANTABILITY,    FITNESS    FOR    A   PARTICULAR    PURPOSE    AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING  FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
])
AC_CONFIG_SRCDIR([lib/])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.16 foreign subdir-objects])
AM_MAINTAINER_MODE

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P


#### basic system inspection

MMUX_CHECK_TARGET_OS
AX_IS_RELEASE([git-directory])

MMUX_CHICKEN_SCHEME

AC_CACHE_SAVE


#### external libraries

dnl AC_CACHE_SAVE


#### finish

AC_CONFIG_FILES([Makefile]
   [lib/config.scm])
MMUX_OUTPUT
AC_OUTPUT

### end of file
