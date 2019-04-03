## Process this file with automake to produce Makefile.in

include meta/automake/mmux-chicken-rules.make

## --------------------------------------------------------------------

nodist_pkgconfig_DATA	= meta/scripts/mmux-chicken-checks.pc


#### documentation

info_TEXINFOS		= doc/mmux-chicken-checks.texi
doc_mmux_chicken_checks_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi


#### library build targets

# All the source files of the extension library.
#
MMUX_CHECKS_MODULES		= \
	lib/mmux.checks.core.scm		\
	lib/mmux.checks.compat.scm		\
	lib/mmux.checks.version.scm		\
	lib/mmux.checks.scm

# All the object files that must be linked in the extension library.
#
MMUX_CHECKS_RUNTIME_OBJECTS		= \
	lib/mmux.checks.core.$(OBJEXT)		\
	lib/mmux.checks.compat.$(OBJEXT)	\
	lib/mmux.checks.version.$(OBJEXT)	\
	lib/mmux.checks.$(OBJEXT)

# All the source files of all the modules' import libraries.
#
MMUX_CHECKS_IMPORT_SOURCES	= \
	lib/mmux.checks.core.import.scm		\
	lib/mmux.checks.compat.import.scm	\
	lib/mmux.checks.version.import.scm	\
	lib/mmux.checks.import.scm

# The module's import libraries needed to compile main module's object
# file.  It does not include the main import library:
#
#   lib/mmux.checks.import.$(MMUX_SOEXT)
#
MMUX_CHECKS_IMPORT_DEPS	= \
	lib/mmux.checks.core.import.$(MMUX_SOEXT)	\
	lib/mmux.checks.compat.import.$(MMUX_SOEXT)	\
	lib/mmux.checks.version.import.$(MMUX_SOEXT)

# The import  libraries needed  to compile new  code using  the shared
# library "(mmux checks)".  We need to build and install all of them.
#
MMUX_CHECKS_IMPORT_SHARED_LIBS	= \
	lib/mmux.checks.core.import.$(MMUX_SOEXT)	\
	lib/mmux.checks.compat.import.$(MMUX_SOEXT)	\
	lib/mmux.checks.version.import.$(MMUX_SOEXT)	\
	lib/mmux.checks.import.$(MMUX_SOEXT)

# This is the "(mmux checks)" shared library file pathname.
#
MMUX_CHECKS_RUNTIME_SHARED_LIBS	= lib/mmux.checks.$(MMUX_SOEXT)

## --------------------------------------------------------------------

EXTRA_DIST	+= $(MMUX_CHECKS_MODULES)

CLEANFILES	+= \
	$(MMUX_CHECKS_RUNTIME_SHARED_LIBS)	\
	$(MMUX_CHECKS_RUNTIME_OBJECTS)		\
	$(MMUX_CHECKS_IMPORT_SHARED_LIBS)	\
	$(MMUX_CHECKS_IMPORT_SOURCES)

## --------------------------------------------------------------------

lib/mmux.checks.version.$(OBJEXT): lib/mmux.checks.version.scm $(MMUX_OBJECTS_DEPS)
	$(MMUX_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<) -prologue lib/config.scm
	@$(MV) mmux.checks.version.import.scm lib/mmux.checks.version.import.scm

lib/mmux.checks.version.import.scm: lib/mmux.checks.version.$(OBJEXT)

lib/mmux.checks.version.import.$(MMUX_SOEXT): lib/mmux.checks.version.import.scm
	$(MMUX_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmux.checks.compat.$(OBJEXT): lib/mmux.checks.compat.scm $(MMUX_OBJECTS_DEPS)
	$(MMUX_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmux.checks.compat.import.scm lib/mmux.checks.compat.import.scm

lib/mmux.checks.compat.import.scm: lib/mmux.checks.compat.$(OBJEXT)

lib/mmux.checks.compat.import.$(MMUX_SOEXT): lib/mmux.checks.compat.import.scm
	$(MMUX_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmux.checks.core.$(OBJEXT): lib/mmux.checks.core.scm lib/mmux.checks.compat.import.$(MMUX_SOEXT) $(MMUX_OBJECTS_DEPS)
	$(MMUX_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmux.checks.core.import.scm lib/mmux.checks.core.import.scm

lib/mmux.checks.core.import.scm: lib/mmux.checks.core.$(OBJEXT)

lib/mmux.checks.core.import.$(MMUX_SOEXT): lib/mmux.checks.core.import.scm
	$(MMUX_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmux.checks.$(OBJEXT): lib/mmux.checks.scm $(MMUX_CHECKS_IMPORT_DEPS) $(MMUX_OBJECTS_DEPS)
	$(MMUX_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmux.checks.import.scm lib/mmux.checks.import.scm

lib/mmux.checks.import.scm: lib/mmux.checks.$(OBJEXT)

lib/mmux.checks.import.$(MMUX_SOEXT): lib/mmux.checks.import.scm
	$(MMUX_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmux.checks.$(MMUX_SOEXT): $(MMUX_CHECKS_RUNTIME_OBJECTS)
	$(MMUX_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMUX_ALL_IMPORT_SHARED_LIBS	= \
	$(MMUX_CHECKS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMUX_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMUX_CHECKS_RUNTIME_SHARED_LIBS)

# These variables are parsed by GNU Automake to build the main library
# targets and install them under the directory $(MMUX_CHICKEN_LIBDIR).
#
# We are shamelessly cheating by  installing them using GNU Automake's
# primary  suffix  "_SCRIPTS"; these  are  not  scripts!  Scripts  are
# installed with the permissions 0755, which, for some reason, is what
# we want  for shared libraries.
#
# We  could use  the  primary suffix  "_DATA", but  we  would get  the
# permissions 0644.
#
sharedlibdir		= $(MMUX_CHICKEN_LIBDIR)
sharedlib_SCRIPTS	= $(MMUX_ALL_RUNTIME_SHARED_LIBS) $(MMUX_ALL_IMPORT_SHARED_LIBS)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

EXTRA_DIST		+= \
	tests/test-version.scm		\
	tests/test-demo.scm

MMUX_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)	\
	tests/test-demo.$(OBJEXT)

MMUX_CHECK_PROGS	= \
	tests/test-version.exe		\
	tests/test-demo.exe

CLEANFILES	+= $(MMUX_CHECK_PROGS) $(MMUX_CHECK_OBJECTS)
TESTS		=  $(MMUX_CHECK_PROGS)

MMUX_CHECK_DEPS	+= $(MMUX_ALL_RUNTIME_SHARED_LIBS) $(MMUX_ALL_IMPORT_SHARED_LIBS)

## --------------------------------------------------------------------

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMUX_CHECK_DEPS)
	$(MMUX_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMUX_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-demo.$(OBJEXT): tests/test-demo.scm $(MMUX_CHECK_DEPS)
	$(MMUX_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-demo.exe: tests/test-demo.$(OBJEXT)
	$(MMUX_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
