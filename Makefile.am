## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

info_TEXINFOS		= doc/mmck-checks.texi
doc_mmck_checks_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi


#### library build targets

# All the source files of the extension library.
#
MMCK_CHECKS_MODULES		= \
	lib/mmck.checks.core.scm		\
	lib/mmck.checks.compat.scm		\
	lib/mmck.checks.version.scm		\
	lib/mmck.checks.scm

# All the object files that must be linked in the extension library.
#
MMCK_CHECKS_RUNTIME_OBJECTS		= \
	lib/mmck.checks.core.$(OBJEXT)		\
	lib/mmck.checks.compat.$(OBJEXT)	\
	lib/mmck.checks.version.$(OBJEXT)	\
	lib/mmck.checks.$(OBJEXT)

# All the source files of all the modules' import libraries.
#
MMCK_CHECKS_IMPORT_SOURCES	= \
	lib/mmck.checks.core.import.scm		\
	lib/mmck.checks.compat.import.scm	\
	lib/mmck.checks.version.import.scm	\
	lib/mmck.checks.import.scm

# The module's import libraries needed to compile main module's object
# file.  It does not include the main import library:
#
#   lib/mmck.checks.import.$(MMCK_SOEXT)
#
MMCK_CHECKS_IMPORT_DEPS	= \
	lib/mmck.checks.core.import.$(MMCK_SOEXT)	\
	lib/mmck.checks.compat.import.$(MMCK_SOEXT)	\
	lib/mmck.checks.version.import.$(MMCK_SOEXT)

# The import  libraries needed  to compile new  code using  the shared
# library "(mmck checks)".  We need to build and install all of them.
#
MMCK_CHECKS_IMPORT_SHARED_LIBS	= \
	lib/mmck.checks.core.import.$(MMCK_SOEXT)	\
	lib/mmck.checks.compat.import.$(MMCK_SOEXT)	\
	lib/mmck.checks.version.import.$(MMCK_SOEXT)	\
	lib/mmck.checks.import.$(MMCK_SOEXT)

# This is the "(mmck checks)" shared library file pathname.
#
MMCK_CHECKS_RUNTIME_SHARED_LIBS	= lib/mmck.checks.$(MMCK_SOEXT)

## --------------------------------------------------------------------

EXTRA_DIST	+= $(MMCK_CHECKS_MODULES)

CLEANFILES	+= \
	$(MMCK_CHECKS_RUNTIME_SHARED_LIBS)	\
	$(MMCK_CHECKS_RUNTIME_OBJECTS)		\
	$(MMCK_CHECKS_IMPORT_SHARED_LIBS)	\
	$(MMCK_CHECKS_IMPORT_SOURCES)

## --------------------------------------------------------------------

lib/mmck.checks.version.$(OBJEXT): lib/mmck.checks.version.scm $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<) -prologue lib/config.scm
	@$(MV) mmck.checks.version.import.scm lib/mmck.checks.version.import.scm

lib/mmck.checks.version.import.scm: lib/mmck.checks.version.$(OBJEXT)

lib/mmck.checks.version.import.$(MMCK_SOEXT): lib/mmck.checks.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.checks.compat.$(OBJEXT): lib/mmck.checks.compat.scm $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.checks.compat.import.scm lib/mmck.checks.compat.import.scm

lib/mmck.checks.compat.import.scm: lib/mmck.checks.compat.$(OBJEXT)

lib/mmck.checks.compat.import.$(MMCK_SOEXT): lib/mmck.checks.compat.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.checks.core.$(OBJEXT): lib/mmck.checks.core.scm lib/mmck.checks.compat.import.$(MMCK_SOEXT) $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.checks.core.import.scm lib/mmck.checks.core.import.scm

lib/mmck.checks.core.import.scm: lib/mmck.checks.core.$(OBJEXT)

lib/mmck.checks.core.import.$(MMCK_SOEXT): lib/mmck.checks.core.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.checks.$(OBJEXT): lib/mmck.checks.scm $(MMCK_CHECKS_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) $(@) $(<)
	@$(MV) mmck.checks.import.scm lib/mmck.checks.import.scm

lib/mmck.checks.import.scm: lib/mmck.checks.$(OBJEXT)

lib/mmck.checks.import.$(MMCK_SOEXT): lib/mmck.checks.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

lib/mmck.checks.$(MMCK_SOEXT): $(MMCK_CHECKS_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_CHECKS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_CHECKS_RUNTIME_SHARED_LIBS)

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
sharedlib_SCRIPTS	= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_SHARED_LIBS)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

EXTRA_DIST		+= \
	tests/test-version.scm		\
	tests/test-demo.scm

MMCK_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)	\
	tests/test-demo.$(OBJEXT)

MMCK_CHECK_PROGS	= \
	tests/test-version.exe		\
	tests/test-demo.exe

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)
TESTS		=  $(MMCK_CHECK_PROGS)

MMCK_CHECK_DEPS	+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_SHARED_LIBS)

## --------------------------------------------------------------------

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-demo.$(OBJEXT): tests/test-demo.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-demo.exe: tests/test-demo.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
