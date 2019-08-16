## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= doc/mmck-chicken-docs.style.css
CLEANFILES		+= doc/mmck-chicken-docs.css
AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css

info_TEXINFOS		= doc/mmck-checks.texi
doc_mmck_checks_TEXINFOS = \
	doc/macros.texi			\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/mmck-checks.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/mmck-checks.html/
	@: > doc/mmck-checks.html/$(am__dirstamp)

doc/mmck-checks.html/mmck-chicken-docs.css: doc/mmck-chicken-docs.style.css doc/mmck-checks.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/mmck-chicken-docs.style.css" "$(builddir)/doc/mmck-checks.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

html-local: doc/mmck-checks.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/mmck-checks.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/mmck-checks.html/mmck-chicken-docs.css "$(DESTDIR)$(htmldir)/mmck-checks.html/"


#### library build targets

# All the source files ".scm" of the extension library.
#
MMCK_CHECKS_MODULES		=

# All  the object  files ".o"  that must  be linked  in the  extension
# library.
#
MMCK_CHECKS_RUNTIME_OBJECTS	=

# All  the  source files  ".import.scm"  of  all the  modules'  import
# libraries.
#
MMCK_CHECKS_IMPORT_SOURCES	=

# The  module's  import  libraries ".import.$(MMCK_SOEXT)"  needed  to
# compile main  module's object  file.  It does  not include  the main
# import library:
#
#   lib/mmck.checks.import.$(MMCK_SOEXT)
#
MMCK_CHECKS_IMPORT_DEPS		=

# The import  libraries ".import.$(MMCK_SOEXT)" needed to  compile new
# code using the shared library "(mmck checks)".  We need to build and
# install all of them.
#
MMCK_CHECKS_IMPORT_SHARED_LIBS	=

# This is the "(mmck checks)" shared library file pathname.
#
MMCK_CHECKS_RUNTIME_SHARED_LIBS	= \
	lib/mmck.checks.$(MMCK_SOEXT)

## --------------------------------------------------------------------

# This is  a list of types  specification files, one for  each module.
# They are  *not* installed.   They are  concatenated to  generate the
# globl types file.
#
MMCK_CHECKS_TYPES_FILES_MODULES	=

# This is  a list of the  types specification files.  To  be installed
# along with shared libraries.
#
MMCK_CHECKS_TYPES_FILES_SHARED_LIBS	= \
	lib/mmck.checks.types

## --------------------------------------------------------------------

# Temporary  files  we need  for  parallel  builds, ".temp",  ".lock",
# ".stamp".
#
MMCK_CHECKS_TEMP_FILES		=


#### library (mmck checks): module version

MMCK_CHECKS_MODULES		+= lib/mmck.checks.version.scm
MMCK_CHECKS_RUNTIME_OBJECTS	+= lib/mmck.checks.version.$(OBJEXT)
MMCK_CHECKS_IMPORT_SOURCES	+= lib/mmck.checks.version.import.scm
MMCK_CHECKS_IMPORT_DEPS		+= lib/mmck.checks.version.import.$(MMCK_SOEXT)
MMCK_CHECKS_IMPORT_SHARED_LIBS	+= lib/mmck.checks.version.import.$(MMCK_SOEXT)
MMCK_CHECKS_TYPES_FILES_MODULES	+= lib/mmck.checks.version.types.scm
MMCK_CHECKS_TEMP_FILES		+= \
	lib/mmck.checks.version.temp		\
	lib/mmck.checks.version.lock		\
	lib/mmck.checks.version.stamp

# A compiler  invokation to build  an object file, also  generates the
# source  file for  the import  library; a  single rule  generates two
# targets.  We implement  the rules as suggested  in the documentation
# of GNU Automake, node "Multiple Outputs".
#
# It is ugly, I know.  But it  is supposed to support all the cases of
# parallel build and dry runs.  (Marco Maggi; May  4, 2019)
#
lib/mmck.checks.version.stamp: lib/mmck.checks.version.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.checks.version.temp
	@touch lib/mmck.checks.version.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.checks.version.$(OBJEXT) $(<) -prologue lib/config.scm \
		-emit-types-file lib/mmck.checks.version.types.scm
	@$(MV) mmck.checks.version.import.scm lib/mmck.checks.version.import.scm
	@mv -f lib/mmck.checks.version.temp $@

lib/mmck.checks.version.types.scm: lib/mmck.checks.version.stamp

lib/mmck.checks.version.import.scm lib/mmck.checks.version.$(OBJEXT): lib/mmck.checks.version.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.checks.version.lock lib/mmck.checks.version.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.checks.version.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.checks.version.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.checks.version.stamp; \
	    $$dry rmdir lib/mmck.checks.version.lock; \
	  else \
	    while test -d lib/mmck.checks.version.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.checks.version.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.checks.version.import.$(MMCK_SOEXT): lib/mmck.checks.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck checks): module compat

MMCK_CHECKS_MODULES		+= lib/mmck.checks.compat.scm
MMCK_CHECKS_RUNTIME_OBJECTS	+= lib/mmck.checks.compat.$(OBJEXT)
MMCK_CHECKS_IMPORT_SOURCES	+= lib/mmck.checks.compat.import.scm
MMCK_CHECKS_IMPORT_DEPS		+= lib/mmck.checks.compat.import.$(MMCK_SOEXT)
MMCK_CHECKS_IMPORT_SHARED_LIBS	+= lib/mmck.checks.compat.import.$(MMCK_SOEXT)
MMCK_CHECKS_TYPES_FILES_MODULES	+= lib/mmck.checks.compat.types.scm
MMCK_CHECKS_TEMP_FILES		+= \
	lib/mmck.checks.compat.temp		\
	lib/mmck.checks.compat.lock		\
	lib/mmck.checks.compat.stamp

lib/mmck.checks.compat.stamp: lib/mmck.checks.compat.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.checks.compat.temp
	@touch lib/mmck.checks.compat.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.checks.compat.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.checks.compat.types.scm
	@$(MV) mmck.checks.compat.import.scm lib/mmck.checks.compat.import.scm
	@mv -f lib/mmck.checks.compat.temp $@

lib/mmck.checks.compat.types.scm: lib/mmck.checks.compat.stamp

lib/mmck.checks.compat.import.scm lib/mmck.checks.compat.$(OBJEXT): lib/mmck.checks.compat.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.checks.compat.lock lib/mmck.checks.compat.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.checks.compat.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.checks.compat.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.checks.compat.stamp; \
	    $$dry rmdir lib/mmck.checks.compat.lock; \
	  else \
	    while test -d lib/mmck.checks.compat.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.checks.compat.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.checks.compat.import.$(MMCK_SOEXT): lib/mmck.checks.compat.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck checks): module core

MMCK_CHECKS_MODULES		+= lib/mmck.checks.core.scm
MMCK_CHECKS_RUNTIME_OBJECTS	+= lib/mmck.checks.core.$(OBJEXT)
MMCK_CHECKS_IMPORT_SOURCES	+= lib/mmck.checks.core.import.scm
MMCK_CHECKS_IMPORT_DEPS		+= lib/mmck.checks.core.import.$(MMCK_SOEXT)
MMCK_CHECKS_IMPORT_SHARED_LIBS	+= lib/mmck.checks.core.import.$(MMCK_SOEXT)
MMCK_CHECKS_TYPES_FILES_MODULES	+= lib/mmck.checks.core.types.scm
MMCK_CHECKS_TEMP_FILES		+= \
	lib/mmck.checks.core.temp	\
	lib/mmck.checks.core.lock	\
	lib/mmck.checks.core.stamp

MMCK_CHECKS_CORE_DEPS = \
	lib/mmck.checks.compat.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.checks.core.stamp: lib/mmck.checks.core.scm $(MMCK_CHECKS_CORE_DEPS)
	@rm -f lib/mmck.checks.core.temp
	@touch lib/mmck.checks.core.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.checks.core.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.checks.core.types.scm
	@$(MV) mmck.checks.core.import.scm lib/mmck.checks.core.import.scm
	@mv -f lib/mmck.checks.core.temp $@

lib/mmck.checks.core.types.scm: lib/mmck.checks.core.stamp

lib/mmck.checks.core.import.scm lib/mmck.checks.core.$(OBJEXT): lib/mmck.checks.core.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.checks.core.lock lib/mmck.checks.core.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.checks.core.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.checks.core.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.checks.core.stamp; \
	    $$dry rmdir lib/mmck.checks.core.lock; \
	  else \
	    while test -d lib/mmck.checks.core.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.checks.core.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.checks.core.import.$(MMCK_SOEXT): lib/mmck.checks.core.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck checks): main module

MMCK_CHECKS_MODULES		+= lib/mmck.checks.scm
MMCK_CHECKS_RUNTIME_OBJECTS	+= lib/mmck.checks.$(OBJEXT)
MMCK_CHECKS_IMPORT_SOURCES	+= lib/mmck.checks.import.scm
MMCK_CHECKS_IMPORT_SHARED_LIBS	+= lib/mmck.checks.import.$(MMCK_SOEXT)
MMCK_CHECKS_TEMP_FILES		+= \
	lib/mmck.checks.temp		\
	lib/mmck.checks.lock		\
	lib/mmck.checks.stamp

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.checks.stamp: lib/mmck.checks.scm $(MMCK_CHECKS_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.checks.temp
	@touch lib/mmck.checks.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.checks.$(OBJEXT) $(<)
	@$(MV) mmck.checks.import.scm lib/mmck.checks.import.scm
	@mv -f lib/mmck.checks.temp $@

lib/mmck.checks.import.scm lib/mmck.checks.$(OBJEXT): lib/mmck.checks.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.checks.lock lib/mmck.checks.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.checks.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.checks.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.checks.stamp; \
	    $$dry rmdir lib/mmck.checks.lock; \
	  else \
	    while test -d lib/mmck.checks.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.checks.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.checks.import.$(MMCK_SOEXT): lib/mmck.checks.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck checks): wrapping up

EXTRA_DIST	+= $(MMCK_CHECKS_MODULES)

CLEANFILES	+= \
	$(MMCK_CHECKS_RUNTIME_SHARED_LIBS)	\
	$(MMCK_CHECKS_RUNTIME_OBJECTS)		\
	$(MMCK_CHECKS_IMPORT_SHARED_LIBS)	\
	$(MMCK_CHECKS_IMPORT_SOURCES)		\
	$(MMCK_CHECKS_TYPES_FILES_MODULES)	\
	$(MMCK_CHECKS_TYPES_FILES_SHARED_LIBS)	\
	$(MMCK_CHECKS_TEMP_FILES)

lib/mmck.checks.$(MMCK_SOEXT): $(MMCK_CHECKS_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)

lib/mmck.checks.types: $(MMCK_CHECKS_TYPES_FILES_MODULES)
	cat $(MMCK_CHECKS_TYPES_FILES_MODULES) >$@


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_CHECKS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_CHECKS_RUNTIME_SHARED_LIBS)

# All the types specification files that we want to install.
#
MMCK_ALL_TYPES_FILES	= \
	$(MMCK_CHECKS_TYPES_FILES_SHARED_LIBS)

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
sharedlib_SCRIPTS	= \
	$(MMCK_ALL_RUNTIME_SHARED_LIBS)		\
	$(MMCK_ALL_IMPORT_SHARED_LIBS)		\
	$(MMCK_ALL_TYPES_FILES)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

# The list of object files.  We will store it in CLEANFILES.
#
MMCK_CHECK_OBJECTS	=

# The list of test programs.  We will set TESTS to it.
#
MMCK_CHECK_PROGS	=

# Dependencies common to all the test programs.
#
MMCK_CHECK_DEPS		+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-version.scm
MMCK_CHECK_OBJECTS	+= tests/test-version.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-version.exe

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-demo.scm
MMCK_CHECK_OBJECTS	+= tests/test-demo.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-demo.exe

tests/test-demo.$(OBJEXT): tests/test-demo.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-demo.exe: tests/test-demo.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### wrapping up tests

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

TESTS		= $(MMCK_CHECK_PROGS)

### end of file
