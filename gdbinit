# gdbinit --
#
# Copy this  file in  the directory  from which we  run the  test programs,  which is
# usually  "$(top_builddir)/tests".  Run  gdb as  "gdb --args"  so that  command line
# arguments in the Makefile are handed to the program rather than to "gdb".
#
# From the commmand line of make:
#
#   $ make check GDB='gdb --args' TESTS=tests/test-demo.exe
#

directory ./lib ./tests
run

### end of file
