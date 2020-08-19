#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for the Memtrace profiling library
LIBNAME=memtrace

CAMLOBJS=\
  buf.cmo location_codec.cmo backtrace_codec.cmo \
  trace.cmo memprof_tracer.cmo memtrace.cmo

# Memtrace depends on Unix and Systhreads
EXTRACAMLFLAGS=-w -40 -I $(ROOTDIR)/otherlibs/systhreads -I $(ROOTDIR)/otherlibs/unix

include ../Makefile.otherlibs.common

.PHONY: depend
depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml >> .depend
endif

include .depend
