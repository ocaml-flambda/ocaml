#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                        Guillaume Bury, OCamlPro                        *
#*                                                                        *
#*   Copyright 2020 OCamlPro                                              *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The Makefile for ocamltest

ROOTDIR = ../..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

run := run_$(UNIX_OR_WIN32)

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

# ocamltest has two components: its core and the OCaml "plugin"
# which is actually built into the tool but clearly separated from its core

sources := \
  buf.mli buf.ml \
  location_codec.mli location_codec.ml \
  backtrace_codec.mli backtrace_codec.ml \
  trace.mli trace.ml \
  memprof_tracer.mli memprof_tracer.ml \
  memtrace.mli memtrace.ml \
  main.ml

# List of .ml files used for ocamldep and to get the list of modules

ml_files := \
  $(filter %.ml, \
    $(subst .ml.in,.ml,$(subst .mll,.ml,$(subst .mly,.ml,$(sources)))) \
  )

cmo_files := $(ml_files:.ml=.cmo)

cmx_files := $(ml_files:.ml=.cmx)

ocaml_objects := $(ml_files:.ml=.$(O))

# List of .mli files for ocamldep
mli_files := \
  $(filter %.mli,$(subst .mly,.mli,$(sources)))

cmi_files := $(mli_files:.mli=.cmi)

native_modules := $(cmx_files)

directories := $(addprefix $(ROOTDIR)/,stdlib compilerlibs driver \
                                       otherlibs/unix otherlibs/systhreads)

include_directories := $(addprefix -I , $(directories))

flags := -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-40-41-42-44-45-48 -warn-error A

ocamlc := $(BEST_OCAMLC) $(flags)

ocamlopt :=  $(BEST_OCAMLOPT) $(flags)

ocamldep := $(BEST_OCAMLDEP)
depflags := -slash
depincludes :=

.PHONY: all allopt opt.opt # all, allopt and opt.opt are synonyms
all: ocamlopt-memtrace.opt
allopt: all
opt.opt: allopt

ocamlopt-memtrace.opt: $(native_modules)
	$(ocamlopt_cmd) -o $@ -cclib -lunix -cclib -lthreadsnat \
	  unix.cmxa threads.cmxa ocamlcommon.cmxa ocamloptcomp.cmxa \
	  $^

%.cmx: %.ml
	$(ocamlopt) -c $<

%.cmi: %.mli
	$(ocamlc) -c $<

.PHONY: clean
clean:
	rm -rf ocamlopt-memtrace.opt
	rm -rf $(o_files) $(ocaml_objects)
	rm -rf $(cmi_files)
	rm -rf $(cmo_files)
	rm -rf $(cmx_files)

ifneq "$(TOOLCHAIN)" "msvc"
.PHONY: depend
depend: $(dependencies_generated_prereqs)
	$(ocamldep) $(depflags) $(depincludes) $(mli_files) $(ml_files) \
	  >> .depend
endif

-include .depend
