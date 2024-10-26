# anonymizer            -*- makefile-gmake -*-
# GNUmakefile

DISPLAY = short
DUNE = opam exec -- dune $1 --display $(DISPLAY)

build all::
	$(call DUNE, build @@default)
.PHONY: build all

install: build
	$(call DUNE, install)
.PHONY: install

doc::
	$(call DUNE, build @doc)
.PHONY: doc

clean::
	$(call DUNE, clean)
.PHONY: clean

sandbox::
	opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes
PHONY: sandbox

deps::
	opam repository add dldc https://dldc.lib.uchicago.edu/opam
	opam install . --deps-only --yes
PHONY: deps

# Local Variables:
# mode: makefile-gmake
# End:
