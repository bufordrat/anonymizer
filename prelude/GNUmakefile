# prelude
# GNUmakefile
# Keith Waclena <http://www.lib.uchicago.edu/keith/>

NAME = prelude

HOST = ocaml.lib.uchicago.edu
DOCHOST = jfcl.lib.uchicago.edu
DOCDIR = $(DOCHOST):$$HOME/web/software/$(NAME)

# extra ocamlbuild arguments
OCBEXTRAS =
# extra ocamldoc arguments
DOCFLAGS = -charset,utf-8,-colorize-code,-short-functors,-css-style,style.css,-inv-merge-ml-mli,-m,A

OS = $(shell uname -s)
FREEBSD = $(filter FreeBSD,$(OS))

TEST = test

OCAMLBUILD = ocamlbuild.native -use-ocamlfind -classic-display -j 8

.PHONY: all
all: cma cmxa

test.native: cmxa  $(NAME).mllib
	@fgrep -q '(*$$' $(NAME).ml || { echo $(NAME).ml does not look like qtest input && false; }
	@egrep -n '\(\*[TQ=]' $(NAME).ml /dev/null && { echo suspicious qtestish comments in $(NAME).ml && false; } || true
	@test ! -f $(TEST).ml || egrep -q '^#[0-9]+' $(TEST).ml || { echo $(TEST).ml does not look like qtest output && false; }
	find . -maxdepth 1 `cat $(NAME).mllib | tr -cs 'A-Za-z1-9_' '\012' | sed 's/$$/.ml -o /;s/^/-iname /'` -false \
	  | xargs qtest -o $(TEST).ml extract
	$(OCAMLBUILD) -cflags -warn-error,+26 -pkgs oUnit,qcheck,ctypes,ctypes.foreign,ctypes.stubs test.native

.PHONY: check
check: test.native
	./test.native $(and $(INSIDE_EMACS),--no-colors)

tests.txt: test.native
	./test.native -l --no-colors > tests.txt

.PHONY: cma cmxa
cma cmxa::
	$(OCAMLBUILD) $(OCBEXTRAS) -lflag -linkall $(NAME).$@

.PHONY: freebsd
freebsd: clean
	ssh $(HOST) gmake -C `pwd | sed "s;$$HOME;~;"` install

.PHONY: install
install: META all uninstall
	ocamlfind install $(NAME) META $(wildcard _build/*.a) _build/*.cm* $(wildcard _build/*.mli)
	@echo
	@echo if you are me, you might want to do:
	@echo make push

.PHONY: push
push::
	hg push ~/web/hg/$(NAME)

.PHONY: uninstall
uninstall:
	ocamlfind remove $(NAME)

## Sketch of doc rule for later
# comma := ,
# doc:	
# mkdir -p _build/$(NAME).docdir
# ln -nfs _build/$(NAME).docdir $(NAME).docdir
# ocamlfind ocamldoc $(subst $(comma), ,$(DOCFLAGS)) -html -package bytes,unix -predicates mt_posix,mt -d $(NAME).docdir -I _build pre.ml $(NAME).ml

.PHONY: doc
doc::
	$(OCAMLBUILD) $(if $(DOCFLAGS),-docflags $(DOCFLAGS)) $(NAME).docdir/index.html
	cp style.css $(NAME).docdir/
ifneq ($(DOCDIR),)
	rsync -a $(NAME).docdir/ style.css $(DOCDIR)
endif

TAGS::
	-ctags -Re --exclude='_build/*'

.PHONY: clean
clean::
	$(RM) qtest.targets.log $(TEST).ml tests.txt test.byte test.native
	$(OCAMLBUILD) -clean
#       kludge need in NFS-mounted directories (at least on FreeBSD)
ifneq ($(FREEBSD),)
	-rmdir _build
endif

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cma .cmxa .byte .native

.ml.cmo .mll.cmo .mly.cmo:
	$(OCAMLBUILD) $(OCBEXTRAS) $@

.mli.cmi:
	$(OCAMLBUILD) $(OCBEXTRAS) $@

.cmo.cma:
	$(OCAMLBUILD) $(OCBEXTRAS) -lflag -linkall $@

.ml.byte:
	$(OCAMLBUILD) $(OCBEXTRAS) $(if $(STATIC),-lflag -ccopt -lflag -static) $@

.ml.native:
	$(OCAMLBUILD) $(OCBEXTRAS) $(if $(STATIC),-lflag -ccopt -lflag -static) $@
