# anonymizer makefile
# (C) Matt Teichman, 2019

DESTDIR = ~
BINDIR = $(DESTDIR)/bin

.PHONY: all
all::
	dune build --display=short main.exe
	ln -f _build/default/main.exe bin/anonymizer
	ln -f _build/default/main.exe bin/decrypter

.PHONY: clean
clean::
	dune clean
	$(RM) bin/anonymizer
	$(RM) bin/decrypter
	$(RM) ./readme.html

install: all
	install _build/default/main.exe $(BINDIR)/anonymizer
	ln -f $(BINDIR)/anonymizer $(BINDIR)/decrypter
