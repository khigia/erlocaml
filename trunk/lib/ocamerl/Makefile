TEST_SOURCES=${wildcard test/*Test.ml}
TEST_BYTE   =${TEST_SOURCES:.ml=.byte}
TEST_SHELL  =${wildcard test/*.sh}

EX_SOURCES=${wildcard ex/*.ml}
EX_BYTE   =${EX_SOURCES:.ml=.byte}

OCAMLBUILD=ocamlbuild -classic-display


.PHONY: all
all:
	$(OCAMLBUILD) ocamerl.cma ${EX_BYTE} ${TEST_BYTE}

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean

.PHONY: test
test: all utest
	for i in ${TEST_SHELL} ; do $$i ; done

.PHONY: utest
utest: all
	for i in ${TEST_BYTE} ; do ./`basename $$i` ; done
