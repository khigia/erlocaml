TEST_SOURCES=${wildcard test/*Test.ml}
TEST_BYTE   =${TEST_SOURCES:.ml=.byte}
TEST_SHELL  =${wildcard test/*.sh}

EX_SOURCES=${wildcard ex/*.ml}
EX_BYTE   =${EX_SOURCES:.ml=.byte}

OCAMLBUILD=ocamlbuild -classic-display


.PHONY: all
all: generated
	$(OCAMLBUILD) ocamerl.cma ${EX_BYTE} ${TEST_BYTE}

.PHONY: generated
generated: test/dataSet.ml

test/dataSet.ml: beam/datagen.beam
	erl -noshell -pa beam -s datagen doit -s init stop > test/dataSet.ml

beam/datagen.beam: test/datagen.erl
	erlc -o beam test/datagen.erl

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	@rm -vf test/dataSet.ml
	@rm -vf beam/*.beam

.PHONY: test
test: all utest
	for i in ${TEST_SHELL} ; do $$i ; done

.PHONY: utest
utest: all
	for i in ${TEST_BYTE} ; do ./`basename $$i` ; done
