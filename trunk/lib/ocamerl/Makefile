TEST_SOURCES=${wildcard test/*Test.ml}
TEST_BYTE   =${TEST_SOURCES:.ml=.byte}
TEST_SHELL  =${wildcard test/*.sh}

EX_SOURCES=${wildcard ex/*.ml}
EX_BYTE   =${EX_SOURCES:.ml=.byte}

OCAMLBUILD=ocamlbuild -classic-display

TARGETS=\
    ocamerl.cma \
    ocamerl/ocamerl.docdir/index.html \
    ${EX_BYTE}

TEST_TARGETS=\
	$(TARGETS) \
    $(TEST_BYTE)


.PHONY: main
main: main_build test

.PHONY: main_build
main_build:
	$(OCAMLBUILD) $(TARGETS)


.PHONY: dev
dev: dev_build utest test

.PHONY: dev_build
dev_build: generated
	$(OCAMLBUILD) $(TEST_TARGETS)


.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	@rm -vf test/dataSet.ml
	@rm -vf beam/*.beam


.PHONY: test
# no rule dependency ... rule used as a function
test:
	@for i in ${TEST_SHELL} ; do echo -n "Running '$$i': " ; $$i > /dev/null ; r=$$? && echo "$$r" ; done

.PHONY: utest
# no rule dependency ... rule used as a function
utest:
	@for i in ${TEST_BYTE} ; do ./`basename $$i` ; done


.PHONY: generated
generated: test/dataSet.ml

test/dataSet.ml: beam/datagen.beam
	erl -noshell -pa beam -s datagen doit -s init stop > test/dataSet.ml

beam/datagen.beam: test/datagen.erl
	mkdir -p beam
	erlc -o beam test/datagen.erl

