TEST_SOURCES=${wildcard test/*Test.ml}
TEST_BYTE   =${TEST_SOURCES:.ml=.byte}
TEST_SHELL  =${wildcard test/*.sh}

EX_ML_SOURCES=${wildcard ex/*.ml}
EX_BYTE   =${EX_ML_SOURCES:.ml=.byte}
EX_BEAM   =$(patsubst ex/%.erl,beam/%.beam,$(wildcard ex/*.erl))

OCAMLBUILD=ocamlbuild -classic-display

NAME=ocamerl
ARCHIVE=$(NAME).cma
XARCHIVE=$(NAME).cmxa

LIB_TARGETS=\
    $(ARCHIVE) \
    $(XARCHIVE) \
    ocamerl/ocamerl.docdir/index.html

TARGETS=\
	$(LIB_TARGETS) \
    $(EX_BYTE)

TEST_TARGETS=\
	$(LIB_TARGETS) \
    $(EX_BYTE) \
    $(TEST_BYTE)


.PHONY: main
main: main_build test

.PHONY: main_build
main_build: ex_build
	$(OCAMLBUILD) $(TARGETS)


.PHONY: dev
dev: dev_build utest test

.PHONY: dev_build
dev_build: ERLBUILDOPT = +debug_info -DEUNIT -Wall
dev_build: generated ex_build
	$(OCAMLBUILD) $(TEST_TARGETS)


.PHONY: ex_build
ex_build: ERLBUILDOPT = +debug_info -DEUNIT -Wall
ex_build: $(EX_BEAM)


.PHONY: install
install: uninstall main_build
	test ! -f _build/$(XARCHIVE) || extra="_build/$(XARCHIVE) _build/"`basename $(XARCHIVE) .cmxa`".a" ; \
	ocamlfind install $(NAME) META _build/$(NAME)/*.mli _build/$(NAME)/*.cmi _build/$(NAME).cmi _build/$(ARCHIVE) $$extra

.PHONY: uninstall
uninstall:
	ocamlfind remove $(NAME)


.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	@rm -vf test/dataSet.ml
	@rm -vf beam/*.beam


.PHONY: test
# no rule dependency ... rule used as a function
test:
	# start erlang first (ensure epmd is running)
	@echo "Run and stop a distributed erlang node to ensure epmd is running."
	erl -sname e1 -setcookie cookie -noshell -s init stop
	@for i in ${TEST_SHELL} ; do \
		echo -n "Running '$$i': " ; \
		$$i > /dev/null 2>&1 ; \
		r=$$? && echo "$$r" ; \
	done

.PHONY: utest
# no rule dependency ... rule used as a function
utest:
	@for i in ${TEST_BYTE} ; do ./`basename $$i` ; done


.PHONY: generated
generated: test/dataSet.ml

test/dataSet.ml: beam/datagen.beam
	erl -noshell -pa beam -s datagen doit -s init stop > test/dataSet.ml

beam/%.beam : test/%.erl
	mkdir -p beam
	erlc ${ERLBUILDOPT} -o beam $<

beam/%.beam : ex/%.erl
	mkdir -p beam
	erlc ${ERLBUILDOPT} -o beam $<


