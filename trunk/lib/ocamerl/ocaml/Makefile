LIB_TARGET_NAME=oe
LIB_TARGET=${LIB_TARGET_NAME}.cma
LIB_SOURCES=\
	trace.ml\
	tools.ml\
	eterm.ml\
	fifo.ml\
	serv.ml\
	epmc.ml\
	enode.ml
LIB_OBJECTS=${LIB_SOURCES:.ml=.cmo}

EXE_SOURCES=$(wildcard ex/*.ml)
EXE_OBJECTS=${EXE_SOURCES:.ml=.cmo}
EXE_TARGETS=${EXE_SOURCES:.ml=}

OCAMLC_OPT=-pp 'camlp4o' -thread unix.cma threads.cma

.PHONY: all clean doc

all: .depends ${LIB_TARGET} ${EXE_TARGETS} otop

.depends: ${LIB_SOURCES} ${EXE_SOURCES}
	ocamldep -pp 'camlp4o' ${LIB_SOURCES} ${EXE_SOURCES} > .depends

-include .depends

${LIB_TARGET}: ${LIB_OBJECTS}
	ocamlc -a -o ${LIB_TARGET} ${LIB_OBJECTS}

${EXE_TARGETS}: ${LIB_TARGET}

otop:
	ocamlmktop -o otop ${OCAMLC_OPT} ${LIB_TARGET} -cclib -lthreads

%:%.ml
	ocamlc ${OCAMLC_OPT} ${LIB_TARGET} -o $* $< $>

%.cmi:%.mli
	ocamlc ${OCAMLC_OPT} -c $<

%.cmo:%.ml
	ocamlc ${OCAMLC_OPT} -c $<

doc:
	-mkdir -p html
	ocamldoc -html -d ./html ${LIB_SOURCES} ${EXE_SOURCES}

clean:
	-rm -f otop
	-rm -f ${LIB_OBJECTS:.cmo=.cmi} ${LIB_OBJECTS} ${LIB_TARGET}
	-rm -f ${EXE_OBJECTS:.cmo=.cmi} ${EXE_OBJECTS} ${EXE_TARGETS}
	-rm -f .depends
	-rm -rf html
