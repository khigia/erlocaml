.PHONY: all clean

SUBDIR=ocaml erl

all:
	for sub in ${SUBDIR} ; do cd $$sub && make && cd .. ; done

clean:
	for sub in ${SUBDIR} ; do cd $$sub && make clean && cd .. ; done
