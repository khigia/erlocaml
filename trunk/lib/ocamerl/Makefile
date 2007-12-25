.PHONY: all clean test

SUBDIR=ocaml erl

all:
	for sub in ${SUBDIR} ; do cd $$sub && make && cd .. ; done

clean:
	for sub in ${SUBDIR} ; do cd $$sub && make clean && cd .. ; done

test:
	cd test && make test && cd ..
	#for sub in ${SUBDIR} ; do cd $$sub && make test && cd .. ; done
