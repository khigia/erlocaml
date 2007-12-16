# Description: ad-hoc (Linux) Makefile for standard Erlang/OTP application
# Author: Ludovic Coquelle <ludovic@affle.co.uk>

# Configuration

CMD_ERL      = erl
CMD_ERLC     = erlc
CMD_DIALYZER = dialyzer
APPNAME      = $(shell pwd | xargs basename | sed -e 's/-.*$$//')

APP_MOD    = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
TEST_MOD   = $(patsubst test/src/%.erl,test/ebin/%.beam,$(wildcard test/src/*.erl))

# need to add some path to erlang modules?
ERL_DEP = \

ERLPATH = -pa ebin $(patsubst %,-pa %/ebin,$(ERL_DEP))
ERLINCL = $(patsubst %,-I%/..,$(ERL_DEP))
BUILDOPT_MAIN = $(ERLINCL)
BUILDOPT_PROD = $(BUILDOPT_MAIN) -smp
BUILDOPT_DEV  = $(BUILDOPT_MAIN) +debug_info -DEUNIT -DLOG_DEBUG -Wall
DOCOPT={todo, true}, {private, true}


# Main targets

.PHONY : build
build : BUILDOPT = ${BUILDOPT_PROD}
build : ${APP_MOD}

.PHONY : help
help:
	@echo "Main rules"
	@echo "  - make build: build application in production mode."
	@echo "  - make dev: run devbuild, checks and doc."
	@echo "Utilities"
	@echo "  - make clean: clean all generated files."
	@echo "  - make logs: print application log send to kernel logger."
	@echo "  - make prodtest: run clean, prodbuild and test."
	@echo "  - make devtest: run clean, devbuild and test."
	@echo "Sub-rules"
	@echo "  - make devbuild: build application in development mode."
	@echo "  - make test: run build, testbuild, run application and launch testing clients."
	@echo "  - make checks: regression xref dialyzer cover."
	@echo "  - make regression: run unit tests in code."
	@echo "  - make xref: run xref."
	@echo "  - make dialyzer: run dialyzer."
	@echo "  - make cover: run cover."
	@echo "  - make doc: generate the documentation from code."

.PHONY : doc
doc:
	@echo "====> Generate documentation <===="
	${CMD_ERL} \
        -noshell \
        ${ERLPATH} \
        -eval 'edoc:application(${APPNAME}, ".", [${DOCOPT}]).' \
        -s init stop

.PHONY : logs
logs:
	@echo "====> Extract logs <===="
	${CMD_ERL} \
        -noshell \
        ${ERLPATH} \
        -boot start_sasl \
        -eval "(rb:start([{report_dir, \"./log/\"}]) =:= ok) andalso rb:show()." \
        -s init stop

.PHONY : clean
clean :
	@echo "====> Clean <===="
	@rm -vf erl_crash.dump
	@rm -vf $(APP_MOD)
	@rm -vf $(TEST_MOD)
	@rm -vf log/*
	@touch doc/remove.me # ensure next command does not failed
	@ls -1 -I *.edoc -I *.txt -I figure doc \
        | sed -e 's#\(.*\)#doc/\1#' \
        | xargs rm -v


# Development targets

.PHONY : dev
dev: devbuild checks doc

.PHONY : devbuild
devbuild: BUILDOPT = ${BUILDOPT_DEV}
devbuild: ${APP_MOD}

.PHONY : checks
checks: regression xref dialyzer cover

.PHONY : regression
regression: devbuild
	@echo "====> Regression tests <===="
	@erl \
		-noshell \
		-kernel error_logger      "{file, \"log/utest-kernel.log\"}" \
		-sasl   sasl_error_logger "{file, \"log/utest-sasl.log\"}" \
		${ERLPATH} \
		-eval "All = filelib:fold_files(\
			\"ebin\",\
			\".*\.beam\",\
			true,\
			fun(F, Acc) -> \
				M = list_to_atom(filename:basename(F, \".beam\")),\
				[M|Acc]\
			end,\
			[]\
		), eunit:test(All, [{verbose,true}])." \
		-s init stop

.PHONY : xref
xref:
	@echo "====> xref <===="
	@${CMD_ERL} -noshell ${ERLPATH} -eval 'io:format("~p~n", [xref:d("ebin")]).' -s init stop

.PHONY : dialyzer
dialyzer:
	@echo "====> dialyzer <===="
	@ls -1 src/*.erl | xargs ${CMD_DIALYZER} ${ERLPATH} -I include --src -c

.PHONY : cover
cover:
	@echo "====> cover <===="
	@echo "... cover not used!"


# application testing

.PHONY : prodtest
prodtest: clean build test

.PHONY : devtest
devtest: clean devbuild test

.PHONY : test
test: devbuild testbuild
	@echo "====> Run tests through EUnit <===="
	@erl \
		-noshell \
		-kernel error_logger      "{file, \"log/test-kernel.log\"}" \
		-sasl   sasl_error_logger "{file, \"log/test-sasl.log\"}" \
		${ERLPATH} -pa test/ebin \
		-config ebin/env_test \
		-eval "application:start(sasl)" \
		-eval "application:start(rtperl)" \
		-eval "All = filelib:fold_files(\
			\"test/ebin\",\
			\".*\.beam\",\
			true,\
			fun(F, Acc) -> \
				M = list_to_atom(filename:basename(F, \".beam\")),\
				[M|Acc]\
			end,\
			[]\
		), eunit:test(All, [{verbose,true}])." \
		-s init stop


# Sub-targets

.PHONY : testbuild
testbuild: BUILDOPT = ${BUILDOPT_DEV}
testbuild: ${TEST_MOD}


# Compiling

ebin/%.beam : src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o ebin $<

test/ebin/%.beam : test/src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o test/ebin $<

