#!/bin/bash

EXIT=0

# start erlang first (ensure epmd is running)
echo; echo "Test $0: Run and stop an erlang node to ensure epmd is running."
erl -sname erl_test -setcookie cookie -noshell -s init stop

echo; echo "Test $0: Run EPMD client."
./ex_epmc.byte set toto 42 get toto
EXIT=$?

echo; echo "Test $0: RESULT=${EXIT} (0=ok)"
exit ${EXIT}
