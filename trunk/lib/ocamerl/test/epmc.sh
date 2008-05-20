#!/bin/bash

EXIT=0

echo; echo "Test $0: Run EPMD client."
./ex_epmc.byte set toto 42 get toto
EXIT=$?

echo; echo "Test $0: RESULT=${EXIT} (0=ok)"
exit ${EXIT}
