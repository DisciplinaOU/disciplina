#!/usr/bin/env bash

# Submits plenty of unique submission requests to Student API.

# Dependencies:
# * httpie
# * /dev/urandom

# Prerequisites:
# * Authentication is disabled
# * $student_secret_seed matches student address passed to
#   --student-api-no-auth parameter
# * $educator_node address referes to working educator node with bot

set -e -o pipefail

student_secret_seed=456
educator_node=":8090"

requests_num=$1
if [[ $requests_num == "" ]]; then
    requests_num=1
fi

if [[ $RUN_SEED == "" ]]; then
    run_seed=$RANDOM
else
    run_seed=$RUN_SEED
fi

for (( i=1; i<=$requests_num; i++ )) do
    echo $student_secret_seed \
        | dscp-keygen --seed --command student-submission:$run_seed-$i \
        | http --check-status POST $educator_node/api/student/v1/submissions
done
