#!/usr/bin/env bash

set -e -o pipefail

# directory with script
base=$(dirname "$0")
# project root
root="$base/../.."

# permanent read-only files: configs, secrets...
files="$root/run"
# gitignored files: databases...
tmp_files="$files/tmp/"

mkdir $tmp_files 2> /dev/null || true

##################
# Parsing params
##################

for var in "$@"
do
    if [[ "$var" == "witness" ]] || [ "$var" == "w" ]; then
        node="witness"
    elif [[ "$var" == "educator" ]] || [ "$var" == "e" ]; then
        node="educator"
    else
        echo "Unknown parameter \"$var\""
        exit 1
    fi
done

##################
# Launch
##################

# educator-only params
educator_params="
--keyfile-password 12345678
--keyfile-path $files/educator.key
--sql-path $tmp_files/educator.db
--student-listen 127.0.0.1:8090
"

# witness params (and educator's as well)
witness_params="
--config $files/config.yaml
--bind 127.0.0.1:4010:4011
--db-path $tmp_files/witness.db
--log-dir $tmp_files/logs
--key $tmp_files/witness.key
--key-gen
"

if [[ "$node" == "educator" ]]; then
    stack exec "disciplina-educator" -- $witness_params $educator_params
elif [[ "$node" == "witness" ]]; then
    stack exec "disciplina-witness" -- $witness_params
elif [[ "$node" == "" ]]; then
    echo "Please specify which node to run (e.g. \"educator\" or just \"e\")"
    exit 1
else
    echo "Unknown node type \"$node\""
    exit 1
fi
