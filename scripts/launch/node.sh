#!/usr/bin/env bash

set -e -o pipefail

# directory with script
base=$(dirname "$0")
# project root
root="$base/../.."

# permanent read-only files: configs, secrets...
files="$root/run"
# gitignored files: databases...
tmp_files="$files/tmp"

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
    elif [[ "$var" == "bot" ]] || [ "$var" == "b" ]; then
        educator_bot=true
    elif [[ "$var" == "--no-clean" ]]; then
        no_clean=true
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
--educator-keyfile-path $files/educator.key
--educator-keyfile-password 12345678
--sql-path $tmp_files/educator.db
--student-listen 127.0.0.1:8090
"

# witness params (and educator's as well)
witness_params="
--config $files/config.yaml
--bind 127.0.0.1:4010:4011
--db-path $tmp_files/witness.db
--log-dir $tmp_files/logs
--witness-listen 127.0.0.1:4020
--witness-keyfile-path $files/witness.key
--witness-keyfile-password xixixixi
"

if [[ "$educator_bot" == true ]]; then
    educator_params="$educator_params --educator-bot"
fi


if [[ "$no_clean" != true ]]; then
    rm -rf $tmp_files
fi

if [[ "$node" == "educator" ]]; then
    stack exec "dscp-educator" -- $witness_params $educator_params
elif [[ "$node" == "witness" ]]; then
    stack exec "dscp-witness" -- $witness_params
elif [[ "$node" == "" ]]; then
    echo "Please specify which node to run (e.g. \"educator\" or just \"e\")"
    exit 1
else
    echo "Unknown node type \"$node\""
    exit 1
fi
