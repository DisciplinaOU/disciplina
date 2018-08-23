#!/usr/bin/env bash

# Launches a single node, be it witness, educator or other one,
# with predefined parameters.

set -e -o pipefail

# directory with script
base=$(dirname "$0")
# project root
root="$base/../.."

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
    elif [[ "$var" == "faucet" ]] || [ "$var" == "f" ]; then
        node="faucet"
    elif [[ "$var" == "--no-clean" ]]; then
        no_clean=true
    else
        echo "Unknown parameter \"$var\""
        exit 1
    fi
done

if [[ "$node" == "" ]]; then
    echo "Please specify which node to run (e.g. \"educator\" or just \"e\")"
    exit 1
else

##################
# Launch
##################

# permanent read-only files: configs, secrets...
files="$root/run"
# gitignored files: databases...
tmp_files="$files/tmp/$node"

mkdir $tmp_files 2> /dev/null || true


witness_web_addr="127.0.0.1:8091"

# educator-only params
educator_params="
--appdir ./tmp/
--educator-keyfile $tmp_files/educator.key
--educator-gen-key
--sql-path $tmp_files/educator.db
--student-listen 127.0.0.1:8090
"

# witness params (and educator's as well)
witness_params="
--appdir ./tmp/
--config ./configuration.yaml
--config-key demo
--bind 127.0.0.1:4010:4011
--db-path $tmp_files/witness.db
--log-dir $tmp_files/logs
--witness-listen $witness_web_addr
--witness-keyfile $tmp_files/witness.key
--witness-gen-key
"

# parameters for faucet
faucet_params="
--appdir ./tmp/
--faucet-listen 127.0.0.1:8095
--witness-backend $witness_web_addr
--translated-amount 20
--config ./configuration.yaml
--config-key demo
--faucet-keyfile $files/faucet.key
--faucet-gen-key
--log-dir $tmp_files/logs
"

if [[ "$educator_bot" == true ]]; then
    educator_params="
$educator_params
--educator-bot
--educator-bot-delay 3s
"
fi



if [[ "$no_clean" != true ]]; then
    rm -rf $tmp_files
fi

if [[ "$node" == "educator" ]]; then
    stack exec "dscp-educator" -- $witness_params $educator_params
elif [[ "$node" == "witness" ]]; then
    stack exec "dscp-witness" -- $witness_params
elif [[ "$node" == "faucet" ]]; then
    stack exec "dscp-faucet" -- $faucet_params
fi
    echo "Unknown node type \"$node\""
    exit 1
fi
