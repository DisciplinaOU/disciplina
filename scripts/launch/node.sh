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
    elif [[ "$var" == "educator" ]] || [ "$var" == "me" ]; then
        node="multi-educator"
    elif [[ "$var" == "bot" ]] || [ "$var" == "b" ]; then
        educator_bot=true
    elif [[ "$var" == "faucet" ]] || [ "$var" == "f" ]; then
        node="faucet"
    elif [[ "$var" == "--no-clean" ]]; then
        no_clean=true
    elif [[ "$var" == "--prof" ]] || [[ "$var" == "--profile" ]]; then
        profiling=true
    else
        echo "Unknown parameter \"$var\""
        exit 1
    fi
done

if [[ "$node" == "" ]]; then
    echo "Please specify which node to run (e.g. \"educator\" or just \"e\")"
    exit 1
fi

##################
# Preliminaries
##################

# permanent read-only files: configs, secrets...
files="$root/run"
# gitignored files: databases...
tmp_files="$files/tmp/$node"

witness_web_addr="127.0.0.1:8091"

educator_keyfile_seed="educator-1"

### Actions ###

if [[ "$no_clean" != true ]]; then
    rm -rf $tmp_files
fi

mkdir $tmp_files 2> /dev/null || true

# Generate educator keyfile
if [[ $node == "educator" ]]; then
    if ! which dscp-keygen > /dev/null; then
        echo "dscp-keygen not found, install it with 'stack install disciplina-tools'"
        exit 2
    fi
    echo $educator_keyfile_seed \
        | dscp-keygen --seed keyfile \
        > "${tmp_files}/educator.key"
    chmod 0600 ${tmp_files}/educator.key
fi

##################
# Launch
##################

# educator-only params
educator_params="
--educator-keyfile $tmp_files/educator.key
--sql-path $tmp_files/educator.db
--educator-listen 127.0.0.1:8090
--educator-api-no-auth
--student-api-no-auth 3BAyX5pNpoFrLJcP5bZ2kXihBfmBVLprSyP1RhcPPddm6Dw42jzEPXZz22
--publication-period 15s
"
multi_educator_params="
--educator-key-dir $files/educator.key
--sql-path $tmp_files/educator.db
--educator-listen 127.0.0.1:8090
--educator-api-no-auth
--student-api-no-auth 3BAyX5pNpoFrLJcP5bZ2kXihBfmBVLprSyP1RhcPPddm6Dw42jzEPXZz22
--publication-period 15s
"
# Note: Student address in --student-api-no-auth parameter corresponds to secret
# key with seed 456 (use dscp-keygen to generate one)

# witness params (and educator's as well)
witness_params="
--appdir ./tmp/
--config ./config.yaml
--config-key singleton
--bind 127.0.0.1:4010:4011
--db-path $tmp_files/witness.db
--witness-listen $witness_web_addr
--comm-n 0
--metrics-server 127.0.0.1:8125
"

# parameters for faucet
faucet_params="
--appdir ./tmp/
--faucet-listen 127.0.0.1:8095
--witness-backend $witness_web_addr
--translated-amount 20
--config ./config.yaml
--config-key singleton
--faucet-keyfile $files/faucet.key
--faucet-gen-key
"

# bot parameters
if [[ "$educator_bot" == true ]]; then
    educator_params="
$educator_params
--educator-bot
--educator-bot-delay 3s
"
fi

# profiling parameters
if [[ "$profiling" == true ]]; then
    common_params="+RTS -p -RTS"
fi

if [[ "$node" == "educator" ]]; then
    if [ -z "$(which dscp-educator)" ]; then
        stack exec "dscp-educator" --nix -- $common_params $witness_params $educator_params
    else
        dscp-educator $common_params $witness_params $educator_params
    fi
elif [[ "$node" == "multi-educator" ]]; then
    if [ -z "$(which dscp-multi-educator)" ]; then
        stack exec "dscp-multi-educator" --nix -- $common_params $witness_params $multi_educator_params
    else
        dscp-multi-educator $common_params $witness_params $multi_educator_params
    fi
elif [[ "$node" == "witness" ]]; then
    if [ -z "$(which dscp-witness)" ]; then
        stack exec "dscp-witness" --nix -- $common_params $witness_params
    else
        dscp-witness $common_params $witness_params
    fi
elif [[ "$node" == "faucet" ]]; then
    if [ -z "$(which dscp-faucet)" ]; then
        stack exec "dscp-faucet" --nix -- $common_params $faucet_params
    else
        dscp-faucet $common_params $faucet_params
    fi
else
    echo "Unknown node type \"$node\""
    exit 1
fi
