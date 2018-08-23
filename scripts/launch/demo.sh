#!/usr/bin/env bash

# Launches a cluster of witness nodes.

set -e -o pipefail

# Make sure we're in a tmux session.
if ! [ -n "$TMUX" ]; then
  echo "You must run this script from the tmux session!"
  exit 1
fi

# Make sure we're using proper version of tmux.
tmux_actual_version=$(tmux -V | awk '{print $2}')
# All tmux versions contain two numbers only.
tmux_proper_versions=("2.3" "2.4" "2.5" "2.6" "2.7" "2.7-rc" "master")
checker=""
for version in "${tmux_proper_versions[@]}"; do
    if [ "${version}" == "${tmux_actual_version}" ]; then
        checker="${version}"; break
    fi
done
# If checker is still empty - we're using wrong tmux version!
if [ "${checker}" == "" ]; then
    echo "Please upgrade tmux to the version ${tmux_proper_versions[0]} or higher."
    exit 1
fi


# directory with script
base=$(dirname "$0")
# project root
root="$base/../.."
# number of nodes to launch
n=4

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
    if [[ "$var" == "--no-clean" ]]; then
        no_clean=true
    elif [[ "$var" -eq "$var" ]]; then # is number
        n=$var
    else
        echo "Unknown parameter \"$var\""
        exit 1
    fi
done

panesCnt=$n

function witness_addr {
    echo "127.0.0.1:80"$1"0:80"$1"1"
}

function witness_args {
    index=$1
    bind=$(witness_addr $index)
    bind_api="127.0.0.1:80"$index"3"
    witness_dir="$tmp_files/node$index"
    mkdir -p $witness_dir

    i=0
    peers=""
    while [[ $i -lt $n ]]; do
        if [[ $i -ne $index ]]; then
            peers=$peers"--peer "$(witness_addr $i)" "
        fi
        i=$(($i+1))
    done

    # witness params (and educator's as well)
    local witness_params="
    --appdir ./tmp/
    --config ./configuration.yaml
    --config-key demo
    --bind $bind
    --witness-listen $bind_api
    --db-path $witness_dir/witness.db
    --log-dir $witness_dir/logs
    --log-config $files/log-config.yaml
    --comm-n $index
    --witness-keyfile $witness_dir/witness.key
    --witness-gen-key
    $peers
    "
    echo $witness_params
}

if [[ "$no_clean" != true ]]; then
    rm -rf $tmp_files
fi

i=0
while [[ $i -lt $panesCnt ]]; do
  im=$((i%4))
  ir=$((i/4))

  if [[ $im == 0 ]]; then
    tmux new-window -n "demo-"`date +%H%M%S`-"$ir"
    tmux split-window -h
    tmux split-window -v
    tmux select-pane -t 0
    tmux split-window -v
  fi

  echo "Launching node $i in tab $im of window $ir"
  tmux select-pane -t $im

  if [[ $i -lt $n ]]; then
    w_exec="stack exec dscp-witness -- "
    w_args=$(witness_args $i)

    echo "$w_exec $w_args"
    tmux send-keys "$w_exec $w_args" C-m
  fi

  i=$((i+1))
done
