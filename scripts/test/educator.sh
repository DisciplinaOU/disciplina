#!/usr/bin/env bash

# Runs educator tests.
# All passed parameters go to test executable.

# Dependencies:
# * Postgres server
# * pg_tmp (http://eradman.com/ephemeralpg/)

# Prerequisites:
# * Specify your database user name via 'PGUSER' variable; default user name is your OS username.
#   If you haven't created a database user yet, run
#   > sudo -u postgres createuser --interactive --pwprompt
# * If your database user's password is not empty, specify it in .pgpass file
#   (see https://www.postgresql.org/docs/9.3/libpq-pgpass.html).

set -e -o pipefail

if ! which initdb > /dev/null; then
    echo -e "Command 'initdb' not found.\nConsider adding '/usr/lib/postgresql/VERSION/bin/' to your PATH."
    exit 2
fi

if [[ "$#" -ne 0 ]]; then
   args="--test-arguments $@"
fi

logs_dir="/tmp/educator-test"
mkdir -p $logs_dir

# A temporal server does not live long, building tests first.
stack test disciplina-educator --no-run-tests

# Running a server in background.
conn_str=$(pg_tmp)

# Executing tests.
export TEST_PG_CONN_STRING=$conn_str
stack test disciplina-educator $args

# pg_tmp cleans up automatically after a timeout
