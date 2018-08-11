#!/bin/sh

# This script checks for trailing spaces and trailing lines

extensions="yaml hs sh"

echo "### Checking for trailing spaces:"
spaces=$(for ext in $extensions; do git ls-files '*'$ext | xargs grep -il '[[:blank:]]$'; done)
for i in $spaces; do echo $i; done

function checkLastLine {
    x=`tail -n 1 $1`
    if [ "$x" == "" ]; then echo $1; fi
}

echo "### Checking for empty lines in the end:"
lastLines=$(for ext in $extensions; do
                for i in $(git ls-files '*'$ext); do
                    checkLastLine $i
                done
            done)
for i in $lastLines; do echo $i; done

if [ "$spaces" != "" ] || [ "$lastLines" != "" ]; then
    echo "No trailing spaces or empty last lines should be present, exiting"
    exit 1
fi
