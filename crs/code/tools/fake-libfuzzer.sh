#!/bin/bash

#To make a much simpler fake-fuzz approach that will follow the actual code/reasoning path of just about everything,
#we just need a fake fuzzer that puts crashes where they belong and prints the string (locating the crash file)
#that the fuzzer task expects to grab to initiate all its reasoning

#For libfuzzer that looks like
#Test unit written to ./crash-1243aa9dab7bd452506fdc4f001ce62c7e98ac73]

# just run this in the CP dir and it will do the rest-- use yq and project.yaml to find the CP
# name, to help look up what POVs to use. then copy them to 'out' and print the magic line for each one.
# - oops, mock-cp CP name is actually "Mock CP".  Sigh.

thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"
PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }

#cpname=`yq '.cp_name' project.yaml`
cpname=`basename $PWD`
dbug cpname is $cpname

for f in `ls $thisdir/stub-blobs/${cpname}` ; do
    dbug "Copying pov blob $thisdir/stub-blobs/${cpname}/$f to out."
    cp $thisdir/stub-blobs/${cpname}/$f out
    echo "Test unit written to ./$f"
done

