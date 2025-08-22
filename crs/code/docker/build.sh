#!/bin/bash -e
# Script to build Docker images used by NF
# (and tag non-build ones' :latest w/ DOCKER_TAG, so you can run full system w/ DOCKER_TAG)

export DOCKER_TAG=${DOCKER_TAG:-${USER}}
dockerdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && pwd )"

export PATH=$PATH:$HOME/bin	# for where parallel might have to be installed

if ! type -a parallel >& /dev/null; then 	# get the parallel tool, if it isn't installed
  echo Installing GNU parallel, one time only... hang on a sec...
  #(wget pi.dk/3 -qO - || curl pi.dk/3/) | bash		# this is the internet-requiring approved way
  (cd $dockerdir/../tools; ./parallel-installer.sh)	# here's our local cached version and torn down installer
  echo will cite | parallel --citation >& /dev/null	# disable noisy citation demands
fi

${dockerdir}/../tools/kill-my-dockers

rm -f /tmp/*$USER*.lock 	# nuke lockfiles that manage starting images

echo Starting docker builds in parallel, cached output may be delayed

#${dockerdir}/../tools/list-nf-images --build | parallel -k -j0 --halt soon,fail=1 echo Building {}\; make -C {} NO_CACHE=--no-cache
# new version uses flaky to retry several times in case internet downloads fail during docker builds
${dockerdir}/../tools/list-lacrosse-images --build | parallel -k -j0 --halt soon,fail=1 echo Building {}\; $dockerdir/../tools/flaky 3 120 make -C {} NO_CACHE=--no-cache

${dockerdir}/../tools/list-lacrosse-images --notbuild | parallel -k docker tag {}:latest {}:$DOCKER_TAG

echo Starting docker syncs, in parallel
${dockerdir}/../tools/list-lacrosse-images --sync | parallel -k -j0 --halt soon,fail=1 echo Syncing {}\; ${dockerdir}/../tools/force-sync-docker-image {}:$DOCKER_TAG
