#!/bin/bash
OUTPUT=$(cat out/$1)
echo "Submitting solution of problem $1..."
curl --user :ZFeaJ4kloUf3UHM9NIxTXPMfopNUKeDwFDc/obo7kXs= -X POST -H "Content-Type: application/json" \
        -d $OUTPUT \
        https://davar.icfpcontest.org/teams/175/solutions
