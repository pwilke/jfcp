#!/bin/bash
OUTPUT=$(cat out/2)
curl --user :ZFeaJ4kloUf3UHM9NIxTXPMfopNUKeDwFDc/obo7kXs= -X POST -H "Content-Type: application/json" \
        -d $OUTPUT \
        https://davar.icfpcontest.org/teams/175/solutions
