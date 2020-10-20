#!/usr/bin/env bash

echo 'running test1...'
stack run test1 > test1.trace
echo 'any diffs?...'
diff test1.trace.sav test1.trace

echo 'running test2...'
stack run test2 > test2.trace
echo 'any diffs?...'
diff test2.trace.sav test2.trace
