#!/usr/bin/env bash

echo 'running...'
stack run test1 > trace.out
echo 'any diffs?...'
diff trace.out.sav trace.out
