#!/usr/bin/env bash

echo 'running...'
stack run > trace.out
echo 'any diffs?...'
diff trace.out.sav trace.out
