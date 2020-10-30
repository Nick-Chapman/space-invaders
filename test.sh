#!/usr/bin/env bash


echo 'running tst...'
stack run tst > tst.out
echo 'any diffs?...'
diff tst.expected tst.out


echo 'running test1...'
stack run test1 > test1.out
echo 'any diffs?...'
diff test1.expected test1.out


echo 'running test2...'
stack run test2 > test2.out
echo 'any diffs?...'
diff test2.expected test2.out
