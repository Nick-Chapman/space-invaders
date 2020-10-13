#!/usr/bin/env bash

stack run | tee trace.out
diff trace.out.sav trace.out
