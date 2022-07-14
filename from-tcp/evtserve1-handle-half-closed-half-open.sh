#!/bin/bash
adirScript=$( cd $( dirname "$0" ) && pwd )
# Linux only

# On Ubuntu, install libkeepalive with:
#
#   sudo apt-get install libkeepalive0

LD_PRELOAD=libkeepalive.so \
    KEEPIDLE=1 \
    KEEPINTVL=1 \
    KEEPCNT=3 \
    racket "$adirScript"/evtserve1-handle-half-closed-half-open.rkt
