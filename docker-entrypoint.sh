#!/bin/bash
# Copyright 2025 Carnegie Mellon University

exec sbcl --dynamic-space-size 20000 --load act-up-v1_3_3 --load 'centipede v1_0' --load quicklisp/setup.lisp --load http-server.lisp --eval '(jh:run-standalone)'
