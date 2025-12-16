#! /bin/sh
# Copyright 2025 Carnegie Mellon University

sbcl --dynamic-space-size 20000 --eval "(in-package :cl-user)" --load act-up-v1_3_3 --load 'IBL' --load http-server --eval "(bha:run-standalone)"
