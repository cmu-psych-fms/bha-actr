#! /bin/sh
# Copyright 2025 Carnegie Mellon University

sbcl --dynamic-space-size 20000 --eval "(in-package :cl-user)" --load act-up-v1_3_3 --load 'centipede v1_0' --load http-server --eval "(kal:run-standalone)"
