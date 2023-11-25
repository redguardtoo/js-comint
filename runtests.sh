#!/bin/bash
set -xeu

cd "$(dirname $0)"
emacs -Q --batch \
      -l ert \
      -l js-comint.el \
      -l test-js-comint.el \
      -f ert-run-tests-batch-and-exit
