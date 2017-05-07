#!/bin/bash

set -e

for file in "${@}"; do
  emacs --batch -q --load /emacs/batch-run.el --visit ${file} --funcall batch-org-html-export-to-html
done
