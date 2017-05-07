#!/bin/sh

set -e

exec emacs --batch -q --load /emacs/batch-run.el ${@}
