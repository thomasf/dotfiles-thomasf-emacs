#!/bin/sh

set -e

export HOME=/emacs

cmd=$(basename ${0})
exec /usr/bin/${cmd} ${@}
