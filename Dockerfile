# dockerfile for building stand alone emacs for batch usage
from ubuntu:17.04

env DEBIAN_FRONTEND noninteractive
run apt-get update && apt-get -y install emacs24-nox

env HOME /emacs
workdir /emacs
env LANG en_US.UTF-8

add emacs.d/batch-install-common.el /emacs/
add emacs.d/batch-install-1.el /emacs/
run emacs --batch --load batch-install-1.el
add emacs.d/batch-install-2.el /emacs/
run emacs --batch --load batch-install-2.el
add emacs.d/batch-run.el /emacs/

# entrypoint emacs --batch --load /emacs/batch-run.el
# add emacs.d/batch-test.org /emacs/
# run emacs --batch -q --load /emacs/batch-run.el --visit batch-test.org --funcall batch-org-html-export-to-html
