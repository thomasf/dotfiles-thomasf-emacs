# dockerfile for building stand alone emacs for batch usage
from ubuntu:17.04

env DEBIAN_FRONTEND noninteractive
run apt-get update && apt-get -y install emacs24-nox graphviz

env HOME /emacs
workdir /emacs
env LANG en_US.UTF-8

add emacs.d/batch/install-common.el /emacs/
add emacs.d/batch/install-1.el /emacs/
run emacs --batch --load install-1.el
add emacs.d/batch/install-2.el /emacs/
run emacs --batch --load install-2.el
add emacs.d/batch/batch-run.el /emacs/

add emacs.d/batch/exec-wrapper.sh /usr/local/bin/emacs
add emacs.d/batch/exec-wrapper.sh /usr/local/bin/emacsclient
add emacs.d/batch/org-export-html.sh /usr/local/bin/org-export-html
add emacs.d/batch/batch-run.sh /usr/local/bin/batch-run

# entrypoint emacs --batch --load /emacs/batch-run.el
# add emacs.d/batch-test.org /emacs/
# run emacs --batch -q --load /emacs/batch-run.el --visit batch-test.org --funcall batch-org-html-export-to-html
