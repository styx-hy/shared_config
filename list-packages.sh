#!/bin/bash

set -eu
set -o pipefail

emacs_list=""

function usage {
    echo "Usage: `basename $0` <gen|install>"
    exit 1
}

function find_installed_packages {
    find $HOME/.emacs.d/elpa -type d -depth 1 | xargs basename | perl -nle '$_ =~ /([\w\d-]+)-([\d\.]+$)/g; print $1 unless ($1 eq $last); $last = $1;'
}

function emacs_make_list {
    # accept all arguments in an array with `@' with null default
    list=(${@:-})

    emacs_list="'(${list[@]})"
}

function emacs_install_list {
    emacs_list=$(<pkg-list)

    cmd="(setq pkgs $emacs_list)"
    emacs -batch \
	-eval "(setq package-archives '((\"gnu\" . \"http://elpa.gnu.org/packages/\") (\"marmalade\" . \"http://marmalade-repo.org/packages/\") (\"melpa\" . \"http://melpa.milkbox.net/packages/\") (\"org\" . \"http://orgmode.org/elpa/\")))" \
	-eval "(require 'cl-lib)" \
	-eval "(list-packages)" \
	-eval "$cmd" \
	-eval "(cl-loop for pkg in pkgs do (if (not (package-installed-p pkg)) (package-install pkg)))"
}

if [[ $# < 1 ]]; then
    usage
fi

act=${1:-}

case $act in
    gen)
	pkg_list=$(find_installed_packages)
	emacs_make_list ${pkg_list[@]}
	echo $emacs_list > pkg-list
	;;
    install)
	emacs_install_list
	;;
esac
