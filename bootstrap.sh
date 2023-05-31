#!/usr/bin/env bash

guix time-machine -C channels.scm -- shell -C emacs git -- emacs -q README.org --batch --eval '(org-babel-tangle)'
