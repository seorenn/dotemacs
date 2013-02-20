#!/bin/bash

git submodule sync
git submodule foreach git pull origin master
git submodule foreach git checkout master

cd emacs-git-gutter
make
cd -

cd js2-mode
make
cd -

cd magit
make
cd -

cd pymacs
make
make install
cd -
