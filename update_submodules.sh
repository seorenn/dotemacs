#!/bin/bash

git submodule sync
git submodule foreach git pull origin master
git submodule foreach git checkout master

cd vendor/pymacs
make
make install
cd -
