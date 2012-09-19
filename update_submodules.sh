#!/bin/bash

git submodule sync
git submodule foreach git pull
