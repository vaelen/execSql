#!/bin/bash

mkdir -p bin
buildapp --output bin/execSql --load execSql.lisp --entry main
