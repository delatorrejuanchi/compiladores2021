#!/usr/bin/env bash

filename="${@%.*}"
stack run -- -o -m "$@"
./bvm/bvm "$filename.byte"
