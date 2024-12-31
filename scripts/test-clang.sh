#!/bin/bash

clang -c lib/support.c -o /dev/null -I$(ocamlc -where) \
    -std=gnu11 -Wall -Wextra -pedantic -Werror
