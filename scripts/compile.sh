#!/bin/bash

set -e

ocaml_std=$(ocamlc -where)
clang -c lib/support.c -I$ocaml_std -std=gnu11 -Wall -Wextra -pedantic -Werror
rm support.o
