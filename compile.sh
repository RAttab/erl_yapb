#! /usr/bin/env bash

set -o errexit -o nounset -o pipefail -o xtrace

declare -a SRC
SRC=(pb)

declare -a TEST
TEST=()

CC=${OTHERC:-gcc}

CFLAGS="-ggdb -O3 -pipe -std=gnu11 -D_GNU_SOURCE"

CFLAGS="$CFLAGS -fPIC"
CFLAGS="$CFLAGS -fvisibility=hidden"
CFLAGS="$CFLAGS -fno-strict-aliasing"

CFLAGS="$CFLAGS -Werror -Wall -Wextra"
CFLAGS="$CFLAGS -Wundef"
CFLAGS="$CFLAGS -Wcast-align"
CFLAGS="$CFLAGS -Wwrite-strings"
CFLAGS="$CFLAGS -Wunreachable-code"
CFLAGS="$CFLAGS -Wformat=2"
CFLAGS="$CFLAGS -Wswitch-default"
CFLAGS="$CFLAGS -Winit-self"
CFLAGS="$CFLAGS -Wno-strict-aliasing"
CFLAGS="$CFLAGS -Wno-implicit-fallthrough"

OBJ=""
for src in "${SRC[@]}"; do
    $CC -c -o "c_src/$src.o" "c_src/$src.c" $CFLAGS
    OBJ="$OBJ c_src/$src.o"
done

$CC -o ./priv/libyapb.so -shared $OBJ

for test in "${TEST[@]}"; do
    $CC -o "priv/test_$test" "c_src/test_$test.c" $CFLAGS
    "priv/test_$test"
done
