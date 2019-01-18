#! /usr/bin/env bash

set -o errexit -o nounset -o pipefail -o xtrace

declare -a SRC
SRC=(htable pb)

declare -a TEST
TEST=(pb)

CC=${OTHERC:-gcc}

OUT="./priv"
IN="./c_src"

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
    $CC -c -o "$IN/$src.o" "$IN/$src.c" $CFLAGS
    OBJ="$OBJ $IN/$src.o"
done

if [ ! -d $OUT ]; then mkdir $OUT; fi

$CC -o $OUT/libyapb.so -shared $OBJ
ar rcs $OUT/libyapb.a $OBJ # for tests

for test in "${TEST[@]}"; do
    $CC -o "$OUT/$test.test" "$IN/${test}_test.c" $OUT/libyapb.a $CFLAGS
    "$OUT/$test.test"
done
