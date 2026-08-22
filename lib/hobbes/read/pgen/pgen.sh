#!/usr/bin/env bash
# Regenerate the parser and lexer from hexpr.y and hexpr.l.
#
# The generated files are checked in, and the generators they were made with
# are pinned: GNU Bison 3.8.2 and flex 2.6.4, as packaged by Ubuntu 24.04.
# Regenerating with anything else produces a large diff of unrelated churn --
# a different flex build of the same version number changes several hundred
# lines -- which buries the real change and cannot be reviewed. So by default
# this script runs the generators in a container pinned to that image, with
# podman or docker, whichever is installed. CI regenerates the same way and
# fails if the checked-in files differ from what the pinned generators
# produce, so a change to hexpr.y or hexpr.l has to come with its regenerated
# output, made by this script.
#
#   lib/hobbes/read/pgen/pgen.sh            # regenerate in the pinned container
#   PGEN_NATIVE=1 lib/hobbes/read/pgen/pgen.sh  # use bison/flex from $PATH (CI, or
#                                               # a machine with exactly these versions)
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

# Ubuntu 24.04 (noble) as of the digest below ships flex 2.6.4-8.2build1 and
# bison 2:3.8.2+dfsg-1build2. The digest pins the image; the versions are
# asserted after install, so a moved image or repository cannot silently
# change what generates these files.
PGEN_IMAGE="docker.io/library/ubuntu:24.04@sha256:7607b6f97024ef850f1bd6e91a89273beb5973d04432c5b87f15f813d64b9c05"
PGEN_FLEX_VERSION="2.6.4"
PGEN_BISON_VERSION="3.8.2"

generate() {
  # the whole version line, not just the number: Apple's flex reports itself
  # as "flex 2.6.4 Apple(flex-35)" and generates several hundred lines
  # differently from GNU flex 2.6.4
  local have_flex have_bison
  have_flex="$(flex --version 2>/dev/null | head -1 || true)"
  have_bison="$(bison --version 2>/dev/null | head -1 || true)"
  if [ "$have_flex" != "flex $PGEN_FLEX_VERSION" ] || [ "$have_bison" != "bison (GNU Bison) $PGEN_BISON_VERSION" ]; then
    echo "pgen.sh: need GNU flex $PGEN_FLEX_VERSION and GNU Bison $PGEN_BISON_VERSION on \$PATH" >&2
    echo "pgen.sh: have '${have_flex:-no flex}' and '${have_bison:-no bison}'" >&2
    echo "pgen.sh: run without PGEN_NATIVE to use the pinned container instead" >&2
    exit 1
  fi

  # generate the LALR(1) parser and token definitions
  bison -d -ohexpr.parse.C hexpr.y
  sed -i 's,#include "hexpr.parse.H",//&,' hexpr.parse.C

  # bison's generated cleanup frees the parser stack only if it was moved off
  # the initial array, but GCC cannot see that and warns (-Wfree-nonheap-object,
  # https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98753) -- which -Werror turns
  # into a failed build. Spell the condition out where GCC can see it. This is
  # the one hand edit the checked-in parser carries.
  sed -i 's|^    YYSTACK_FREE (yyss);$|    YYSTACK_FREE (yyss == yyssa ? nullptr : yyss);|' hexpr.parse.C
  sed -i 's|^#ifndef yyoverflow$|#ifndef yyoverflow\n // false positive https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98753|' hexpr.parse.C

  # obey internal convention for division of source and header files
  mv hexpr.parse.H ../../../../include/hobbes/read/pgen/

  # generate the lexer to tokenize string input
  flex -ohexpr.lex.C hexpr.l
}

if [ -n "${PGEN_NATIVE:-}" ]; then
  generate
  exit 0
fi

if command -v podman >/dev/null 2>&1; then
  RUNTIME=podman
elif command -v docker >/dev/null 2>&1; then
  RUNTIME=docker
else
  echo "pgen.sh: neither podman nor docker found; install one, or set PGEN_NATIVE=1 with flex $PGEN_FLEX_VERSION and bison $PGEN_BISON_VERSION on \$PATH" >&2
  exit 1
fi

# the repository root is mounted so the header can land in include/
ROOT="$(cd ../../../.. && pwd)"
exec "$RUNTIME" run --rm \
  -v "$ROOT:/hobbes:Z" -w /hobbes/lib/hobbes/read/pgen \
  -e PGEN_NATIVE=1 \
  "$PGEN_IMAGE" \
  bash -c 'export DEBIAN_FRONTEND=noninteractive
           apt-get update -qq >/dev/null
           apt-get install -y -qq --no-install-recommends bison flex >/dev/null
           exec ./pgen.sh'
