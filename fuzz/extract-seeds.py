#!/usr/bin/env python3
"""Pull Hobbes expressions out of the C++ test suite as fuzzer seeds.

The type-checker harness (fuzz-typecheck-expr) only gets past the parser on
input that parses, and random bytes almost never do. The test suite holds
hundreds of expressions that do, covering most of the language, in string
literals handed to compileFn / EXPTEST / define and friends. This writes each
distinct one to a file in the output directory, named by a hash of its
content so that repeated runs are idempotent and the files sort stably.

    extract-seeds.py <out-dir> <test-source>...

The seeds are generated into the build, not checked in: fuzz/corpus/ is for
reproducers, and a corpus that tracks the tests needs nothing regenerated
when a test is added. Some of the expressions refer to functions or types
that only the test binary defines, and fail to type check against a bare
cc. That is fine for a seed -- the fuzzer keeps what adds coverage and drops
the rest -- so no attempt is made to filter them out.
"""

import hashlib
import os
import re
import sys

# the calls whose last string-literal argument is a Hobbes expression
CALLS = re.compile(r"\b(?:compileFn|compileSafeFn|EXPTEST|define|unsweetenExpression|readExpr|machineCodeForExpr)\b(?:\s*<[^;{}]*?>)?\s*\(")

ESCAPES = {"n": "\n", "t": "\t", "r": "\r", "\\": "\\", '"': '"', "'": "'", "0": "\0"}


def unescape(lit):
    """Decode the body of a C++ string literal (the part between the quotes)."""
    out = []
    i = 0
    while i < len(lit):
        c = lit[i]
        if c == "\\" and i + 1 < len(lit):
            e = lit[i + 1]
            if e in ESCAPES:
                out.append(ESCAPES[e])
                i += 2
                continue
            if e == "x":
                m = re.match(r"[0-9a-fA-F]+", lit[i + 2:])
                if m:
                    out.append(chr(int(m.group(0), 16) & 0xFF))
                    i += 2 + len(m.group(0))
                    continue
        out.append(c)
        i += 1
    return "".join(out)


def call_args(src, start):
    """Scan the argument list that opens at src[start] == '('.

    Returns (string_literal_args, end_index). Adjacent string literals are
    concatenated as the C++ compiler would. Arguments that are not string
    literals are ignored; nested calls are skipped over.
    """
    assert src[start] == "("
    depth = 0
    i = start
    strings = []
    current = None  # literal pieces of the argument being read, or None
    while i < len(src):
        c = src[i]
        if c == '"':
            j = i + 1
            while j < len(src) and src[j] != '"':
                if src[j] == "\\":
                    j += 1
                j += 1
            if depth == 1:
                if current is None:
                    current = []
                current.append(src[i + 1:j])
            i = j + 1
            continue
        if c == "'":
            # a char literal; skip it so a quote inside does not open a string
            j = i + 1
            while j < len(src) and src[j] != "'":
                if src[j] == "\\":
                    j += 1
                j += 1
            i = j + 1
            continue
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                if current is not None:
                    strings.append(unescape("".join(current)))
                return strings, i + 1
        elif c == "," and depth == 1:
            if current is not None:
                strings.append(unescape("".join(current)))
            current = None
        i += 1
    return strings, i


def expressions(src):
    pos = 0
    while True:
        m = CALLS.search(src, pos)
        if not m:
            return
        strings, pos = call_args(src, m.end() - 1)
        if strings:
            yield strings[-1]


def main(argv):
    if len(argv) < 3:
        sys.stderr.write("usage: extract-seeds.py <out-dir> <test-source>...\n")
        return 2
    out = argv[1]
    os.makedirs(out, exist_ok=True)
    seen = set()
    for path in argv[2:]:
        with open(path, encoding="utf-8", errors="replace") as f:
            src = f.read()
        for e in expressions(src):
            e = e.strip()
            if not e or e in seen:
                continue
            seen.add(e)
            name = hashlib.sha1(e.encode("utf-8")).hexdigest()[:16] + ".hob"
            with open(os.path.join(out, name), "w", encoding="utf-8") as f:
                f.write(e)
    sys.stderr.write("extract-seeds: %d expressions -> %s\n" % (len(seen), out))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
