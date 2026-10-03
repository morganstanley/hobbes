#!/usr/bin/env python3
"""Compare the line coverage of the test suite with that of the fuzz corpora.

    coverage-compare.py <src-root> tests.lcov fuzz.lcov [harness=harness.lcov ...]

Each lcov file is one llvm-cov export of the same binaries under a different
profile (fuzz/coverage.sh writes them), so every instrumented line appears in
all of them and the only difference is whether its count is zero. Lines are
then sorted into four bins -- covered by both, by the tests only, by the
fuzzers only, by neither -- per source directory and per file, and written as
a Markdown report on stdout. The per-harness files, when given, add a column
saying how many lines each harness reaches that nothing else does.
"""

import collections
import os
import sys


def read_lcov(path, root):
    """{source file relative to root: {line: hit count}}"""
    files = {}
    cur = None
    with open(path) as f:
        for raw in f:
            line = raw.strip()
            if line.startswith("SF:"):
                name = os.path.relpath(line[3:], root)
                cur = files.setdefault(name, {})
            elif line.startswith("DA:") and cur is not None:
                num, count = line[3:].split(",")[:2]
                num = int(num)
                cur[num] = cur.get(num, 0) + int(count)
            elif line == "end_of_record":
                cur = None
    return files


def area(name):
    """the directory a file is reported under: lib/hobbes/eval, include/hobbes/lang, bin/hog, ..."""
    parts = name.split(os.sep)
    if len(parts) >= 3 and parts[1] == "hobbes":
        return os.sep.join(parts[:3]) if len(parts) > 3 else os.sep.join(parts[:2])
    return os.sep.join(parts[:2]) if len(parts) > 2 else parts[0]


def pct(n, d):
    return f"{100.0 * n / d:.1f}%" if d else "-"


def main(argv):
    if len(argv) < 4:
        sys.stderr.write(__doc__)
        return 2
    root, tests_path, fuzz_path = argv[1:4]
    tests = read_lcov(tests_path, root)
    fuzz = read_lcov(fuzz_path, root)
    harnesses = []
    for spec in argv[4:]:
        label, path = spec.split("=", 1)
        harnesses.append((label, read_lcov(path, root)))

    # per file: the four bins, plus each harness's lines that only it reaches
    per_file = {}
    for name in sorted(set(tests) | set(fuzz)):
        t, z = tests.get(name, {}), fuzz.get(name, {})
        lines = set(t) | set(z)
        tl = {n for n in lines if t.get(n, 0) > 0}
        zl = {n for n in lines if z.get(n, 0) > 0}
        uniq = {}
        for label, h in harnesses:
            mine = {n for n in lines if h.get(name, {}).get(n, 0) > 0}
            others = set(tl)
            for other, oh in harnesses:
                if other != label:
                    others |= {n for n in lines if oh.get(name, {}).get(n, 0) > 0}
            uniq[label] = len(mine - others)
        per_file[name] = {
            "lines": len(lines),
            "both": len(tl & zl),
            "tests_only": len(tl - zl),
            "fuzz_only": len(zl - tl),
            "neither": len(lines - tl - zl),
            "unique": uniq,
        }

    def total(rows):
        acc = collections.Counter()
        uniq = collections.Counter()
        for r in rows:
            for k in ("lines", "both", "tests_only", "fuzz_only", "neither"):
                acc[k] += r[k]
            uniq.update(r["unique"])
        acc["tests"] = acc["both"] + acc["tests_only"]
        acc["fuzz"] = acc["both"] + acc["fuzz_only"]
        acc["union"] = acc["lines"] - acc["neither"]
        return acc, uniq

    out = sys.stdout
    all_rows, all_uniq = total(per_file.values())
    out.write("# Test coverage vs fuzzing coverage\n\n")
    out.write(f"Instrumented lines: {all_rows['lines']}\n\n")
    out.write("| | Lines | Share |\n|---|---|---|\n")
    out.write(f"| Tests | {all_rows['tests']} | {pct(all_rows['tests'], all_rows['lines'])} |\n")
    out.write(f"| Fuzzing | {all_rows['fuzz']} | {pct(all_rows['fuzz'], all_rows['lines'])} |\n")
    out.write(f"| Either | {all_rows['union']} | {pct(all_rows['union'], all_rows['lines'])} |\n")
    out.write(f"| Both | {all_rows['both']} | {pct(all_rows['both'], all_rows['lines'])} |\n")
    out.write(f"| Tests only | {all_rows['tests_only']} | {pct(all_rows['tests_only'], all_rows['lines'])} |\n")
    out.write(f"| Fuzzing only | {all_rows['fuzz_only']} | {pct(all_rows['fuzz_only'], all_rows['lines'])} |\n\n")

    if harnesses:
        out.write("Lines each harness reaches that neither the tests nor any other harness do:\n\n")
        out.write("| Harness | Unique lines |\n|---|---|\n")
        for label, _ in harnesses:
            out.write(f"| {label} | {all_uniq[label]} |\n")
        out.write("\n")

    areas = collections.defaultdict(list)
    for name, r in per_file.items():
        areas[area(name)].append(r)
    out.write("## By directory\n\n")
    out.write("| Directory | Lines | Tests | Fuzzing | Either | Fuzzing only | Tests only |\n")
    out.write("|---|---|---|---|---|---|---|\n")
    for a in sorted(areas, key=lambda a: -total(areas[a])[0]["lines"]):
        r, _ = total(areas[a])
        out.write(f"| {a} | {r['lines']} | {pct(r['tests'], r['lines'])} | {pct(r['fuzz'], r['lines'])} | "
                  f"{pct(r['union'], r['lines'])} | {r['fuzz_only']} | {r['tests_only']} |\n")
    out.write("\n")

    for key, title in (("fuzz_only", "Files where fuzzing reaches the most lines the tests miss"),
                       ("tests_only", "Files where the tests reach the most lines fuzzing misses")):
        out.write(f"## {title}\n\n| File | Lines | {key.replace('_', ' ').capitalize()} |\n|---|---|---|\n")
        top = sorted(per_file.items(), key=lambda kv: -kv[1][key])[:15]
        for name, r in top:
            if r[key] == 0:
                break
            out.write(f"| {name} | {r['lines']} | {r[key]} |\n")
        out.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
