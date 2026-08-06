#!/usr/bin/env python3
"""Group fuzz crash artifacts by root cause and write one markdown report each.

Replays every artifact under a harness, extracts a signature from the
sanitizer output (error class + first hobbes source location), groups
duplicates, and writes reports/<slug>.md with everything needed to fix it.
"""
import os
import pathlib
import re
import subprocess
import sys
from collections import OrderedDict


FUZZ = pathlib.Path(os.environ.get("FUZZ_HOME", pathlib.Path.cwd()))
ART = FUZZ / "artifacts"
REPORTS = FUZZ / "reports"
HARNESSES = ["type-decode", "fregion-reader", "parse-expr"]

ENV = dict(os.environ)
ENV["ASAN_OPTIONS"] = "detect_container_overflow=0"
ENV.setdefault("UBSAN_OPTIONS", "print_stacktrace=1")

ERR_RE = re.compile(
    r"(?:ERROR: (?:AddressSanitizer|LeakSanitizer|libFuzzer): ([a-z0-9-]+)"
    r"|SUMMARY: (?:AddressSanitizer|UndefinedBehaviorSanitizer): ([a-z0-9-]+)"
    r"|runtime error: (.+))")
HOBBES_LOC_RE = re.compile(r"(?:/[\w./-]*)?((?:lib|include|bin)/hobbes/[\w./-]+\.[CH]):(\d+)")
FRAME_RE = re.compile(r"^\s*#\d+ .*", re.M)


def replay(harness, artifact):
    binary = FUZZ / "build" / "fuzz" / f"fuzz-{harness}"
    cmd = [str(binary), str(artifact)]
    if harness == "parse-expr":
        cmd.insert(1, "-detect_leaks=0")
    try:
        r = subprocess.run(cmd, env=ENV, capture_output=True, text=True, timeout=180)
        return (r.stdout or "") + (r.stderr or "")
    except subprocess.TimeoutExpired:
        return "TIMEOUT: harness did not terminate within 180s"


def signature(out):
    kind = None
    m = ERR_RE.search(out)
    if m:
        kind = (m.group(1) or m.group(2) or m.group(3) or "").strip()
    if out.startswith("TIMEOUT"):
        kind = "timeout"
    loc = None
    for lm in HOBBES_LOC_RE.finditer(out):
        loc = f"{lm.group(1)}:{lm.group(2)}"
        break
    if not kind:
        return None
    # Normalise value-specific detail so the same defect groups together:
    # "load of value 248, which is not a valid value for type 'bool'" and
    # "load of value 16, ..." are one bug, not two.
    kind = re.sub(r"\b0x[0-9a-f]+\b", "0xN", kind)
    kind = re.sub(r"\b\d+\b", "N", kind)
    kind = re.sub(r"load of value N, which is not a valid value for type '(\w+)'",
                  r"invalid-\1-load", kind)
    kind = re.sub(r"\s+", " ", kind).strip()
    return (kind[:80], loc or "no-hobbes-frame")


def slug(harness, sig):
    kind, loc = sig
    base = re.sub(r"[^a-z0-9]+", "-", f"{harness}-{kind}-{loc}".lower()).strip("-")
    return base[:90]


def main():
    REPORTS.mkdir(exist_ok=True)
    artifacts = sorted(p for p in ART.glob("*") if p.is_file())
    if not artifacts:
        print("no artifacts")
        return 0

    groups = OrderedDict()
    for a in artifacts:
        for h in HARNESSES:
            out = replay(h, a)
            sig = signature(out)
            if sig:
                groups.setdefault((h, sig), {"artifacts": [], "output": out})
                groups[(h, sig)]["artifacts"].append(a)
                break

    written = []
    for (h, sig), data in groups.items():
        kind, loc = sig
        out = data["output"]
        arts = data["artifacts"]
        smallest = min(arts, key=lambda p: p.stat().st_size)
        frames = [f for f in FRAME_RE.findall(out) if "hobbes" in f or "fuzz" in f][:12]
        hexdump = subprocess.run(["xxd", str(smallest)], capture_output=True,
                                 text=True).stdout[:1200]

        surface = {
            "type-decode": "binary type descriptions (`hobbes::decode`), which the RPC "
                           "layer runs on bytes from an unauthenticated peer",
            "fregion-reader": "structured data files (`hobbes::fregion::reader`) — header, "
                              "page table and environment records from a mapped image",
            "parse-expr": "source text through the lexer/LALR parser (`cc::readExpr`)",
        }[h]

        md = f"""# {kind} in {loc}

**Harness:** `fuzz-{h}`
**Untrusted-input surface:** {surface}
**Distinct reproducers:** {len(arts)}
**Smallest reproducer:** `artifacts/{smallest.name}` ({smallest.stat().st_size} bytes)

## Reproduce

```bash
cd ~/fuzz
ASAN_OPTIONS=detect_container_overflow=0 \\
  ./build/fuzz/fuzz-{h}{' -detect_leaks=0' if h == 'parse-expr' else ''} artifacts/{smallest.name}
```

## Sanitizer report

```
{chr(10).join(l for l in out.splitlines() if 'ERROR:' in l or 'SUMMARY:' in l or 'runtime error' in l)[:1500]}
```

## Stack (hobbes frames)

```
{chr(10).join(frames) if frames else '(no hobbes frames captured — see full log)'}
```

## Reproducer bytes

```
{hexdump}```

## Triage notes

- [ ] Confirm against the threat model in `doc/en/security.rst` — is this surface
      trusted or untrusted input?
- [ ] If untrusted-input memory unsafety, treat as security-relevant: report via
      the responsible disclosure program in `SECURITY.md`, **not** a public issue.
- [ ] Fix + regression test (follow the pattern of PRs #510-#513).
- [ ] Commit the reproducer as a corpus seed once fixed.

## All reproducers in this group

{chr(10).join(f'- `artifacts/{a.name}` ({a.stat().st_size} bytes)' for a in arts)}
"""
        path = REPORTS / f"{slug(h, sig)}.md"
        path.write_text(md)
        written.append((path.name, len(arts)))

    index = ["# Fuzzing findings", "",
             f"{len(groups)} distinct issue(s) from {len(artifacts)} artifact(s).", ""]
    for name, n in written:
        index.append(f"- [{name}]({name}) — {n} reproducer(s)")
    (REPORTS / "README.md").write_text("\n".join(index) + "\n")

    for name, n in written:
        print(f"{name}  ({n} reproducers)")
    print(f"\n{len(groups)} distinct issues from {len(artifacts)} artifacts -> {REPORTS}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
