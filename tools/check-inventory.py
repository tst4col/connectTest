#!/usr/bin/env python3
"""Verify that manifest.json's file inventory matches the reviewed runtime tree.

Development check alongside tools/check-manifest.py (which compares the
dependency versions with rproj.lock). This script checks the app file list
and per-file checksums that rsconnect::writeManifest recorded:

  * exactly the reviewed runtime files are listed (appFiles allowlist), and
    nothing else;
  * every listed file exists in the repository;
  * each recorded 32-hex checksum equals the file's MD5 (the rsconnect
    checksum format observed in manifest.json).

Anything else in the repository (tests, tools, README, rig files) is not an
application file and must not appear in the manifest.
"""

import hashlib
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MANIFEST = ROOT / "manifest.json"

# Must match tools/generate-manifest.R
EXPECTED_FILES = [
    "app.R",
    "R/database.R",
    "R/snapshot.R",
    "R/gate.R",
    "R/import.R",
]


def main() -> int:
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    files = manifest.get("files")

    problems = []

    if not isinstance(files, dict):
        print("FAIL: manifest 'files' is not a mapping", file=sys.stderr)
        return 1

    listed = sorted(files.keys())
    expected = sorted(EXPECTED_FILES)
    if listed != expected:
        problems.append(f"file list mismatch: manifest={listed} expected={expected}")

    for name in expected:
        entry = files.get(name)
        path = ROOT / name
        if not path.is_file():
            problems.append(f"listed file missing from tree: {name}")
            continue
        digest = hashlib.md5(path.read_bytes()).hexdigest()
        if entry is None:
            problems.append(f"not listed in manifest: {name}")
            continue
        checksum = entry.get("checksum")
        if not isinstance(checksum, str):
            problems.append(f"no checksum recorded for {name}")
            continue
        if len(checksum) == 32 and checksum == digest:
            continue
        # rsconnect may prefix the hash with an algorithm name; verify both
        # raw and suffix forms.
        if checksum.startswith("md5;") and checksum[4:] == digest:
            continue
        problems.append(f"checksum mismatch for {name}: manifest={checksum} actual md5={digest}")

    if problems:
        for problem in problems:
            print(f"FAIL: {problem}")
        return 1

    print(f"PASS: manifest inventory matches {len(expected)} reviewed runtime files "
          f"(md5 checksums verified)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
