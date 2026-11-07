#!/usr/bin/env python3
"""Compare manifest.json with the Linux target in rproj.lock.

This is a development check, not a second dependency authority. The lock is
the rig resolution; manifest.json is generated from the installed project
library and must agree with that resolution.
"""

import json
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MANIFEST = ROOT / "manifest.json"
LOCK = ROOT / "rproj.lock"


def main() -> int:
    with MANIFEST.open(encoding="utf-8") as handle:
        manifest = json.load(handle)
    with LOCK.open("rb") as handle:
        lock = tomllib.load(handle)

    r_version = manifest.get("platform")
    candidates = [
        target
        for target in lock.get("targets", [])
        if target.get("r_version") == r_version
        and "linux-gnu" in target.get("platform", "")
    ]
    if not candidates:
        print(f"No Linux lock target found for R {r_version}", file=sys.stderr)
        return 1
    target = candidates[0]

    locked = {pkg["package"]: pkg["version"] for pkg in target.get("packages", [])}
    deployed = {
        name: details["description"]["Version"]
        for name, details in manifest.get("packages", {}).items()
    }

    problems = []
    for name in sorted(set(deployed) - set(locked)):
        problems.append(f"manifest-only package: {name} {deployed[name]}")
    for name in sorted(set(locked) - set(deployed)):
        problems.append(f"lock-only package: {name} {locked[name]}")
    for name in sorted(set(locked) & set(deployed)):
        if locked[name] != deployed[name]:
            problems.append(
                f"version mismatch: {name} lock={locked[name]} manifest={deployed[name]}"
            )

    print(f"manifest R: {r_version}; lock target: {target['platform']} R {target['r_version']}")
    print(f"manifest packages: {len(deployed)}; lock packages: {len(locked)}")
    if problems:
        for problem in problems:
            print(f"FAIL: {problem}")
        return 1
    print("PASS: manifest.json agrees with rproj.lock for this Linux target")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
