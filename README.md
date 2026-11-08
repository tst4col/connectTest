# connectTest – Stage B

Disposable experiment for the feasibility question in
`06-connectTest-contract.md`: can an R Shiny application on Posit Connect
Cloud Free create a per-session DuckDB database, edit generic German content,
offer a consistent standalone database download that reopens locally **and**
import a trusted owner-created native DuckDB file back into the app?

**Stages A + B implemented locally.** Per stage:

- Stage A (deployed): per-session working database, editing, standalone
  snapshot download, cleanup; no upload, no JSON import, no durable backend.
- Stage B (local implementation, not hosted yet): an owner-gated upload path
  for native Stage A downloads — open of a read-only hardened candidate copy,
  catalog/value validation, seed-free materialization and an all-or-nothing
  replacement of the working collection with a bounded cleanup registry.

There is still: no JSON import, no renderer, no LLM/API key and no durable
backend. Hosted operation of Stage B is **not** proven by local tests. The
upload gate is only a restriction of who may upload; the app provides **no
protection against hostile/manipulated files** and no platform-private
access. Functional round trips do not establish hostile-file safety.

## What the app does

- Each browser session gets its own temporary directory, working database
  (`working.duckdb`) and DuckDB connection. Nothing is shared between sessions.
- The database contains an identified experimental schema
  (`connectTest.stageA`, version 1), one recipe and one portable collection
  setting, both with German text and umlauts/ß.
- Recipe and setting saves use parameterized SQL inside transactions.
- "Momentaufnahme erzeugen" creates a standalone download:
  1. set the per-session mutation guard;
  2. checkpoint the working database;
  3. close the working connection;
  4. verify that no WAL remains;
  5. copy to a `.partial` file;
  6. open the copy read-only and validate schema identity and sample data;
  7. reopen the working database;
  8. atomically rename the validated copy to its final name.

  On a copy or reopen failure, the working database is reopened if possible,
  the previous snapshot is retained, and the UI reports the failure. If
  reopening the original path fails, a fresh recovery candidate is copied
  from the validated partial and promoted only after it reopens successfully.
  Only when both reopening and recovery fail is the session disconnected;
  the current working database and the validated partial copy are then left
  on disk for inspection.
- The download handler only serves an already-created, non-changing snapshot.
  It never serves the active working database.
- The dirty status is based on an observed generation counter: changes since
  the last successful snapshot. Creating a download copy does **not** clear
  it. The app explicitly says it cannot know whether the browser downloaded
  or durably saved the file.
- **Stage B owner upload (new):**
  - `CONNECTTEST_UPLOAD_ENABLED` plus `CONNECTTEST_UPLOAD_PASSWORD` enable
    the upload panel. Missing/blank secret or a disabled flag fails closed.
    The password is cleared after use and never echoed.
  - 5 failed attempts trigger a 300 s session cooldown; every attempt also
    hits a shared 10-per-60-s process window with a fixed timestamp cap.
  - A successful check flips a server-derived session flag; a client
    boolean cannot satisfy it. Uploads are checked before copying/opening
    and again at the import-service boundary.
  - Uploads ≤ 10 MiB (12 MiB platform request cap) are copied to a fresh
    per-session candidate path and opened **read-only at instance
    creation** with locked engine settings (one thread, 128 MiB memory,
    64 MiB temp, no external access, no extension install/load, local
    filesystem disabled for the candidate). Settings are verified on the
    live instance before the configuration lock.
  - The catalog must contain exactly `main` (unexpected schemas — even
    empty ones — are rejected), the three Stage A tables in `main` with the
    exact column set and types, the identity `connectTest.stageA` version 1,
    exactly one recipe and one setting, and no views, macros, sequences,
    indexes or user functions. Newer schema versions are rejected; there is
    no migration system.
  - Valid rows are materialized seed-free into a fresh working database and
    inserted verbatim (values, ids and timestamps preserved). Before any
    change, the preparation runs fully; the committed state switch —
    including snapshot invalidation — happens in one block, after which
    only best-effort retirement cleanup runs. A failing old connection is
    retired into a bounded (8-entry) registry that keeps the connection
    handle; its database file is never unlinked while the connection is
    valid, and cleanup retries at the next import attempt and at session
    end. When the registry is full, imports fail closed until it drains.
  - After an import, the previous snapshot reference is invalidated: no
    download can be offered as if it belonged to the imported collection.
- Cleanup closes the connection and removes the session directory when the
  Shiny session ends. If any connection could not be closed, the residual
  resources are reported and the directory is retained.

## Local setup

The repository is a rig project. `.rvenv/` is machine-local and ignored;
`.Renviron`, `.rvenvlib/`, `rproj.toml` and `rproj.lock` are the
rig-generated, secret-free project bootstrap and are kept.

```sh
# Restore the project-local R and library (R 4.6.0 is pinned in rproj.toml).
rig proj sync

# Service-level tests: schema, transactions, snapshots, failure recovery,
# session isolation, cleanup.
.rvenv/bin/Rscript --vanilla tests/run-stage-a-tests.R

# Stage B service-level tests: gate, throttling, size gate, catalog/identity
# and value validation, materialization, rollback, retirement registry, and
# round trips. Synthetic test credentials are set by the script itself; no
# real credential is read or printed.
.rvenv/bin/Rscript --vanilla tests/run-stage-b-tests.R

# Start locally on loopback only (uploads stay disabled without the gate
# variables below).
.rvenv/bin/Rscript -e 'shiny::runApp(".", host = "127.0.0.1", port = 7867)'
```

With the owner gate enabled (synthetic password for supervised local checks
only; never a real credential inside this file):

```sh
CONNECTTEST_UPLOAD_ENABLED=true CONNECTTEST_UPLOAD_PASSWORD=<synthetic> \
  .rvenv/bin/Rscript -e 'shiny::runApp(".", host = "127.0.0.1", port = 7869)'
```

Then open <http://127.0.0.1:7867>. For a real two-browser-session check, open
two separate browser profiles or one normal and one private window; service
tests alone do not prove browser-session isolation.

An optional Playwright script performs that check with two browser contexts,
downloads both databases, verifies that a download does not clear the dirty
indicator, and verifies that a reload starts fresh. Playwright is test-only
tooling and is not a deployment dependency:

```sh
python3 -m venv /tmp/connectTest-pwvenv
/tmp/connectTest-pwvenv/bin/pip install playwright
/tmp/connectTest-pwvenv/bin/playwright install chromium
/tmp/connectTest-pwvenv/bin/python tests/browser-sessions.py http://127.0.0.1:7867
```

The optional Stage B browser journey script additionally exercises the gate,
unauthenticated rejection, upload/import and a cross-session round trip. Run
the server with the gate enabled as above (port 7869) and pass the same
synthetic password in the environment for the script; it is typed into the
form and never printed:

```sh
/tmp/connectTest-pwvenv/bin/python tests/browser-stage-b.py http://127.0.0.1:7869
```

### Recorded local versions

Captured on 2026-11-07 after the Stage B implementation in this checkout:

```text
R              4.6.0 (2026-04-24) "Because it was There"
rig            0.11.0
R duckdb pkg   2.0.0
DuckDB engine  v2.0.0
shiny          1.14.0
DBI            1.3.0
R platform     x86_64-pc-linux-gnu
rig target     x86_64-unknown-linux-gnu-ubuntu-26.04
```

Captured on 2026-11-06 before Stage B: identical values for R, rig, duckdb
package, engine, shiny and DBI.

Re-capture them with:

```sh
rig --version
.rvenv/bin/Rscript --vanilla -e '
  cat(R.version.string, "\n")
  for (p in c("shiny", "DBI", "duckdb")) cat(p, as.character(packageVersion(p)), "\n")
  con = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = tempfile(fileext = ".duckdb"))
  cat("DuckDB engine", DBI::dbGetQuery(con, "SELECT version() AS v")$v, "\n")
  DBI::dbDisconnect(con, shutdown = TRUE)
'
```

Database file compatibility is a separate fact from the R package version.
The local tests reopen snapshots from a different directory, but hosted
compatibility is not tested in Stage A.

A deployment-like local simulation (not hosted) copied the project without
`.rvenv`, used a separate library containing the manifest packages, and
started the app on loopback successfully (HTTP 200). In that simulation the
committed rig `.Renviron` printed the benign message
`! Project is not synced. Run: rig proj sync` before loading the app. This
shows the bootstrap is not fatal without `.rvenv`, but it does not prove
Connect Cloud behavior.

## Dependency management and deployment file

rig remains the only dependency authority:

- `rproj.toml` declares the direct dependencies and pins R 4.6.0.
- `rproj.lock` is the resolved multi-platform lock.
- `manifest.json` is the single Connect Cloud dependency file, generated from
  the installed project library with `rsconnect`. There is deliberately no
  `renv.lock`.

To regenerate `manifest.json`:

```sh
# rsconnect is development tooling, not an application dependency. Install it
# into rig's tools library; it is not added to rproj.toml.
rig pkg install -r 4.6.0 -l tools rsconnect

# rsconnect lives in the tools library, outside the project venv:
R_LIBS=<home>/R/x86_64-pc-linux-gnu-library/4.6/__tools \
  .rvenv/bin/Rscript tools/generate-manifest.R
python3 tools/check-manifest.py
python3 tools/check-inventory.py
```

`tools/check-manifest.py` (Python 3.11+ for `tomllib`) compares every manifest
package and version with the Linux target in `rproj.lock`. At the time of writing it reported
`PASS: manifest.json agrees with rproj.lock for this Linux target` for
R 4.6.0, 32 packages, including shiny 1.14.0, DBI 1.3.0 and duckdb 2.0.0.
`tools/check-inventory.py` additionally verifies the manifest's file list
against the reviewed runtime files and recalculates each recorded checksum.
Do not assume that lock export is lossless; rerun both checks after any
dependency or source change. (Both PASS after the Stage B change; see the
implementation report.)

## Manual deployment (NOT EXECUTED)

These steps are for the owner after independent review. The implementation
session did not create an account, publish, push, or use credentials, and no
password value appears in this file or any shell example.

### Publishing the Stage B application

1. Review the proposed public file list and confirm no private artifact or
   secret is present.
2. Push the reviewed revision of `task/stage-b-upload` to the public
   repository `https://github.com/tst4col/connectTest` (repository pushes are
   separately authorized; not done here).
3. In Posit Connect Cloud Free, connect/update the content on the public
   repository; `manifest.json` requests the runtime files `app.R`,
   `R/database.R`, `R/gate.R`, `R/import.R`, `R/snapshot.R` and R 4.6.0.
4. Check the current official Connect Cloud R documentation at deployment
   time (2026-11-06 source listed R 4.0.0–4.6.0).
5. Publish manually. Disable automatic republishing if the platform offers
   that setting; otherwise record how a reviewed revision is selected.

### Supplying the owner gate (connect content settings)

6. In the content's environment/variable settings (Advanced settings →
   Variables), add:
   - `CONNECTTEST_UPLOAD_ENABLED` = `true` (or `1`/`yes`/`on` — plain value,
     not marked secret),
   - `CONNECTTEST_UPLOAD_PASSWORD` — choose the value at the keyboard at
     deploy time, mark it **secret/encrypted**, do not put it into the
     repository, a shell command or a log.
7. Restart/republish so the variables are visible as `Sys.getenv()` values.
8. Open the app; the upload panel should say "Upload verfügbar; bitte
   Passwort eingeben." With the variables absent or blank it must show that
   uploads are deactivated (fail closed).
9. Hosted checks to record after enabling:
   - wrong password rejected; a burst of failed attempts triggers the
     cooldown message;
   - correct password enables the upload panel for the browser session;
   - upload a file created locally through the app (download first), verify
     the imported values (umlauts, ß) reappear, edit, snapshot, download,
     reopen locally, upload again;
   - a second authenticated browser session shows distinct values;
   - a fresh browser session (unauthenticated) receives the rejection and
     never reaches an import;
   - hosted R/package/engine versions via the app footer and deployment
     logs; note the "built under R 4.6.1" warnings if they recur;
   - temporary-state behavior: importing then restarting/republishing loses
     the imported collection (record only; no deletion-timing promises).
10. Stop before enabling hosted uploads if anything fails; report the cause.

## Known limitations

- The working copy and unreleased snapshot files are temporary server files.
  The app cannot promise immediate deletion or recoverability after a
  restart/republish.
- Download completion does not prove that the user durably saved the file.
  The UI and dirty status say so.
- The snapshot mechanism assumes one synchronous Shiny session and one
  working connection per session. It does not add an async framework or
  multi-process lock. The mutation guard is a per-session flag.
- If reopening and recovery from the validated copy both fail, the session is
  disconnected. The current working database and the validated partial copy
  are left on disk for inspection, but no further recovery is attempted and
  session cleanup removes the temporary directory (if a connection could not
  be closed at all, the residual resource is reported and the directory is
  retained).
- Upload validation is defense in depth: opening an untrusted file runs
  DuckDB-internal code before any application check; the size gate, the
  locked settings and the catalog allowlist bound that exposure but do not
  make a crafted file safe. There is no hostile-file sandbox claim.
- No R-level timeout guarantees native query interruption; an unexpectedly
  slow import blocks only that worker.
- The retirement registry (8 entries) can postpone obsolete-file deletion;
  such files remain inside the session directory until a successful drain or
  session end.
- Local loopback and service tests do not establish Connect Cloud runtime
  behavior, hosted filesystem behavior, DuckDB compatibility or free-tier
  limits. Damaged/crafted native-file tests were NOT run here (they require a
  genuinely isolated disposable environment).
- No browser automation result is implied by the service tests; actual
  browser checks are reported separately in the review evidence.
