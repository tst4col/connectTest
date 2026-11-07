# connectTest – Stage A

Disposable experiment for the feasibility question in
`06-connectTest-contract.md`: can an R Shiny application on Posit Connect
Cloud Free create a per-session DuckDB database, edit generic German content,
and offer a consistent standalone database download that reopens locally?

**Stage A only.** There is no upload, no JSON import, no authentication, no
durable backend, no renderer and no LLM/API key. Hosted operation is not
proven by this repository's local tests.

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
- Cleanup closes the connection and removes the session directory when the
  Shiny session ends.

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

# Start locally on loopback only.
.rvenv/bin/Rscript -e 'shiny::runApp(".", host = "127.0.0.1", port = 7867)'
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

### Recorded local versions

Captured on 2026-11-06 in this checkout:

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

.rvenv/bin/Rscript tools/generate-manifest.R
python3 tools/check-manifest.py
```

`tools/check-manifest.py` (Python 3.11+ for `tomllib`) compares every manifest
package and version with the Linux target in `rproj.lock`. At the time of writing it reported
`PASS: manifest.json agrees with rproj.lock for this Linux target` for
R 4.6.0, 32 packages, including shiny 1.14.0, DBI 1.3.0 and duckdb 2.0.0.
Do not assume that lock export is lossless; rerun the check after any
dependency change.

## Manual deployment (NOT EXECUTED)

These steps are for the owner after independent Stage A review. The
implementation session did not create an account, publish, push, or use
credentials.

1. Review the proposed public file list and confirm no private artifact or
   secret is present.
2. In Posit Connect Cloud Free, create/choose the owner account and connect
   the public repository `https://github.com/tst4col/connectTest`.
3. Check the current official Connect Cloud R documentation at deployment
   time. On 2026-11-06 it listed R 4.0.0–4.6.0, and `manifest.json` requests
   R 4.6.0.
4. Confirm the repository contains `app.R`, `R/database.R`, `R/snapshot.R`
   and the generated `manifest.json`, and does not contain a `renv.lock`.
5. Publish manually. Disable automatic republishing if the platform offers
   that setting; otherwise record how a reviewed revision is selected.
6. Record the actual hosted R version, package versions, DuckDB engine,
   build result and any runtime limitations.
7. On the hosted app, repeat: edit recipe and setting, create snapshot,
   download, reopen the file locally from a different directory, and check
   two independent browser sessions. Report actual observations, not the
   deployment guide.
8. Stop if the hosted checkpoint fails. Do not start Stage B.

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
  session cleanup still removes the temporary directory.
- Local loopback and service tests do not establish Connect Cloud runtime
  behavior, hosted filesystem behavior, DuckDB compatibility or free-tier
  limits.
- Stage A has no upload path, no authentication/authorization, no durable
  storage, no hostile-file sandbox and no schema migration framework.
- No browser automation result is implied by the service tests; actual
  browser checks are reported separately in the review evidence.
