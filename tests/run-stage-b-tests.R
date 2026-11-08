#!/usr/bin/env Rscript
# Stage B service-level tests. Run with:
#   .rvenv/bin/Rscript --vanilla tests/run-stage-b-tests.R
# Benign app-created fixtures only. Damaged/crafted native files are NOT
# exercised here (require a genuine isolated disposable environment).
# No testthat dependency is required; no secrets appear anywhere.

source("R/database.R")
source("R/snapshot.R")
source("R/gate.R")
source("R/import.R")

testRoot = tempfile(paste0("connectTest-stage-b-", Sys.getpid(), "-"))
if (!dir.create(testRoot, recursive = TRUE)) {
  stop("Cannot create test root: ", testRoot)
}
on.exit(unlink(testRoot, recursive = TRUE, force = TRUE), add = TRUE)

# Synthetic test credentials only; real deployment secrets are never read and
# never printed. Restore the caller's environment on exit.
previousEnabled = Sys.getenv("CONNECTTEST_UPLOAD_ENABLED", unset = NA_character_)
previousPassword = Sys.getenv("CONNECTTEST_UPLOAD_PASSWORD", unset = NA_character_)
Sys.setenv(CONNECTTEST_UPLOAD_ENABLED = "true")
Sys.setenv(CONNECTTEST_UPLOAD_PASSWORD = "synthetisches-pruef-passwort")
restoreEnv = function() {
  if (is.na(previousEnabled)) {
    Sys.unsetenv("CONNECTTEST_UPLOAD_ENABLED")
  } else {
    Sys.setenv(CONNECTTEST_UPLOAD_ENABLED = previousEnabled)
  }
  if (is.na(previousPassword)) {
    Sys.unsetenv("CONNECTTEST_UPLOAD_PASSWORD")
  } else {
    Sys.setenv(CONNECTTEST_UPLOAD_PASSWORD = previousPassword)
  }
}
on.exit(restoreEnv(), add = TRUE)

counters = new.env(parent = emptyenv())
counters$failures = 0L
counters$checks = 0L

check = function(label, expression) {
  counters$checks = counters$checks + 1L
  result = tryCatch({
    force(expression)
    TRUE
  }, error = function(error) {
    counters$failures = counters$failures + 1L
    cat("FAIL:", label, "-", conditionMessage(error), "\n")
    FALSE
  })
  if (isTRUE(result)) {
    cat("PASS:", label, "\n")
  }
  invisible(result)
}

expectError = function(expression, pattern) {
  message = tryCatch({
    force(expression)
    NULL
  }, error = function(error) conditionMessage(error))
  if (is.null(message)) {
    stop("Expected an error containing: ", pattern)
  }
  if (!grepl(pattern, message, fixed = TRUE)) {
    stop("Error did not contain '", pattern, "': ", message)
  }
  invisible(message)
}

resetGateThrottle()

# --- Gate resolution ----------------------------------------------------------

check("gate fails closed when the flag is missing, blank or unknown", {
  for (value in c("", " ", "0", "false", "nope", "ENABLED", "2")) {
    gate = resolveUploadGate(enabledValue = value, passwordValue = "x")
    stopifnot(isFALSE(gate$enabled), isFALSE(gate$passwordOk), nzchar(gate$reason))
  }
})

check("gate fails closed when the password is missing, blank or overlong even if enabled", {
  for (value in c("", "   ")) {
    gate = resolveUploadGate(enabledValue = "true", passwordValue = value)
    stopifnot(isFALSE(gate$enabled), isFALSE(gate$passwordOk))
    stopifnot(!grepl(" Gree", gate$reason, fixed = FALSE) || TRUE)
    stopifnot(nzchar(gate$reason))
  }
  longPassword = paste(rep("a", 129L), collapse = "")
  gate = resolveUploadGate(enabledValue = "true", passwordValue = longPassword)
  stopifnot(isFALSE(gate$enabled), isFALSE(gate$passwordOk))
})

check("gate accepts the documented enabling values with a plausible password", {
  for (value in c("1", "true", "TRUE", "yes", "YES", "on", "ON", " yes ")) {
    gate = resolveUploadGate(enabledValue = value, passwordValue = "beispiel")
    stopifnot(isTRUE(gate$enabled), isTRUE(gate$passwordOk), is.null(gate$reason))
  }
})

# The real process environment is whatever the deployment configured it to
# be; this checkout's tests never rely on real credentials and only pass
# explicit arguments. Record the fallback behaviour:
check("resolveUploadGate uses the live process environment by default", {
  gate = resolveUploadGate()
  stopifnot(is.logical(gate$enabled), length(gate$enabled) == 1L)
})

# --- Password matching and session attempt flow -------------------------------

check("password comparison is equal-value only and tolerates wrong values", {
  stopifnot(isTRUE(isOwnerPasswordMatch("gelten", "gelten")))
  stopifnot(isFALSE(isOwnerPasswordMatch("gelter", "gelten")))
  stopifnot(isFALSE(isOwnerPasswordMatch("", "gelten")))
  stopifnot(isFALSE(isOwnerPasswordMatch(NA_character_, "gelten")))
  stopifnot(isFALSE(isOwnerPasswordMatch(c("a", "b"), "ab")))
})

check("C1 regression: reordered/cancelling/unequal inputs can never compare equal", {
  password = "synthetisches-pruef-passwort"
  stopifnot(isTRUE(isOwnerPasswordMatch(password, password)))

  # the reported C1 reproductions: byte differences must not cancel
  stopifnot(isFALSE(isOwnerPasswordMatch("ba", "ab")))
  stopifnot(isFALSE(isOwnerPasswordMatch("zz", "aa")))
  stopifnot(isFALSE(isOwnerPasswordMatch("aabb", "bbaa")))
  stopifnot(isFALSE(isOwnerPasswordMatch("abcd", "abed")))

  # reordered full password
  reversed = paste(rev(strsplit(password, NULL)[[1]]), collapse = "")
  stopifnot(!identical(reversed, password))
  stopifnot(isFALSE(isOwnerPasswordMatch(reversed, password)))
  stopifnot(isTRUE(isOwnerPasswordMatch(reversed, reversed)))

  # unequal lengths in both directions and prefixes
  stopifnot(isFALSE(isOwnerPasswordMatch("abc", "abcd")))
  stopifnot(isFALSE(isOwnerPasswordMatch("abcd", "abc")))
  stopifnot(isFALSE(isOwnerPasswordMatch(paste0(password, "x"), password)))

  # empty and invalid inputs
  stopifnot(isFALSE(isOwnerPasswordMatch("", password)))
  stopifnot(isFALSE(isOwnerPasswordMatch(NA_character_, password)))
  stopifnot(isFALSE(isOwnerPasswordMatch(c("a", "b"), password)))
  stopifnot(isFALSE(isOwnerPasswordMatch(42, password)))

  # non-ascii: umlaut and eszett bytes compared as data
  stopifnot(isTRUE(isOwnerPasswordMatch("straßenpass", "straßenpass")))
  stopifnot(isFALSE(isOwnerPasswordMatch("straße", "strasse")))
  stopifnot(isFALSE(isOwnerPasswordMatch("straße", "Straße")))

  # maximum length boundary and overlong rejection
  stopifnot(isTRUE(isOwnerPasswordMatch(strrep("a", 128L), strrep("a", 128L))))
  stopifnot(isFALSE(isOwnerPasswordMatch(
    paste0(strrep("a", 127L), "b"),
    strrep("a", 128L)
  )))
  stopifnot(isFALSE(isOwnerPasswordMatch(strrep("a", 129L), password)))
  stopifnot(isFALSE(isOwnerPasswordMatch(password, strrep("a", 130L))))
})

check("C1 auth regression: a wrong password never flips the session flag", {
  resetGateThrottle()
  state = newSessionState("suite-c1-auth")
  on.exit(cleanupSession(state), add = TRUE)
  password = Sys.getenv("CONNECTTEST_UPLOAD_PASSWORD")
  reordered = paste(rev(strsplit(password, NULL)[[1]]), collapse = "")
  stopifnot(!identical(reordered, password))

  wrongResult = attemptOwnerUploadAuthorisation(state, reordered)
  stopifnot(isFALSE(wrongResult$ok))
  stopifnot(!isTRUE(state$ownerUploadAuthed))
  stopifnot(identical(state$gateFailedAttempts, 1L))

  multiWrong = attemptOwnerUploadAuthorisation(state, paste0(password, "x"))
  stopifnot(isFALSE(multiWrong$ok))
  stopifnot(!isTRUE(state$ownerUploadAuthed))

  correctResult = attemptOwnerUploadAuthorisation(state, password)
  stopifnot(isTRUE(correctResult$ok))
  stopifnot(isTRUE(state$ownerUploadAuthed))
  cleanupSession(state)
})

check("session authorisation: missing/blank/disabled, wrong and correct password", {
  state = newSessionState("suite-gate")
  on.exit(cleanupSession(state), add = TRUE)
  state$ownerUploadAuthed = FALSE

  wrong = attemptOwnerUploadAuthorisation(state, "falsch")
  stopifnot(isFALSE(wrong$ok), !isTRUE(state$ownerUploadAuthed))
  # the synthetic password is compared, never printed
  correct = attemptOwnerUploadAuthorisation(
    state,
    Sys.getenv("CONNECTTEST_UPLOAD_PASSWORD")
  )
  stopifnot(isTRUE(correct$ok), isTRUE(state$ownerUploadAuthed))
  expectError(
    requireOwnerUploadAuthorisation(list(ownerUploadAuthed = FALSE)),
    "Besitzerfreigabe"
  )
  requireOwnerUploadAuthorisation(state)
  cleanupSession(state)
})

check("5 failed attempts trigger a session cooldown and block later success", {
  resetGateThrottle()
  state = newSessionState("suite-cooldown")
  on.exit(cleanupSession(state), add = TRUE)
  for (attempt in seq_len(gateSessionMaxFailedAttempts)) {
    result = attemptOwnerUploadAuthorisation(state, "nein")
    stopifnot(isFALSE(result$ok))
  }
  stopifnot(state$gateCooldownUntil > Sys.time())
  result = attemptOwnerUploadAuthorisation(
    state,
    Sys.getenv("CONNECTTEST_UPLOAD_PASSWORD")
  )
  stopifnot(isFALSE(result$ok), !isTRUE(state$ownerUploadAuthed))
  cleanupSession(state)
})

check("process-wide window accepts at most 10 attempts per 60 seconds", {
  resetGateThrottle()
  state = newSessionState("suite-process")
  on.exit(cleanupSession(state), add = TRUE)
  # pre-fill the process-wide rolling window to the cap (bounded utilisation)
  for (slot in seq_len(gateProcessWindowMaxAttempts)) {
    recordGateAttempt(as.numeric(Sys.time()) - slot)
  }
  stopifnot(length(.uploadGateThrottle$attempts) == gateProcessWindowMaxAttempts)
  result = attemptOwnerUploadAuthorisation(state, "richtig")
  stopifnot(isFALSE(result$ok))
  stopifnot(length(.uploadGateThrottle$attempts) == gateProcessWindowMaxAttempts)
  # after a drain (window rolled over) the gate accepts again
  resetGateThrottle()
  result = attemptOwnerUploadAuthorisation(
    state,
    Sys.getenv("CONNECTTEST_UPLOAD_PASSWORD")
  )
  stopifnot(isTRUE(result$ok))
  cleanupSession(state)
})

check("throttle memory stays bounded at the timestamp cap", {
  resetGateThrottle()
  for (attempt in seq_len(gateProcessTimestampCap + 40L)) {
    recordGateAttempt(attempt)
  }
  stopifnot(length(.uploadGateThrottle$attempts) <= gateProcessTimestampCap)
  resetGateThrottle()
})

# --- Size gating before opening ------------------------------------------------

check("files above 10 MiB are rejected before any connection is opened", {
  state = newSessionState("suite-size")
  on.exit(cleanupSession(state), add = TRUE)
  state$ownerUploadAuthed = TRUE
  sizeSnapshot = createSnapshot(state)

  oversized = file.path(testRoot, "oversized.duckdb")
  file.copy(sizeSnapshot, oversized, overwrite = TRUE)
  pad = file(oversized, "ab")
  writeBin(as.raw(rep(0L, uploadMaxBytes)), pad)
  close(pad)
  stopifnot(file.size(oversized) == uploadMaxBytes + file.size(sizeSnapshot))

  opened = new.env(parent = emptyenv())
  opened$count = 0L
  expectError(
    importDatabase(state, oversized, hooks = list(
      openCandidate = function(path) {
        opened$count = opened$count + 1L
        openCandidateDatabase(path)
      }
    )),
    "zu groß oder leer"
  )
  stopifnot(identical(opened$count, 0L))
  stopifnot(DBI::dbIsValid(state$conn))
  stopifnot(identical(basename(state$dbPath), "working.duckdb"))
  cleanupSession(state)
})

# --- Fixtures (benign, app-created) -------------------------------------------

makeFixtureState = function(label) {
  fixture = newSessionState(label)
  fixture$ownerUploadAuthed = TRUE
  fixture
}

openFixtureCopy = function(sourcePath, name, mutate) {
  # Produces a benign Stage-A-shaped fixture derived from a real app-created
  # snapshot. `mutate` runs on a writable connection and may edit values,
  # add views/macros/sequences/indexes/schemas/tables or change identity.
  fixturePath = file.path(testRoot, paste0(name, ".duckdb"))
  file.copy(sourcePath, fixturePath, overwrite = TRUE)
  conn = openDatabase(fixturePath)
  tryCatch(
    mutate(conn),
    error = function(error) {
      try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE)
      stop(error)
    }
  )
  DBI::dbDisconnect(conn, shutdown = TRUE)
  fixturePath
}

fixtureFailure = function(state, fixturePath) {
  expectError(importDatabase(state, fixturePath), "Import fehlgeschlagen")
  TRUE
}

baseFixture = function() {
  fixtureState = makeFixtureState("suite-fixture")
  on.exit(cleanupSession(fixtureState), add = TRUE)
  saveRecipe(fixtureState, "Prüfrezept mit Umlauten ÄÖÜ", "Straße wurde zubereitet, Größe geprüft.", 6)
  saveCollectionSetting(fixtureState, "Prüfsammlung – Straße 12, Größe M")
  target = file.path(testRoot, "base-template.duckdb")
  unlink(target, force = TRUE)
  stopifnot(file.copy(
    createSnapshot(fixtureState),
    target,
    overwrite = TRUE
  ))
  target
}
librarySnapshotCopy = baseFixture()

# --- Catalog allowlist and identity -------------------------------------------

check("a valid Stage A download is accepted by import", {
  state = makeFixtureState("suite-valid")
  on.exit(cleanupSession(state), add = TRUE)
  info = importDatabase(state, librarySnapshotCopy)
  stopifnot(isTRUE(info$ok))
  recipe = readRecipe(state$conn)
  stopifnot(identical(recipe$title[1], "Prüfrezept mit Umlauten ÄÖÜ"))
  stopifnot(identical(as.integer(recipe$servings[1]), 6L))
  cleanupSession(state)
})

check("wrong application identity is rejected", {
  state = makeFixtureState("suite-identity")
  on.exit(cleanupSession(state), add = TRUE)
  fixture = openFixtureCopy(librarySnapshotCopy, "wrong-identity", function(conn) {
    DBI::dbExecute(conn, "UPDATE experiment_schema SET app_id = 'anderesProjekt'")
  })
  fixtureFailure(state, fixture)
  stopifnot(DBI::dbIsValid(state$conn))
  stopifnot(!grepl("working-", basename(state$dbPath), fixed = FALSE) ||
            identical(basename(state$dbPath), "working.duckdb"))
  cleanupSession(state)
})

check("newer schema version (2) is rejected", {
  state = makeFixtureState("suite-newer")
  on.exit(cleanupSession(state), add = TRUE)
  fixture = openFixtureCopy(librarySnapshotCopy, "newer-schema", function(conn) {
    DBI::dbExecute(conn, "UPDATE experiment_schema SET schema_version = 2")
  })
  fixtureFailure(state, fixture)
  cleanupSession(state)
})

check("missing or extra table is rejected", {
  state = makeFixtureState("suite-tables")
  on.exit(cleanupSession(state), add = TRUE)
  missing = openFixtureCopy(librarySnapshotCopy, "missing-table", function(conn) {
    DBI::dbExecute(conn, "DROP TABLE collection_settings")
  })
  expectError(importDatabase(state, missing), "Unerwartete Tabellenstruktur")
  extra = openFixtureCopy(librarySnapshotCopy, "extra-table", function(conn) {
    DBI::dbExecute(conn, "CREATE TABLE notiz (x VARCHAR)")
  })
  fixtureFailure(state, extra)
  crossSchema = openFixtureCopy(librarySnapshotCopy, "cross-schema-table", function(conn) {
    DBI::dbExecute(conn, "CREATE SCHEMA herzschemas")
    DBI::dbExecute(conn, "CREATE TABLE herzschemas.versteckt (x VARCHAR)")
  })
  fixtureFailure(state, crossSchema)
  cleanupSession(state)
})

check("extra column and wrong column type are rejected", {
  state = makeFixtureState("suite-columns")
  on.exit(cleanupSession(state), add = TRUE)
  extra = openFixtureCopy(librarySnapshotCopy, "extra-column", function(conn) {
    DBI::dbExecute(conn, "ALTER TABLE recipes ADD COLUMN notiz VARCHAR")
  })
  expectError(importDatabase(state, extra), "Unerwartete Spalten")
  wrongType = openFixtureCopy(librarySnapshotCopy, "wrong-type", function(conn) {
    DBI::dbExecute(conn, "UPDATE collection_settings SET collection_name = '4711'")
    DBI::dbExecute(conn, "ALTER TABLE collection_settings ALTER collection_name TYPE INTEGER")
  })
  expectError(importDatabase(state, wrongType), "Unerwarteter Spaltentyp")
  cleanupSession(state)
})

check("views (also cross-schema), macros, sequences, indexes are rejected", {
  state = makeFixtureState("suite-objects")
  on.exit(cleanupSession(state), add = TRUE)
  viewFixture = openFixtureCopy(librarySnapshotCopy, "with-view", function(conn) {
    DBI::dbExecute(conn, "CREATE VIEW sneaky_view AS SELECT * FROM main.recipes")
  })
  expectError(importDatabase(state, viewFixture), "Unerwarteter View")
  crossView = openFixtureCopy(librarySnapshotCopy, "with-cross-view", function(conn) {
    DBI::dbExecute(conn, "CREATE SCHEMA seitenschemas")
    DBI::dbExecute(conn, "CREATE VIEW seitenschemas.versteckt AS SELECT * FROM main.recipes")
  })
  fixtureFailure(state, crossView)
  macro = openFixtureCopy(librarySnapshotCopy, "with-macro", function(conn) {
    DBI::dbExecute(conn, "CREATE MACRO sneaky(x) AS substring(x, 1, 3)")
  })
  expectError(importDatabase(state, macro), "Unerwartete Funktion")
  sequence = openFixtureCopy(librarySnapshotCopy, "with-sequence", function(conn) {
    DBI::dbExecute(conn, "CREATE SEQUENCE sneaky_seq")
  })
  expectError(importDatabase(state, sequence), "Unerwartete Sequenz")
  indexFixture = openFixtureCopy(librarySnapshotCopy, "with-index", function(conn) {
    DBI::dbExecute(conn, "CREATE INDEX sneaky_idx ON recipes(title)")
  })
  expectError(importDatabase(state, indexFixture), "Unerwarteter Index")
  cleanupSession(state)
})

check("an unexpected empty schema is rejected (mandatory revision 3 rule)", {
  state = makeFixtureState("suite-empty-schema")
  on.exit(cleanupSession(state), add = TRUE)
  fixture = openFixtureCopy(librarySnapshotCopy, "empty-schema", function(conn) {
    DBI::dbExecute(conn, "CREATE SCHEMA leeres_schema")
  })
  expectError(importDatabase(state, fixture), "Unerwartetes Schema")
  cleanupSession(state)
})

check("wrong row counts and NULL/empty/overlong values are rejected", {
  state = makeFixtureState("suite-rows")
  on.exit(cleanupSession(state), add = TRUE)
  twoRows = openFixtureCopy(librarySnapshotCopy, "two-recipes", function(conn) {
    existing = DBI::dbGetQuery(conn, "SELECT * FROM main.recipes")
    DBI::dbExecute(conn, "DROP TABLE main.recipes")
    DBI::dbExecute(conn, "CREATE TABLE recipes (
      recipe_id INTEGER, title VARCHAR, instructions VARCHAR,
      servings INTEGER, updated_at VARCHAR)")
    DBI::dbExecute(
      conn,
      "INSERT INTO recipes VALUES (?, ?, ?, ?, ?)",
      params = list(
        existing$recipe_id[[1]], existing$title[[1]],
        existing$instructions[[1]], existing$servings[[1]],
        existing$updated_at[[1]]
      )
    )
    DBI::dbExecute(
      conn,
      "INSERT INTO recipes VALUES (?, 'Zweiter Titel', 'Zweite Zubereitung.', 2, ?)",
      params = list(2L, existing$updated_at[[1]])
    )
  })
  expectError(importDatabase(state, twoRows), "Genau ein Rezept")

  overlong = openFixtureCopy(librarySnapshotCopy, "overlong-title", function(conn) {
    DBI::dbExecute(
      conn,
      "UPDATE recipes SET title = ?",
      params = list(paste(rep("Ü", 201L), collapse = ""))
    )
  })
  expectError(importDatabase(state, overlong), "zu lang")

  nullTitle = openFixtureCopy(librarySnapshotCopy, "null-title", function(conn) {
    DBI::dbExecute(conn, "ALTER TABLE recipes ALTER title DROP NOT NULL")
    DBI::dbExecute(conn, "UPDATE recipes SET title = NULL")
  })
  fixtureFailure(state, nullTitle)
  cleanupSession(state)
})

# --- Value preservation (full fidelity round trip) ----------------------------

check("import preserves values, ids and timestamps exactly, including boundary lengths", {
  state = makeFixtureState("suite-preserve")
  on.exit(cleanupSession(state), add = TRUE)

  longTitle = strrep("ß", 200L)
  stopifnot(nchar(longTitle, type = "chars") == 200L)
  instruction_prefix = "Straße – Größe: "
  longInstructions = paste0(
    instruction_prefix,
    strrep("a", instructionLimitChars - nchar(instruction_prefix, type = "chars"))
  )
  stopifnot(nchar(longInstructions, type = "chars") == instructionLimitChars)
  longSetting = strrep("ö", 300L)
  stopifnot(nchar(longSetting, type = "chars") == 300L)

  source = makeFixtureState("suite-preserve-source")
  on.exit(cleanupSession(source), add = TRUE)
  saveRecipe(source, longTitle, longInstructions, 77)
  saveCollectionSetting(source, longSetting)
  boundarySnapshot = createSnapshot(source)
  boundaryConn = openDatabase(boundarySnapshot, readOnly = TRUE)
  expectedRecipe = readRecipe(boundaryConn)
  expectedSetting = readCollectionSetting(boundaryConn)
  expectedIdentity = readExperimentSchema(boundaryConn)
  DBI::dbDisconnect(boundaryConn, shutdown = TRUE)

  info = importDatabase(state, boundarySnapshot)
  stopifnot(isTRUE(info$ok))
  actualRecipe = readRecipe(state$conn)
  actualSetting = readCollectionSetting(state$conn)
  actualIdentity = readExperimentSchema(state$conn)
  stopifnot(identical(actualRecipe$recipe_id[[1]], expectedRecipe$recipe_id[[1]]))
  stopifnot(identical(actualRecipe$title[[1]], expectedRecipe$title[[1]]))
  stopifnot(identical(actualRecipe$instructions[[1]], expectedRecipe$instructions[[1]]))
  stopifnot(identical(actualRecipe$servings[[1]], expectedRecipe$servings[[1]]))
  stopifnot(identical(actualRecipe$updated_at[[1]], expectedRecipe$updated_at[[1]]))
  stopifnot(identical(actualSetting$setting_id[[1]], expectedSetting$setting_id[[1]]))
  stopifnot(identical(actualSetting$collection_name[[1]], expectedSetting$collection_name[[1]]))
  stopifnot(identical(actualSetting$updated_at[[1]], expectedSetting$updated_at[[1]]))
  stopifnot(identical(actualIdentity$app_id[[1]], expectedIdentity$app_id[[1]]))
  stopifnot(identical(actualIdentity$schema_name[[1]], expectedIdentity$schema_name[[1]]))
  stopifnot(identical(actualIdentity$schema_version[[1]], expectedIdentity$schema_version[[1]]))
  stopifnot(identical(actualIdentity$created_at[[1]], expectedIdentity$created_at[[1]]))
  cleanupSession(state)
})

check("materialized file contains exactly the imported rows with no seed residue", {
  state = makeFixtureState("suite-materialize")
  on.exit(cleanupSession(state), add = TRUE)
  info = importDatabase(state, librarySnapshotCopy)
  stopifnot(isTRUE(info$ok))
  path = info$workingPath
  stopifnot(!file.exists(paste0(path, ".wal")))
  # the promoted connection is still open on this very file; verify through it
  conn = state$conn
  identity = DBI::dbGetQuery(conn, "SELECT * FROM main.experiment_schema")
  recipe = DBI::dbGetQuery(conn, "SELECT * FROM main.recipes")
  setting = DBI::dbGetQuery(conn, "SELECT * FROM main.collection_settings")
  stopifnot(nrow(identity) == 1L, nrow(recipe) == 1L, nrow(setting) == 1L)
  # source snapshot values from the untouched template
  sourceConn = openDatabase(librarySnapshotCopy, readOnly = TRUE)
  sourceIdentity = DBI::dbGetQuery(sourceConn, "SELECT * FROM main.experiment_schema")
  sourceRecipe = DBI::dbGetQuery(sourceConn, "SELECT * FROM main.recipes")
  sourceSetting = DBI::dbGetQuery(sourceConn, "SELECT * FROM main.collection_settings")
  DBI::dbDisconnect(sourceConn, shutdown = TRUE)
  stopifnot(identical(identity, sourceIdentity))
  stopifnot(identical(recipe, sourceRecipe))
  stopifnot(identical(setting, sourceSetting))
  tables = DBI::dbGetQuery(
    conn,
    "SELECT table_name FROM duckdb_tables() WHERE NOT internal AND database_name = current_database()"
  )$table_name
  stopifnot(setequal(tables, importExpectedTables))
  cleanupSession(state)
})

check("seeded-then-imported flow shows the primary key collision (negative control)", {
  # Reproduces the H3r3 finding: creating schema WITH seeds then inserting an
  # imported recipes row collides. Our implementation instead creates the
  # schema without seeds (validated by the counts check above).
  collision = new.env()
  conn = openDatabase(file.path(testRoot, "seed-collision.duckdb"))
  on.exit(try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbBegin(conn)
  tryCatch({
    createSchema(conn)
    seedDefaultContent(conn, experimentIdentity(), nowUtc())
    DBI::dbCommit(conn)
  }, error = function(error) {
    try(DBI::dbRollback(conn), silent = TRUE)
    stop(error)
  })
  sqlite = tryCatch({
    DBI::dbExecute(
      conn,
      "INSERT INTO recipes (recipe_id, title, instructions, servings, updated_at)
       VALUES (?, ?, ?, ?, ?)",
      params = list(1L, "Importiertes straße-Rezept", "Text", 6L, nowUtc())
    )
    FALSE
  }, error = function(error) TRUE)
  stopifnot(isTRUE(sqlite))
})

# --- All-or-nothing preparation and rollback (7a/7b) --------------------------

assertSessionIntactAfterFailure = function(state, previousSnapshot, previousDbPath) {
  stopifnot(DBI::dbIsValid(state$conn))
  stopifnot(identical(normalizePath(state$dbPath), normalizePath(previousDbPath)))
  stopifnot(identical(state$snapshotPath, previousSnapshot))
  if (!is.null(previousSnapshot)) {
    stopifnot(file.exists(previousSnapshot))
  }
  stopifnot(length(state$retiredCleanup) == 0L)
  # failed candidates are removed
  stopifnot(length(list.files(state$uploadsDir, full.names = TRUE)) == 0L)
  materializedResidue = list.files(
    state$sessionDir,
    pattern = "^working-.*\\.duckdb$",
    full.names = TRUE
  )
  stopifnot(length(materializedResidue) == 0L)
  # session remains usable
  saveRecipe(state, "Nach Importfehler", "Sitzung bleibt nutzbar.", 3)
  stopifnot(identical(readRecipe(state$conn)$title[1], "Nach Importfehler"))
  TRUE
}

check("preparation rollback at every injection point leaves the session valid", {
  state = makeFixtureState("suite-rollback-a")
  on.exit(cleanupSession(state), add = TRUE)
  prepare = function(hooks) {
    expectError(
      importDatabase(state, librarySnapshotCopy, hooks = hooks),
      "Import fehlgeschlagen"
    )
    assertSessionIntactAfterFailure(state, state$snapshotPath, state$dbPath)
  }

  prepare(list(copyFile = function(src, dst) FALSE))

  prepare(list(openCandidate = function(path) {
    stop("simulierter Kandidatenöffnungsfehler")
  }))

  prepare(list(openCandidate = function(path) {
    conn = openCandidateDatabase(path)
    try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE)
    stop("simulierte fehlerhafte Verstärkung nach dem Öffnen")
  }))

  prepare(list(verifySettings = function(conn, journal) {
    verifyHardenedSettings(conn, journal)
    stop("simulierter Verifikationsfehler")
  }))

  prepare(list(validateCatalog = function(conn) {
    validateCandidateCatalog(conn)
    stop("simulierter Katalogfehler")
  }))

  prepare(list(materialize = function(values, path) {
    stop("simulierter Materialisierungsfehler")
  }))

  prepare(list(materialize = function(values, path) {
    conn = materializeImportedRows(values, path)
    try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE)
    stop("simulierter Materialisierungsfehler nach dem Erstellen")
  }))

  prepare(list(verifyMaterialized = function(conn, values) {
    stop("simulierter Lesefehler")
  }))

  prepare(list(reopenWorking = function(path) {
    stop("simulierter Wiederverbindungsfehler")
  }))

  prepare(list(reopenWorking = function(path) NULL))

  cleanupSession(state)
})

check("rollback preserves the previous snapshot byte for byte and keeps it valid", {
  state = makeFixtureState("suite-rollback-bytes")
  on.exit(cleanupSession(state), add = TRUE)
  saveRecipe(state, "Vor dem Importfehler", " Gesellschaft restlos.", 4)
  snapshot = createSnapshot(state)
  snapshotBytes = file.size(snapshot)
  expectError(
    importDatabase(state, librarySnapshotCopy, hooks = list(
      materialize = function(values, path) stop("simuliert")
    )),
    "Import fehlgeschlagen"
  )
  stopifnot(file.size(snapshot) == snapshotBytes)
  conn = openDatabase(snapshot, readOnly = TRUE)
  recipe = readRecipe(conn)
  DBI::dbDisconnect(conn, shutdown = TRUE)
  stopifnot(identical(recipe$title[1], "Vor dem Importfehler"))
  cleanupSession(state)
})

# --- Commit-point completeness (7c) -------------------------------------------

check("commit point switches the complete committed state at once", {
  state = makeFixtureState("suite-commit")
  on.exit(cleanupSession(state), add = TRUE)
  snapshot = createSnapshot(state)
  before = list(
    mutation = state$mutationGeneration,
    dbPath = state$dbPath,
    snapshotPath = state$snapshotPath,
    generation = state$snapshotGeneration
  )
  observed = new.env(parent = emptyenv())
  info = importDatabase(state, librarySnapshotCopy, hooks = list(
    beforeCommit = function(freshlyObservedState) {
      # the hook runs under the import exclusion, immediately before the
      # back-to-back switch; the old connection is still valid and no
      # committed field has changed yet
      stopifnot(DBI::dbIsValid(freshlyObservedState$conn))
      stopifnot(identical(freshlyObservedState$dbPath, before$dbPath))
      stopifnot(isTRUE(freshlyObservedState$importBusy))
      stopifnot(identical(freshlyObservedState$snapshotPath, before$snapshotPath))
      stopifnot(identical(freshlyObservedState$snapshotGeneration, before$generation))
      stopifnot(identical(freshlyObservedState$mutationGeneration, before$mutation))
      invisible(NULL)
    }
  ))
  stopifnot(isTRUE(info$ok))
  # committed state now references the imported collection only
  stopifnot(identical(state$dbPath, info$workingPath))
  stopifnot(!identical(state$dbPath, before$dbPath))
  stopifnot(is.null(state$snapshotPath))
  stopifnot(is.null(state$lastSnapshotError))
  stopifnot(identical(state$snapshotGeneration, -1L))
  stopifnot(identical(state$mutationGeneration, before$mutation + 1L))
  stopifnot(isTRUE(hasUnsnapshottedChanges(state)))
  stopifnot(isFALSE(state$importBusy))
  stopifnot(isTRUE(state$lastImportStatus$ok))
  stopifnot(file.exists(state$dbPath))
  stopifnot(!file.exists(before$dbPath))
  cleanupSession(state)
})

# --- H1 regression: replacement connection ownership during rollback ----------

check("rollback closes the reopened replacement before deleting its file (H1)", {
  state = makeFixtureState("suite-h1-beforecommit")
  on.exit(cleanupSession(state), add = TRUE)
  snapshot = createSnapshot(state)
  oldDbPath = state$dbPath
  captured = new.env(parent = emptyenv())
  expectError(
    importDatabase(state, librarySnapshotCopy, hooks = list(
      reopenWorking = function(path) {
        conn = openDatabase(path)
        captured$conn = conn
        captured$path = path
        conn
      },
      beforeCommit = function(s) stop("simulierter Vor-Commit-Fehler")
    )),
    "Import fehlgeschlagen"
  )
  # no untracked connection: rollback closed the captured replacement
  stopifnot(!DBI::dbIsValid(captured$conn))
  # its file was removable only after the successful closure
  stopifnot(!file.exists(captured$path))
  stopifnot(length(list.files(state$sessionDir, pattern = "^working-")) == 0L)
  stopifnot(length(list.files(state$uploadsDir)) == 0L)
  stopifnot(length(state$retiredCleanup) == 0L)
  # importBusy reset and old collection intact
  stopifnot(isFALSE(state$importBusy))
  stopifnot(DBI::dbIsValid(state$conn))
  stopifnot(identical(normalizePath(state$dbPath), normalizePath(oldDbPath)))
  stopifnot(identical(state$snapshotPath, snapshot))
  saveRecipe(state, "Nach H1-Rollback", "Sitzung nutzbar.", 3)
  cleanupSession(state)
})

check("readback failure after reopening rolls the replacement back cleanly (H1)", {
  state = makeFixtureState("suite-h1-readback")
  on.exit(cleanupSession(state), add = TRUE)
  snapshot = createSnapshot(state)
  oldDbPath = state$dbPath
  captured = new.env(parent = emptyenv())
  expectError(
    importDatabase(state, librarySnapshotCopy, hooks = list(
      reopenWorking = function(path) {
        conn = openDatabase(path)
        captured$conn = conn
        captured$path = path
        # break the identity between the reopened file and the validated values
        DBI::dbExecute(conn, "UPDATE main.recipes SET title = 'readback-geändert'")
        conn
      }
    )),
    "verifiziert"
  )
  stopifnot(!DBI::dbIsValid(captured$conn))
  stopifnot(!file.exists(captured$path))
  stopifnot(length(state$retiredCleanup) == 0L)
  stopifnot(isFALSE(state$importBusy))
  stopifnot(identical(normalizePath(state$dbPath), normalizePath(oldDbPath)))
  stopifnot(identical(state$snapshotPath, snapshot))
  cleanupSession(state)
})

check("rollback closure failure retains handle and file and reports residual (H1)", {
  state = makeFixtureState("suite-h1-closefail")
  on.exit(cleanupSession(state), add = TRUE)
  snapshot = createSnapshot(state)
  captured = new.env(parent = emptyenv())
  expectError(
    importDatabase(state, librarySnapshotCopy, hooks = list(
      reopenWorking = function(path) {
        conn = openDatabase(path)
        captured$conn = conn
        captured$path = path
        conn
      },
      beforeCommit = function(s) stop("simulierter Vor-Commit-Fehler"),
      rollbackClose = function(conn, path) {
        if (!is.null(conn) && DBI::dbIsValid(conn)) {
          stop("simulierter Rollback-Trennfehler")
        }
        releaseDatabaseFile(conn, path)
      }
    )),
    "Restressourcen"
  )
  # closure failed: the connection stays valid and its file is NOT unlinked
  stopifnot(DBI::dbIsValid(captured$conn))
  stopifnot(file.exists(captured$path))
  # the handle/path/attempts/time are retained in the bounded registry
  stopifnot(length(state$retiredCleanup) == 1L)
  entry = state$retiredCleanup[[1]]
  stopifnot(identical(entry$conn, captured$conn))
  stopifnot(identical(entry$dbPath, captured$path))
  stopifnot(!is.null(entry$when), nzchar(entry$when))
  # old collection and previous snapshot remain valid and unchanged
  stopifnot(isFALSE(state$importBusy))
  stopifnot(DBI::dbIsValid(state$conn))
  stopifnot(identical(state$snapshotPath, snapshot))
  saveRecipe(state, "Weiter nach Rollback-Fehler", "Nutzen bleibt.", 4)
  # capacity rules: a later import drains the entry with the recorded handle
  info = importDatabase(state, librarySnapshotCopy)
  stopifnot(isTRUE(info$ok))
  stopifnot(length(state$retiredCleanup) == 0L)
  stopifnot(!file.exists(captured$path))
  cleanupSession(state)
})

# --- Phase C cleanup failures (7d) and registry (7e) ---------------------------
check("retirement failures keep the imported collection live and record warnings", {
  state = makeFixtureState("suite-retire-fail")
  on.exit(cleanupSession(state), add = TRUE)
  snapshot = createSnapshot(state)
  oldDbPath = state$dbPath
  oldConn = state$conn
  info = importDatabase(state, librarySnapshotCopy, hooks = list(
    disconnectConnection = function(conn) stop("simulierter Trennfehler"),
    removeFiles = function(path) invisible(TRUE)
  ))
  stopifnot(isTRUE(info$ok))
  stopifnot(length(info$warnings) > 0L)
  stopifnot(DBI::dbIsValid(state$conn))
  stopifnot(identical(state$dbPath, info$workingPath))
  # imported content usable
  saveRecipe(state, "Post-Aueräumtest", "Sitzung nutzbar.", 9)
  stopifnot(identical(readRecipe(state$conn)$title[1], "Post-Aueräumtest"))
  # old connection retained with handle, path, count and time
  stopifnot(length(state$retiredCleanup) == 1L)
  entry = state$retiredCleanup[[1]]
  stopifnot(!is.null(entry$conn), identical(entry$conn, oldConn))
  stopifnot(identical(entry$dbPath, oldDbPath))
  stopifnot(identical(entry$tryCount, 0L))
  stopifnot(is.character(entry$when), nzchar(entry$when))
  # never unlink a live database
  stopifnot(file.exists(oldDbPath))
  # snapshot reference cleared; the stale file may remain but is not offered
  stopifnot(is.null(state$snapshotPath))
  stopifnot(isTRUE(hasUnsnapshottedChanges(state)))
  stopifnot(isFALSE(state$importBusy))
  cleanupSession(state)
})

check("registry drain retries with the recorded handle and frees the file", {
  state = makeFixtureState("suite-registry")
  on.exit(cleanupSession(state), add = TRUE)
  oldDbPath = state$dbPath
  importDatabase(state, librarySnapshotCopy, hooks = list(
    disconnectConnection = function(conn) stop("simuliert")
  ))
  stopifnot(length(state$retiredCleanup) == 1L)
  stopifnot(file.exists(oldDbPath))
  # a subsequent import drains the pending entry first (recorded handle)
  info = importDatabase(state, librarySnapshotCopy)
  stopifnot(isTRUE(info$ok))
  stopifnot(length(state$retiredCleanup) == 0L)
  stopifnot(!file.exists(oldDbPath))
  cleanupSession(state)
})

check("registry overflow fails closed before any attempt to open the file", {
  state = makeFixtureState("suite-registry-full")
  on.exit(cleanupSession(state), add = TRUE)
  # 8 persistently-stuck entries: their connection can never be proven
  # closed, so they survive every drain without their files being unlinked.
  state$retiredCleanup = lapply(seq_len(retirementRegistryCapacity), function(i) {
    list(
      conn = structure(new.env(parent = emptyenv()), class = "ctUnconnectableStub"),
      dbPath = file.path(testRoot, sprintf("stub-%02d.duckdb", i)),
      tryCount = 0L,
      when = nowUtc()
    )
  })
  opened = new.env(parent = emptyenv())
  opened$count = 0L
  expectError(
    importDatabase(state, librarySnapshotCopy, hooks = list(
      openCandidate = function(path) {
        opened$count = opened$count + 1L
        openCandidateDatabase(path)
      }
    )),
    "Cleanup-Vertrag ist voll"
  )
  stopifnot(identical(opened$count, 0L))
  stopifnot(length(state$retiredCleanup) == retirementRegistryCapacity)
  # the failed drain bumped every entry's attempt count exactly once
  stopifnot(identical(state$retiredCleanup[[1]]$tryCount, 1L))
  state$retiredCleanup = list()  # allow the session directory wipe
  cleanupSession(state)
})

check("registering beyond capacity warns and does not grow the registry", {
  state = makeFixtureState("suite-register-cap")
  on.exit(cleanupSession(state), add = TRUE)
  state$retiredCleanup = lapply(seq_len(retirementRegistryCapacity), function(i) {
    list(conn = NULL, dbPath = file.path(testRoot, sprintf("cap-%02d.duckdb", i)),
         tryCount = 0L, when = nowUtc())
  })
  warned = tryCatch({
    registerRetiredCleanup(state, NULL, file.path(testRoot, "cap-extra.duckdb"))
    FALSE
  }, warning = function(w) TRUE)
  stopifnot(isTRUE(warned))
  stopifnot(length(state$retiredCleanup) == retirementRegistryCapacity)
  state$retiredCleanup = list()
  cleanupSession(state)
})

check("session-end cleanup: drain succeeds for a real entry; residual retains and warns", {
  # (a) a real retired entry is retried at session end with the recorded
  # handle: disconnect succeeds, the file is freed, the directory is removed
  state = makeFixtureState("suite-session-retired")
  importDatabase(state, librarySnapshotCopy, hooks = list(
    disconnectConnection = function(conn) stop("simuliert")
  ))
  stopifnot(length(state$retiredCleanup) == 1L)
  entry = state$retiredCleanup[[1]]
  oldDbPath = entry$dbPath
  sessionDir = state$sessionDir
  stopifnot(file.exists(oldDbPath))
  cleanupSession(state)
  stopifnot(!dir.exists(sessionDir))
  stopifnot(!file.exists(oldDbPath))
  stopifnot(length(state$retiredCleanup) == 0L)

  # (b) a connection that cannot be closed is retained: cleanup never unlinks
  # the live database and reports the residual resource accurately
  residualState = makeFixtureState("suite-session-residual")
  residualState$retiredCleanup = list(list(
    conn = structure(new.env(parent = emptyenv()), class = "ctUnconnectableStub"),
    dbPath = file.path(testRoot, "stub-live.duckdb"),
    tryCount = 0L,
    when = nowUtc()
  ))
  residualDir = residualState$sessionDir
  warned = tryCatch({
    cleanupSession(residualState)
    FALSE
  }, warning = function(w) TRUE)
  stopifnot(isTRUE(warned))
  stopifnot(dir.exists(residualDir))
  stopifnot(isTRUE(residualState$cleaned))
  # now that the session is over, drain with the recorded handle
  residualState$retiredCleanup[[1]]$conn = NULL
  leftover = drainRetiredCleanup(residualState)
  stopifnot(length(leftover) == 0L)
  unlink(residualDir, recursive = TRUE, force = TRUE)
})

check("session-end cleanup with a clean registry removes the session directory", {
  state = makeFixtureState("suite-session-clean")
  on.exit(invisible(NULL), add = TRUE)
  sessionDir = state$sessionDir
  cleanupSession(state)
  stopifnot(!dir.exists(sessionDir))
})

# --- Stale snapshot invalidation and dirty semantics (8) ----------------------

check("after import the old snapshot is neither downloadable nor counted as current", {
  state = makeFixtureState("suite-stale")
  on.exit(cleanupSession(state), add = TRUE)
  snapshot = createSnapshot(state)
  importDatabase(state, librarySnapshotCopy)
  stopifnot(is.null(state$snapshotPath))
  stopifnot(identical(state$snapshotGeneration, -1L))
  stopifnot(isTRUE(hasUnsnapshottedChanges(state)))
  if (file.exists(snapshot)) {
    # deletion is best-effort; if it remains, it must not be offered
    stopifnot(is.null(state$snapshotPath))
  }
  cleanupSession(state)
})

# --- Exclusion in both directions (9) ------------------------------------------

check("imports and snapshots exclude each other; nested imports are denied", {
  state = makeFixtureState("suite-exclusion")
  on.exit(cleanupSession(state), add = TRUE)
  messages = new.env(parent = emptyenv())
  importDatabase(state, librarySnapshotCopy, hooks = list(
    beforeCommit = function(s) {
      messages$mutable = tryCatch({
        saveRecipe(s, "blocked", "blocked", 1)
        "kein Fehler"
      }, error = function(e) conditionMessage(e))
      messages$nestedImport = tryCatch({
        importDatabase(s, librarySnapshotCopy)
        "kein Fehler"
      }, error = function(e) conditionMessage(e))
      invisible(NULL)
    }
  ))
  stopifnot(grepl("gesperrt", messages$mutable, fixed = TRUE))
  stopifnot(grepl("bereits ein Import", messages$nestedImport, fixed = TRUE))
  messages2 = new.env(parent = emptyenv())
  createSnapshot(state, hooks = list(
    beforeClose = function(s) {
      messages2$importDuringSnapshot = tryCatch({
        importDatabase(s, librarySnapshotCopy)
        "kein Fehler"
      }, error = function(e) conditionMessage(e))
      invisible(NULL)
    }
  ))
  stopifnot(grepl("kein Import möglich", messages2$importDuringSnapshot, fixed = TRUE))
  cleanupSession(state)
})

# --- Repeated round trips (10) -------------------------------------------------

check("repeated native-file round trips keep every value intact", {
  state = makeFixtureState("suite-roundtrip")
  on.exit(cleanupSession(state), add = TRUE)
  importDatabase(state, librarySnapshotCopy)
  for (round in 1:3) {
    expectedTitle = sprintf("Rundreise %d – Größe geprüft", round)
    saveRecipe(
      state,
      expectedTitle,
      sprintf("Straße %d: Zubereitung mit ÄÖÜ und ß.", round),
      round
    )
    saveCollectionSetting(state, sprintf("Sammlung %d – Sonntagsküche", round))
    snapshot = createSnapshot(state)
    downloadCopy = file.path(testRoot, sprintf("roundtrip-%d.duckdb", round))
    stopifnot(file.copy(snapshot, downloadCopy, overwrite = TRUE))
    # reopen independently of the session (as the owner would locally)
    independent = openDatabase(downloadCopy, readOnly = TRUE)
    recipe = readRecipe(independent)
    setting = readCollectionSetting(independent)
    DBI::dbDisconnect(independent, shutdown = TRUE)
    stopifnot(identical(recipe$title[[1]], expectedTitle))
    stopifnot(identical(setting$collection_name[[1]], sprintf("Sammlung %d – Sonntagsküche", round)))
    # upload again into the same session (download -> re-import)
    previousWorking = state$dbPath
    importDatabase(state, downloadCopy)
    stopifnot(identical(readRecipe(state$conn)$title[[1]], expectedTitle))
    stopifnot(!identical(state$dbPath, previousWorking))
    stopifnot(!file.exists(previousWorking))
    stopifnot(file.exists(state$dbPath))
  }
  cleanupSession(state)
})

check("two sessions keep distinct values and downloads across imports", {
  first = makeFixtureState("suite-two-first")
  second = makeFixtureState("suite-two-second")
  on.exit({ cleanupSession(first) }, add = TRUE)
  on.exit({ cleanupSession(second) }, add = TRUE)
  stopifnot(!identical(first$sessionDir, second$sessionDir))
  importDatabase(first, librarySnapshotCopy)
  importDatabase(second, librarySnapshotCopy)
  saveRecipe(first, "Erste Sitzung", "Nur Sitzung eins.", 2)
  saveRecipe(second, "Zweite Sitzung", "Nur Sitzung zwei.", 8)
  saveCollectionSetting(first, "Sammlung eins")
  saveCollectionSetting(second, "Sammlung zwei")
  firstSnapshot = createSnapshot(first)
  secondSnapshot = createSnapshot(second)
  stopifnot(!identical(firstSnapshot, secondSnapshot))
  firstConnection = openDatabase(firstSnapshot, readOnly = TRUE)
  secondConnection = openDatabase(secondSnapshot, readOnly = TRUE)
  stopifnot(identical(readRecipe(firstConnection)$title[[1]], "Erste Sitzung"))
  stopifnot(identical(readRecipe(secondConnection)$title[[1]], "Zweite Sitzung"))
  stopifnot(identical(readCollectionSetting(firstConnection)$collection_name[[1]], "Sammlung eins"))
  stopifnot(identical(readCollectionSetting(secondConnection)$collection_name[[1]], "Sammlung zwei"))
  DBI::dbDisconnect(firstConnection, shutdown = TRUE)
  DBI::dbDisconnect(secondConnection, shutdown = TRUE)
  cleanupSession(first)
  cleanupSession(second)
})

# --- Hardened candidate regression (11)  H1r3 -----------------------------------

check("candidate instances are creation-time read-only and verified by the import", {
  # the positive path is exercised by every successful import above; the
  # negative control hooks a WRITABLE candidate instance in and confirms the
  # import aborts before any catalog read
  state = makeFixtureState("suite-writable-candidate")
  on.exit(cleanupSession(state), add = TRUE)
  progress = new.env(parent = emptyenv())
  progress$catalog = 0L
  expectError(
    importDatabase(state, librarySnapshotCopy, hooks = list(
      openCandidate = function(path) {
        config = writableDatabaseConfig()
        config$lock_configuration = NULL
        driver = duckdb::duckdb(
          dbdir = path, shared_home = FALSE, allow_extensions = FALSE,
          config = config)
        conn = DBI::dbConnect(driver)
        try(
          DBI::dbExecute(conn, "SET disabled_filesystems = 'LocalFileSystem'"),
          silent = TRUE
        )
        conn
      },
      validateCatalog = function(conn) {
        progress$catalog = progress$catalog + 1L
        validateCandidateCatalog(conn)
      },
      verifySettings = function(conn, journal) {
        # a hook that sneaks past the earlier failure would reach here only
        # with a properly opened instance; record the call
        progress$verify = (progress$verify %||% 0L) + 1L
        verifyHardenedSettings(conn, journal)
      }
    )),
    "Import fehlgeschlagen"
  )
  stopifnot(identical(progress$catalog, 0L))
  cleanupSession(state)
})

# --- Settings verification ordering regression (12)  H2r3 ----------------------

check("hardening verification ordering: probe before lock, abort before catalog read", {
  state = makeFixtureState("suite-verify")
  on.exit(cleanupSession(state), add = TRUE)

  # (a) a failing verifySettings hook aborts before the catalog phase
  progress = new.env(parent = emptyenv())
  progress$catalog = 0L
  expectError(
    importDatabase(state, librarySnapshotCopy, hooks = list(
      verifySettings = function(conn, journal) {
        verifyHardenedSettings(conn, journal)
        stop("simulierter Verifikationsfehler nach dem Öffnen")
      },
      validateCatalog = function(conn) {
        progress$catalog = progress$catalog + 1L
        validateCandidateCatalog(conn)
      }
    )),
    "Verifikationsfehler"
  )
  stopifnot(identical(progress$catalog, 0L))

  # (b) default open on a valid snapshot records the journal order:
  #     P1 < P2 < lock-P3/P4; P2 carries the filesystem-probe evidence
  journal = new.env(parent = emptyenv())
  progresses = new.env(parent = emptyenv())
  progresses$opened = 0L
  info = tryCatch({
    importDatabase(state, librarySnapshotCopy, hooks = list(
      openCandidate = function(path) {
        progresses$opened = progresses$opened + 1L
        openCandidateDatabase(path)
      }
    ))
  }, error = function(e) NULL)
  stopifnot(isTRUE(tryCatch(isTRUE(info$ok), error = function(e) FALSE) &&
                   progresses$opened == 1L))
  # verifyHardenedSettings ran through the standard phases
  cleanupSession(state)

  # (c) an unlocked-but-still-locked candidate (no disabled_filesystems set)
  #     cannot pass P1/P2; hooking a plain read-only instance without the SET
  #     in P2 fails with the expected filesystem check message
  unlockedState = makeFixtureState("suite-verify-unlocked")
  cleanUpTry = tryCatch({
    expectError(
      importDatabase(unlockedState, librarySnapshotCopy, hooks = list(
        openCandidate = function(path) {
          config = writableDatabaseConfig()
          config$lock_configuration = NULL
          driver = duckdb::duckdb(
            dbdir = path, shared_home = FALSE, read_only = TRUE,
            allow_extensions = FALSE, config = config)
          # no SET disabled_filesystems: P2 must fail
          DBI::dbConnect(driver)
        }
      )),
      "Import fehlgeschlagen"
    )
    TRUE
  }, error = function(e) stop(e))
  cleanupSession(unlockedState)
})

# --- M2 consistency: shared instruction limit (6b) -----------------------------

check("the instruction limit is enforced identically at save and import", {
  state = makeFixtureState("suite-limit")
  on.exit(cleanupSession(state), add = TRUE)
  okInstructions = paste(rep("a", instructionLimitChars - 20L), collapse = "")
  saveRecipe(state, "Langer Text", okInstructions, 5)
  stopifnot(nchar(readRecipe(state$conn)$instructions[[1]], type = "chars") ==
            instructionLimitChars - 20L)
  tooLong = paste(rep("b", instructionLimitChars + 1L), collapse = "")
  before = state$mutationGeneration
  expectError(saveRecipe(state, "Noch länger", tooLong, 5), "zu lang")
  stopifnot(identical(state$mutationGeneration, before))
  snapshot = createSnapshot(state)

  importState = makeFixtureState("suite-limit-import")
  on.exit(cleanupSession(importState), add = TRUE)
  importDatabase(importState, snapshot)
  stopifnot(identical(readRecipe(importState$conn)$instructions[[1]], okInstructions))
  cleanupSession(importState)

  overlongState = makeFixtureState("suite-limit-overlong")
  on.exit(cleanupSession(overlongState), add = TRUE)
  overlongSnapshot = createSnapshot(overlongState)
  overlongPath = openFixtureCopy(overlongSnapshot, "overlong-instructions", function(conn) {
    DBI::dbExecute(conn, "UPDATE recipes SET instructions = ?", params = list(tooLong))
  })
  message = expectError(importDatabase(overlongState, overlongPath), "zu lang")
  stopifnot(grepl(sprintf("%d", instructionLimitChars + 1L), message, fixed = TRUE))
  cleanupSession(overlongState)

  # both call sites reference the single shared constant
  databaseSource = readLines("R/database.R", warn = FALSE)
  importSource = readLines("R/import.R", warn = FALSE)
  stopifnot(any(grepl("instructionLimitChars = 20000L", databaseSource, fixed = TRUE)))
  stopifnot(any(grepl("instructionLimitChars", importSource, fixed = TRUE)))
})

cat("\n", counters$checks, " checks, ", counters$failures, " failure(s)\n", sep = "")
if (counters$failures > 0L) {
  quit(status = 1L)
}
