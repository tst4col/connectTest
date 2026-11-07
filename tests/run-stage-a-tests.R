#!/usr/bin/env Rscript
# Stage A service-level tests. Run with:
#   .rvenv/bin/Rscript --vanilla tests/run-stage-a-tests.R
# No testthat dependency is required.

source("R/database.R")
source("R/snapshot.R")

testRoot = tempfile(paste0("connectTest-stage-a-", Sys.getpid(), "-"))
if (!dir.create(testRoot, recursive = TRUE)) {
  stop("Cannot create test root: ", testRoot)
}
on.exit(unlink(testRoot, recursive = TRUE, force = TRUE), add = TRUE)

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

readText = function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# --- Schema and defaults -----------------------------------------------------

state = newSessionState("suite-main")
on.exit(cleanupSession(state), add = TRUE)

check("schema identity is connectTest.stageA version 1", {
  schema = readExperimentSchema(state$conn)
  stopifnot(identical(schema$app_id[1], "connectTest"))
  stopifnot(identical(schema$schema_name[1], "connectTest.stageA"))
  stopifnot(identical(as.integer(schema$schema_version[1]), 1L))
})

check("default recipe and setting contain German text including umlauts and eszett", {
  recipe = readRecipe(state$conn)
  setting = readCollectionSetting(state$conn)
  stopifnot(grepl("Käsespätzle", recipe$title[1], fixed = TRUE))
  stopifnot(grepl("Röstzwiebeln", recipe$instructions[1], fixed = TRUE))
  stopifnot(grepl("Sonntagsküche", setting$collection_name[1], fixed = TRUE))
  stopifnot(grepl("Größe", setting$collection_name[1], fixed = TRUE))
})

check("initial state is truthfully marked as not yet snapshotted", {
  stopifnot(isTRUE(hasUnsnapshottedChanges(state)))
})

# --- Transactional saves and parameterized SQL -------------------------------

check("recipe save persists German values and increments generation", {
  before = state$mutationGeneration
  saveRecipe(
    state,
    "Käsespätzle für Gäste",
    "Zwiebeln würfeln, in Butter rösten und die Soße abschmecken. Größe der Portion prüfen.",
    6
  )
  recipe = readRecipe(state$conn)
  stopifnot(identical(recipe$title[1], "Käsespätzle für Gäste"))
  stopifnot(identical(as.integer(recipe$servings[1]), 6L))
  stopifnot(identical(state$mutationGeneration, before + 1L))
})

check("collection setting save persists German values", {
  saveCollectionSetting(state, "Große Sonntagsküche für die ganze Familie – Straße 7")
  setting = readCollectionSetting(state$conn)
  stopifnot(grepl("Straße", setting$collection_name[1], fixed = TRUE))
  stopifnot(grepl("Größe", setting$collection_name[1], fixed = TRUE) ||
              grepl("Große", setting$collection_name[1], fixed = TRUE))
})

check("parameterized SQL treats an injection-shaped title as data", {
  hostile = "Titel'; DROP TABLE recipes; --"
  saveRecipe(state, hostile, "Keine Ausführung von SQL aus Eingaben.", 2)
  recipe = readRecipe(state$conn)
  stopifnot(identical(recipe$title[1], hostile))
  tables = DBI::dbGetQuery(
    state$conn,
    "SELECT table_name FROM information_schema.tables WHERE table_schema = 'main'"
  )$table_name
  stopifnot("recipes" %in% tables)
})

check("invalid input is rejected without a generation change", {
  before = state$mutationGeneration
  expectError(saveRecipe(state, "", "Text", 2), "Rezepttitel")
  expectError(saveRecipe(state, NA_character_, "Text", 2), "Rezepttitel")
  expectError(saveRecipe(state, "Titel", "", 2), "Zubereitung")
  expectError(saveRecipe(state, "Titel", "Text", 0), "Portionszahl")
  expectError(saveRecipe(state, "Titel", "Text", 101), "Portionszahl")
  expectError(saveCollectionSetting(state, NA_character_), "Sammlungseinstellung")
  stopifnot(identical(state$mutationGeneration, before))
})

check("dirty generation becomes clean only after a successful snapshot", {
  stopifnot(isTRUE(hasUnsnapshottedChanges(state)))
  oldSnapshot = createSnapshot(state)
  stopifnot(!isTRUE(hasUnsnapshottedChanges(state)))
  stopifnot(file.exists(oldSnapshot))
  saveRecipe(state, "Nach der Momentaufnahme", "Weitere Änderung.", 3)
  stopifnot(isTRUE(hasUnsnapshottedChanges(state)))
  createSnapshot(state)
  stopifnot(!isTRUE(hasUnsnapshottedChanges(state)))
})

# --- Snapshot success and standalone reopen ---------------------------------

check("snapshot has no companion WAL and expected schema/data", {
  snapshotPath = state$snapshotPath
  stopifnot(file.exists(snapshotPath))
  stopifnot(!file.exists(paste0(snapshotPath, ".wal")))
  conn = openDatabase(snapshotPath, readOnly = TRUE)
  on.exit(try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE), add = TRUE)
  schema = readExperimentSchema(conn)
  recipe = readRecipe(conn)
  stopifnot(identical(schema$app_id[1], "connectTest"))
  stopifnot(identical(as.integer(schema$schema_version[1]), 1L))
  stopifnot(identical(recipe$title[1], "Nach der Momentaufnahme"))
})

check("working connection remains usable after a successful snapshot", {
  saveRecipe(state, "Weiterarbeiten nach Momentaufnahme", "Funktioniert.", 4)
  recipe = readRecipe(state$conn)
  stopifnot(identical(recipe$title[1], "Weiterarbeiten nach Momentaufnahme"))
})

check("snapshot reopens from a different directory without the original WAL", {
  snapshotPath = state$snapshotPath
  separateDir = file.path(testRoot, "different directory with spaces")
  dir.create(separateDir, recursive = TRUE)
  separatePath = file.path(separateDir, "portable-copy.duckdb")
  stopifnot(file.copy(snapshotPath, separatePath, overwrite = TRUE))
  stopifnot(!file.exists(paste0(separatePath, ".wal")))
  conn = openDatabase(separatePath, readOnly = TRUE)
  recipe = readRecipe(conn)
  setting = readCollectionSetting(conn)
  DBI::dbDisconnect(conn, shutdown = TRUE)
  stopifnot(identical(recipe$title[1], "Nach der Momentaufnahme"))
  stopifnot(grepl("Straße", setting$collection_name[1], fixed = TRUE))
})

check("snapshot validation rejects an unsupported schema version", {
  fakePath = file.path(testRoot, "wrong-version.duckdb")
  stopifnot(file.copy(state$snapshotPath, fakePath, overwrite = TRUE))
  conn = openDatabase(fakePath)
  DBI::dbExecute(conn, "UPDATE experiment_schema SET schema_version = 2")
  DBI::dbDisconnect(conn, shutdown = TRUE)
  expectError(validateSnapshotFile(fakePath), "unerwartete Anwendungsidentität")
})

# --- Mutation exclusion during snapshot -------------------------------------

check("mutation and nested snapshot attempts are rejected during a snapshot", {
  messages = new.env(parent = emptyenv())
  messages$mutation = NULL
  messages$nested = NULL
  snapshotPath = createSnapshot(state, hooks = list(
    beforeClose = function(state) {
      messages$mutation = tryCatch({
        saveRecipe(state, "Darf nicht ankommen", "Gesperrt.", 2)
        NULL
      }, error = function(error) conditionMessage(error))
      messages$nested = tryCatch({
        createSnapshot(state)
        NULL
      }, error = function(error) conditionMessage(error))
    }
  ))
  stopifnot(!is.null(messages$mutation))
  stopifnot(grepl("gesperrt", messages$mutation, fixed = TRUE))
  stopifnot(!is.null(messages$nested))
  stopifnot(grepl("bereits", messages$nested, fixed = TRUE))
  conn = openDatabase(snapshotPath, readOnly = TRUE)
  recipe = readRecipe(conn)
  DBI::dbDisconnect(conn, shutdown = TRUE)
  stopifnot(!identical(recipe$title[1], "Darf nicht ankommen"))
  stopifnot(identical(recipe$title[1], "Weiterarbeiten nach Momentaufnahme"))
})

# --- Snapshot failure recovery ----------------------------------------------

check("copy failure preserves working data and the previous snapshot", {
  previousSnapshot = state$snapshotPath
  previousTitle = readRecipe(state$conn)$title[1]
  expectError(
    createSnapshot(state, destinationDir = state$dbPath),
    "Momentaufnahme fehlgeschlagen"
  )
  stopifnot(file.exists(previousSnapshot))
  stopifnot(identical(state$snapshotPath, previousSnapshot))
  stopifnot(!is.null(state$conn) && DBI::dbIsValid(state$conn))
  stopifnot(identical(readRecipe(state$conn)$title[1], previousTitle))
  stopifnot(length(list.files(state$exportDir, pattern = "\\.partial$")) == 0L)
  saveRecipe(state, "Nach Kopierfehler", "Arbeitskopie bleibt nutzbar.", 5)
  stopifnot(identical(readRecipe(state$conn)$title[1], "Nach Kopierfehler"))
})

check("reopen failure reports failure, leaves the database file readable, and keeps old snapshot", {
  previousSnapshot = state$snapshotPath
  previousTitle = readRecipe(state$conn)$title[1]
  message = expectError(
    createSnapshot(state, hooks = list(
      openDatabase = function(path) stop("simulierter Wiederverbindungsfehler")
    )),
    "Wiederverbinden fehlgeschlagen"
  )
  stopifnot(grepl("simulierter Wiederverbindungsfehler", message, fixed = TRUE))
  stopifnot(file.exists(state$dbPath))
  stopifnot(file.exists(previousSnapshot))
  stopifnot(identical(state$snapshotPath, previousSnapshot))

  partials = list.files(state$exportDir, pattern = "\\.partial$", full.names = TRUE)
  stopifnot(length(partials) == 1L)
  partialConn = openDatabase(partials[[1]], readOnly = TRUE)
  stopifnot(identical(readRecipe(partialConn)$title[1], previousTitle))
  DBI::dbDisconnect(partialConn, shutdown = TRUE)

  conn = openDatabase(state$dbPath, readOnly = TRUE)
  stopifnot(identical(readRecipe(conn)$title[1], previousTitle))
  DBI::dbDisconnect(conn, shutdown = TRUE)
})

check("a failed reopen recovers the working database from the validated copy", {
  recoveryState = newSessionState("suite-recovery")
  on.exit(cleanupSession(recoveryState), add = TRUE)
  saveRecipe(recoveryState, "Wiederherstellung", "Inhalt vor dem Fehler.", 4)
  createSnapshot(recoveryState)
  saveRecipe(recoveryState, "Wiederherstellung nach Änderung", "Neuer Inhalt.", 5)
  attempts = new.env(parent = emptyenv())
  attempts$count = 0L
  message = expectError(
    createSnapshot(recoveryState, hooks = list(
      openDatabase = function(path) {
        attempts$count = attempts$count + 1L
        if (attempts$count <= 2L) {
          stop("simulierter Wiederverbindungsfehler")
        }
        openDatabase(path)
      }
    )),
    "wiederhergestellt"
  )
  stopifnot(grepl("simulierter Wiederverbindungsfehler", message, fixed = TRUE))
  stopifnot(identical(attempts$count, 3L))
  stopifnot(!is.null(recoveryState$conn) && DBI::dbIsValid(recoveryState$conn))
  stopifnot(isTRUE(recoveryState$recoveredFromSnapshot))
  stopifnot(grepl("working-recovery-", basename(recoveryState$dbPath), fixed = TRUE))
  stopifnot(file.exists(recoveryState$dbPath))
  stopifnot(identical(readRecipe(recoveryState$conn)$title[1], "Wiederherstellung nach Änderung"))
  saveRecipe(recoveryState, "Weiter nach Wiederherstellung", "Nutzbar.", 6)
  stopifnot(identical(readRecipe(recoveryState$conn)$title[1], "Weiter nach Wiederherstellung"))
  postRecovery = createSnapshot(recoveryState)
  stopifnot(file.exists(postRecovery))
  stopifnot(!isTRUE(hasUnsnapshottedChanges(recoveryState)))
  cleanupSession(recoveryState)
})

check("complete failure of a repeated recovery keeps the latest data readable off-session", {
  repeated = newSessionState("suite-repeated-recovery")
  on.exit(cleanupSession(repeated), add = TRUE)

  saveRecipe(repeated, "Erster Stand", "Vor der ersten Wiederherstellung.", 2)
  saveCollectionSetting(repeated, "Erste Sammlung")
  createSnapshot(repeated)

  saveRecipe(repeated, "Wiederherstellung nach Änderung", "Neuer Inhalt.", 5)
  saveCollectionSetting(repeated, "Sammlung nach Änderung")

  attempts = new.env(parent = emptyenv())
  attempts$count = 0L
  expectError(
    createSnapshot(repeated, hooks = list(
      openDatabase = function(path) {
        attempts$count = attempts$count + 1L
        if (attempts$count <= 2L) {
          stop("simulierter Wiederverbindungsfehler")
        }
        openDatabase(path)
      }
    )),
    "wiederhergestellt"
  )
  stopifnot(identical(attempts$count, 3L))
  stopifnot(grepl("working-recovery-", basename(repeated$dbPath), fixed = TRUE))
  workingPath = repeated$dbPath
  stopifnot(file.exists(workingPath))

  saveRecipe(repeated, "Letzter Stand vor Totalfehler", "Muss unabhängig lesbar bleiben.", 7)
  saveCollectionSetting(repeated, "Letzte Sammlung vor Totalfehler")

  failure = expectError(
    createSnapshot(repeated, hooks = list(
      openDatabase = function(path) {
        stop("simulierter Wiederverbindungsfehler bei jedem Versuch")
      }
    )),
    "Momentaufnahme fehlgeschlagen"
  )
  stopifnot(grepl("Wiederverbinden fehlgeschlagen", failure, fixed = TRUE))
  stopifnot(grepl(workingPath, failure, fixed = TRUE))
  stopifnot(is.null(repeated$conn))
  stopifnot(file.exists(workingPath))
  stopifnot(!file.exists(paste0(workingPath, ".wal")))

  partials = list.files(repeated$exportDir, pattern = "\\.partial$", full.names = TRUE)
  stopifnot(length(partials) == 1L)
  stopifnot(grepl(partials[[1]], failure, fixed = TRUE))

  workingConn = openDatabase(workingPath, readOnly = TRUE)
  workingRecipe = readRecipe(workingConn)
  workingSetting = readCollectionSetting(workingConn)
  DBI::dbDisconnect(workingConn, shutdown = TRUE)
  stopifnot(identical(workingRecipe$title[1], "Letzter Stand vor Totalfehler"))
  stopifnot(identical(as.integer(workingRecipe$servings[1]), 7L))
  stopifnot(identical(workingSetting$collection_name[1], "Letzte Sammlung vor Totalfehler"))

  partialConn = openDatabase(partials[[1]], readOnly = TRUE)
  partialRecipe = readRecipe(partialConn)
  partialSetting = readCollectionSetting(partialConn)
  DBI::dbDisconnect(partialConn, shutdown = TRUE)
  stopifnot(identical(partialRecipe$title[1], "Letzter Stand vor Totalfehler"))
  stopifnot(identical(as.integer(partialRecipe$servings[1]), 7L))
  stopifnot(identical(partialSetting$collection_name[1], "Letzte Sammlung vor Totalfehler"))

  cleanupSession(repeated)
})

# --- Independent sessions and distinct downloads -----------------------------

check("two session states keep distinct paths, values, and snapshots", {
  first = newSessionState("suite-first")
  second = newSessionState("suite-second")
  on.exit({
    cleanupSession(first)
    cleanupSession(second)
  }, add = TRUE)

  stopifnot(!identical(first$sessionDir, second$sessionDir))
  stopifnot(!identical(first$dbPath, second$dbPath))

  saveRecipe(first, "Erste Sitzung", "Nur in Sitzung eins.", 2)
  saveRecipe(second, "Zweite Sitzung", "Nur in Sitzung zwei.", 8)
  saveCollectionSetting(first, "Sammlung eins")
  saveCollectionSetting(second, "Sammlung zwei")

  firstSnapshot = createSnapshot(first)
  secondSnapshot = createSnapshot(second)
  stopifnot(!identical(firstSnapshot, secondSnapshot))

  firstDownload = file.path(testRoot, "download-first.duckdb")
  secondDownload = file.path(testRoot, "download-second.duckdb")
  stopifnot(file.copy(firstSnapshot, firstDownload, overwrite = TRUE))
  stopifnot(file.copy(secondSnapshot, secondDownload, overwrite = TRUE))

  firstConn = openDatabase(firstDownload, readOnly = TRUE)
  secondConn = openDatabase(secondDownload, readOnly = TRUE)
  stopifnot(identical(readRecipe(firstConn)$title[1], "Erste Sitzung"))
  stopifnot(identical(readRecipe(secondConn)$title[1], "Zweite Sitzung"))
  stopifnot(identical(readCollectionSetting(firstConn)$collection_name[1], "Sammlung eins"))
  stopifnot(identical(readCollectionSetting(secondConn)$collection_name[1], "Sammlung zwei"))
  stopifnot(!identical(readRecipe(firstConn)$title[1], readRecipe(secondConn)$title[1]))
  DBI::dbDisconnect(firstConn, shutdown = TRUE)
  DBI::dbDisconnect(secondConn, shutdown = TRUE)

  cleanupSession(first)
  cleanupSession(second)
  stopifnot(!dir.exists(first$sessionDir))
  stopifnot(!dir.exists(second$sessionDir))
})

check("copying a snapshot like a download does not clear the dirty state", {
  downloadState = newSessionState("suite-download")
  on.exit(cleanupSession(downloadState), add = TRUE)
  createSnapshot(downloadState)
  saveRecipe(downloadState, "Neu nach Download", "Bleibt schmutzig.", 3)
  stopifnot(isTRUE(hasUnsnapshottedChanges(downloadState)))
  downloadCopy = file.path(testRoot, "download-does-not-clear.duckdb")
  stopifnot(file.copy(downloadState$snapshotPath, downloadCopy, overwrite = TRUE))
  stopifnot(isTRUE(hasUnsnapshottedChanges(downloadState)))
  cleanupSession(downloadState)
})

# --- Cleanup ----------------------------------------------------------------

check("cleanup closes the connection and removes the session directory", {
  doomed = newSessionState("suite-cleanup")
  sessionDir = doomed$sessionDir
  stopifnot(dir.exists(sessionDir))
  cleanupSession(doomed)
  stopifnot(!dir.exists(sessionDir))
  stopifnot(is.null(doomed$conn))
  expectError(saveRecipe(doomed, "Titel", "Text", 2), "beendet")
})

cat("\n", counters$checks, " checks, ", counters$failures, " failure(s)\n", sep = "")
if (counters$failures > 0L) {
  quit(status = 1L)
}
