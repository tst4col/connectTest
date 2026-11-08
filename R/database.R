# Database service functions for the connectTest Stage A experiment.
# These functions contain no Shiny code so they can be tested directly.

experimentIdentity = function() {
  list(
    appId = "connectTest",
    schemaName = "connectTest.stageA",
    schemaVersion = 1L
  )
}

# Shared value limits enforced at save, export-import validation and download
# (M2r2). The constant has a single source; saveRecipe and the import
# validator both reference it.
instructionLimitChars = 20000L

nowUtc = function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Writable bundle for app-created and materialized working databases
# (H2r2): full lockdown minus `disabled_filesystems` (a writable transaction
# fails at commit while the local filesystem is disabled), with
# `lock_configuration` at instantiation. All values are strings; config
# applies only at instance creation, which is why every open passes the full
# bundle.
writableDatabaseConfig = function() {
  list(
    memory_limit = "128MiB",
    threads = "1",
    max_temp_directory_size = "64MiB",
    enable_external_access = "false",
    autoinstall_known_extensions = "false",
    autoload_known_extensions = "false",
    allow_community_extensions = "false",
    allow_unsigned_extensions = "false",
    allow_persistent_secrets = "false",
    lock_configuration = "true"
  )
}

openDatabase = function(dbPath, readOnly = FALSE) {
  if (!is.character(dbPath) || length(dbPath) != 1L || !nzchar(dbPath)) {
    stop("Ein Datenbankpfad wird benötigt.", call. = FALSE)
  }
  if (readOnly && !file.exists(dbPath)) {
    stop("Die Datenbankdatei existiert nicht.", call. = FALSE)
  }
  # H1r3: read-only applies at INSTANCE CREATION, not at a later
  # dbConnect() call (probe7 A: a later read_only=. silently permitted an
  # INSERT that persisted). driver-level allow_extensions=FALSE blocks
  # INSTALL/LOAD before any engine check.
  driver = duckdb::duckdb(
    dbdir = dbPath,
    read_only = readOnly,
    shared_home = FALSE,
    allow_extensions = FALSE,
    config = writableDatabaseConfig()
  )
  DBI::dbConnect(driver)
}

createSchema = function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE experiment_schema (
      app_id VARCHAR NOT NULL,
      schema_name VARCHAR NOT NULL,
      schema_version INTEGER NOT NULL,
      created_at VARCHAR NOT NULL
    )")
  DBI::dbExecute(conn, "
    CREATE TABLE recipes (
      recipe_id INTEGER PRIMARY KEY,
      title VARCHAR NOT NULL,
      instructions VARCHAR NOT NULL,
      servings INTEGER NOT NULL,
      updated_at VARCHAR NOT NULL,
      CHECK (recipe_id = 1),
      CHECK (servings BETWEEN 1 AND 100)
    )")
  DBI::dbExecute(conn, "
    CREATE TABLE collection_settings (
      setting_id INTEGER PRIMARY KEY,
      collection_name VARCHAR NOT NULL,
      updated_at VARCHAR NOT NULL,
      CHECK (setting_id = 1)
    )")
  invisible(TRUE)
}

seedDefaultContent = function(conn, identity, createdAt) {
  DBI::dbExecute(
    conn,
    "INSERT INTO experiment_schema (app_id, schema_name, schema_version, created_at)
     VALUES (?, ?, ?, ?)",
    params = list(identity$appId, identity$schemaName, identity$schemaVersion, createdAt)
  )
  DBI::dbExecute(
    conn,
    "INSERT INTO recipes (recipe_id, title, instructions, servings, updated_at)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      1L,
      "Käsespätzle mit Röstzwiebeln",
      paste0(
        "Zwiebeln in Ringe schneiden und in Butter goldbraun rösten. ",
        "Spätzle abwechselnd mit geriebenem Bergkäse in eine Form schichten, ",
        "würzen und mit Röstzwiebeln servieren. Dazu passt ein grüner Salat. ",
        "Für die ganze Familie reicht die Portion mit Beilage aus."
      ),
      4L,
      createdAt
    )
  )
  DBI::dbExecute(
    conn,
    "INSERT INTO collection_settings (setting_id, collection_name, updated_at)
     VALUES (?, ?, ?)",
    params = list(
      1L,
      "Sonntagsküche für die Familie – Größe der Portionen: vier Personen",
      createdAt
    )
  )
  invisible(TRUE)
}

initializeDatabase = function(conn) {
  identity = experimentIdentity()
  createdAt = nowUtc()
  DBI::dbBegin(conn)
  tryCatch({
    createSchema(conn)
    seedDefaultContent(conn, identity, createdAt)
    DBI::dbCommit(conn)
  }, error = function(error) {
    try(DBI::dbRollback(conn), silent = TRUE)
    stop(error)
  })
  invisible(TRUE)
}

assertMutable = function(state) {
  if (!is.environment(state)) {
    stop("Ungültiger Sitzungszustand.", call. = FALSE)
  }
  if (isTRUE(state$snapshotBusy)) {
    stop("Während der Momentaufnahme sind Änderungen gesperrt.", call. = FALSE)
  }
  if (isTRUE(state$importBusy)) {
    stop("Während des Imports sind Änderungen gesperrt.", call. = FALSE)
  }
  if (!isTRUE(state$active) || isTRUE(state$cleaned)) {
    stop("Die Sitzung ist beendet.", call. = FALSE)
  }
  if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
    stop("Die Arbeitsdatenbank ist nicht verbunden.", call. = FALSE)
  }
  invisible(TRUE)
}

readRecipe = function(conn) {
  recipe = DBI::dbGetQuery(
    conn,
    "SELECT recipe_id, title, instructions, servings, updated_at
     FROM recipes WHERE recipe_id = 1"
  )
  if (nrow(recipe) != 1L) {
    stop("Genau ein Rezept wurde erwartet.", call. = FALSE)
  }
  recipe
}

readCollectionSetting = function(conn) {
  setting = DBI::dbGetQuery(
    conn,
    "SELECT setting_id, collection_name, updated_at
     FROM collection_settings WHERE setting_id = 1"
  )
  if (nrow(setting) != 1L) {
    stop("Genau eine Sammlungseinstellung wurde erwartet.", call. = FALSE)
  }
  setting
}

readExperimentSchema = function(conn) {
  schema = DBI::dbGetQuery(
    conn,
    "SELECT app_id, schema_name, schema_version, created_at
     FROM experiment_schema"
  )
  if (nrow(schema) != 1L) {
    stop("Genau eine Schemaidentität wurde erwartet.", call. = FALSE)
  }
  schema
}

saveRecipe = function(state, title, instructions, servings) {
  assertMutable(state)

  title = trimws(as.character(title)[1])
  instructions = trimws(as.character(instructions)[1])
  servings = suppressWarnings(as.integer(servings)[1])

  if (is.na(title) || !nzchar(title)) {
    stop("Der Rezepttitel darf nicht leer sein.", call. = FALSE)
  }
  if (nchar(title, type = "chars") > 200L) {
    stop("Der Rezepttitel ist zu lang (höchstens 200 Zeichen).", call. = FALSE)
  }
  if (is.na(instructions) || !nzchar(instructions)) {
    stop("Die Zubereitung darf nicht leer sein.", call. = FALSE)
  }
  instructionLength = nchar(instructions, type = "chars")
  if (instructionLength > instructionLimitChars) {
    stop(
      sprintf(
        "Die Zubereitung ist zu lang (%d Zeichen; höchstens %d Zeichen).",
        instructionLength,
        instructionLimitChars
      ),
      call. = FALSE
    )
  }
  if (is.na(servings) || servings < 1L || servings > 100L) {
    stop("Die Portionszahl muss zwischen 1 und 100 liegen.", call. = FALSE)
  }

  updatedAt = nowUtc()
  DBI::dbWithTransaction(state$conn, {
    changed = DBI::dbExecute(
      state$conn,
      "UPDATE recipes
       SET title = ?, instructions = ?, servings = ?, updated_at = ?
       WHERE recipe_id = 1",
      params = list(title, instructions, servings, updatedAt)
    )
    if (changed != 1L) {
      stop("Das Rezept konnte nicht eindeutig gespeichert werden.", call. = FALSE)
    }
  })

  state$mutationGeneration = state$mutationGeneration + 1L
  invisible(TRUE)
}

saveCollectionSetting = function(state, collectionName) {
  assertMutable(state)

  collectionName = trimws(as.character(collectionName)[1])
  if (is.na(collectionName) || !nzchar(collectionName)) {
    stop("Die Sammlungseinstellung darf nicht leer sein.", call. = FALSE)
  }
  if (nchar(collectionName, type = "chars") > 300L) {
    stop("Die Sammlungseinstellung ist zu lang (höchstens 300 Zeichen).", call. = FALSE)
  }

  updatedAt = nowUtc()
  DBI::dbWithTransaction(state$conn, {
    changed = DBI::dbExecute(
      state$conn,
      "UPDATE collection_settings
       SET collection_name = ?, updated_at = ?
       WHERE setting_id = 1",
      params = list(collectionName, updatedAt)
    )
    if (changed != 1L) {
      stop("Die Sammlungseinstellung konnte nicht eindeutig gespeichert werden.", call. = FALSE)
    }
  })

  state$mutationGeneration = state$mutationGeneration + 1L
  invisible(TRUE)
}

newSessionState = function(sessionLabel = "session") {
  label = gsub("[^A-Za-z0-9_-]", "", as.character(sessionLabel)[1])
  if (!nzchar(label)) {
    label = "session"
  }

  sessionDir = tempfile(pattern = paste0("connectTest-", label, "-"), tmpdir = tempdir())
  if (!dir.create(sessionDir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Das Sitzungsverzeichnis konnte nicht erstellt werden.", call. = FALSE)
  }
  sessionDir = normalizePath(sessionDir, mustWork = TRUE)
  exportDir = file.path(sessionDir, "exports")
  if (!dir.create(exportDir, recursive = TRUE, showWarnings = FALSE)) {
    unlink(sessionDir, recursive = TRUE, force = TRUE)
    stop("Das Exportverzeichnis konnte nicht erstellt werden.", call. = FALSE)
  }
  uploadsDir = file.path(sessionDir, "uploads")
  if (!dir.create(uploadsDir, recursive = TRUE, showWarnings = FALSE)) {
    unlink(sessionDir, recursive = TRUE, force = TRUE)
    stop("Das Uploadverzeichnis konnte nicht erstellt werden.", call. = FALSE)
  }

  state = new.env(parent = emptyenv())
  state$sessionLabel = label
  state$sessionDir = sessionDir
  state$dbPath = file.path(sessionDir, "working.duckdb")
  state$exportDir = exportDir
  state$uploadsDir = uploadsDir
  state$conn = NULL
  state$mutationGeneration = 0L
  state$snapshotGeneration = -1L
  state$snapshotCounter = 0L
  state$snapshotPath = NULL
  state$snapshotCreatedAt = NULL
  state$lastSnapshotError = NULL
  state$snapshotBusy = FALSE
  state$recoveredFromSnapshot = FALSE
  # Stage B import state
  state$importAttemptCounter = 0L
  state$workingCounter = 0L
  state$importBusy = FALSE
  state$lastImportStatus = NULL
  state$retiredCleanup = list()
  # Stage B owner-gate state (server-derived; no client value can set this)
  state$ownerUploadAuthed = FALSE
  state$gateFailedAttempts = 0L
  state$gateCooldownUntil = as.POSIXct(structure(0, class = c("POSIXct", "POSIXt")))
  state$active = TRUE
  state$cleaned = FALSE

  tryCatch({
    state$conn = openDatabase(state$dbPath)
    initializeDatabase(state$conn)
  }, error = function(error) {
    cleanupSession(state)
    stop(error)
  })

  state
}

# Bounded cleanup registry for connections whose retirement failed after the
# committed state switch (M2r3). Entries retain the old connection handle
# (retry uses exactly that handle), the path, the attempt count and the time.
retirementRegistryCapacity = 8L

registerRetiredCleanup = function(state, conn, dbPath) {
  entry = list(conn = conn, dbPath = dbPath, tryCount = 0L, when = nowUtc())
  if (length(state$retiredCleanup) >= retirementRegistryCapacity) {
    warning(
      paste0(
        "Der Aufräumvertrag ist voll (",
        retirementRegistryCapacity,
        " Einträge). Die Datei '",
        dbPath,
        "' bleibt als Restressource bestehen, bis Einträge abgebaut sind."
      ),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  state$retiredCleanup[[length(state$retiredCleanup) + 1L]] = entry
  invisible(TRUE)
}

# Closes a connection and only afterwards removes its database files. Never
# unlinks the database while the connection remains valid; reports the
# residual resource instead. Returns the residual paths (empty on success).
releaseDatabaseFile = function(conn, dbPath) {
  residual = character(0)
  if (!is.null(conn)) {
    closed = tryCatch({
      if (!DBI::dbIsValid(conn)) {
        TRUE
      } else {
        DBI::dbDisconnect(conn, shutdown = TRUE)
        !DBI::dbIsValid(conn)
      }
    }, error = function(error) FALSE)
    if (!closed) {
      # The connection could not be (or was not proven to be) closed: the
      # database file must not be unlinked.
      return(c(dbPath, paste0(dbPath, ".tmp")))
    }
  }
  if (is.null(dbPath)) {
    return(character(0))
  }
  unlink(c(dbPath, paste0(dbPath, ".wal")), recursive = TRUE, force = TRUE)
  unlink(paste0(dbPath, ".tmp"), recursive = TRUE, force = TRUE)
  if (file.exists(dbPath)) {
    residual = dbPath
  }
  residual
}

# Retries disconnects with the recorded connection handles. Entries whose
# closure and file removal both succeed are dropped; failing entries stay
# (with the handle, an incremented attempt count and their path) and their
# residual paths are reported.
drainRetiredCleanup = function(state) {
  if (length(state$retiredCleanup) == 0L) {
    return(character(0))
  }
  remaining = list()
  residual = character(0)
  for (entry in state$retiredCleanup) {
    leftover = releaseDatabaseFile(entry$conn, entry$dbPath)
    if (length(leftover) > 0L) {
      entry$tryCount = entry$tryCount + 1L
      remaining[[length(remaining) + 1L]] = entry
      residual = c(residual, leftover)
    }
  }
  state$retiredCleanup = remaining
  residual
}

cleanupSession = function(state) {
  if (!is.environment(state) || isTRUE(state$cleaned)) {
    return(invisible(FALSE))
  }
  state$cleaned = TRUE
  state$active = FALSE

  residual = drainRetiredCleanup(state)

  conn = state$conn
  state$conn = NULL
  residual = c(residual, releaseDatabaseFile(conn, state$dbPath))

  if (length(residual) > 0L) {
    warning(
      paste0(
        "Sitzungsaufbereitung unvollständig: folgende Ressourcen bleiben ",
        "bestehen, solange ihre Verbindung noch gültig ist: ",
        paste0("'", unique(residual), "'", collapse = ", "),
        "."
      ),
      call. = FALSE
    )
    return(invisible(unique(residual)))
  }

  if (!is.null(state$sessionDir) && dir.exists(state$sessionDir)) {
    unlink(state$sessionDir, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

hasUnsnapshottedChanges = function(state) {
  isTRUE(state$mutationGeneration > state$snapshotGeneration)
}
