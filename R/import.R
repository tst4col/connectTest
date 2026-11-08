# Stage B trusted-owner native-file import service.
#
# Journey per assignment: owner-created generic Stage A download -> gate ->
# candidate copy -> hardened read-only read -> validation -> seed-free
# materialization -> all-or-nothing promotion with a single committed state
# switch -> best-effort retirement cleanup with a bounded registry.
#
# Validation limits and engine settings are defense in depth; opening any
# untrusted file runs DuckDB-internal code before any metadata check. A gate
# password is not a hostile-file sandbox.

uploadMaxBytes = 10L * 1024L^2
uploadRequestMaxBytes = 12L * 1024L^2

importTableColumns = list(
  experiment_schema = c(
    app_id = "VARCHAR",
    schema_name = "VARCHAR",
    schema_version = "INTEGER",
    created_at = "VARCHAR"
  ),
  recipes = c(
    recipe_id = "INTEGER",
    title = "VARCHAR",
    instructions = "VARCHAR",
    servings = "INTEGER",
    updated_at = "VARCHAR"
  ),
  collection_settings = c(
    setting_id = "INTEGER",
    collection_name = "VARCHAR",
    updated_at = "VARCHAR"
  )
)

importExpectedTables = c("experiment_schema", "recipes", "collection_settings")

# --- hardened candidate handling ---------------------------------------------

# H1r3: read-only is applied at INSTANCE CREATION. openCandidateDatabase only
# creates + opens the instance and issues the single affected SET; every
# verification runs afterwards in the import flow so that a hooked-in
# writable instance is rejected before any catalog read (test 11/12).
openCandidateDatabase = function(candidatePath) {
  config = writableDatabaseConfig()
  config$lock_configuration = NULL
  driver = duckdb::duckdb(
    dbdir = candidatePath,
    read_only = TRUE,
    shared_home = FALSE,
    allow_extensions = FALSE,
    config = config
  )
  conn = tryCatch(DBI::dbConnect(driver), error = function(error) {
    try(duckdb::duckdb_shutdown(driver), silent = TRUE)
    stop(error)
  })
  step = tryCatch({
    DBI::dbExecute(conn, "SET disabled_filesystems = 'LocalFileSystem'")
    TRUE
  }, error = function(error) {
    FALSE
  })
  if (!isTRUE(step)) {
    try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE)
    stop(
      "Die Kandidatendatenbank konnte nicht gehärtet werden (Dateisystem-Sperre).",
      call. = FALSE
    )
  }
  conn
}

currentSettingValue = function(conn, name) {
  DBI::dbGetQuery(
    conn,
    sprintf("SELECT current_setting('%s') AS v", name)
  )$v[[1]]
}

normalizeSetting = function(value) {
  text = tolower(gsub("\\s+", "", as.character(value)))
  gsub("\\.0", "", text)
}

expectedHardenedSettings = local({
  config = writableDatabaseConfig()
  config$lock_configuration = NULL
  config
})

filesystemProbeExpectedMessage = "has been disabled previously, it cannot be re-enabled"
configLockedMessage = "configuration has been locked"

# Verification order (H2r3, normative):
#   P1 every instantiation-time member, re-read from the live instance
#   P2 filesystem re-enable probe — MUST fail, and MUST run before the lock
#   P3 SET lock_configuration = true, re-read as TRUE
#   P4 a further generic SET must be refused (locked)
# The filesystem probe is NEVER repeated after locking: post-lock it degrades
# to the generic locked message, which proves nothing about the filesystem.
verifyHardenedSettings = function(conn, journal = NULL) {
  note = if (is.environment(journal)) {
    function(label, value) {
      assign(label, value, envir = journal)
    }
  } else {
    function(label, value) invisible(NULL)
  }

  for (name in names(expectedHardenedSettings)) {
    actual = currentSettingValue(conn, name)
    if (!identical(
      normalizeSetting(actual),
      normalizeSetting(expectedHardenedSettings[[name]])
    )) {
      note("P1FAILED", c(name, actual))
      stop(
        sprintf(
          "Gehärtete Einstellung '%s' entspricht nicht der Vorgabe.",
          name
        ),
        call. = FALSE
      )
    }
  }
  note("P1", "ok")

  # Re-enable probe must fail with the filesystem-specific message WHILE the
  # configuration is not locked yet.
  failure = tryCatch({
    DBI::dbExecute(conn, "SET disabled_filesystems = ''")
    NULL
  }, error = function(error) conditionMessage(error))
  if (is.null(failure) || !grepl(filesystemProbeExpectedMessage, failure, fixed = TRUE)) {
    note("P2FAILED", if (is.null(failure)) "re-enable unexpectedly succeeded" else failure)
    stop(
      "Die Überprüfung der Dateisystem-Sperre ist fehlgeschlagen.",
      call. = FALSE
    )
  }
  note("P2", "filesystem-probe-blocked-before-lock")

  DBI::dbExecute(conn, "SET lock_configuration = true")
  locked = currentSettingValue(conn, "lock_configuration")
  if (!isTRUE(as.logical(locked))) {
    note("P3FAILED", locked)
    stop("Die Konfiguration konnte nicht verriegelt werden.", call. = FALSE)
  }
  note("P3", "locked")

  failure = tryCatch({
    DBI::dbExecute(conn, "SET memory_limit = '256MiB'")
    NULL
  }, error = function(error) conditionMessage(error))
  if (is.null(failure) || !grepl(configLockedMessage, failure, fixed = TRUE)) {
    note("P4FAILED", if (is.null(failure)) "SET unexpectedly succeeded" else failure)
    stop(
      "Die verriegelte Konfiguration ließ eine Änderung zu (Überprüfung fehlgeschlagen).",
      call. = FALSE
    )
  }
  note("P4", "further-set-refused")

  engine = tryCatch(
    DBI::dbGetQuery(conn, "SELECT version() AS v")$v[[1]],
    error = function(error) NA_character_
  )
  note("engine", engine)
  invisible(TRUE)
}

# Non-behavioral verification that the candidate instance is attached
# read_only (duckdb_databases()$readonly; probe for the current database only).
assertCandidateReadOnly = function(conn, candidatePath) {
  databases = DBI::dbGetQuery(
    conn,
    "SELECT database_name, readonly FROM duckdb_databases()
     WHERE database_name = current_database() AND NOT internal"
  )
  if (nrow(databases) != 1L) {
    stop("Die Kandidatendatenbank konnte nicht eindeutig ermittelt werden.", call. = FALSE)
  }
  if (!isTRUE(databases$readonly[[1]])) {
    stop(
      paste0(
        "Die Kandidatendatenbank '",
        sanitizePathForMessage(candidatePath),
        "' ist nicht im Nur-Lese-Modus geöffnet."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

sanitizePathForMessage = function(path) {
  basename <- as.character(path)[1]
  gsub("[^A-Za-z0-9._-]", "_", basename)
}

# --- catalog allowlist --------------------------------------------------------

# Evaluated across the WHOLE current database (M1r2/M1r3):
#   1. schemas: duckdb_schemas() filtered by
#      database_name = current_database() AND NOT internal. DuckDB marks the
#      ordinary main schema as internal, so a valid file yields ZERO rows
#      here; any (even empty) unexpected schema appears and is rejected.
#   2. approved tables only, all in main.
#   3. no user views/sequences/indexes/functions (macros) anywhere user-
#      visible; views are never evaluated, only their presence rejected.
validateCandidateCatalog = function(conn) {
  unexpected = DBI::dbGetQuery(
    conn,
    "SELECT schema_name FROM duckdb_schemas()
     WHERE database_name = current_database() AND NOT internal"
  )$schema_name
  unexpected = setdiff(unexpected, "main")
  if (length(unexpected) > 0L) {
    stop(
      sprintf(
        "Unerwartetes Schema in der Datei: '%s'.",
        paste(unexpected, collapse = "', '")
      ),
      call. = FALSE
    )
  }

  tables = DBI::dbGetQuery(
    conn,
    "SELECT schema_name, table_name FROM duckdb_tables()
     WHERE NOT internal AND database_name = current_database()"
  )
  if (!setequal(tables$table_name, importExpectedTables)) {
    stop(
      sprintf(
        "Unerwartete Tabellenstruktur: '%s'.",
        paste(sort(unique(tables$table_name)), collapse = "', '")
      ),
      call. = FALSE
    )
  }
  if (!all(tables$schema_name == "main")) {
    stop(
      sprintf(
        "Tabelle '%s' liegt außerhalb des Hauptschemas.",
        tables$table_name[tables$schema_name != "main"][1]
      ),
      call. = FALSE
    )
  }

  views = DBI::dbGetQuery(
    conn,
    "SELECT view_name FROM duckdb_views()
     WHERE NOT internal AND database_name = current_database()"
  )$view_name
  if (length(views) > 0L) {
    stop(
      sprintf("Unerwarteter View in der Datei: '%s'.", paste(views, collapse = "', '")),
      call. = FALSE
    )
  }

  sequences = DBI::dbGetQuery(
    conn,
    "SELECT sequence_name FROM duckdb_sequences()
     WHERE database_name = current_database()"
  )$sequence_name
  if (length(sequences) > 0L) {
    stop(
      sprintf(
        "Unerwartete Sequenz in der Datei: '%s'.",
        paste(sequences, collapse = "', '")
      ),
      call. = FALSE
    )
  }

  indexes = DBI::dbGetQuery(
    conn,
    "SELECT index_name FROM duckdb_indexes()
     WHERE database_name = current_database()"
  )$index_name
  if (length(indexes) > 0L) {
    stop(
      sprintf(
        "Unerwarteter Index in der Datei: '%s'.",
        paste(indexes, collapse = "', '")
      ),
      call. = FALSE
    )
  }

  functions = DBI::dbGetQuery(
    conn,
    "SELECT function_name FROM duckdb_functions()
     WHERE NOT internal AND database_name = current_database()"
  )$function_name
  if (length(functions) > 0L) {
    stop(
      sprintf(
        "Unerwartete Funktion/Makro in der Datei: '%s'.",
        paste(functions, collapse = "', '")
      ),
      call. = FALSE
    )
  }

  for (tableName in names(importTableColumns)) {
    columns = DBI::dbGetQuery(
      conn,
      "SELECT column_name, data_type FROM information_schema.columns
       WHERE table_schema = ? AND table_name = ?",
      params = list("main", tableName)
    )
    expected = importTableColumns[[tableName]]
    if (!setequal(names(expected), columns$column_name)) {
      stop(
        sprintf(
          "Unerwartete Spalten in Tabelle '%s': '%s'.",
          tableName,
          paste(sort(columns$column_name), collapse = "', '")
        ),
        call. = FALSE
      )
    }
    actualTypes = columns$data_type[match(names(expected), columns$column_name)]
    mismatches = names(expected)[
      toupper(actualTypes) != toupper(expected)
    ]
    if (length(mismatches) > 0L) {
      stop(
        sprintf(
          "Unerwarteter Spaltentyp in Tabelle '%s', Spalte '%s'.",
          tableName,
          mismatches[[1]]
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

validateImportedIdentity = function(conn) {
  rows = DBI::dbGetQuery(
    conn,
    "SELECT app_id, schema_name, schema_version, created_at FROM main.experiment_schema"
  )
  if (nrow(rows) != 1L) {
    stop(
      sprintf(
        "Genau eine Schemaidentität wurde erwartet (Gefunden: %d).",
        nrow(rows)
      ),
      call. = FALSE
    )
  }
  identity = experimentIdentity()
  if (!identical(rows$app_id[[1]], identity$appId) ||
      !identical(rows$schema_name[[1]], identity$schemaName) ||
      !identical(as.integer(rows$schema_version[[1]]), identity$schemaVersion)) {
    stop(
      sprintf(
        paste0(
          "Neuere oder andere Schemaidentität wird nicht unterstützt ",
          "(erwartet: %s / %s / Version %d)."
        ),
        identity$appId,
        identity$schemaName,
        identity$schemaVersion
      ),
      call. = FALSE
    )
  }
  if (is.na(rows$created_at[[1]]) || !nzchar(rows$created_at[[1]])) {
    stop("Die Schemaidentität enthält keinen Erstellungszeitpunkt.", call. = FALSE)
  }
  list(
    appId = rows$app_id[[1]],
    schemaName = rows$schema_name[[1]],
    schemaVersion = as.integer(rows$schema_version[[1]]),
    createdAt = rows$created_at[[1]]
  )
}

# --- value validation ---------------------------------------------------------

validateTextValue = function(value, maxChars, tableName, columnName, fieldLabel) {
  if (is.na(value) || is.null(value)) {
    stop(
      sprintf("Feld '%s' in Tabelle '%s' darf nicht leer sein.", fieldLabel, tableName),
      call. = FALSE
    )
  }
  text = as.character(value)
  if (!nzchar(text)) {
    stop(
      sprintf("Feld '%s' in Tabelle '%s' darf nicht leer sein.", fieldLabel, tableName),
      call. = FALSE
    )
  }
  length = nchar(text, type = "chars")
  if (length > maxChars) {
    stop(
      sprintf(
        paste0(
          "Feld '%s' in Tabelle '%s' ist zu lang (%d Zeichen; ",
          "höchstens %d Zeichen)."
        ),
        fieldLabel,
        tableName,
        length,
        maxChars
      ),
      call. = FALSE
    )
  }
  text
}

validateTimestampValue = function(value, tableName, columnName) {
  if (is.na(as.character(value)) || !nzchar(as.character(value))) {
    stop(
      sprintf(
        "Feld '%s' in Tabelle '%s' enthält keinen Zeitstempel.",
        columnName,
        tableName
      ),
      call. = FALSE
    )
  }
  as.character(value)
}

readAndValidateImportedRows = function(conn) {
  validateCandidateCatalog(conn)
  identity = validateImportedIdentity(conn)

  recipes = DBI::dbGetQuery(
    conn,
    "SELECT recipe_id, title, instructions, servings, updated_at FROM main.recipes"
  )
  if (nrow(recipes) != 1L) {
    stop(
      sprintf("Genau ein Rezept wurde erwartet (Gefunden: %d).", nrow(recipes)),
      call. = FALSE
    )
  }
  recipeId = suppressWarnings(as.integer(recipes$recipe_id[[1]]))
  if (is.na(recipeId) || recipeId != 1L) {
    stop("Die Rezept-ID muss 1 sein.", call. = FALSE)
  }
  servings = suppressWarnings(as.integer(recipes$servings[[1]]))
  if (is.na(servings) || servings < 1L || servings > 100L) {
    stop("Die Portionszahl muss zwischen 1 und 100 liegen.", call. = FALSE)
  }
  title = validateTextValue(
    recipes$title[[1]], 200L, "recipes", "title", "Rezepttitel"
  )
  instructions = validateTextValue(
    recipes$instructions[[1]],
    instructionLimitChars,
    "recipes",
    "instructions",
    "Zubereitung"
  )
  recipeUpdatedAt = validateTimestampValue(
    recipes$updated_at[[1]], "recipes", "updated_at"
  )

  settings = DBI::dbGetQuery(
    conn,
    "SELECT setting_id, collection_name, updated_at FROM main.collection_settings"
  )
  if (nrow(settings) != 1L) {
    stop(
      sprintf(
        "Genau eine Sammlungseinstellung wurde erwartet (Gefunden: %d).",
        nrow(settings)
      ),
      call. = FALSE
    )
  }
  settingId = suppressWarnings(as.integer(settings$setting_id[[1]]))
  if (is.na(settingId) || settingId != 1L) {
    stop("Die Einstellungs-ID muss 1 sein.", call. = FALSE)
  }
  collectionName = validateTextValue(
    settings$collection_name[[1]], 300L, "collection_settings", "collection_name",
    "Sammlungseinstellung"
  )
  settingUpdatedAt = validateTimestampValue(
    settings$updated_at[[1]], "collection_settings", "updated_at"
  )

  list(
    identity = identity,
    recipe = list(
      recipeId = 1L,
      title = title,
      instructions = instructions,
      servings = servings,
      updatedAt = recipeUpdatedAt
    ),
    setting = list(
      settingId = 1L,
      collectionName = collectionName,
      updatedAt = settingUpdatedAt
    )
  )
}

# --- seed-free materialization (H3r3) -----------------------------------------

# Opens a fresh materialization database with the writable bundle, creates
# the canonical Stage A schema WITHOUT seed rows, and inserts the validated
# imported rows verbatim in one transaction. Returns the still-open
# connection for the re-read check.
materializeImportedRows = function(values, workingPath) {
  conn = openDatabase(workingPath)
  onRollback = function(error) {
    try(DBI::dbRollback(conn), silent = TRUE)
    try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE)
    stop(error)
  }
  DBI::dbBegin(conn)
  tryCatch({
    createSchema(conn)
    DBI::dbExecute(
      conn,
      "INSERT INTO main.experiment_schema (app_id, schema_name, schema_version, created_at)
       VALUES (?, ?, ?, ?)",
      params = list(
        values$identity$appId,
        values$identity$schemaName,
        values$identity$schemaVersion,
        values$identity$createdAt
      )
    )
    DBI::dbExecute(
      conn,
      "INSERT INTO main.recipes (recipe_id, title, instructions, servings, updated_at)
       VALUES (?, ?, ?, ?, ?)",
      params = list(
        values$recipe$recipeId,
        values$recipe$title,
        values$recipe$instructions,
        values$recipe$servings,
        values$recipe$updatedAt
      )
    )
    DBI::dbExecute(
      conn,
      "INSERT INTO main.collection_settings (setting_id, collection_name, updated_at)
       VALUES (?, ?, ?)",
      params = list(
        values$setting$settingId,
        values$setting$collectionName,
        values$setting$updatedAt
      )
    )
    DBI::dbCommit(conn)
  }, error = onRollback)
  conn
}

verifyMaterializedImport = function(conn, values) {
  recheck = readAndValidateImportedRows(conn)
  expected = list(
    values$recipe$recipeId,
    values$recipe$title,
    values$recipe$instructions,
    values$recipe$servings,
    values$recipe$updatedAt,
    values$setting$settingId,
    values$setting$collectionName,
    values$setting$updatedAt,
    values$identity$appId,
    values$identity$schemaName,
    values$identity$schemaVersion,
    values$identity$createdAt
  )
  actual = list(
    recheck$recipe$recipeId,
    recheck$recipe$title,
    recheck$recipe$instructions,
    recheck$recipe$servings,
    recheck$recipe$updatedAt,
    recheck$setting$settingId,
    recheck$setting$collectionName,
    recheck$setting$updatedAt,
    recheck$identity$appId,
    recheck$identity$schemaName,
    recheck$identity$schemaVersion,
    recheck$identity$createdAt
  )
  if (!all(vapply(seq_along(expected), function(i) {
    identical(expected[[i]], actual[[i]])
  }, logical(1L)))) {
    stop(
      "Der materialisierte Inhalt weicht vom geprüften Import ab.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# --- the import service -------------------------------------------------------

# Best-effort retirement of the replaced collection: disconnect the old
# connection (registering it with handle/path/attempts/time on failure),
# unlink the old working file only after a successful disconnect, and attempt
# to remove the upload copy and the obsolete snapshot file. Never reverses
# the committed state. Returns recorded warnings.
# Default file removal for Phase C; a hook may substitute failable behavior
# for the tests. Rollback preparation keeps the reliable unlink: it must not
# depend on test hooks.
retireImportedPredecessor = function(state, oldConn, oldDbPath, oldSnapshotPath, candidatePath, disconnectHook, removeFilesHook) {
  cleanupWarnings = character(0)

  closedOld = FALSE
  if (!is.null(oldConn)) {
    closedOld = tryCatch({
      disconnectHook(oldConn)
      !DBI::dbIsValid(oldConn)
    }, error = function(error) FALSE)
  } else {
    closedOld = TRUE
  }
  if (closedOld) {
    leftover = releaseDatabaseFile(NULL, oldDbPath)
    if (length(leftover) > 0L) {
      cleanupWarnings = append(
        cleanupWarnings,
        sprintf(
          "Alte Arbeitsdatei %s ließ sich nicht löschen.",
          sanitizePathForMessage(leftover[[1]])
        )
      )
    }
  } else {
    # Keep the handle for the bounded registry; the file must NOT be
    # unlinked while the connection remains valid.
    if (!isTRUE(registerRetiredCleanup(state, oldConn, oldDbPath))) {
      cleanupWarnings = append(
        cleanupWarnings,
        sprintf(
          paste0(
            "Alte Verbindung zu %s blieb bestehen; der begrenzte Vertrag ist ",
            "voll, weitere Imports sind gesperrt, bis er abgebaut ist."
          ),
          sanitizePathForMessage(oldDbPath)
        )
      )
    }
  }

  removeFilesHook(c(candidatePath, paste0(candidatePath, ".wal")))
  removeFilesHook(paste0(candidatePath, ".tmp"))
  if (file.exists(candidatePath)) {
    cleanupWarnings = append(
      cleanupWarnings,
      sprintf(
        "Upload-Kopie %s ließ sich nicht löschen.",
        sanitizePathForMessage(candidatePath)
      )
    )
  }

  if (!is.null(oldSnapshotPath) && file.exists(oldSnapshotPath)) {
    removeFilesHook(oldSnapshotPath)
    if (file.exists(oldSnapshotPath)) {
      cleanupWarnings = append(
        cleanupWarnings,
        sprintf(
          paste0(
            "Veraltete Momentaufnahme %s ließ sich nicht löschen; der ",
            "Downloadverweis bleibt gelöscht."
          ),
          sanitizePathForMessage(oldSnapshotPath)
        )
      )
    }
  }

  cleanupWarnings
}

importDatabase = function(state, sourcePath, hooks = list()) {
  # Server-side authorisation check at the import-service boundary.
  requireOwnerUploadAuthorisation(state)

  # A1 quota drain of the bounded retirement registry (M2r3); imports fail
  # closed while the registry is at capacity.
  drainRetiredCleanup(state)
  if (length(state$retiredCleanup) >= retirementRegistryCapacity) {
    stop(
      paste0(
        "Der Cleanup-Vertrag ist voll. Der Import bleibt gesperrt, bis ",
        "ausstehende Aufräumkungen abgeschlossen sind."
      ),
      call. = FALSE
    )
  }

  if (!is.environment(state)) {
    stop("Ungültiger Sitzungszustand.", call. = FALSE)
  }
  if (isTRUE(state$importBusy)) {
    stop("Es läuft bereits ein Import.", call. = FALSE)
  }
  if (isTRUE(state$snapshotBusy)) {
    stop("Während der Momentaufnahme ist kein Import möglich.", call. = FALSE)
  }
  if (!isTRUE(state$active) || isTRUE(state$cleaned)) {
    stop("Die Sitzung ist beendet.", call. = FALSE)
  }
  if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
    stop("Die Arbeitsdatenbank ist nicht verbunden.", call. = FALSE)
  }

  hookOf = function(name, default) {
    if (is.function(hooks[[name]])) hooks[[name]] else default
  }
  copyHook = hookOf("copyFile", function(src, dst) suppressWarnings(file.copy(
    src,
    dst,
    overwrite = FALSE,
    copy.mode = FALSE,
    copy.date = FALSE
  )))
  openCandidateHook = hookOf("openCandidate", openCandidateDatabase)
  verifySettingsHook = hookOf("verifySettings", verifyHardenedSettings)
  validateCatalogHook = hookOf("validateCatalog", validateCandidateCatalog)
  readValuesHook = hookOf("readValues", readAndValidateImportedRows)
  materializeHook = hookOf("materialize", materializeImportedRows)
  verifyMaterializedHook = hookOf("verifyMaterialized", verifyMaterializedImport)
  reopenWorkingHook = hookOf("reopenWorking", openDatabase)
  beforeCommitHook = hooks$beforeCommit
  disconnectHook = hookOf("disconnectConnection", function(conn) {
    DBI::dbDisconnect(conn, shutdown = TRUE)
    invisible(TRUE)
  })
  removeFilesHook = hookOf("removeFiles", function(path) {
    unlink(path, recursive = TRUE, force = TRUE)
    invisible(TRUE)
  })

  state$importBusy = TRUE
  on.exit({
    state$importBusy = FALSE
  }, add = TRUE)

  # Unique candidate AND working paths for every attempt, including rapid
  # successive imports; nothing is ever overwritten.
  repeat {
    state$importAttemptCounter = state$importAttemptCounter + 1L
    state$workingCounter = state$workingCounter + 1L
    stamp = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    candidatePath = file.path(
      state$uploadsDir,
      sprintf("import-%s-%03d.duckdb", stamp, state$importAttemptCounter)
    )
    workingPath = file.path(
      state$sessionDir,
      sprintf("working-%s-%03d.duckdb", stamp, state$workingCounter)
    )
    if (!file.exists(candidatePath) && !file.exists(workingPath)) {
      break
    }
  }

  open = new.env(parent = emptyenv()) # still-open instances needing rollback
  open$candidateConn = NULL
  open$candidatePath = candidatePath
  open$materializedConn = NULL
  open$materializedPath = workingPath
  open$replacementConn = NULL
  open$replacementPath = workingPath
  journal = new.env(parent = emptyenv())
  committed = FALSE
  rollbackResidual = character(0)

  rollbackCloseHook = hookOf("rollbackClose", function(conn, path) {
    releaseDatabaseFile(conn, path)
  })

  # Closes every connection the import opened before removing any database
  # file. The file of a still-live (or unproven-closed) connection is never
  # unlinked: its handle/path are retained in the bounded cleanup registry
  # and the residual is reported (H1 fix). Returns the residual paths.
  rollbackPreparation = function() {
    residual = character(0)
    retainedPaths = character(0)

    # 1) close every tracked connection first (file removal included)
    for (slot in c("candidate", "materialized", "replacement")) {
      conn = open[[paste0(slot, "Conn")]]
      if (is.null(conn)) {
        next
      }
      path = open[[paste0(slot, "Path")]]
      open[[paste0(slot, "Conn")]] = NULL
      open[[paste0(slot, "Path")]] = NULL
      leftover = tryCatch(
        rollbackCloseHook(conn, path),
        error = function(error) {
          # conservative: closure unproven -> preserve file and handle
          c(path, paste0(path, ".tmp"))
        }
      )
      if (length(leftover) > 0L) {
        residual = c(residual, leftover)
        retainedPaths = c(retainedPaths, path)
        registerRetiredCleanup(state, conn, path)
      }
    }

    # 2) then remove remaining files, but never one whose connection is
    #    still retained
    for (slot in c("candidate", "materialized", "replacement")) {
      path = open[[paste0(slot, "Path")]]
      open[[paste0(slot, "Path")]] = NULL
      if (is.null(path) || path %in% retainedPaths) {
        next
      }
      leftover = releaseDatabaseFile(NULL, path)
      residual = c(residual, leftover)
    }
    rollbackResidual <<- unique(residual)
    rollbackResidual
  }

  result = tryCatch({
    # --- PHASE A: prepare (fully rollback-tested; old collection untouched)
    fileSize = tryCatch(file.size(sourcePath), error = function(error) NA_real_)
    if (is.na(fileSize) || fileSize <= 0 || fileSize > uploadMaxBytes) {
      stop(
        sprintf(
          paste0(
            "Die Datei ist zu groß oder leer (%d Bytes erlaubt; ",
            "Gefunden: %s Bytes)."
          ),
          uploadMaxBytes,
          format(fileSize, big.mark = ".", decimal.mark = ",")
        ),
        call. = FALSE
      )
    }
    if (is.function(hooks$beforeSize)) hooks$beforeSize(state)

    copied = copyHook(sourcePath, candidatePath)
    if (!isTRUE(copied) || !file.exists(candidatePath) ||
        file.size(candidatePath) != fileSize) {
      stop("Die hochgeladene Datei konnte nicht kopiert werden.", call. = FALSE)
    }

    # Copy exists -> creation-time read-only instance (H1r3), then the SET
    # and verification before any catalog or data read.
    conn = openCandidateHook(candidatePath)
    open$candidateConn = conn
    verifySettingsHook(conn, journal)
    assertCandidateReadOnly(conn, candidatePath)

    validateCatalogHook(conn)
    values = readValuesHook(conn)

    # Reads done: release the candidate instance before materializing.
    closeCandidate = tryCatch({
      DBI::dbDisconnect(conn, shutdown = TRUE)
      TRUE
    }, error = function(error) FALSE)
    if (!isTRUE(closeCandidate) || (DBI::dbIsValid(conn))) {
      stop("Die Kandidatenverbindung konnte nicht geschlossen werden.", call. = FALSE)
    }
    open$candidateConn = NULL

    materializedConn = materializeHook(values, workingPath)
    open$materializedConn = materializedConn
    verifyMaterializedHook(materializedConn, values)

    wal = paste0(workingPath, ".wal")
    closedMaterial = tryCatch({
      DBI::dbDisconnect(materializedConn, shutdown = TRUE)
      TRUE
    }, error = function(error) FALSE)
    if (!isTRUE(closedMaterial)) {
      stop("Die Materialisierungsverbindung konnte nicht geschlossen werden.", call. = FALSE)
    }
    open$materializedConn = NULL
    if (file.exists(wal)) {
      stop(
        "Nach dem Schließen besteht noch eine WAL-Datei der Materialisierung.",
        call. = FALSE
      )
    }

    # --- PHASE B: commit decision (still rollback-tested)
    newConn = reopenWorkingHook(workingPath)
    # the reopened replacement is owned by the import until the commit point;
    # rollback must close it before deleting the file (H1 fix)
    open$replacementConn = newConn
    if (is.null(newConn) || !DBI::dbIsValid(newConn)) {
      stop("Die Arbeitsdatenbank des Imports konnte nicht geöffnet werden.", call. = FALSE)
    }
    readback = tryCatch(
      DBI::dbGetQuery(
        newConn,
        "SELECT title FROM main.recipes WHERE recipe_id = ?",
        params = list(1L)
      ),
      error = function(error) NULL
    )
    if (is.null(readback) || nrow(readback) != 1L ||
        !identical(readback$title[[1]], values$recipe$title)) {
      try(DBI::dbDisconnect(newConn, shutdown = TRUE), silent = TRUE)
      stop(
        "Die Arbeitsdatenbank des Imports konnte nicht verifiziert werden.",
        call. = FALSE
      )
    }

    # == COMMIT POINT: complete committed-state switch (back-to-back, no
    # interleaved code) while importBusy excludes every mutation path.
    if (is.function(beforeCommitHook)) beforeCommitHook(state)
    oldConn = state$conn
    oldDbPath = state$dbPath
    oldSnapshotPath = state$snapshotPath

    state$conn = newConn
    state$dbPath = workingPath
    state$snapshotPath = NULL
    state$snapshotGeneration = -1L
    state$lastSnapshotError = NULL
    state$mutationGeneration = state$mutationGeneration + 1L
    state$recoveredFromSnapshot = FALSE
    committed = TRUE
    # ownership of the replacement connection transferred to committed state
    open$replacementConn = NULL
    open$replacementPath = NULL

    # --- PHASE C: retirement cleanup (best-effort; never reverses;
    # failures are recorded, not re-attempted against committed state)
    cleanupWarnings = tryCatch(
      retireImportedPredecessor(
        state,
        oldConn,
        oldDbPath,
        oldSnapshotPath,
        candidatePath,
        disconnectHook,
        removeFilesHook
      ),
      error = function(error) {
        sprintf("Aufräumschritt fehlgeschlagen: %s", conditionMessage(error))
      }
    )

    list(
      ok = TRUE,
      workingPath = workingPath,
      candidatePath = candidatePath,
      when = nowUtc(),
      journal = as.list(journal),
      warnings = cleanupWarnings
    )
  }, error = function(error) {
    if (!committed) {
      residual = rollbackPreparation()
      message = conditionMessage(error)
      if (length(residual) > 0L) {
        message = paste0(
          message,
          " Restressourcen (Verbindung bleibt im begrenzten Vertrag erhalten): ",
          paste0("'", unique(residual), "'", collapse = ", ")
        )
      }
      list(ok = FALSE, message = message, when = nowUtc(), residual = residual)
    } else {
      # Defensive: nothing between commit point and Phase C can throw, but if
      # code ever does after the switch, the committed state stands and the
      # event is recorded as a warning rather than disguised as a failure.
      list(
        ok = TRUE,
        workingPath = state$dbPath,
        candidatePath = "",
        when = nowUtc(),
        journal = as.list(journal),
        warnings = sprintf(
          "Nach dem Umschalten fehlgeschlagener Schritt: %s",
          conditionMessage(error)
        )
      )
    }
  })

  state$lastImportStatus = result
  if (!isTRUE(result$ok)) {
    stop(paste0("Import fehlgeschlagen: ", result$message), call. = FALSE)
  }
  invisible(result)
}
