# Database service functions for the connectTest Stage A experiment.
# These functions contain no Shiny code so they can be tested directly.

experimentIdentity = function() {
  list(
    appId = "connectTest",
    schemaName = "connectTest.stageA",
    schemaVersion = 1L
  )
}

nowUtc = function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

openDatabase = function(dbPath, readOnly = FALSE) {
  if (!is.character(dbPath) || length(dbPath) != 1L || !nzchar(dbPath)) {
    stop("Ein Datenbankpfad wird benötigt.", call. = FALSE)
  }
  if (readOnly && !file.exists(dbPath)) {
    stop("Die Datenbankdatei existiert nicht.", call. = FALSE)
  }
  driver = duckdb::duckdb(shared_home = FALSE)
  DBI::dbConnect(driver, dbdir = dbPath, read_only = readOnly)
}

initializeDatabase = function(conn) {
  identity = experimentIdentity()
  createdAt = nowUtc()

  DBI::dbBegin(conn)
  tryCatch({
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
  if (nchar(title) > 200L) {
    stop("Der Rezepttitel ist zu lang (höchstens 200 Zeichen).", call. = FALSE)
  }
  if (is.na(instructions) || !nzchar(instructions)) {
    stop("Die Zubereitung darf nicht leer sein.", call. = FALSE)
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
  if (nchar(collectionName) > 300L) {
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

  state = new.env(parent = emptyenv())
  state$sessionLabel = label
  state$sessionDir = sessionDir
  state$dbPath = file.path(sessionDir, "working.duckdb")
  state$exportDir = exportDir
  state$conn = NULL
  state$mutationGeneration = 0L
  state$snapshotGeneration = -1L
  state$snapshotCounter = 0L
  state$snapshotPath = NULL
  state$snapshotCreatedAt = NULL
  state$lastSnapshotError = NULL
  state$snapshotBusy = FALSE
  state$recoveredFromSnapshot = FALSE
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

cleanupSession = function(state) {
  if (!is.environment(state) || isTRUE(state$cleaned)) {
    return(invisible(FALSE))
  }
  state$cleaned = TRUE
  state$active = FALSE

  if (!is.null(state$conn)) {
    try(DBI::dbDisconnect(state$conn, shutdown = TRUE), silent = TRUE)
  }
  state$conn = NULL
  state$snapshotPath = NULL

  if (!is.null(state$sessionDir) && dir.exists(state$sessionDir)) {
    unlink(state$sessionDir, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

hasUnsnapshottedChanges = function(state) {
  isTRUE(state$mutationGeneration > state$snapshotGeneration)
}
