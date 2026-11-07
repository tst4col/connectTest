# Standalone snapshot creation for the connectTest Stage A experiment.
# A snapshot closes the working connection under the session's mutation guard,
# copies the checkpointed database file, validates the copy read-only, reopens
# the working database, and only then publishes the new snapshot path.

validateSnapshotFile = function(path) {
  conn = openDatabase(path, readOnly = TRUE)
  on.exit(try(DBI::dbDisconnect(conn, shutdown = TRUE), silent = TRUE), add = TRUE)

  identity = experimentIdentity()
  schema = readExperimentSchema(conn)
  recipe = readRecipe(conn)
  setting = readCollectionSetting(conn)

  if (!identical(schema$app_id[1], identity$appId) ||
      !identical(schema$schema_name[1], identity$schemaName) ||
      !identical(as.integer(schema$schema_version[1]), identity$schemaVersion)) {
    stop("Die kopierte Datenbank hat eine unerwartete Anwendungsidentität.", call. = FALSE)
  }
  if (!nzchar(recipe$title[1]) || !nzchar(setting$collection_name[1])) {
    stop("Die kopierte Datenbank enthält keine vollständigen Beispieldaten.", call. = FALSE)
  }
  invisible(TRUE)
}

recoverWorkingDatabase = function(state, partialPath, reopen) {
  if (!file.exists(partialPath)) {
    return(NULL)
  }
  valid = tryCatch({
    validateSnapshotFile(partialPath)
    TRUE
  }, error = function(error) FALSE)
  if (!valid) {
    return(NULL)
  }

  # Use a fresh candidate path. The current working database must never be
  # overwritten or removed during recovery, and a candidate is only promoted
  # after it has reopened successfully.
  recoveryPath = tempfile(
    pattern = "working-recovery-",
    tmpdir = state$sessionDir,
    fileext = ".duckdb"
  )
  if (identical(recoveryPath, state$dbPath)) {
    return(NULL)
  }
  copied = suppressWarnings(file.copy(
    partialPath,
    recoveryPath,
    overwrite = FALSE,
    copy.mode = FALSE,
    copy.date = FALSE
  ))
  if (!isTRUE(copied) || !file.exists(recoveryPath)) {
    unlink(recoveryPath, force = TRUE)
    return(NULL)
  }

  recovered = tryCatch(reopen(recoveryPath), error = function(error) NULL)
  if (is.null(recovered) || !DBI::dbIsValid(recovered)) {
    if (!is.null(recovered)) {
      try(DBI::dbDisconnect(recovered, shutdown = TRUE), silent = TRUE)
    }
    # Remove only the failed candidate. The validated partial and the current
    # working database remain available for a later recovery attempt.
    unlink(recoveryPath, force = TRUE)
    return(NULL)
  }

  state$dbPath = recoveryPath
  state$recoveredFromSnapshot = TRUE
  recovered
}

createSnapshot = function(state, destinationDir = state$exportDir, hooks = list()) {
  if (!is.environment(state)) {
    stop("Ungültiger Sitzungszustand.", call. = FALSE)
  }
  if (!isTRUE(state$active) || isTRUE(state$cleaned)) {
    stop("Die Sitzung ist beendet.", call. = FALSE)
  }
  if (isTRUE(state$snapshotBusy)) {
    stop("Es läuft bereits eine Momentaufnahme.", call. = FALSE)
  }
  if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
    stop("Die Arbeitsdatenbank ist nicht verbunden.", call. = FALSE)
  }

  state$snapshotBusy = TRUE
  state$recoveredFromSnapshot = FALSE
  on.exit({
    state$snapshotBusy = FALSE
  }, add = TRUE)

  generation = state$mutationGeneration
  state$snapshotCounter = state$snapshotCounter + 1L
  stamp = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  finalPath = file.path(
    destinationDir,
    sprintf("connectTest-%s-%02d.duckdb", stamp, state$snapshotCounter)
  )
  partialPath = paste0(finalPath, ".partial")
  dir.create(destinationDir, recursive = TRUE, showWarnings = FALSE)
  unlink(partialPath, force = TRUE)

  reopen = if (is.function(hooks$openDatabase)) hooks$openDatabase else openDatabase

  result = tryCatch({
    if (is.function(hooks$beforeClose)) {
      hooks$beforeClose(state)
    }

    DBI::dbExecute(state$conn, "CHECKPOINT")
    DBI::dbDisconnect(state$conn, shutdown = TRUE)
    state$conn = NULL

    walPath = paste0(state$dbPath, ".wal")
    if (file.exists(walPath)) {
      stop("Nach dem Schließen ist noch eine WAL-Datei vorhanden.", call. = FALSE)
    }
    if (!file.exists(state$dbPath)) {
      stop("Die Arbeitsdatenbank fehlt nach dem Schließen.", call. = FALSE)
    }

    copied = suppressWarnings(file.copy(
      state$dbPath,
      partialPath,
      overwrite = TRUE,
      copy.mode = FALSE,
      copy.date = FALSE
    ))
    if (!isTRUE(copied) || !file.exists(partialPath) || file.size(partialPath) <= 0) {
      stop("Die Datenbankdatei konnte nicht kopiert werden.", call. = FALSE)
    }

    if (is.function(hooks$beforeReopen)) {
      hooks$beforeReopen(state)
    }
    state$conn = reopen(state$dbPath)
    if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
      stop("Die Arbeitsdatenbank konnte nicht wieder geöffnet werden.", call. = FALSE)
    }

    validateSnapshotFile(partialPath)
    partialWal = paste0(partialPath, ".wal")
    if (file.exists(partialWal)) {
      stop("Die validierte Momentaufnahme hat eine unerwartete WAL-Datei.", call. = FALSE)
    }

    if (!file.rename(partialPath, finalPath)) {
      stop("Die Momentaufnahme konnte nicht an ihren Zielpfad verschoben werden.", call. = FALSE)
    }

    state$snapshotPath = finalPath
    state$snapshotGeneration = generation
    state$snapshotCreatedAt = nowUtc()
    state$lastSnapshotError = NULL

    list(ok = TRUE, path = finalPath)
  }, error = function(error) {
    list(ok = FALSE, message = conditionMessage(error))
  })

  if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
    state$conn = NULL
    recovered = tryCatch(reopen(state$dbPath), error = function(error) error)
    reopenError = if (inherits(recovered, "error")) conditionMessage(recovered) else NULL
    recoveredOk = !inherits(recovered, "error") &&
      !is.null(recovered) &&
      DBI::dbIsValid(recovered)

    if (!recoveredOk) {
      if (!is.null(recovered) && !inherits(recovered, "error")) {
        try(DBI::dbDisconnect(recovered, shutdown = TRUE), silent = TRUE)
      }
      recovered = recoverWorkingDatabase(state, partialPath, reopen)
      recoveredOk = !is.null(recovered) && DBI::dbIsValid(recovered)
    }

    if (!recoveredOk) {
      state$conn = NULL
      preservedPaths = unique(c(state$dbPath, partialPath))
      preservedPaths = preservedPaths[file.exists(preservedPaths)]
      preservedText = if (length(preservedPaths) > 0L) {
        paste0(
          "Folgende Dateien bleiben auf dem Datenträger erhalten: ",
          paste0("'", preservedPaths, "'", collapse = ", "),
          "."
        )
      } else {
        "Es konnte keine Datenbankdatei auf dem Datenträger gefunden werden."
      }
      state$lastSnapshotError = paste0(
        result$message,
        " / Wiederverbinden fehlgeschlagen: ",
        if (is.null(reopenError)) "unbekannter Fehler" else reopenError,
        ". ",
        preservedText
      )
      stop(
        paste0(
          "Momentaufnahme fehlgeschlagen: ", state$lastSnapshotError,
          " In dieser Sitzung ist keine Arbeitsdatenbankverbindung aktiv."
        ),
        call. = FALSE
      )
    }

    state$conn = recovered
    if (isTRUE(state$recoveredFromSnapshot)) {
      result$message = paste0(
        result$message,
        " / Arbeitsdatenbank wurde aus der validierten Kopie wiederhergestellt."
      )
    }
  }

  unlink(partialPath, force = TRUE)

  if (!isTRUE(result$ok)) {
    state$lastSnapshotError = result$message
    stop(paste0("Momentaufnahme fehlgeschlagen: ", result$message), call. = FALSE)
  }

  invisible(result$path)
}
