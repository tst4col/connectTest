# Owner-only upload gate for the Stage B experiment.
#
# Two runtime secrets, both supplied by the owner through Connect
# configuration and never stored in a database, repository, log or test:
#   CONNECTTEST_UPLOAD_ENABLED   exactly one of "1","true","yes","on"
#                                compared case-insensitively after trimming
#   CONNECTTEST_UPLOAD_PASSWORD  nonempty after trimming, <= 128 bytes
#
# Missing/blank secret or a disabled flag fails closed. A gate password is
# only a restriction of who may upload; it is not a hostile-file sandbox and
# it grants no platform-private access.

uploadGateEnabledValues = c("1", "true", "yes", "on")
uploadGatePasswordMaxBytes = 128L

gateSessionMaxFailedAttempts = 5L
gateSessionCooldownSeconds = 300L
gateProcessWindowSeconds = 60L
gateProcessWindowMaxAttempts = 10L
gateProcessTimestampCap = 256L

# Process-wide, fixed-capacity attempt log (bounded memory: timestamps are
# cropped to the cap and window-filtered; no per-caller growth).
.uploadGateThrottle = new.env(parent = emptyenv())
.uploadGateThrottle$attempts = numeric(0)

resolveUploadGate = function(
  enabledValue = Sys.getenv("CONNECTTEST_UPLOAD_ENABLED"),
  passwordValue = Sys.getenv("CONNECTTEST_UPLOAD_PASSWORD")
) {
  enabled = is.character(enabledValue) && length(enabledValue) == 1L &&
    tolower(trimws(enabledValue)) %in% uploadGateEnabledValues
  if (!enabled) {
    return(list(
      enabled = FALSE,
      passwordOk = FALSE,
      reason = "Uploads sind deaktiviert oder nicht freigeschaltet."
    ))
  }

  password = trimws(suppressWarnings(as.character(passwordValue)[1]))
  if (is.na(password)) {
    password = ""
  }
  passwordBytes = nchar(password, type = "bytes")
  if (passwordBytes == 0L) {
    # Fail closed: uploads stay disabled without a configured secret. The
    # reason must exist for diagnostics but reveals no password material.
    return(list(
      enabled = FALSE,
      passwordOk = FALSE,
      reason = "Uploads sind deaktiviert: kein Passwort konfiguriert."
    ))
  }
  if (passwordBytes > uploadGatePasswordMaxBytes) {
    return(list(
      enabled = FALSE,
      passwordOk = FALSE,
      reason = "Uploads sind deaktiviert: das konfigurierte Passwort ist zu lang."
    ))
  }

  list(enabled = TRUE, passwordOk = TRUE, reason = NULL)
}

resetGateThrottle = function() {
  .uploadGateThrottle$attempts = numeric(0)
  invisible(TRUE)
}

# Fixed-capacity rolling-window attempt log, shared by the whole process
# (bounded memory: at most gateProcessTimestampCap timestamps are kept).
recordGateAttempt = function(nowValue = as.numeric(Sys.time())) {
  attempts = c(.uploadGateThrottle$attempts, nowValue)
  attempts = attempts[attempts > nowValue - gateProcessWindowSeconds]
  if (length(attempts) > gateProcessTimestampCap) {
    attempts = tail(attempts, gateProcessTimestampCap)
  }
  .uploadGateThrottle$attempts = attempts
  invisible(TRUE)
}

gateProcessThrottleAllows = function(nowValue = as.numeric(Sys.time())) {
  attempts = .uploadGateThrottle$attempts
  attempts = attempts[attempts > nowValue - gateProcessWindowSeconds]
  length(attempts) < gateProcessWindowMaxAttempts
}

# Fixed-width byte comparison for base R. Differences accumulate with
# bitwise OR so they can never cancel (XOR cancellation is the C1 failure
# mode). Invalid (non-string, NA, wrong length) and overlong inputs are
# rejected before any byte comparison. No proven constant-time guarantee is
# claimed for base R (best effort, assumption A5).
isOwnerPasswordMatch = function(candidate, expected) {
  if (!is.character(candidate) || length(candidate) != 1L || is.na(candidate)) {
    return(FALSE)
  }
  if (!is.character(expected) || length(expected) != 1L || is.na(expected)) {
    return(FALSE)
  }

  candidateBytes = charToRaw(candidate)
  expectedBytes = charToRaw(expected)
  # Overlong inputs are rejected outright (the configured password cannot
  # exceed the cap; a longer candidate can never be equal to it).
  if (length(candidateBytes) > uploadGatePasswordMaxBytes ||
      length(expectedBytes) > uploadGatePasswordMaxBytes) {
    return(FALSE)
  }

  width = uploadGatePasswordMaxBytes
  accumulator = 0L
  for (offset in seq_len(width)) {
    left = if (offset <= length(candidateBytes)) as.integer(candidateBytes[[offset]]) else 0L
    right = if (offset <= length(expectedBytes)) as.integer(expectedBytes[[offset]]) else 0L
    accumulator = bitwOr(accumulator, bitwXor(left, right))
  }
  # Any length difference is folded into the same non-cancelling accumulator.
  accumulator = bitwOr(
    accumulator,
    bitwXor(length(candidateBytes), length(expectedBytes))
  )
  accumulator == 0L
}

authFailureGeneric = function() {
  "Passwort nicht akzeptiert. Upload ist weiter gesperrt."
}

# Verifies a password attempt against server state; the client value is never
# logged or echoed. On success the session-sticky server flag
# state$ownerUploadAuthed flips to TRUE (review question 4; kept from the
# reviewed design). Bounded: 5 failures trigger a 300 s session cooldown;
# every attempt hits the 10-per-60-s process-wide window.
attemptOwnerUploadAuthorisation = function(state, passwordValue, nowValue = Sys.time()) {
  gate = resolveUploadGate()
  if (!isTRUE(gate$enabled)) {
    return(list(ok = FALSE, reason = gate$reason))
  }
  if (inherits(state$gateCooldownUntil, "POSIXct") &&
      nowValue < state$gateCooldownUntil) {
    remaining = as.integer(difftime(state$gateCooldownUntil, nowValue, units = "secs"))
    return(list(
      ok = FALSE,
      reason = sprintf(
        "Zu viele fehlgeschlagene Versuche. Wiederholung in %d Sekunden möglich.",
        max(remaining, 0L)
      )
    ))
  }
  if (!gateProcessThrottleAllows(as.numeric(nowValue))) {
    return(list(
      ok = FALSE,
      reason = "Zu viele Versuche im Zeitfenster. Bitte kurz warten."
    ))
  }
  recordGateAttempt(as.numeric(nowValue))

  expected = trimws(suppressWarnings(as.character(Sys.getenv("CONNECTTEST_UPLOAD_PASSWORD"))[1]))
  if (isOwnerPasswordMatch(passwordValue, expected)) {
    state$ownerUploadAuthed = TRUE
    state$gateFailedAttempts = 0L
    return(list(ok = TRUE, reason = NULL))
  }

  state$gateFailedAttempts = state$gateFailedAttempts + 1L
  if (state$gateFailedAttempts >= gateSessionMaxFailedAttempts) {
    state$gateCooldownUntil = nowValue + gateSessionCooldownSeconds
    state$gateFailedAttempts = 0L
    return(list(
      ok = FALSE,
      reason = sprintf(
        "Passwort nicht akzeptiert. Wiederholung in %d Sekunden möglich.",
        gateSessionCooldownSeconds
      )
    ))
  }
  list(ok = FALSE, reason = authFailureGeneric())
}

# Import-service-boundary check. The proof comes from server state only; no
# client-supplied boolean can satisfy it.
requireOwnerUploadAuthorisation = function(state) {
  gate = resolveUploadGate()
  if (!isTRUE(gate$enabled)) {
    stop("Uploads sind in dieser Konfiguration deaktiviert.", call. = FALSE)
  }
  if (!is.environment(state) || !isTRUE(state$ownerUploadAuthed)) {
    stop("Für den Upload ist eine Besitzerfreigabe erforderlich.", call. = FALSE)
  }
  invisible(TRUE)
}
