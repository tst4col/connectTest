#!/usr/bin/env Rscript
# Regenerate the single Connect Cloud dependency file from the installed
# project library. rsconnect is development tooling, not an application
# dependency; install it into rig's tools library, for example:
#   rig pkg install -r 4.6.0 -l tools rsconnect
# Then run from the repository root:
#   .rvenv/bin/Rscript tools/generate-manifest.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    "rsconnect is required to generate manifest.json. ",
    "Install it into the rig tools library; it is not an application dependency.",
    call. = FALSE
  )
}

appFiles = c(
  "app.R",
  "R/database.R",
  "R/snapshot.R",
  "R/gate.R",
  "R/import.R"
)

missing = appFiles[!file.exists(appFiles)]
if (length(missing) > 0L) {
  stop("Missing application file(s): ", paste(missing, collapse = ", "), call. = FALSE)
}

rsconnect::writeManifest(
  appDir = ".",
  appFiles = appFiles,
  appMode = "shiny",
  contentCategory = "application",
  dependencyResolution = "strict",
  quiet = FALSE
)

cat("Wrote manifest.json for R", as.character(getRversion()), "\n")
