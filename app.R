source("R/database.R")
source("R/snapshot.R")

library(shiny)
library(DBI)
library(duckdb)

ui = fluidPage(
  titlePanel("connectTest – Stage A Experiment"),
  div(
    class = "alert alert-warning",
    strong("Temporäre Arbeitskopie."),
    " Diese Anwendung legt für jede Browser-Sitzung eine eigene DuckDB-Datenbank an.",
    " Die gehostete Kopie ist nur vorübergehend und wird beim Ende der Sitzung verworfen.",
    " Änderungen bleiben nur erhalten, wenn Sie eine eigenständige Datenbank herunterladen",
    " und die Datei bei sich dauerhaft speichern. Ein Download ersetzt kein dauerhaftes",
    " Speichern; die Anwendung kann nicht erkennen, ob Ihr Browser die Datei gespeichert hat."
  ),
  fluidRow(
    column(
      width = 6,
      wellPanel(
        h3("Rezept bearbeiten"),
        textInput("recipeTitle", "Titel", value = ""),
        numericInput("recipeServings", "Portionen", value = 4, min = 1, max = 100, step = 1),
        textAreaInput(
          "recipeInstructions",
          "Zubereitung",
          value = "",
          rows = 7,
          resize = "vertical"
        ),
        actionButton("saveRecipe", "Rezept speichern", class = "btn-primary")
      )
    ),
    column(
      width = 6,
      wellPanel(
        h3("Sammlungseinstellung bearbeiten"),
        textInput("collectionName", "Sammlungsname und Portionshinweis", value = ""),
        actionButton("saveCollectionSetting", "Einstellung speichern", class = "btn-primary")
      ),
      wellPanel(
        h3("Momentaufnahme und Download"),
        p(
          "Die Momentaufnahme schließt die Arbeitsdatenbank kurz, kopiert sie als",
          "eigenständige Datei und öffnet die Arbeitsdatenbank wieder. Währenddessen",
          "sind Änderungen gesperrt. Erst eine erfolgreich erzeugte Momentaufnahme",
          "kann heruntergeladen werden."
        ),
        actionButton("makeSnapshot", "Momentaufnahme erzeugen"),
        uiOutput("downloadUi"),
        uiOutput("snapshotStatus")
      )
    )
  ),
  uiOutput("dirtyStatus"),
  h3("Aktueller Inhalt der Arbeitsdatenbank"),
  tableOutput("recipePreview"),
  tableOutput("settingPreview"),
  p(
    class = "text-muted",
    "Experimentelles Schema: connectTest.stageA, Version 1. Keine dauerhafte Speicherung,",
    " kein Upload und keine Anmeldung in Stage A."
  )
)

server = function(input, output, session) {
  state = newSessionState(session$token)
  session$onSessionEnded(function() {
    cleanupSession(state)
  })

  stateVersion = reactiveVal(0L)
  bump = function() {
    stateVersion(isolate(stateVersion()) + 1L)
  }

  initialRecipe = readRecipe(state$conn)
  initialSetting = readCollectionSetting(state$conn)

  updateTextInput(session, "recipeTitle", value = initialRecipe$title[1])
  updateNumericInput(session, "recipeServings", value = initialRecipe$servings[1])
  updateTextAreaInput(session, "recipeInstructions", value = initialRecipe$instructions[1])
  updateTextInput(session, "collectionName", value = initialSetting$collection_name[1])

  observeEvent(input$saveRecipe, {
    tryCatch({
      saveRecipe(
        state,
        input$recipeTitle,
        input$recipeInstructions,
        input$recipeServings
      )
      bump()
      showNotification("Rezept wurde in der Arbeitsdatenbank gespeichert.", type = "message")
    }, error = function(error) {
      showNotification(
        paste("Rezept nicht gespeichert:", conditionMessage(error)),
        type = "error",
        duration = 8
      )
    })
  }, ignoreInit = TRUE)

  observeEvent(input$saveCollectionSetting, {
    tryCatch({
      saveCollectionSetting(state, input$collectionName)
      bump()
      showNotification("Sammlungseinstellung wurde in der Arbeitsdatenbank gespeichert.", type = "message")
    }, error = function(error) {
      showNotification(
        paste("Einstellung nicht gespeichert:", conditionMessage(error)),
        type = "error",
        duration = 8
      )
    })
  }, ignoreInit = TRUE)

  observeEvent(input$makeSnapshot, {
    tryCatch({
      createSnapshot(state)
      bump()
      showNotification("Momentaufnahme wurde erzeugt.", type = "message")
    }, error = function(error) {
      bump()
      showNotification(
        conditionMessage(error),
        type = "error",
        duration = 12
      )
    })
  }, ignoreInit = TRUE)

  output$downloadUi = renderUI({
    stateVersion()
    if (is.null(state$snapshotPath) || !file.exists(state$snapshotPath)) {
      return(tags$p(class = "text-muted", "Noch keine Momentaufnahme vorhanden."))
    }
    tagList(
      downloadButton("downloadDatabase", "Standalone-Datenbank herunterladen"),
      if (hasUnsnapshottedChanges(state)) {
        tags$p(
          class = "text-danger",
          "Die angebotene Momentaufnahme ist älter als die aktuellen Änderungen.",
          "Erzeugen Sie vor dem Download eine neue Momentaufnahme."
        )
      }
    )
  })

  output$snapshotStatus = renderUI({
    stateVersion()
    if (!is.null(state$lastSnapshotError)) {
      tags$div(
        class = "alert alert-danger",
        strong("Letzter Fehler:"),
        " ",
        state$lastSnapshotError
      )
    } else if (is.null(state$snapshotPath)) {
      tags$p(class = "text-muted", "Noch keine Momentaufnahme erzeugt.")
    } else {
      tagList(
        tags$p(
          strong("Momentaufnahme erzeugt:"),
          " ",
          basename(state$snapshotPath),
          " (",
          state$snapshotCreatedAt,
          " UTC)"
        ),
        tags$p(
          class = "text-muted",
          "Die App kann nicht feststellen, ob der Browser die Datei heruntergeladen",
          "oder dauerhaft gespeichert hat. Prüfen Sie den Speicherort selbst."
        )
      )
    }
  })

  output$dirtyStatus = renderUI({
    stateVersion()
    if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
      return(tags$div(
        class = "alert alert-danger",
        strong("Arbeitsdatenbank nicht verbunden."),
        " In dieser Sitzung sind keine weiteren Änderungen möglich."
      ))
    }
    if (hasUnsnapshottedChanges(state)) {
      if (isTRUE(state$snapshotGeneration < 0L)) {
        tags$div(
          class = "alert alert-warning",
          strong("Status: Noch keine Momentaufnahme."),
          " Die Arbeitskopie enthält den Ausgangsstand oder bereits Änderungen und ist",
          " nur vorübergehend. Erzeugen und speichern Sie eine Momentaufnahme, um einen",
          " Stand außerhalb der Sitzung zu behalten."
        )
      } else {
        tags$div(
          class = "alert alert-warning",
          strong("Status: Änderungen seit der letzten Momentaufnahme."),
          " Die Arbeitskopie ist weiterhin temporär; ein Download ist erforderlich,",
          " um einen Stand außerhalb der Sitzung zu behalten."
        )
      }
    } else {
      tags$div(
        class = "alert alert-success",
        strong("Status: Keine Änderungen seit der letzten Momentaufnahme."),
        " Das bedeutet nicht, dass der Browser die Momentaufnahme dauerhaft gespeichert hat."
      )
    }
  })

  output$recipePreview = renderTable({
    stateVersion()
    if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
      return(data.frame(Hinweis = "Keine Verbindung zur Arbeitsdatenbank."))
    }
    recipe = readRecipe(state$conn)
    data.frame(
      recipe_id = recipe$recipe_id,
      title = recipe$title,
      servings = recipe$servings,
      instructions = recipe$instructions,
      updated_at = recipe$updated_at,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$settingPreview = renderTable({
    stateVersion()
    if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
      return(data.frame(Hinweis = "Keine Verbindung zur Arbeitsdatenbank."))
    }
    setting = readCollectionSetting(state$conn)
    data.frame(
      setting_id = setting$setting_id,
      collection_name = setting$collection_name,
      updated_at = setting$updated_at,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$downloadDatabase = downloadHandler(
    filename = function() {
      if (is.null(state$snapshotPath)) {
        "connectTest.duckdb"
      } else {
        basename(state$snapshotPath)
      }
    },
    content = function(file) {
      path = state$snapshotPath
      if (is.null(path) || !file.exists(path)) {
        stop("Es ist keine Momentaufnahme zum Herunterladen vorhanden.")
      }
      if (!file.copy(path, file, overwrite = TRUE)) {
        stop("Die Momentaufnahme konnte nicht bereitgestellt werden.")
      }
    },
    contentType = "application/octet-stream"
  )
}

shinyApp(ui, server)
