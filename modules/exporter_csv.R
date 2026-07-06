# ==============================================================================
# MODULE : Export CSV/Excel
# ==============================================================================

# UI
exporter_csv_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(width = 12, h3("Exporter la liste des questionnaires et la liste des anomalies"))),
    br(),
    fluidRow(
      materialSwitch(inputId = ns("showComment"), label = span("Commentaire : Afficher/Masquer", style = "color : #797979;"),
        value = TRUE, status = "primary", right = TRUE)
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("showComment")),
      fluidRow(column(width = 12, verbatimTextOutput(ns("consigne"))))
    ),
    br(),
    fluidRow(column(width = 10, selectInput(ns("exportType"), "Export des :",
      c(Questionnaires = "quest", Anomalies = "ano")))),

    # Export Questionnaires
    conditionalPanel(
      condition = sprintf("input['%s'] == 'quest'", ns("exportType")),
      fluidPage(
        fluidRow(column(width = 2, "Sélection des questionnaires")), br(),
        fluidRow(column(width = 2, selectizeInput(ns("inputEnquete"), "Enquête :", choices = NULL, multiple = TRUE))), br(),
        fluidRow(
          column(width = 1, offset = 1, downloadButton(ns("saveQuestCSV"), "CSV")),
          column(width = 1, offset = 1, downloadButton(ns("saveQuestExcel"), "Excel"))
        )
      )
    ),

    # Export Anomalies
    conditionalPanel(
      condition = sprintf("input['%s'] == 'ano'", ns("exportType")),
      fluidPage(
        fluidRow(column(width = 2, "Sélection des anomalies")), br(),
        fluidRow(column(width = 2, selectizeInput(ns("inputAnoEnquete"), "Enquête :", choices = NULL, multiple = TRUE))), br(),
        fluidRow(
          column(width = 1, offset = 1, downloadButton(ns("saveAnoCSV"), "CSV")),
          column(width = 1, offset = 1, downloadButton(ns("saveAnoExcel"), "Excel"))
        )
      )
    )
  )
}

# Server
exporter_csv_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$consigne <- renderText({ cst_commentaire_export_csv })

    # Alimentation des inputs
    observe({
      req(donnees_globales$df_questionnaire)
      updateSelectizeInput(session, "inputEnquete", choices = donnees_globales$df_questionnaire %>% pull("ENQUETE") %>% unique() %>% sort())
      updateSelectizeInput(session, "inputAnoEnquete", choices = donnees_globales$df_questionnaire %>% pull("ENQUETE") %>% unique() %>% sort())
    })

    # Export Questionnaires
    output$saveQuestCSV <- downloadHandler(
      filename = function() paste(Sys.Date(), "_QUESTIONNAIRES.csv"),
      content = function(file) {
        chemin <- "Logs/Donnees_traitees/QUESTIONNAIRES.csv"
        if (file.exists(chemin)) {
          write.csv2(read_csv2(chemin, guess_max = 10000) %>%
            filter(ENQUETE %in% input$inputEnquete | is.null(input$inputEnquete)),
            file, na = "")
        }
      }
    )

    output$saveQuestExcel <- downloadHandler(
      filename = function() paste(Sys.Date(), "_QUESTIONNAIRES.xlsx"),
      content = function(file) {
        chemin <- "Logs/Donnees_traitees/QUESTIONNAIRES.csv"
        if (file.exists(chemin)) {
          write.xlsx2(read_csv2(chemin, guess_max = 10000) %>%
            filter(ENQUETE %in% input$inputEnquete | is.null(input$inputEnquete)),
            file, showNA = FALSE)
        }
      }
    )

    # Export Anomalies
    output$saveAnoCSV <- downloadHandler(
      filename = function() paste(Sys.Date(), "_ANOMALIES.csv"),
      content = function(file) {
        chemin <- "Logs/Donnees_traitees/ANOMALIES.csv"
        if (file.exists(chemin)) {
          write_csv2(read_csv2(chemin, guess_max = 10000) %>%
            filter(if (is.null(input$inputAnoEnquete)) TRUE
              else str_detect(IDENTIFIANT_SUIVALIAA, paste0(input$inputAnoEnquete, collapse = "|"))),
            file, na = "")
        }
      }
    )

    output$saveAnoExcel <- downloadHandler(
      filename = function() paste(Sys.Date(), "_ANOMALIES.xlsx"),
      content = function(file) {
        chemin <- "Logs/Donnees_traitees/ANOMALIES.csv"
        if (file.exists(chemin)) {
          write.xlsx2(read_csv2(chemin, guess_max = 10000) %>%
            filter(if (is.null(input$inputAnoEnquete)) TRUE
              else str_detect(IDENTIFIANT_SUIVALIAA, paste0(input$inputAnoEnquete, collapse = "|"))),
            file, showNA = FALSE)
        }
      }
    )
  })
}