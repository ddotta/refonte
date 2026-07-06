# ==============================================================================
# MODULE : Anomalies archivées
# ==============================================================================

# UI
anomalies_archivees_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(width = 12, h3("Visualisation des anomalies des collectes terminées"))),
    br(),
    fluidRow(
      column(width = 3,
        materialSwitch(inputId = ns("switchFiltres"), label = span("Filtres : Afficher/Masquer", style = "color : #797979;"),
          value = TRUE, status = "primary", right = TRUE)
      ),
      column(width = 3, offset = 6,
        pickerInput(ns("choixColonnes"), "Colonnes à afficher",
          choices = NULL, multiple = TRUE,
          options = list(`selected-text-format` = "count",
            `count-selected-text` = "{0} colonnes selectionnées sur {1}"))
      )
    ),
    br(),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("switchFiltres")),
      fluidRow(column(width = 12, "Filtres sur les questionnaires et anomalies :")), br(),
      fluidRow(
        column(width = 3, selectizeInput(ns("filtreEnquete"), "Enquête :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreCampagne"), "Campagne :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreVague"), "Vague :", choices = NULL, multiple = TRUE))
      ),
      fluidRow(
        column(width = 3, selectizeInput(ns("filtreSiret"), "SIRET :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreRaison"), "Raison sociale :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreGestionnaire"), "Gestionnaire référent :", choices = NULL, multiple = TRUE))
      ),
      fluidRow(
        column(width = 4, offset = 5, actionButton(ns("resetFiltres"), "Réinitialiser les filtres"))
      )
    ),
    br(),
    fluidRow(
      column(width = 1, offset = 1, downloadButton(ns("saveCSV"), "CSV")),
      column(width = 1, downloadButton(ns("saveExcel"), "Excel"))
    ),
    br(),
    fluidRow(column(width = 12, dataTableOutput(ns("table"), width = "100%", height = "auto")))
  )
}

# Server
anomalies_archivees_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(donnees_globales$df_anomalies_archivees)
      colonnes <- colnames(donnees_globales$df_anomalies_archivees)

      updatePickerInput(session, "choixColonnes",
        choices = colonnes,
        selected = c(colonnes[1:6], colonnes[16:17], colonnes[19:22], colonnes[27:28]))

      updateSelectizeInput(session, "filtreEnquete",
        choices = donnees_globales$df_anomalies_archivees %>% pull("ENQUETE") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreCampagne",
        choices = donnees_globales$df_anomalies_archivees %>% pull("CAMPAGNE") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreGestionnaire",
        choices = donnees_globales$df_anomalies_archivees %>% pull("GESTIONNAIRE_REF") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreSiret",
        choices = donnees_globales$df_anomalies_archivees %>% pull("SIRET") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreRaison",
        choices = donnees_globales$df_anomalies_archivees %>% pull("RAISON_SOCIALE") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreVague",
        choices = donnees_globales$df_anomalies_archivees %>% pull("VAGUE") %>% unique() %>% sort())
    })

    observeEvent(input$resetFiltres, {
      updateSelectizeInput(session, "filtreEnquete", selected = "")
      updateSelectizeInput(session, "filtreCampagne", selected = "")
      updateSelectizeInput(session, "filtreGestionnaire", selected = "")
      updateSelectizeInput(session, "filtreSiret", selected = "")
      updateSelectizeInput(session, "filtreRaison", selected = "")
      updateSelectizeInput(session, "filtreVague", selected = "")
    })

    donneesFiltrees <- reactive({
      req(donnees_globales$df_anomalies_archivees)
      df <- donnees_globales$df_anomalies_archivees %>%
        filter(ENQUETE %in% input$filtreEnquete | is.null(input$filtreEnquete)) %>%
        filter(CAMPAGNE %in% input$filtreCampagne | is.null(input$filtreCampagne)) %>%
        filter(GESTIONNAIRE_REF %in% input$filtreGestionnaire | is.null(input$filtreGestionnaire)) %>%
        filter(SIRET %in% input$filtreSiret | is.null(input$filtreSiret)) %>%
        filter(RAISON_SOCIALE %in% input$filtreRaison | is.null(input$filtreRaison)) %>%
        filter(VAGUE %in% input$filtreVague | is.null(input$filtreVague))
      return(df)
    })

    output$table <- renderDataTable(
      donneesFiltrees() %>% select(input$choixColonnes),
      extensions = c("FixedHeader"),
      options = list(dom = 'lfrtip',
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        language = fr, scrollY = 300, fixedHeader = TRUE,
        pageLength = 50, lengthMenu = c(5, 10, 50), scrollX = TRUE),
      class = "nowrap"
    )

    output$saveCSV <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_ANOMALIE_ARCHIVEE.csv"),
      content = function(file) {
        write_csv2(donneesFiltrees(), paste0(cst_chemin_sauvegarde_donnees, Sys.Date(), "_SUIVAL_IAA_ANOMALIE_ARCHIVEE.csv"))
        write_csv2(donneesFiltrees(), file)
      }
    )

    output$saveExcel <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_ANOMALIE_ARCHIVEE.xlsx"),
      content = function(file) {
        write.xlsx(donneesFiltrees(), paste0(cst_chemin_sauvegarde_donnees, Sys.Date(), "_SUIVAL_IAA_ANOMALIE_ARCHIVEE.xlsx"),
          sheetName = "ANOMALIE_ARCHIVEE", append = FALSE)
        write.xlsx(donneesFiltrees(), file, sheetName = "ANOMALIE_ARCHIVEE", append = FALSE)
      }
    )
  })
}