# ==============================================================================
# MODULE : Suivi questionnaires (responsable d'enquête)
# ==============================================================================

# UI
suivi_questionnaires_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(width = 12, h3("Suivi des questionnaires à destination du responsable d'enquête"))),
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
    fluidRow(
      column(width = 6, align = "center",
        selectizeInput(ns("inputGestionnaire"), "Gestionnaire référent :", choices = NULL, multiple = TRUE)
      ),
      column(width = 6, align = "center",
        selectizeInput(ns("inputEnquete"), "Enquête :", choices = NULL, multiple = TRUE)
      )
    ),
    br(),
    fluidRow(column(width = 12, align = "center", tableOutput(ns("tableValeurs")))),
    br(),
    fluidRow(column(width = 12, align = "center", tableOutput(ns("tablePourcentages")))),
    br(),
    fluidRow(
      column(width = 1, offset = 1, downloadButton(ns("saveCSV"), "CSV")),
      column(width = 1, offset = 1, downloadButton(ns("saveExcel"), "Excel"))
    )
  )
}

# Server
suivi_questionnaires_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$consigne <- renderText({ cst_commentaire_suivi_questionnaire })

    observe({
      req(donnees_globales$df_questionnaire)
      updateSelectizeInput(session, "inputEnquete",
        choices = donnees_globales$df_questionnaire %>% pull("ENQUETE") %>% unique() %>% sort())
      updateSelectizeInput(session, "inputGestionnaire",
        choices = donnees_globales$df_questionnaire %>% pull("GESTIONNAIRE_REF") %>% unique() %>% sort())
    })

    questionnaireFiltree <- reactive({
      req(donnees_globales$df_questionnaire)
      donnees_globales$df_questionnaire %>%
        filter(ENQUETE %in% input$inputEnquete | is.null(input$inputEnquete)) %>%
        filter(GESTIONNAIRE_REF %in% input$inputGestionnaire | is.null(input$inputGestionnaire))
    })

    # Tableau des valeurs
    output$tableValeurs <- renderTable({
      df <- questionnaireFiltree() %>%
        mutate(ETAT_QUEST = case_when(
          ETAT_QUEST == 0 ~ "Non traité",
          ETAT_QUEST == 1 ~ "En cours",
          ETAT_QUEST == 2 ~ "Traité"
        )) %>%
        rename(PRIORITE = PRIORITE_QUEST, ETAT = ETAT_QUEST) %>%
        tabyl(PRIORITE, ETAT) %>%
        adorn_totals(where = c("row", "col")) %>%
        adorn_title(placement = "combined")
      data.frame(df)
    }, digits = 0)

    # Tableau des pourcentages
    output$tablePourcentages <- renderTable({
      df <- questionnaireFiltree() %>%
        mutate(ETAT_QUEST = case_when(
          ETAT_QUEST == 0 ~ "Non traité",
          ETAT_QUEST == 1 ~ "En cours",
          ETAT_QUEST == 2 ~ "Traité"
        )) %>%
        rename(PRIORITE = PRIORITE_QUEST, ETAT = ETAT_QUEST) %>%
        tabyl(PRIORITE, ETAT) %>%
        adorn_totals(where = c("row", "col")) %>%
        adorn_percentages("all") %>%
        adorn_pct_formatting(1) %>%
        adorn_title(placement = "combined")
      data.frame(df)
    }, digits = 0)

    # Export
    rapportGestionnaire <- reactive({
      dataValeur <- data.frame(
        questionnaireFiltree() %>%
          mutate(ETAT_QUEST = case_when(
            ETAT_QUEST == 0 ~ "Non traité",
            ETAT_QUEST == 1 ~ "En cours",
            ETAT_QUEST == 2 ~ "Traité"
          )) %>%
          rename(PRIORITE = PRIORITE_QUEST, ETAT = ETAT_QUEST) %>%
          tabyl(PRIORITE, ETAT) %>%
          adorn_totals(where = c("row", "col")) %>%
          adorn_title(placement = "combined")
      )
      dataPourcentage <- data.frame(
        questionnaireFiltree() %>%
          mutate(ETAT_QUEST = case_when(
            ETAT_QUEST == 0 ~ "Non traité",
            ETAT_QUEST == 1 ~ "En cours",
            ETAT_QUEST == 2 ~ "Traité"
          )) %>%
          rename(PRIORITE = PRIORITE_QUEST, ETAT = ETAT_QUEST) %>%
          tabyl(PRIORITE, ETAT) %>%
          adorn_totals(where = c("row", "col")) %>%
          adorn_percentages("all") %>%
          adorn_pct_formatting(2) %>%
          adorn_title(placement = "combined")
      )
      dataValeur['Tableau_en_%'] <- ""
      cbind(dataValeur, dataPourcentage)
    })

    output$saveCSV <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_SUIVI_GESTIONNAIRE.csv"),
      content = function(file) { write_csv2(rapportGestionnaire(), file) }
    )

    output$saveExcel <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_SUIVI_GESTIONNAIRE.xlsx"),
      content = function(file) { write.xlsx2(rapportGestionnaire(), file, sheetName = "GESTIONNAIRE") }
    )
  })
}