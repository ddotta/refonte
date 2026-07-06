# ==============================================================================
# MODULE : Suivi des traitements
# ==============================================================================

# UI
suivi_traitements_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(width = 12, h3("Avancement du traitement des anomalies"))),
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
    fluidRow(column(width = 10, selectInput(ns("rapportage"), "Niveau de rapportage :",
      c(Questionnaires = "quest", Anomalies = "ano")))),

    # Rapport par questionnaire
    conditionalPanel(
      condition = sprintf("input['%s'] == 'quest'", ns("rapportage")),
      fluidPage(
        fluidRow(column(width = 2, "Selon l'état des questionnaires")), br(),
        fluidRow(
          column(width = 2, selectizeInput(ns("inputEnquete"), "Enquête :", choices = NULL, multiple = TRUE)),
          column(width = 2, offset = 2, selectizeInput(ns("inputCampagne"), "Campagne :", choices = NULL, multiple = TRUE))
        ), br(),
        fluidRow(
          column(width = 1, offset = 1, downloadButton(ns("saveQuestCSV"), "CSV")),
          column(width = 1, offset = 1, downloadButton(ns("saveQuestExcel"), "Excel"))
        ), br(),
        fluidRow(column(width = 12, dataTableOutput(ns("tableQuest"), width = "100%", height = "auto"))),
        fluidRow(column(width = 12, plotOutput(ns("pieQuest"))))
      )
    ),

    # Rapport par anomalie
    conditionalPanel(
      condition = sprintf("input['%s'] == 'ano'", ns("rapportage")),
      fluidPage(
        fluidRow(column(width = 2, "Selon l'état des anomalies")), br(),
        fluidRow(
          column(width = 2, selectizeInput(ns("inputAnoEnquete"), "Enquête :", choices = NULL, multiple = TRUE)),
          column(width = 2, offset = 2, selectizeInput(ns("inputAnoCampagne"), "Campagne :", choices = NULL, multiple = TRUE))
        ), br(),
        fluidRow(
          column(width = 1, offset = 1, downloadButton(ns("saveAnoCSV"), "CSV")),
          column(width = 1, offset = 1, downloadButton(ns("saveAnoExcel"), "Excel"))
        ), br(),
        fluidRow(column(width = 12, dataTableOutput(ns("tableAno"), width = "100%", height = "auto"))),
        fluidRow(column(width = 12, plotOutput(ns("pieAno"))))
      )
    )
  )
}

# Server
suivi_traitements_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$consigne <- renderText({ cst_commentaire_suivi_traitement })

    # ==========================================================================
    # Rapport par questionnaire
    # ==========================================================================
    observe({
      req(donnees_globales$df_questionnaire)
      updateSelectizeInput(session, "inputEnquete", choices = donnees_globales$df_questionnaire %>% pull("ENQUETE") %>% unique() %>% sort())
      updateSelectizeInput(session, "inputCampagne", choices = donnees_globales$df_questionnaire %>% pull("CAMPAGNE") %>% unique() %>% sort())
    })

    questionnaireFiltree <- reactive({
      req(donnees_globales$df_questionnaire)
      donnees_globales$df_questionnaire %>%
        select(ENQUETE, CAMPAGNE, ETAT_QUEST) %>%
        filter(ENQUETE %in% input$inputEnquete | is.null(input$inputEnquete)) %>%
        filter(CAMPAGNE %in% input$inputCampagne | is.null(input$inputCampagne))
    })

    rapportQuestionnaire <- reactive({
      data <- questionnaireFiltree() %>%
        group_by(ENQUETE, CAMPAGNE, ETAT_QUEST) %>%
        summarise(NUMBER = n(), .groups = 'keep') %>%
        ungroup() %>%
        group_by(ENQUETE, CAMPAGNE) %>%
        mutate(TOTAL = sum(NUMBER)) %>%
        ungroup() %>%
        mutate(ETAT_QUEST = case_when(
          ETAT_QUEST == 0 ~ "NB Non traités",
          ETAT_QUEST == 1 ~ "NB En cours",
          ETAT_QUEST == 2 ~ "NB Traités"
        )) %>% pivot_wider(names_from = ETAT_QUEST, values_from = NUMBER, values_fill = 0)

      for (col in c("NB Non traités", "NB En cours", "NB Traités")) {
        if (col %!in% colnames(data)) data <- data %>% mutate(!!col := 0)
      }

      data <- data %>% mutate(
        'Part Non traités' = paste0(round(.data[['NB Non traités']] / TOTAL * 100, digits = 1), "%"),
        'Part En cours' = paste0(round(.data[['NB En cours']] / TOTAL * 100, digits = 1), "%"),
        'Part Traités' = paste0(round(.data[['NB Traités']] / TOTAL * 100, digits = 1), "%")
      ) %>% select(-TOTAL)
      return(data)
    })

    output$tableQuest <- renderDataTable(
      rapportQuestionnaire(),
      extensions = c("FixedHeader"),
      options = list(dom = 'lfrtip',
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        language = fr, pageLength = 10)
    )

    output$pieQuest <- renderPlot({
      data <- rapportQuestionnaire() %>%
        select('NB Non traités', 'NB En cours', 'NB Traités') %>%
        summarise_all(sum) %>%
        pivot_longer(everything(), names_to = "GROUP", values_to = "VALUES") %>%
        arrange(GROUP)

      data_pos <- data %>%
        mutate(csum = rev(cumsum(rev(VALUES))),
               pos = VALUES / 2 + lead(csum, 1),
               pos = if_else(is.na(pos), VALUES / 2, pos),
               pourcentage = round(VALUES / max(csum) * 100, digits = 1))

      ggplot(data, aes(x = "", y = VALUES, fill = GROUP)) +
        geom_bar(stat = "identity", width = 5, color = "white") +
        coord_polar("y", start = 0) +
        geom_label_repel(data = data_pos,
          aes(y = pos, label = paste0(VALUES, " (", pourcentage, "%)")),
          size = 4.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = "Questionnaires")) +
        theme_void() +
        scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"))
    })

    # ==========================================================================
    # Rapport par anomalie
    # ==========================================================================
    observe({
      req(donnees_globales$df_questionnaire)
      updateSelectizeInput(session, "inputAnoEnquete", choices = donnees_globales$df_questionnaire %>% pull("ENQUETE") %>% unique() %>% sort())
      updateSelectizeInput(session, "inputAnoCampagne", choices = donnees_globales$df_questionnaire %>% pull("CAMPAGNE") %>% unique() %>% sort())
    })

    filteredDataParAnomalie <- reactive({
      req(donnees_globales$df_questionnaire)
      interargir_dB_cherche_et_calcule_les_erreurs(input$inputAnoEnquete, input$inputAnoCampagne, NULL)
    })

    output$tableAno <- renderDataTable(
      filteredDataParAnomalie(),
      extensions = c("FixedHeader"),
      options = list(dom = 'lfrtip',
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        language = fr, pageLength = 10)
    )

    output$pieAno <- renderPlot({
      data <- filteredDataParAnomalie() %>%
        select(NB_ANO_NON_TRAITEES, NB_ANO_EN_COURS, NB_ANO_EN_ATTENTE,
               NB_ANO_CORRIGEES, NB_ANO_FORCEES) %>%
        summarise_all(sum, na.rm = TRUE) %>%
        pivot_longer(everything(), names_to = "GROUP", values_to = "VALUES") %>%
        arrange(GROUP)

      data_pos <- data %>%
        mutate(csum = rev(cumsum(rev(VALUES))),
               pos = VALUES / 2 + lead(csum, 1),
               pos = if_else(is.na(pos), VALUES / 2, pos))

      ggplot(data, aes(x = "", y = VALUES, fill = GROUP)) +
        geom_bar(stat = "identity", width = 5, color = "white") +
        coord_polar("y", start = 0) +
        geom_label_repel(data = data_pos,
          aes(y = pos, label = VALUES),
          size = 4.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = "Anomalies")) +
        theme_void()
    })

    # Export CSV/Excel
    output$saveQuestCSV <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_RAPPORTAGE_QUESTIONNAIRE.csv"),
      content = function(file) { write_csv2(rapportQuestionnaire(), file) }
    )
    output$saveQuestExcel <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_RAPPORTAGE_QUESTIONNAIRE.xlsx"),
      content = function(file) { write.xlsx2(rapportQuestionnaire(), file, sheetName = "RAPPORTAGE_QUESTIONNAIRE") }
    )
    output$saveAnoCSV <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_RAPPORTAGE_ANOMALIES.csv"),
      content = function(file) { write_csv2(filteredDataParAnomalie(), file) }
    )
    output$saveAnoExcel <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_RAPPORTAGE_ANOMALIES.xlsx"),
      content = function(file) { write.xlsx2(filteredDataParAnomalie(), file, sheetName = "RAPPORTAGE_ANOMALIES") }
    )
  })
}