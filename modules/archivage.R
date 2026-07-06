# ==============================================================================
# MODULE : Archivage d'une campagne
# ==============================================================================

# UI
archivage_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    fluidRow(column(width = 12, h3("Archivage des anomalies d'une collecte terminée"))),
    br(),
    fluidRow(
      column(width = 3,
        materialSwitch(inputId = ns("showComment"), label = span("Commentaire : Afficher/Masquer", style = "color : #797979;"),
          value = TRUE, status = "primary", right = TRUE)
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("showComment")),
      fluidRow(column(width = 12, verbatimTextOutput(ns("consigne"))))
    ),
    br(),
    fluidRow(
      column(width = 2, offset = 2, "Enquête :"),
      column(width = 4, offset = 2,
        selectInput(ns("inputChoixEnquete"), label = NULL, choices = NULL, selected = NULL, multiple = FALSE)
      )
    ),
    br(),
    fluidRow(
      column(width = 2, offset = 2, "Campagne :"),
      column(width = 2, offset = 2,
        selectInput(ns("inputCampagne"), label = NULL, choices = NULL, multiple = FALSE, selected = NULL)
      )
    ),
    br(), br(),
    fluidRow(
      column(width = 3, offset = 6,
        add_busy_spinner(spin = "fulfilling-square", color = "#3c8dbc",
          position = "top-left", margins = c("60%", "50%")),
        actionButton(ns("actionArchive"), label = "Archiver")
      )
    )
  )
}

# Server
archivage_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$consigne <- renderText({ cst_commentaire_archivage })

    observe({
      req(donnees_globales$df_questionnaire)
      choicesEnquete <- donnees_globales$df_questionnaire %>% pull("ENQUETE") %>% unique() %>% sort()
      choicesCampagne <- donnees_globales$df_questionnaire %>% pull("CAMPAGNE") %>% unique() %>% sort()
      updateSelectizeInput(session, "inputChoixEnquete", choices = choicesEnquete)
      updateSelectizeInput(session, "inputCampagne", choices = choicesCampagne)
    })

    observeEvent(input$actionArchive, {
      req(input$inputChoixEnquete, input$inputCampagne)
      ns <- session$ns

      showModal(modalDialog(
        paste("Êtes-vous sûr de vouloir archiver ", input$inputChoixEnquete, input$inputCampagne, " ?"),
        size = 's',
        footer = tagList(
          actionButton(ns("actionValidation"), "Valider"),
          actionButton(ns("retourValidation"), "Retour")
        )
      ))
    })

    observeEvent(input$retourValidation, {
      removeModal()
    })

    observeEvent(input$actionValidation, {
      shinyjs::disable("retourValidation")

      c_identifiant <- donnees_globales$df_questionnaire %>%
        filter(ENQUETE == input$inputChoixEnquete, CAMPAGNE == input$inputCampagne) %>%
        pull(IDENTIFIANT_SUIVALIAA)

      # Sauvegarde en CSV externe
      dir.create("DonneesExternes", showWarnings = FALSE, recursive = TRUE)
      write_csv2(donnees_globales$df_questionnaire %>% filter(IDENTIFIANT_SUIVALIAA %in% c_identifiant),
        paste0("DonneesExternes/", input$inputChoixEnquete, input$inputCampagne, "_QUESTIONNAIRE.csv"))

      write_csv2(interagir_dB_recuperer_anomalies(c_identifiant),
        paste0("DonneesExternes/", input$inputChoixEnquete, input$inputCampagne, "_ANOMALIES.csv"))

      # Suppression dans le fichier de référence
      if (file.exists(cst_chemin_vers_fichier_enquete)) {
        df_table_enquete <- read_csv2(cst_chemin_vers_fichier_enquete, show_col_types = FALSE, lazy = FALSE)
        df_table_enquete <- df_table_enquete %>%
          unite(ENQUETE_CAMPAGNE, c(NOM_ENQUETE, CAMPAGNE), sep = "_") %>%
          filter(ENQUETE_CAMPAGNE != paste(input$inputChoixEnquete, input$inputCampagne, sep = "_")) %>%
          separate(ENQUETE_CAMPAGNE, c("NOM_ENQUETE", "CAMPAGNE"), sep = "_")
        write_csv2(df_table_enquete, cst_chemin_vers_fichier_enquete)
      }

      # Mise à jour des données globales
      donnees_globales$df_questionnaire <<- donnees_globales$df_questionnaire %>%
        filter(IDENTIFIANT_SUIVALIAA %!in% c_identifiant)
      donnees_globales$df_anomalies_archivees <- construire_la_liste_des_anomalies_archivees()

      # Suppression en base
      interagir_dB_supprimer_dans_questionnaire(c_identifiant)

      showNotification(paste0("L'enquête ", paste(input$inputChoixEnquete, input$inputCampagne, sep = "_"),
        " a bien été archivée."), type = "message")
      removeModal()
    })
  })
}