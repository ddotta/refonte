# ==============================================================================
# SUIVAL IAA - Application de suivi et traitement des anomalies de collecte
# + Questionnaire dynamique Pogues
# ==============================================================================

options(encoding = "UTF-8")

# Chargement de l'initialisation globale
source("global.R", local = TRUE)

# ==============================================================================
# Sources des modules (un fichier = 1 module = UI + Server + helpers)
# ==============================================================================

source("modules/header.R", local = TRUE)
source("modules/sidebar.R", local = TRUE)
source("modules/recherche_traitement.R", local = TRUE)
source("modules/suivi_traitements.R", local = TRUE)
source("modules/exporter_csv.R", local = TRUE)
source("modules/suivi_questionnaires.R", local = TRUE)
source("modules/anomalies_archivees.R", local = TRUE)
source("modules/integration.R", local = TRUE)
source("modules/archivage.R", local = TRUE)
source("modules/questionnaire_pogues.R", local = TRUE)

# ==============================================================================
# UI
# ==============================================================================

ui <- dashboardPage(
  header_ui,
  sidebar_ui,
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      tabItem("recherche-et-traitement", recherche_traitement_ui("recherche")),
      tabItem("suivi-traitement", suivi_traitements_ui("suivi")),
      tabItem("export-quest-ano", exporter_csv_ui("export")),
      tabItem("suivi-questionnaire", suivi_questionnaires_ui("suivi-questionnaire")),
      tabItem("anomalies-archivees", anomalies_archivees_ui("anomalies-archivees")),
      tabItem("integration", integration_ui("integration")),
      tabItem("archivage", archivage_ui("archivage")),
      tabItem("questionnaire-pogues", questionnaire_pogues_ui("questionnaire"))
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # Dernière mise à jour
  output$derniereMiseAJour <- renderText({
    if (is.null(donnees_globales$df_questionnaire) || nrow(donnees_globales$df_questionnaire) == 0) {
      return("Pas de questionnaire chargé")
    } else {
      maxDate <- max(as.Date(donnees_globales$df_questionnaire %>% pull("DATE_MAJ_SUIVAL"), format = '%d/%m/%Y'))
      return(paste0("Mise à jour : ", format(maxDate, '%d/%m/%Y')))
    }
  })

  # Affichage de l'environnement
  output$Environnement <- renderText({
    str_to_upper(Sys.getenv("ENVIRONNEMENT"))
  })

  # Quitter l'application
  observeEvent(input$quitter, {
    stopApp()
  })

  # Appel des modules serveur
  recherche_traitement_server("recherche")
  suivi_traitements_server("suivi")
  exporter_csv_server("export")
  suivi_questionnaires_server("suivi-questionnaire")
  anomalies_archivees_server("anomalies-archivees")
  integration_server("integration")
  archivage_server("archivage")
  questionnaire_pogues_server("questionnaire")
}

# ==============================================================================
# Lancement
# ==============================================================================

shinyApp(ui = ui, server = server)