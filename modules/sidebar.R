# ==============================================================================
# MODULE : Sidebar (navigation)
# ==============================================================================

sidebar_ui <- dashboardSidebar(
  sidebarMenu(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "font-awesome/css/all.min.css")
    ),
    br(),
    textOutput("Environnement"),
    br(),
    textOutput("derniereMiseAJour"),

    # Onglet 1 : Anomalies
    menuItem("Anomalies",
      tabName = "anomalies",
      icon = icon("cogs", class = "fas fa-cogs"),
      startExpanded = TRUE,
      menuSubItem("Recherche et traitement", tabName = "recherche-et-traitement")
    ),

    # Onglet 2 : Suivi
    menuItem("Suivi",
      tabName = "suivi",
      icon = icon("search", class = "fas fa-search"),
      startExpanded = FALSE,
      menuSubItem("Suivi traitements", tabName = "suivi-traitement"),
      menuSubItem("Exporter CSV", tabName = "export-quest-ano"),
      menuSubItem("Suivi questionnaires", tabName = "suivi-questionnaire")
    ),

    # Onglet 3 : Historique
    menuItem("Historique",
      tabName = "historique",
      icon = icon("calculator", class = "fas fa-calculator"),
      startExpanded = FALSE,
      menuSubItem("Anomalies archivées", tabName = "anomalies-archivees")
    ),

    # Onglet 4 : Gestion
    menuItem("Gestion",
      tabName = "gestion",
      icon = icon("clipboard", class = "fas fa-clipboard"),
      startExpanded = FALSE,
      menuSubItem("Intégration", tabName = "integration"),
      menuSubItem("Archivage", tabName = "archivage")
    ),

    # Onglet 5 : Questionnaire (Pogues)
    menuItem("Questionnaire",
      tabName = "questionnaire-pogues",
      icon = icon("file-alt", class = "fas fa-file-alt"),
      startExpanded = FALSE,
      menuSubItem("Saisie questionnaire", tabName = "questionnaire-pogues")
    )
  )
)