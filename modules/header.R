# ==============================================================================
# MODULE : Header
# ==============================================================================

header_ui <- dashboardHeader(
  title = tags$div(HTML(paste0(cst_nom_application, " ", tags$img(src = "logo.png", height = "50px")))),
  tags$li(
    class = "dropdown",
    tags$div(class = "icon-user", icon("user", class = "far fa-user"))
  ),
  tags$li(
    class = "dropdown",
    tags$div(class = "utilisateur", get_utilisateur())
  ),
  dropdownMenu(
    type = "notifications",
    icon = icon("question-circle"),
    badgeStatus = NULL,
    headerText = "Aide :",
    tags$li(downloadButton(outputId = "pdf", label = "Télécharger PDF"))
  ),
  tags$li(
    class = "dropdown",
    extendShinyjs(
      text = "shinyjs.closeWindow = function() { window.close(); }",
      functions = c("closeWindow")
    )
  ),
  tags$li(
    class = "dropdown",
    tags$div(class = "quit", actionButton(inputId = "quitter", label = NULL, icon = icon("power-off", class = "fas fa-power-off")))
  )
)