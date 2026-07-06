# ==============================================================================
# MODULE : Intégration d'une nouvelle collecte
# ==============================================================================

# UI
integration_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(width = 12, offset = 0, h3("Intégration d'une nouvelle collecte"))),
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
        selectizeInput(ns("inputChoixEnquete"), label = NULL, choices = NULL,
          multiple = FALSE, options = list(create = TRUE)))
    ),
    fluidRow(column(width = 4, offset = 6, htmlOutput(ns("errorEnquete")))),
    br(),
    fluidRow(
      column(width = 2, offset = 2, "Campagne :"),
      column(width = 2, offset = 2,
        numericInput(ns("inputCampagne"), label = NULL, value = 2020, min = 2020, max = 9999, step = 1))
    ),
    fluidRow(column(width = 4, offset = 6, htmlOutput(ns("errorCampagne")))),
    br(),
    fluidRow(
      column(width = 4, offset = 2, "Programme de détection d'anomalies :"),
      column(width = 4,
        shinyFilesButton(ns("inputChoixFichier"), "Parcourir...",
          title = "Merci de choisir un fichier R :", multiple = FALSE,
          buttonType = "default", class = NULL))
    ),
    fluidRow(column(width = 4, offset = 6, htmlOutput(ns("errorChemin")))),
    br(), br(), br(),
    fluidRow(
      column(width = 4, offset = 6, actionButton(ns("actionIntegrate"), label = "Intégrer"))
    )
  )
}

# Server
integration_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    shinyFileChoose(input, "inputChoixFichier", roots = c(wd = cst_chemin_root), session = session)

    output$consigne <- renderText({ cst_commentaire_integration })

    observe({
      if (file.exists(cst_chemin_vers_fichier_nom_enquete)) {
        tryCatch({
          updateSelectizeInput(session, "inputChoixEnquete",
            choices = read.delim(cst_chemin_vers_fichier_nom_enquete) %>% pull(NOM_ENQUETES))
        }, error = function(e) {})
      }
    })

    observeEvent(input$inputCampagne, {
      req(input$inputCampagne)
      if (input$inputCampagne > 9999) {
        updateNumericInput(session, "inputCampagne", value = 9999)
      }
    })

    observeEvent(input$actionIntegrate, {
      campagnePrete <- FALSE
      enquetePrete <- FALSE
      cheminPret <- FALSE

      # Vérification enquête
      if (input$inputChoixEnquete != "" && !is.null(input$inputChoixEnquete)) {
        if (file.exists(cst_chemin_vers_fichier_nom_enquete)) {
          contenu <- tryCatch(read.csv2(cst_chemin_vers_fichier_nom_enquete), error = function(e) NULL)
          if (!is.null(contenu) && nrow(contenu) > 0) {
            list_enquete <- read_csv2(cst_chemin_vers_fichier_nom_enquete, show_col_types = FALSE, lazy = FALSE)
            if (input$inputChoixEnquete %!in% list_enquete$NOM_ENQUETES) {
              list_enquete <- list_enquete %>%
                add_row(NOM_ENQUETES = input$inputChoixEnquete) %>%
                arrange(NOM_ENQUETES)
              write_csv2(list_enquete, cst_chemin_vers_fichier_nom_enquete)
            }
            enquetePrete <- TRUE
            output$errorEnquete <- renderText({ "" })
          } else {
            nouvelle_ligne <- tibble(NOM_ENQUETES = input$inputChoixEnquete)
            write_csv2(nouvelle_ligne, cst_chemin_vers_fichier_nom_enquete)
            enquetePrete <- TRUE
            output$errorEnquete <- renderText({ "" })
          }
        } else {
          # Fichier n'existe pas, on le crée
          dir.create(dirname(cst_chemin_vers_fichier_nom_enquete), showWarnings = FALSE, recursive = TRUE)
          write_csv2(tibble(NOM_ENQUETES = input$inputChoixEnquete), cst_chemin_vers_fichier_nom_enquete)
          enquetePrete <- TRUE
          output$errorEnquete <- renderText({ "" })
        }
      } else {
        output$errorEnquete <- renderText({ "<div style='color:red'>Ce champ est obligatoire</div>" })
      }

      # Vérification campagne
      if (is.na(input$inputCampagne)) {
        output$errorCampagne <- renderText({ "<div style='color:red'>Ce champ est obligatoire</div>" })
      } else {
        campagnePrete <- TRUE
        output$errorCampagne <- renderText({ "" })
      }

      # Vérification fichier
      if (is.null(input$inputChoixFichier) || length(input$inputChoixFichier) == 1) {
        output$errorChemin <- renderText({ "<div style='color:red'>Ce champ est obligatoire</div>" })
      } else {
        file_selected <- parseFilePaths(c(wd = cst_chemin_root), input$inputChoixFichier)
        if (!str_ends(file_selected$datapath, ".R")) {
          output$errorChemin <- renderText({ "<div style='color:red'>Le fichier n'a pas la bonne extension (.R)</div>" })
        } else {
          cheminPret <- TRUE
          output$errorChemin <- renderText({ "" })
        }
      }

      if (enquetePrete && campagnePrete && cheminPret) {
        couple_a_tester <- paste(input$inputChoixEnquete, input$inputCampagne, sep = "_")

        if (file.exists(cst_chemin_vers_fichier_enquete)) {
          file <- tryCatch(read.delim(file = cst_chemin_vers_fichier_enquete, sep = ";"), error = function(e) NULL)
          if (!is.null(file) && nrow(file) > 0) {
            enquete_dans_le_fichier <- read_delim(cst_chemin_vers_fichier_enquete,
              col_types = list("CAMPAGNE" = col_integer()),
              show_col_types = FALSE, delim = ";", lazy = FALSE)

            couples_pour_comparer <- enquete_dans_le_fichier %>%
              select(-CHEMIN_ESPACE) %>%
              unite(col = COUPLE, c(NOM_ENQUETE, CAMPAGNE)) %>%
              pull(COUPLE)

            if (couple_a_tester %in% couples_pour_comparer) {
              output$errorEnquete <- renderText({ "<div style='color:red'>Le couple Enquête/Campagne existe déjà</div>" })
              output$errorCampagne <- renderText({ "<div style='color:red'>Le couple Enquête/Campagne existe déjà</div>" })
            } else {
              output$errorEnquete <- renderText({ "" })
              output$errorCampagne <- renderText({ "" })
              fichierSelectionne <- parseFilePaths(c(wd = cst_chemin_root), input$inputChoixFichier)

              enquete_dans_le_fichier <- enquete_dans_le_fichier %>%
                add_row(NOM_ENQUETE = input$inputChoixEnquete,
                        CAMPAGNE = input$inputCampagne,
                        CHEMIN_ESPACE = fichierSelectionne$datapath)
              write_csv2(enquete_dans_le_fichier, cst_chemin_vers_fichier_enquete)

              showNotification("L'enquête a bien été ajoutée à SUIVAL", type = "message")
              updateSelectizeInput(session, "inputChoixEnquete", selected = "")
              updateNumericInput(session, "inputCampagne", value = 2020)
            }
          } else {
            # Fichier vide
            fichierSelectionne <- parseFilePaths(c(wd = cst_chemin_root), input$inputChoixFichier)
            nouvelleLigne <- tibble(
              NOM_ENQUETE = input$inputChoixEnquete,
              CAMPAGNE = input$inputCampagne,
              CHEMIN_ESPACE = fichierSelectionne$datapath
            )
            write_csv2(nouvelleLigne, cst_chemin_vers_fichier_enquete)
            showNotification("L'enquête a bien été ajoutée à SUIVAL", type = "message")
            updateSelectizeInput(session, "inputChoixEnquete", selected = "")
            updateNumericInput(session, "inputCampagne", value = 2020)
          }
        } else {
          # Fichier n'existe pas, on le crée
          dir.create(dirname(cst_chemin_vers_fichier_enquete), showWarnings = FALSE, recursive = TRUE)
          fichierSelectionne <- parseFilePaths(c(wd = cst_chemin_root), input$inputChoixFichier)
          nouvelleLigne <- tibble(
            NOM_ENQUETE = input$inputChoixEnquete,
            CAMPAGNE = input$inputCampagne,
            CHEMIN_ESPACE = fichierSelectionne$datapath
          )
          write_csv2(nouvelleLigne, cst_chemin_vers_fichier_enquete)
          showNotification("L'enquête a bien été ajoutée à SUIVAL", type = "message")
          updateSelectizeInput(session, "inputChoixEnquete", selected = "")
          updateNumericInput(session, "inputCampagne", value = 2020)
        }
      }
    })
  })
}