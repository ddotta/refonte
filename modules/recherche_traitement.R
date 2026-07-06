# ==============================================================================
# MODULE : Recherche et traitement des anomalies
# ==============================================================================

# UI
recherche_traitement_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(width = 12, h3("Recherche de questionnaires"))),
    br(),
    fluidRow(
      column(width = 3,
        materialSwitch(inputId = ns("switch_filtres"), label = span("Filtres : Afficher/Masquer", style = "color : #797979;"),
          value = TRUE, status = "primary", right = TRUE)
      ),
      column(width = 3, offset = 6,
        actionButton(ns("resetFiltres"), "Réinitialiser les filtres",
          icon = icon("sync-alt", class = "fas fa-sync-alt"), style = "color: #334fff")
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("switch_filtres")),
      fluidRow(column(width = 3, "Filtres sur les questionnaires :")), br(),
      fluidRow(
        column(width = 3, selectizeInput(ns("filtreEnquete"), "Enquête :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreCampagne"), "Campagne :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreGestionnaire"), "Gestionnaire référent :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreEtat"), "État du questionnaire :",
          choices = c("Non traité", "En cours", "Traité"), multiple = TRUE))
      ),
      fluidRow(
        column(width = 3, selectizeInput(ns("filtreSiret"), "Siret :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtreRaison"), "Raison sociale :", choices = NULL, multiple = TRUE)),
        column(width = 3, selectizeInput(ns("filtrePriorite"), "Priorité :", choices = NULL, multiple = TRUE))
      ),
      fluidRow(
        column(width = 4, offset = 5,
          pickerInput(ns("pickerColumn"), "Colonnes à afficher",
            choices = NULL, multiple = TRUE,
            options = list(`selected-text-format` = "count",
              `count-selected-text` = "{0} colonnes selectionnées sur {1}"))
        )
      )
    ),
    br(),
    fluidRow(
      column(2, align = "right", downloadButton(ns("saveCSV"), "CSV")),
      column(2, align = "left", downloadButton(ns("saveExcel"), "Excel")),
      column(8, align = "right", actionButton(ns("openModal"), "Traiter le questionnaire"))
    ),
    br(),
    fluidRow(column(width = 12, dataTableOutput(ns("table"), width = "100%", height = "auto")))
  )
}

# Server
recherche_traitement_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    server_values <- reactiveValues(
      selectedQuestionnaire = NULL,
      selectedAnomalie = NULL,
      etatCourant = NULL
    )

    # Mise à jour des filtres
    observe({
      req(donnees_globales$df_questionnaire)
      colonnes <- colnames(donnees_globales$df_questionnaire %>% select(
        SIRET, RAISON_SOCIALE, ETAT_QUEST, PRIORITE_QUEST, NB_ANO_TOT,
        NB_ANO_NON_TRAITEES, NB_ANO_EN_COURS, NB_ANO_FORCEES, NB_ANO_CORRIGEES,
        NB_ANO_EN_ATTENTE, GESTIONNAIRE_REF, DATE_MODIFICATION, DATE_VALIDATION,
        DATE_INTERNET, DATE_COURRIER, ENQUETE, CAMPAGNE))

      updatePickerInput(session, "pickerColumn",
        choices = colonnes,
        selected = c(colonnes[1:5], colonnes[10:13], colonnes[16:17]))

      updateSelectizeInput(session, "filtreEnquete",
        choices = donnees_globales$df_questionnaire %>% pull("ENQUETE") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreCampagne",
        choices = donnees_globales$df_questionnaire %>% pull("CAMPAGNE") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreGestionnaire",
        choices = donnees_globales$df_questionnaire %>% pull("GESTIONNAIRE_REF") %>% unique() %>% sort())
      updateSelectizeInput(session, "filtreSiret",
        choices = donnees_globales$df_questionnaire %>% pull("SIRET") %>% unique() %>% sort(), server = TRUE)
      updateSelectizeInput(session, "filtreRaison",
        choices = donnees_globales$df_questionnaire %>% pull("RAISON_SOCIALE") %>% unique() %>% sort(), server = TRUE)
      updateSelectizeInput(session, "filtrePriorite",
        choices = donnees_globales$df_questionnaire %>% pull("PRIORITE_QUEST") %>% unique() %>% sort())
    })

    # Reset filtres
    observeEvent(input$resetFiltres, {
      updateSelectizeInput(session, "filtreEnquete", selected = "")
      updateSelectizeInput(session, "filtreCampagne", selected = "")
      updateSelectizeInput(session, "filtreGestionnaire", selected = "")
      updateSelectizeInput(session, "filtreEtat", selected = "")
      updateSelectizeInput(session, "filtreSiret", selected = "",
        choices = donnees_globales$df_questionnaire %>% pull("SIRET") %>% unique() %>% sort(), server = TRUE)
      updateSelectizeInput(session, "filtreRaison", selected = "",
        choices = donnees_globales$df_questionnaire %>% pull("RAISON_SOCIALE") %>% unique() %>% sort(), server = TRUE)
      updateSelectizeInput(session, "filtrePriorite", selected = "")
    })

    # Filtrage
    questionnairesFiltres <- reactive({
      req(donnees_globales$df_questionnaire)
      df <- donnees_globales$df_questionnaire %>%
        filter(ENQUETE %in% input$filtreEnquete | is.null(input$filtreEnquete)) %>%
        filter(CAMPAGNE %in% input$filtreCampagne | is.null(input$filtreCampagne)) %>%
        filter(GESTIONNAIRE_REF %in% input$filtreGestionnaire | is.null(input$filtreGestionnaire)) %>%
        mutate(ETAT_QUEST = case_when(
          ETAT_QUEST == 0 ~ "Non traité",
          ETAT_QUEST == 1 ~ "En cours",
          ETAT_QUEST == 2 ~ "Traité"
        )) %>%
        filter(ETAT_QUEST %in% input$filtreEtat | is.null(input$filtreEtat)) %>%
        filter(SIRET %in% input$filtreSiret | is.null(input$filtreSiret)) %>%
        filter(RAISON_SOCIALE %in% input$filtreRaison | is.null(input$filtreRaison)) %>%
        filter(PRIORITE_QUEST %in% input$filtrePriorite | is.null(input$filtrePriorite))
      return(df)
    })

    # Tableau
    output$table <- renderDataTable(
      questionnairesFiltres() %>% select(input$pickerColumn),
      extensions = c("FixedHeader"),
      options = list(dom = 'lfrtip',
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        language = fr, scrollY = 300, fixedHeader = TRUE,
        pageLength = 50, lengthMenu = c(5, 10, 50), scrollX = TRUE),
      class = "nowrap", selection = "single"
    )

    observeEvent(input$table_rows_selected, {
      server_values$selectedQuestionnaire <- questionnairesFiltres() %>% slice(input$table_rows_selected)
    })

    # Export CSV
    output$saveCSV <- downloadHandler(
      filename = function() paste0(Sys.Date(), "_SUIVAL_IAA_QUESTIONNAIRE.csv"),
      content = function(file) {
        write_csv2(questionnairesFiltres(), paste0(cst_chemin_sauvegarde_donnees, Sys.Date(), "_SUIVAL_IAA_QUESTIONNAIRE.csv"))
        write_csv2(questionnairesFiltres(), file)
      }
    )

    # Export Excel
    output$saveExcel <- downloadHandler(
      filename = function() paste(Sys.Date(), "_SUIVAL_IAA_QUESTIONNAIRE.xlsx"),
      content = function(file) {
        write.xlsx(questionnairesFiltres(), paste0(cst_chemin_sauvegarde_donnees, Sys.Date(), "_SUIVAL_IAA_QUESTIONNAIRE.xlsx"),
          sheetName = "QUESTIONNAIRE", append = FALSE)
        write.xlsx(questionnairesFiltres(), file, sheetName = "QUESTIONNAIRE", append = FALSE)
      }
    )

    # ==========================================================================
    # MODAL : Traiter le questionnaire
    # ==========================================================================
    ouvrirModalQuestionnaire <- function() {
      ns <- session$ns
      req(server_values$selectedQuestionnaire)

      df_ano <- df_anomalies()
      colonnes <- colnames(df_ano %>% select(DATE, CODE, LIBELLE, LIGNE, ECRAN, VARIABLES, ETAT, COMMENT_VALIDATION))
      updatePickerInput(session, "pickerAnomalies", choices = colonnes,
        selected = c(colonnes[2:4], colonnes[7:8]))

      showModal(modalDialog(
        size = "l",
        title = "Visualisation et traitement d'un questionnaire",
        fluidPage(
          fluidRow(
            column(3, p("Date export Capibara : ")),
            column(3, textOutput(ns("dateExport"))),
            column(3, p("Date mise à jour SUIVAL : ")),
            column(3, textOutput(ns("dateUpdate")))
          ), br(),
          h3("Données du questionnaire"), br(),
          fluidRow(
            column(2, htmlOutput(ns("textEnquete"))),
            column(2, htmlOutput(ns("textCampagne"))),
            column(2, htmlOutput(ns("textGestionnaire"))),
            column(2, htmlOutput(ns("textEtatQuest"))),
            column(2, htmlOutput(ns("textPriorite")))
          ), br(),
          fluidRow(
            column(3, htmlOutput(ns("textSIRET"))),
            column(3, offset = 3, htmlOutput(ns("textRSociale")))
          ), br(),
          fluidRow(
            column(2, htmlOutput(ns("textLabelAno"))),
            column(1, offset = 2, htmlOutput(ns("textAnoTot"))),
            column(2, htmlOutput(ns("textAnoNonTraite"))),
            column(1, htmlOutput(ns("textAnoEnCours"))),
            column(1, htmlOutput(ns("textAnoForcees"))),
            column(1, htmlOutput(ns("textAnoEnAttente"))),
            column(1, htmlOutput(ns("textAnoCorr")))
          ), br(),
          fluidRow(column(2, p("Commentaires enquêté :")), column(10, textOutput(ns("commentEnq")))),
          fluidRow(column(2, p("Commentaires gestionnaire :")), column(10, textOutput(ns("commentGest")))),
          br(), h3("Liste des anomalies"), br(),
          fluidRow(
            column(3, materialSwitch(ns("switchFiltreAno"), label = span("Filtres", style = "color:#797979;"),
              value = TRUE, status = "primary", right = TRUE)),
            column(3, offset = 6,
              pickerInput(ns("pickerAnomalies"), "Colonnes", choices = NULL, multiple = TRUE,
                options = list(`selected-text-format` = "count",
                  `count-selected-text` = "{0} colonnes selectionnées sur {1}")))
          ), br(),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("switchFiltreAno")),
            fluidRow(column(3, p("Filtres sur les anomalies :"))),
            fluidRow(column(4, p("État de l'anomalie :"))),
            fluidRow(column(4,
              selectInput(ns("filtreEtatAno"), label = NULL,
                choices = cst_mapping_etat$ETAT_LETTRE, multiple = TRUE)))
          ), br(),
          fluidRow(
            column(1, offset = 1, downloadButton(ns("saveAnoCSV"), "CSV")),
            column(1, downloadButton(ns("saveAnoExcel"), "Excel")),
            column(2, offset = 5, actionButton(ns("openModalAnomalie"), "Traiter l'anomalie"))
          ), br(),
          fluidRow(column(12, dataTableOutput(ns("tableAnomalies"), width = "100%", height = "auto"))),
          br(), br(),
          uiOutput(ns("anomalieAutreEnquete"))
        ),
        footer = fluidRow(column(2, offset = 8, actionButton(ns("retourQuest"), label = "Retour")))
      ))
    }

    observeEvent(input$openModal, {
      if (!is.null(input$table_rows_selected)) {
        sauvegardeFiltreQuestionnaire(list(
          input$filtreEnquete, input$filtreCampagne, input$filtreGestionnaire,
          input$filtreEtat, input$filtreSiret, input$filtreRaison, input$filtrePriorite))
        sauvegardeFiltreAnomalie(list(input$filtreEtatAno))
        ouvrirModalQuestionnaire()
      }
    })

    # Données réactives du questionnaire sélectionné
    df_anomalies <- reactive({
      req(server_values$selectedQuestionnaire)
      identifiant <- server_values$selectedQuestionnaire %>% pull("IDENTIFIANT_SUIVALIAA")
      data <- interagir_dB_recuperer_anomalies(identifiant)
      if (is.data.frame(data) && nrow(data) > 0) {
        data <- data %>% rename(DATE = DATE_ANOMALIE, CODE = CODE_ANOMALIE,
          LIGNE = LIGNE_ANOMALIE, LIBELLE = LIB_ANOMALIE) %>% mutate(ETAT = "")
      }
      return(data)
    })

    # Outputs du modal
    output$dateExport <- renderText({ server_values$selectedQuestionnaire %>% pull("DATE_EXPORT_CAPI") })
    output$dateUpdate <- renderText({ server_values$selectedQuestionnaire %>% pull("DATE_MAJ_SUIVAL") })
    output$textEnquete <- renderText({ paste("<span style='text-decoration:underline'>Enquête</span> : ", server_values$selectedQuestionnaire$ENQUETE) })
    output$textCampagne <- renderText({ paste("<span style='text-decoration:underline'>Campagne</span> : ", server_values$selectedQuestionnaire %>% pull("CAMPAGNE")) })
    output$textGestionnaire <- renderText({ paste("<span style='text-decoration:underline'>Gestionnaire</span> : ", server_values$selectedQuestionnaire %>% pull("GESTIONNAIRE_REF")) })
    output$textEtatQuest <- renderText({
      etat <- server_values$selectedQuestionnaire %>% pull("ETAT_QUEST")
      paste("<span style='text-decoration:underline'>État questionnaire</span> : ", if (etat == 0) "Non traité" else if (etat == 1) "En cours" else "Traité")
    })
    output$textPriorite <- renderText({ paste("<span style='text-decoration:underline'>Priorité</span> : ", server_values$selectedQuestionnaire %>% pull("PRIORITE_QUEST")) })
    output$textSIRET <- renderText({ paste("<span style='text-decoration:underline'>SIRET</span> : ", server_values$selectedQuestionnaire %>% pull("SIRET")) })
    output$textRSociale <- renderText({ paste("<span style='text-decoration:underline'>Raison sociale</span> : ", server_values$selectedQuestionnaire %>% pull("RAISON_SOCIALE")) })
    output$textLabelAno <- renderText({ "<span style='text-decoration:underline'>Nombre d'anomalies</span> :" })
    output$textAnoTot <- renderText({ paste("Total : ", server_values$selectedQuestionnaire %>% pull("NB_ANO_TOT")) })
    output$textAnoNonTraite <- renderText({ paste("Non traitées : ", server_values$selectedQuestionnaire %>% pull("NB_ANO_NON_TRAITEES")) })
    output$textAnoEnCours <- renderText({ paste("En cours : ", server_values$selectedQuestionnaire %>% pull("NB_ANO_EN_COURS")) })
    output$textAnoForcees <- renderText({ paste("Forcées : ", server_values$selectedQuestionnaire %>% pull("NB_ANO_FORCEES")) })
    output$textAnoEnAttente <- renderText({ paste("En attente : ", server_values$selectedQuestionnaire %>% pull("NB_ANO_EN_ATTENTE")) })
    output$textAnoCorr <- renderText({ paste("Corrigées : ", server_values$selectedQuestionnaire %>% pull("NB_ANO_CORRIGEES")) })
    output$commentEnq <- renderText({ server_values$selectedQuestionnaire %>% pull("COMMENT_QUEST") })
    output$commentGest <- renderText({ server_values$selectedQuestionnaire %>% pull("COMMENT_GEST") })

    # Anomalies filtrées
    anomaliesFiltrees <- reactive({
      req(df_anomalies())
      df <- df_anomalies() %>%
        filter(ETAT_ANOMALIE %in% (cst_mapping_etat %>% filter(ETAT_LETTRE %in% input$filtreEtatAno) %>% pull(ETAT_CHIFFRE))
               | is.null(input$filtreEtatAno)) %>%
        mutate(ETAT = case_when(
          ETAT_ANOMALIE == 0 ~ "Non traité",
          ETAT_ANOMALIE == 1 ~ "En cours de traitement",
          ETAT_ANOMALIE == 2 ~ "Corrigée dans l'enquête",
          ETAT_ANOMALIE == 3 ~ "Corrigée",
          ETAT_ANOMALIE == 4 ~ "Forcée"
        ))
      return(df)
    })

    # Tableau anomalies
    output$tableAnomalies <- renderDataTable(
      anomaliesFiltrees() %>% select(input$pickerAnomalies, INFOS_COMP),
      extensions = c("FixedHeader"),
      options = list(dom = 'lfrtip',
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        language = fr, scrollY = 300, fixedHeader = TRUE,
        pageLength = 50, lengthMenu = c(5, 10, 50), scrollX = TRUE,
        rowCallback = JS(
          "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
          "var full_text = aData[6]",
          "$('td:eq(6)', nRow).attr('title', full_text);",
          "$('td:eq(6)', nRow).tooltip({",
          "'delay': 0, 'placement': 'left', 'track': true, 'fade': 250, 'container': 'body', 'z-index': 9999",
          "});",
          "}")
      ),
      selection = "multiple",
      class = "nowrap"
    )

    observeEvent(input$tableAnomalies_rows_selected, {
      server_values$selectedAnomalie <- anomaliesFiltrees() %>% slice(input$tableAnomalies_rows_selected)
    })

    # Anomalies autres enquêtes
    output$anomalieAutreEnquete <- renderUI({
      ns <- session$ns
      siren <- server_values$selectedQuestionnaire %>% pull("SIRET") %>% substr(0, 9)
      df_autres <- NULL
      if (!is.null(donnees_globales$df_anomalies_archivees)) {
        df_autres <- donnees_globales$df_anomalies_archivees %>%
          mutate(SIREN = substr(SIRET, 0, 9)) %>% filter(SIREN == siren) %>% select(-SIREN)
      }
      if (is.null(df_autres) || nrow(df_autres) == 0) {
        return(tagList(h3("Anomalies de la campagne précédente"), "Pas d'anomalies à afficher"))
      }
      tagList(
        h3("Anomalies de la campagne précédente"), br(),
        fluidRow(
          column(6, materialSwitch(ns("switchAutresFiltres"), label = span("Cas multi-enquêtes : Afficher/Masquer", style = "color:#797979;"),
            value = FALSE, status = "primary", right = TRUE)),
          column(3, offset = 3,
            pickerInput(ns("pickerAutresColonnes"), "Colonnes",
              choices = colnames(df_autres %>% select(CAMPAGNE, CODE, LIBELLE, LIGNE, ETAT, COMMENT_VALIDATION, INFOS_COMP)),
              selected = c("CAMPAGNE", "CODE", "LIBELLE", "LIGNE", "ETAT"),
              multiple = TRUE,
              options = list(`selected-text-format` = "count",
                `count-selected-text` = "{0} colonnes selectionnées sur {1}")))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("switchAutresFiltres")),
          fluidRow(
            column(3, selectizeInput(ns("filtreAutreEnquete"), "Enquête :",
              choices = df_autres %>% pull("ENQUETE") %>% unique(), multiple = TRUE)),
            column(3, selectizeInput(ns("filtreAutreCampagne"), "Campagne :",
              choices = df_autres %>% pull("CAMPAGNE") %>% unique(), multiple = TRUE))
          )
        ),
        fluidRow(column(12, dataTableOutput(ns("tableAnomaliesArchivees"), width = "100%", height = "auto")))
      )
    })

    output$tableAnomaliesArchivees <- renderDataTable({
      ns <- session$ns
      siren <- server_values$selectedQuestionnaire %>% pull("SIRET") %>% substr(0, 9)
      df <- donnees_globales$df_anomalies_archivees %>%
        mutate(SIREN = substr(SIRET, 0, 9)) %>% filter(SIREN == siren) %>% select(-SIREN) %>%
        filter(ENQUETE %in% input$filtreAutreEnquete | is.null(input$filtreAutreEnquete)) %>%
        filter(CAMPAGNE %in% input$filtreAutreCampagne | is.null(input$filtreAutreCampagne))
      dt_cols <- input$pickerAutresColonnes
      if (!is.null(dt_cols) && length(dt_cols) > 0) df <- df %>% select(dt_cols)
      df
    },
    extensions = c("FixedHeader"),
    options = list(dom = 'lfrtip',
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      language = fr, pageLength = 10, scrollX = TRUE),
    selection = "none"
    )

    # Retour du modal
    observeEvent(input$retourQuest, {
      removeModal()
    })

    # ==========================================================================
    # MODAL : Traiter l'anomalie
    # ==========================================================================
    observeEvent(input$openModalAnomalie, {
      if (!is.null(input$tableAnomalies_rows_selected)) {
        req(server_values$selectedAnomalie)
        ns <- session$ns

        etat_actuel <- server_values$selectedAnomalie[1, ] %>% pull("ETAT_ANOMALIE")
        updateSelectInput(session, "nouvelEtat",
          choices = cst_mapping_etat %>% filter(ETAT_LETTRE != "Corrigée") %>% pull(ETAT_LETTRE),
          selected = cst_mapping_etat %>% filter(ETAT_CHIFFRE == etat_actuel) %>% pull("ETAT_LETTRE"))
        output$errorMessage <- renderText({ "" })
        updateTextInput(session, "commentaireAno", value = server_values$selectedAnomalie[1, ] %>% pull("COMMENT_VALIDATION"))

        showModal(modalDialog(
          size = "l", title = "Traitement d'une anomalie",
          fluidPage(
            fluidRow(
              column(3, p("Date export Capibara : ")), column(3, textOutput(ns("dateExport"))),
              column(3, p("Date mise à jour SUIVAL : ")), column(3, textOutput(ns("dateUpdate")))
            ), br(),
            h3("Données du questionnaire"),
            fluidRow(
              column(2, htmlOutput(ns("textEnquete"))), column(2, htmlOutput(ns("textCampagne"))),
              column(2, htmlOutput(ns("textGestionnaire"))), column(2, htmlOutput(ns("textEtatQuest"))),
              column(2, htmlOutput(ns("textPriorite")))
            ), br(),
            fluidRow(
              column(3, htmlOutput(ns("textSIRET"))), column(3, offset = 3, htmlOutput(ns("textRSociale")))
            ), br(),
            fluidRow(
              column(2, htmlOutput(ns("textLabelAno"))), column(1, offset = 2, htmlOutput(ns("textAnoTot"))),
              column(2, htmlOutput(ns("textAnoNonTraite"))), column(1, htmlOutput(ns("textAnoEnCours"))),
              column(1, htmlOutput(ns("textAnoForcees"))), column(1, htmlOutput(ns("textAnoEnAttente"))),
              column(1, htmlOutput(ns("textAnoCorr")))
            ), br(),
            h3("Anomalie"), br(),
            fluidRow(column(12, dataTableOutput(ns("tableAnomalieDetail"), width = "100%", height = "auto"))), br(),
            fluidRow(
              column(2, selectInput(ns("nouvelEtat"), "Liste d'état", choices = NULL)),
              column(6, offset = 4, textInput(ns("commentaireAno"), "Commentaire"))
            ),
            fluidRow(column(2, offset = 6, htmlOutput(ns("errorMessage"))))
          ),
          footer = fluidRow(
            column(2, offset = 3, actionButton(ns("changerEtat"), "Changer état")),
            column(2, offset = 3, actionButton(ns("retourAno"), "Retour"))
          )
        ))

        if (any(server_values$selectedAnomalie %>% pull(ETAT) %in% c("Corrigée"))) {
          shinyjs::disable("commentaireAno")
          shinyjs::disable("nouvelEtat")
          shinyjs::disable("changerEtat")
        }
      }
    })

    output$tableAnomalieDetail <- renderDataTable(
      server_values$selectedAnomalie %>%
        select(IDENTIFIANT_SUIVALIAA, CODE, ETAT, LIBELLE, ECRAN, VARIABLES,
               INFOS_COMP, DATE, COMMENT_VALIDATION, ID_LIGNE_SR, LIGNE),
      extensions = c("FixedHeader"),
      options = list(dom = 'lfrtip',
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        language = fr, scrollY = 200, pageLength = 50, scrollX = TRUE),
      selection = "none"
    )

    # Changement d'état
    observeEvent(input$changerEtat, {
      if (input$nouvelEtat == "Forcée" && (input$commentaireAno == "" | is.null(input$commentaireAno))) {
        output$errorMessage <- renderText("<span style='color:red'>Ce champ ne doit pas être vide</span>")
        showNotification("Veuillez remplir le champ commentaire", duration = 2, type = "error")
        return()
      }

      shinyjs::disable("changerEtat")
      int_etat <- cst_mapping_etat %>% filter(ETAT_LETTRE == input$nouvelEtat) %>% pull(ETAT_CHIFFRE)
      colonne_nouvel_etat <- cst_mapping_etat %>% filter(ETAT_CHIFFRE == int_etat) %>% pull("ETAT_COLONNE")

      for (i in 1:nrow(server_values$selectedAnomalie)) {
        # Mise à jour en base
        interagir_dB_mise_a_jour_anomalies(
          server_values$selectedAnomalie[i, ] %>% pull("IDENTIFIANT_SUIVALIAA"),
          server_values$selectedAnomalie[i, ] %>% pull("CODE"),
          server_values$selectedAnomalie[i, ] %>% pull("ID_LIGNE_SR"),
          int_etat, input$commentaireAno)

        colonne_etat_precedent <- cst_mapping_etat %>%
          filter(ETAT_CHIFFRE == server_values$selectedAnomalie[i, ] %>% pull("ETAT_ANOMALIE")) %>%
          pull("ETAT_COLONNE")

        server_values$selectedQuestionnaire <- server_values$selectedQuestionnaire %>% mutate(
          "{colonne_etat_precedent}" := !!sym(colonne_etat_precedent) - 1,
          "{colonne_nouvel_etat}" := !!sym(colonne_nouvel_etat) + 1,
          ETAT_QUEST = case_when(
            NB_ANO_TOT == NB_ANO_NON_TRAITEES ~ 0,
            NB_ANO_TOT == NB_ANO_CORRIGEES + NB_ANO_FORCEES ~ 2,
            TRUE ~ 1
          ))

        donnees_globales$df_questionnaire <<- mettreAjourQuestionnaire(
          donnees_globales$df_questionnaire,
          server_values$selectedQuestionnaire %>% pull(IDENTIFIANT_SUIVALIAA),
          colonne_etat_precedent, colonne_nouvel_etat, input$commentaireAno)
      }

      interagir_dB_mise_a_jour_questionnaire(server_values$selectedQuestionnaire)
      ouvrirModalQuestionnaire()
    })

    # Retour
    observeEvent(input$retourAno, {
      ouvrirModalQuestionnaire()
    })
  })
}