# ==============================================================================
# MODULE : Questionnaire dynamique Pogues
# ==============================================================================

# UI
questionnaire_pogues_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .questionnaire-module { padding: 15px; }
        .questionnaire-module .module-header { background-color: #2c3e50; color: white; padding: 10px 15px; border-radius: 5px; margin-bottom: 15px; }
        .questionnaire-module .module-header h3 { margin: 0; font-size: 16px; }
        .questionnaire-module .question-text { font-size: 15px; font-weight: 500; margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #2c3e50; border-radius: 3px; }
        .questionnaire-module .help-text { font-size: 13px; color: #555; font-style: italic; padding: 8px; margin-bottom: 10px; background-color: #eaf2f8; border-left: 3px solid #3498db; border-radius: 3px; }
        .questionnaire-module .nav-buttons { margin-top: 20px; padding-top: 15px; border-top: 1px solid #ddd; }
        .questionnaire-module .welcome-container { max-width: 700px; margin: 60px auto; text-align: center; }
        .questionnaire-module .survey-card { display: inline-block; width: 280px; margin: 15px; padding: 30px 20px; background-color: #fff; border: 2px solid #ecf0f1; border-radius: 10px; cursor: pointer; transition: all 0.3s ease; vertical-align: top; }
        .questionnaire-module .survey-card:hover { border-color: #2c3e50; box-shadow: 0 4px 15px rgba(0,0,0,0.1); transform: translateY(-2px); }
        .questionnaire-module .survey-card .icon { font-size: 48px; color: #2c3e50; margin-bottom: 15px; }
        .questionnaire-module .unit-select-container { max-width: 1000px; margin: 20px auto; }
        .questionnaire-module .sidebar-nav { background-color: #f8f9fa; border-radius: 5px; padding: 10px; min-height: 400px; }
        .questionnaire-module .sidebar-nav .nav-pills > li > a { border-radius: 3px; color: #2c3e50; padding: 6px 12px; font-size: 13px; border-bottom: 1px solid #ecf0f1; }
        .questionnaire-module .sidebar-nav .nav-pills > li.active > a { background-color: #2c3e50; color: white; }
        .questionnaire-module .unit-table { margin-top: 10px; max-height: 400px; overflow-y: auto; }
        .questionnaire-module .unit-table td { font-size: 12px; }
        .questionnaire-module .info-section { background-color: #f8f9fa; border-radius: 4px; padding: 15px; margin-bottom: 15px; border-left: 4px solid #3498db; }
        .questionnaire-module .info-section h5 { color: #2c3e50; margin-top: 0; border-bottom: 1px solid #ddd; padding-bottom: 5px; }
        .questionnaire-module form { margin-bottom: 0; }
        .questionnaire-module .form-group { margin-bottom: 5px; }
        .questionnaire-module .loading-container { text-align: center; padding: 100px 0; }
        .questionnaire-module .production-table th { background-color: #2c3e50; color: white; font-size: 12px; text-align: center; }
        .questionnaire-module .production-table td { text-align: center; vertical-align: middle; }
        .questionnaire-module .unit-highlight { background-color: #fef9e7 !important; }
        .shiny-notification { position: fixed; top: 10px; right: 10px; z-index: 99999; }
      "))
    ),
    div(class = "questionnaire-module", uiOutput(ns("app_content")))
  )
}

# Champs "adresse de collecte" et "coordonnees du correspondant" éditables.
# Source unique utilisée à la fois pour l'affichage des champs et pour leur
# sauvegarde, afin d'éviter toute duplication de la liste des champs.
UNIT_EDIT_FIELDS <- c("street_number", "street_type", "street_name", "address_supplement",
                      "zip_code", "city", "contact_name", "contact_tel", "contact_email")

# Valeur à afficher pour un champ unité éditable : priorité à une valeur déjà
# modifiée et sauvegardée (env_vars$UNIT_XXX), sinon valeur brute de l'unité.
# isolate() : on ne veut pas qu'une frappe clavier invalide toute la page.
get_unit_field <- function(ui, env_vars, field) {
  vn <- paste0("UNIT_", toupper(field))
  saved <- isolate(env_vars[[vn]])
  if (!is.null(saved) && !is.na(saved) && nchar(as.character(saved)) > 0) return(as.character(saved))
  if (!is.null(ui) && nrow(ui) > 0 && field %in% names(ui)) return(as.character(ui[[field]][1]))
  ""
}

# Helper: nom de variable pour cellule tableau
get_table_var_name <- function(q, row_idx, col_idx, pogues) {
  if (!is.null(q$var_mapping) && length(q$var_mapping) > 0) {
    key <- paste(row_idx, col_idx)
    vn <- q$var_mapping[[key]]
    if (!is.null(vn)) return(vn)
  }
  cell_map <- list()
  if (length(q$mapping) > 0) {
    for (m in q$mapping) {
      parts <- strsplit(m$MappingTarget, " ")[[1]]
      if (length(parts) == 2) cell_map[[paste(as.numeric(parts[1]), as.numeric(parts[2]))]] <- m$MappingSource
    }
    key <- paste(row_idx, col_idx); src <- cell_map[[key]]
    if (!is.null(src)) {
      for (v_name in names(pogues$variables)) {
        if (!is.null(pogues$variables[[v_name]]) && pogues$variables[[v_name]]$id == src) return(v_name)
      }
    }
  }
  dims <- q$dimensions; md <- list(); for (d in dims) if (d$type == "MEASURE") md <- append(md, list(d))
  if (col_idx <= length(md) && !is.null(md[[col_idx]]$name)) return(md[[col_idx]]$name)
  paste0(q$name, col_idx)
}

# Server
questionnaire_pogues_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    survey_selected <- reactiveVal(FALSE); survey_name <- reactiveVal(NULL)
    show_info <- reactiveVal(FALSE); unit_selected <- reactiveVal(FALSE)
    selected_unit_id <- reactiveVal(NULL); pogues <- reactiveVal(NULL)
    db_path <- reactiveVal(NULL); enquete_id <- reactiveVal(NULL)
    current_module <- reactiveVal(NULL); env_vars <- reactiveValues()
    # Stocke les infos unité pour adresse/contact
    unit_info_data <- reactiveVal(NULL)

    observe({
      for (survey_key in names(AVAILABLE_SURVEYS)) {
        btn_name <- paste0("select_", survey_key)
        if (!is.null(input[[btn_name]]) && input[[btn_name]] > 0) { survey_selected(TRUE); survey_name(survey_key); show_info(TRUE); break }
      }
    })
    observeEvent(input$btn_info_next, { show_info(FALSE) })
    observeEvent(input$btn_back_to_info, { show_info(TRUE) })

    # Unités de l'enquête en cours : source unique, réutilisée pour peupler le
    # selectize, afficher le tableau cliquable et le détail de l'unité (évite
    # de recharger 3 fois les mêmes données).
    units_r <- reactive({
      name <- survey_name()
      req(name)
      list_survey_units(name)
    })

    # Peuple le selectize UNIQUEMENT quand l'écran de sélection d'unité est
    # réellement affiché (survey_selected && !unit_selected && !show_info).
    # Avant ce correctif, la mise à jour partait dès que survey_name() était
    # connu, c'est-à-dire pendant l'écran d'information de l'enquête, alors que
    # le champ selectizeInput n'existait pas encore côté navigateur : le
    # message était perdu et la recherche par unité ne fonctionnait jamais.
    observe({
      req(survey_selected(), !unit_selected(), !show_info())
      units <- units_r()
      if (is.null(units) || nrow(units) == 0) return()
      lbls <- paste0(units$id, " - ", units$corporate_name, " (", units$city, ")")
      choices <- setNames(units$id, lbls)
      # onFlushed : on attend que le HTML du selectizeInput soit bien envoyé
      # au client avant d'envoyer la mise à jour des choix.
      session$onFlushed(function() {
        updateSelectizeInput(session, "unit_selector", choices = choices,
          selected = character(0), server = TRUE)
      }, once = TRUE)
    })

    # Clic sur une ligne du tableau (class = "unit-table") → sélectionne
    # l'unité correspondante dans le selectize
    observeEvent(input$click_unit_row, {
      unit_id <- input$click_unit_row
      req(unit_id, nchar(unit_id) > 0)
      updateSelectizeInput(session, "unit_selector", selected = unit_id)
    })

    # Sélection via bouton "Démarrer"
    observeEvent(input$btn_select_unit, {
      unit_id <- input$unit_selector
      if (!is.null(unit_id) && unit_id != "") {
        selected_unit_id(unit_id)
        unit_selected(TRUE)
        name <- survey_name()
        ui <- get_unit_info_from_rem(name, unit_id)
        if (!is.null(ui) && nrow(ui) > 0) unit_info_data(ui)
      }
    })

    output$unit_details <- renderUI({
      uid <- input$unit_selector
      if (is.null(uid) || uid == "") return(div("Selectionnez une unite"))
      units <- units_r()
      if (is.null(units)) return(div(""))
      ui <- units[units$id == uid, ]; if (nrow(ui) == 0) return(div(""))
      tagList(
        div(style = "display: flex; gap: 20px; flex-wrap: wrap;",
          div(style = "flex: 1; min-width: 250px;",
            h5(icon("building"), " Identite"),
            p(paste0("SIRET : ", uid)),
            p(paste0("Raison sociale : ", ui$corporate_name[1])),
            p(paste0("APE : ", ui$ape[1]))
          ),
          div(style = "flex: 1; min-width: 250px;",
            h5(icon("map-marker"), " Adresse"),
            p(paste(ui$street_number[1], ui$street_type[1], ui$street_name[1])),
            if (nchar(ui$address_supplement[1]) > 0) p(ui$address_supplement[1]),
            p(paste(ui$zip_code[1], ui$city[1])),
            if (nchar(ui$cedex_name[1]) > 0) p(paste(ui$cedex_code[1], ui$cedex_name[1]))
          ),
          div(style = "flex: 1; min-width: 250px;",
            h5(icon("user"), " Contact"),
            p(paste0("Nom : ", ui$contact_name[1])),
            p(paste0("Tel : ", ui$contact_tel[1])),
            p(paste0("Email : ", ui$contact_email[1]))
          )
        )
      )
    })

    output$btn_download_csv <- downloadHandler(
      filename = function() { n <- survey_name(); if (is.null(n)) "export.csv" else paste0("interrogations_", n, ".csv") },
      content = function(file) { n <- survey_name(); if (is.null(n)) return(); df <- export_interrogations_csv(n); if (!is.null(df)) write.csv2(df, file, row.names = FALSE, fileEncoding = "UTF-8") }
    )

    # Chargement questionnaire
    observe({
      name <- survey_name(); uid <- selected_unit_id()
      if (is.null(name) || is.null(uid)) return()
      pogues_path <- find_pogues_file(name)
      if (is.null(pogues_path) || !file.exists(pogues_path)) return()
      p <- load_pogues(pogues_path); pogues(p)
      db <- paste0(tolower(p$name), "_questionnaire.db"); db_path(db); init_db(db)
      eid <- paste0(p$name, "_", uid, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      enquete_id(eid); create_enquete(db, eid, p$questionnaire_id, p$name)
      unit_data <- load_unit_data(name, uid)
      for (var_name in names(unit_data)) {
        val <- unit_data[[var_name]]
        if (is.list(val)) env_vars[[var_name]] <- val else env_vars[[var_name]] <- val
        if (is.list(val)) {
          for (i in seq_along(val)) { v <- val[[i]]; if (!is.null(v) && !is.na(v)) save_response(db, eid, p$questionnaire_id, var_name, as.character(v), ligne = i, colonne = 1) }
        } else { save_response(db, eid, p$questionnaire_id, var_name, as.character(val)) }
      }
      mods <- build_module_order(p, reactiveValuesToList(env_vars), list())
      if (length(mods) > 0) current_module(mods[1]) else current_module(NULL)
    })

    # Charger réponses existantes
    observe({
      p <- pogues(); db <- db_path(); eid <- enquete_id()
      if (is.null(p) || is.null(db) || is.null(eid)) return()
      er <- load_responses(db, eid)
      if (nrow(er) > 0) {
        vg <- er %>% group_by(variable_name) %>% summarise(valeurs = list(valeur), lignes = list(ligne), .groups = "keep")
        for (i in 1:nrow(vg)) {
          vn <- vg$variable_name[i]; lignes <- unlist(vg$lignes[i]); valeurs <- vg$valeurs[[i]]
          if (length(lignes) > 1) { vals <- list(); for (j in seq_along(lignes)) { l <- lignes[j]; while (length(vals) < l) vals[[length(vals) + 1]] <- NA; vals[[l]] <- valeurs[[j]] }; env_vars[[vn]] <- vals }
          else { env_vars[[vn]] <- as.character(valeurs[1]) }
        }
      }
    })

    observeEvent(input$btn_change_survey, {
      survey_selected(FALSE); survey_name(NULL); unit_selected(FALSE); selected_unit_id(NULL)
      pogues(NULL); db_path(NULL); enquete_id(NULL); current_module(NULL); unit_info_data(NULL)
      for (nm in names(env_vars)) env_vars[[nm]] <- NULL
    })
    observeEvent(input$btn_change_unit, {
      unit_selected(FALSE); selected_unit_id(NULL); pogues(NULL); db_path(NULL); enquete_id(NULL); current_module(NULL)
      for (nm in names(env_vars)) env_vars[[nm]] <- NULL
    })

    # Navigation
    # Ordre des modules calculé une seule fois par changement pertinent puis
    # réutilisé partout (au lieu d'appeler build_module_order() 4 fois).
    module_order <- reactive({
      p <- pogues(); req(p)
      build_module_order(p, reactiveValuesToList(env_vars), list())
    })
    current_index <- reactive({
      req(current_module())
      order <- module_order()
      idx <- which(order == current_module())
      if (length(idx) == 0) 1 else idx
    })
    observeEvent(input$btn_next, { order <- module_order(); idx <- current_index(); if (idx < length(order)) current_module(order[idx + 1]) })
    observeEvent(input$btn_prev, { order <- module_order(); idx <- current_index(); if (idx > 1) current_module(order[idx - 1]) })
    observe({ p <- pogues(); req(p); for (mn in names(p$modules)) { bn <- paste0("nav_", mn); if (!is.null(input[[bn]]) && input[[bn]] > 0) current_module(mn) } })

    # Sauvegarde réponses
    observe({
      p <- pogues(); db <- db_path(); eid <- enquete_id()
      if (is.null(p) || is.null(db) || is.null(eid)) return()
      for (var_name in names(p$variables)) {
        inp <- paste0("q_", var_name); val <- input[[inp]]
        if (!is.null(val) && length(val) == 1) { env_vars[[var_name]] <- as.character(val); save_response(db, eid, p$questionnaire_id, var_name, as.character(val)) }
      }
      for (mn in names(p$modules)) {
        mod <- p$modules[[mn]]; if (is.null(mod)) next
        for (qn in names(mod$questions)) {
          q <- mod$questions[[qn]]; if (q$type != "TABLE") next
          dims <- q$dimensions; pd <- NULL; md <- list()
          for (d in dims) { if (d$type == "PRIMARY") pd <- d; if (d$type == "MEASURE") md <- append(md, list(d)) }
          nr <- 3
          if (!is.null(pd$size)) { sv <- pd$size; sval <- env_vars[[sv]]; nr <- if (!is.null(sval)) as.numeric(sval) else as.numeric(sv) }
          if (is.na(nr) || nr < 1) nr <- 1
          nc <- length(md)
          for (ri in 1:nr) {
            for (ci in 1:nc) {
              inp <- paste0("tab_", qn, "_", ri, "_", ci); val <- input[[inp]]
              if (!is.null(val) && length(val) == 1) {
                vn <- get_table_var_name(q, ri, ci, p)
                cur <- env_vars[[vn]]; if (is.null(cur) || !is.list(cur)) cur <- list()
                while (length(cur) < ri) cur[[length(cur) + 1]] <- NA
                cur[[ri]] <- as.character(val); env_vars[[vn]] <- cur
                save_response(db, eid, p$questionnaire_id, vn, as.character(val), ligne = ri, colonne = ci)
              }
            }
          }
        }
      }
      # Sauvegarder adresse de collecte / coordonnées du correspondant modifiées
      if (!is.null(unit_info_data())) {
        uid <- isolate(selected_unit_id()); nm <- isolate(survey_name())
        if (!is.null(uid) && !is.null(nm)) {
          for (field in UNIT_EDIT_FIELDS) {
            inp <- paste0("edit_", field); val <- input[[inp]]
            if (!is.null(val) && length(val) == 1) {
              vn <- paste0("UNIT_", toupper(field))
              env_vars[[vn]] <- as.character(val)
              save_response(db, eid, p$questionnaire_id, vn, as.character(val))
            }
          }
        }
      }
    })

    # Outputs
    output$progress_bar <- renderUI({
      req(pogues(), current_module())
      order <- module_order()
      idx <- current_index(); total <- length(order) - 1
      prog <- min(100, round((idx - 1) / max(total, 1) * 100))
      div(div(style = "display: flex; justify-content: space-between; font-size: 12px;", span("Debut"), span(ifelse(idx >= total, "Fin", paste0(prog, "%")))),
        div(class = "progress", div(class = "progress-bar", role = "progressbar", style = paste0("width: ", prog, "%;"), `aria-valuenow` = prog, `aria-valuemin` = 0, `aria-valuemax` = 100)))
    })
    output$nav_sidebar <- renderUI({ p <- pogues(); req(p, current_module()); render_nav_sidebar(p, current_module()) })
    output$module_indicator <- renderUI({
      p <- pogues(); req(p, current_module()); mod <- current_module(); mi <- p$modules[[mod]]
      title <- if (!is.null(mi)) resolve_vtl(mi$label, reactiveValuesToList(env_vars)) else if (mod == "QUESTIONNAIRE_END") "Fin du questionnaire" else mod
      div(class = "module-header", fluidRow(column(12, h3(title))))
    })
    output$prev_button <- renderUI({ req(current_module()); if (current_index() > 1) actionButton(session$ns("btn_prev"), "◀ Precedent", class = "btn btn-default") })
    output$next_button <- renderUI({
      req(pogues(), current_module())
      order <- module_order(); idx <- current_index()
      label <- if (idx < length(order)) "Suivant ▶" else "Terminer"
      actionButton(session$ns("btn_next"), label, class = "btn btn-primary")
    })
    output$main_content <- renderUI({
      p <- pogues(); req(p, current_module())
      if (current_module() == "QUESTIONNAIRE_END") render_fin_module(enquete_id())
      else render_module(current_module(), p, reactiveValuesToList(env_vars))
    })

    observeEvent(input$btn_submit, {
      db <- db_path(); eid <- enquete_id()
      if (is.null(db) || is.null(eid)) return()
      finalize_enquete(db, eid)
      showModal(modalDialog(title = "Questionnaire soumis", paste0("L'enquete ", eid, " a ete soumise avec succes."), easyClose = TRUE, footer = modalButton("Fermer")))
    })

    output$app_content <- renderUI({
      p <- pogues()
      if (!survey_selected()) {
        return(div(class = "welcome-container",
          h1("Questionnaire"),
          p("Selectionnez l'enquete a laquelle vous souhaitez repondre"),
          lapply(names(AVAILABLE_SURVEYS), function(sk) {
            s <- AVAILABLE_SURVEYS[[sk]]
            div(class = "survey-card", div(class = "icon", icon(s$icon)), h3(s$label), p(s$description), br(),
              actionButton(session$ns(paste0("select_", sk)), "Demarrer", class = "btn btn-primary btn-select"))
          })
        ))
      }
      if (survey_selected() && !unit_selected() && show_info()) return(render_survey_info_fun(survey_name()))
      if (survey_selected() && !unit_selected() && !show_info()) return(render_unit_selection_fun(survey_name(), units_r()))
      if (unit_selected() && !is.null(p)) {
        ui <- unit_info_data()
        return(fluidPage(
          div(style = "text-align: center; padding: 15px 0; border-bottom: 2px solid #2c3e50; margin-bottom: 20px;",
            h2(p$label, style = "color: #2c3e50; margin: 0;"), h5(p$owner, style = "color: #7f8c8d;"),
            h6(paste0("Enquete : ", enquete_id()), style = "color: #95a5a6;"),
            actionLink(session$ns("btn_change_unit"), "Changer d'unite", style = "font-size: 12px; text-decoration: underline; cursor: pointer; margin-right: 15px;"),
            actionLink(session$ns("btn_change_survey"), "Changer d'enquete", style = "font-size: 12px; text-decoration: underline; cursor: pointer;")
          ),
          # Panel adresse et contact éditables
          if (!is.null(ui) && nrow(ui) > 0) {
            div(class = "info-section",
              fluidRow(
                column(6,
                  h5(icon("map-marker"), " Adresse de collecte"),
                  textInput(session$ns("edit_street_number"), "N° voie", value = get_unit_field(ui, env_vars, "street_number"), width = "80px"),
                  textInput(session$ns("edit_street_type"), "Type voie", value = get_unit_field(ui, env_vars, "street_type"), width = "80px"),
                  textInput(session$ns("edit_street_name"), "Nom de la voie", value = get_unit_field(ui, env_vars, "street_name"), width = "100%"),
                  textInput(session$ns("edit_address_supplement"), "Complement", value = get_unit_field(ui, env_vars, "address_supplement"), width = "100%"),
                  fluidRow(
                    column(4, textInput(session$ns("edit_zip_code"), "Code postal", value = get_unit_field(ui, env_vars, "zip_code"))),
                    column(8, textInput(session$ns("edit_city"), "Ville", value = get_unit_field(ui, env_vars, "city"), width = "100%"))
                  )
                ),
                column(6,
                  h5(icon("user"), " Coordonnees du correspondant"),
                  textInput(session$ns("edit_contact_name"), "Nom prenom", value = get_unit_field(ui, env_vars, "contact_name"), width = "100%"),
                  textInput(session$ns("edit_contact_tel"), "Telephone", value = get_unit_field(ui, env_vars, "contact_tel"), width = "100%"),
                  textInput(session$ns("edit_contact_email"), "Email", value = get_unit_field(ui, env_vars, "contact_email"), width = "100%"),
                  br(),
                  p(style = "font-size: 11px; color: #999; font-style: italic;", icon("info-circle"), " Les modifications sont automatiquement sauvegardees en base.")
                )
              )
            )
          },
          div(class = "progress-bar-custom", uiOutput(session$ns("progress_bar"))),
          fluidRow(
            column(3, div(class = "sidebar-nav", uiOutput(session$ns("nav_sidebar")))),
            column(9,
              uiOutput(session$ns("module_indicator")), uiOutput(session$ns("main_content")),
              div(class = "nav-buttons", fluidRow(
                column(6, uiOutput(session$ns("prev_button"))),
                column(6, uiOutput(session$ns("next_button")), align = "right")
              ))
            )
          )
        ))
      }
      div(class = "loading-container", icon("spinner", class = "fa-spin fa-3x"), h3("Chargement..."))
    })
  })
}

# ==============================================================================
# FONCTIONS DE RENDU
# ==============================================================================
render_survey_info_fun <- function(survey_name) {
  ns <- getDefaultReactiveDomain()$ns
  context <- load_survey_context(survey_name); dates <- extract_survey_dates(context); meta <- extract_survey_metadata(context)
  if (is.null(dates) || is.null(meta)) {
    return(div(class = "unit-select-container", h2("Informations de l'enquete"), p("Aucune information contextuelle trouvee."),
      actionButton(ns("btn_info_next"), "Continuer →", class = "btn btn-primary btn-lg", width = "100%"), br(), br(),
      actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default")))
  }
  date_row <- function(lbl, dv, ic = "calendar") { if (is.null(dv) || dv == "") return(NULL); tags$tr(tags$td(icon(ic)), tags$td(lbl, style = "font-weight: 500;"), tags$td(format_iso_date(dv), style = "font-weight: 600; color: #2c3e50;")) }
  div(class = "unit-select-container",
    h2(meta$operation_label %||% survey_name),
    if (!is.null(meta$short_objectives)) div(style = "background: #eaf2f8; border-left: 4px solid #3498db; padding: 15px; border-radius: 3px;", p(meta$short_objectives)),
    div(h4(icon("info-circle"), " References"), tags$table(class = "table table-condensed", tags$tbody(
      if (!is.null(meta$operation_short_label)) tags$tr(tags$td("Code enquete"), tags$td(meta$operation_short_label)),
      if (!is.null(meta$year)) tags$tr(tags$td("Annee"), tags$td(meta$year))
    ))),
    div(h4(icon("clock"), " Calendrier"), tags$table(class = "table table-condensed", tags$tbody(
      date_row("Debut collecte", dates$collection_start), date_row("Date retour", dates$return_date),
      date_row("Relance 1", dates$followup_letter1), date_row("Relance 2", dates$followup_letter2),
      date_row("Mise en demeure", dates$formal_notice), date_row("Fin collecte", dates$collection_end)
    ))),
    br(), actionButton(ns("btn_info_next"), "Continuer →", class = "btn btn-primary btn-lg", width = "100%"), br(), br(),
    actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default"))
}

render_unit_selection_fun <- function(survey_name, units) {
  ns <- getDefaultReactiveDomain()$ns
  if (is.null(units) || nrow(units) == 0) {
    return(div(class = "unit-select-container", h2("Selection de l'unite"), p("Aucune donnee d'unite trouvee."),
      actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default")))
  }
  choices <- setNames(units$id, paste0(units$id, " - ", units$corporate_name, " (", units$city, ")"))
  tagList(
    div(class = "unit-select-container",
      h2(paste0("Selectionnez l'unite pour ", survey_name)),
      p(paste0(nrow(units), " unite(s) disponible(s).")),
      p(style = "font-size: 12px; color: #666;", icon("search"), " Tapez un SIRET, un nom d'entreprise ou une ville pour filtrer la liste."),
      selectizeInput(ns("unit_selector"), "Unite :", choices = NULL, multiple = FALSE, width = "100%",
        options = list(placeholder = "Rechercher par SIRET, raison sociale ou ville...", maxOptions = 1000)),
      div(class = "info-section", h5("Details de l'unite"), uiOutput(ns("unit_details"))),
      div(class = "unit-table",
        tags$table(class = "table table-condensed table-hover",
          tags$thead(tags$tr(tags$th("SIRET"), tags$th("Raison sociale"), tags$th("Ville"), tags$th("Code postal"), tags$th("APE"), tags$th("Statut"))),
          tags$tbody(lapply(1:min(nrow(units), 30), function(i) {
            tags$tr(style = "cursor: pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'}); var t=this.closest('table'); if(t) t.querySelectorAll('tr').forEach(function(r){r.classList.remove('unit-highlight');}); this.classList.add('unit-highlight');",
                ns("click_unit_row"), units$id[i]),
              tags$td(units$id[i]), tags$td(units$corporate_name[i]), tags$td(units$city[i]), tags$td(units$zip_code[i]),
              tags$td(units$ape[i]), tags$td(units$statut_label[i]))
          }))
        )
      ),
      if (nrow(units) > 30) div(style = "text-align: center; font-size: 12px;", paste0("... et ", nrow(units) - 30, " unite(s) supplementaire(s)"))
    ),
    br(),
    actionButton(ns("btn_select_unit"), "Demarrer le questionnaire", class = "btn btn-primary btn-lg", width = "100%"), br(), br(),
    downloadButton(ns("btn_download_csv"), "Telecharger le CSV", class = "btn btn-success", width = "100%"), br(), br(),
    actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default"),
    actionButton(ns("btn_back_to_info"), "← Informations de l'enquete", class = "btn btn-default pull-left")
  )
}