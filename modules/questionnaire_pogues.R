# ==============================================================================
# MODULE : Questionnaire dynamique Pogues
# ==============================================================================

# UI
questionnaire_pogues_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "questionnaire.css"),
      tags$script(src = "editable-table.js")
    ),
    div(class = "questionnaire-module", uiOutput(ns("app_content")))
  )
}

# Champs "adresse de collecte" et "coordonnees du correspondant" éditables.
UNIT_EDIT_FIELDS <- c(
  "street_number", "street_type", "street_name", "address_supplement",
  "zip_code", "city", "contact_name", "contact_tel", "contact_email"
)

get_unit_field <- function(ui, env_vars, field) {
  vn <- paste0("UNIT_", toupper(field))
  saved <- isolate(env_vars[[vn]])
  if (!is.null(saved) && !is.na(saved) && nchar(as.character(saved)) > 0) {
    return(as.character(saved))
  }
  if (!is.null(ui) && nrow(ui) > 0 && field %in% names(ui)) {
    return(as.character(ui[[field]][1]))
  }
  ""
}

apply_unit_overrides <- function(units, overrides) {
  if (is.null(units) || length(overrides) == 0) {
    return(units)
  }
  for (uid in names(overrides)) {
    idx <- which(units$id == uid)
    if (length(idx) == 1) {
      ov <- overrides[[uid]]
      for (f in names(ov)) if (f %in% names(units)) units[idx, f] <- ov[[f]]
    }
  }
  units
}

build_unit_edit_fields_ui <- function(ns, ui, env_vars_ctx = NULL) {
  val <- function(field) {
    if (!is.null(env_vars_ctx)) {
      return(get_unit_field(ui, env_vars_ctx, field))
    }
    if (!is.null(ui) && nrow(ui) > 0 && field %in% names(ui)) {
      return(as.character(ui[[field]][1]))
    }
    ""
  }
  fluidRow(
    column(
      6,
      h5(icon("map-marker"), " Adresse de collecte"),
      textInput(ns("modal_street_number"), "N° voie", value = val("street_number"), width = "80px"),
      textInput(ns("modal_street_type"), "Type voie", value = val("street_type"), width = "80px"),
      textInput(ns("modal_street_name"), "Nom de la voie", value = val("street_name"), width = "100%"),
      textInput(ns("modal_address_supplement"), "Complement", value = val("address_supplement"), width = "100%"),
      fluidRow(
        column(4, textInput(ns("modal_zip_code"), "Code postal", value = val("zip_code"))),
        column(8, textInput(ns("modal_city"), "Ville", value = val("city"), width = "100%"))
      )
    ),
    column(
      6,
      h5(icon("user"), " Coordonnees du correspondant"),
      textInput(ns("modal_contact_name"), "Nom prenom", value = val("contact_name"), width = "100%"),
      textInput(ns("modal_contact_tel"), "Telephone", value = val("contact_tel"), width = "100%"),
      textInput(ns("modal_contact_email"), "Email", value = val("contact_email"), width = "100%")
    )
  )
}

get_table_var_name <- function(q, row_idx, col_idx, pogues) {
  if (!is.null(q$var_mapping) && length(q$var_mapping) > 0) {
    key <- paste(row_idx, col_idx)
    vn <- q$var_mapping[[key]]
    if (!is.null(vn)) {
      return(vn)
    }
  }
  cell_map <- list()
  if (length(q$mapping) > 0) {
    for (m in q$mapping) {
      parts <- strsplit(m$MappingTarget, " ")[[1]]
      if (length(parts) == 2) cell_map[[paste(as.numeric(parts[1]), as.numeric(parts[2]))]] <- m$MappingSource
    }
    key <- paste(row_idx, col_idx)
    src <- cell_map[[key]]
    if (!is.null(src)) {
      for (v_name in names(pogues$variables)) {
        if (!is.null(pogues$variables[[v_name]]) && pogues$variables[[v_name]]$id == src) {
          return(v_name)
        }
      }
    }
  }
  dims <- q$dimensions
  md <- list()
  for (d in dims) if (d$type == "MEASURE") md <- append(md, list(d))
  if (col_idx <= length(md) && !is.null(md[[col_idx]]$name)) {
    return(md[[col_idx]]$name)
  }
  paste0(q$name, col_idx)
}

# Server
questionnaire_pogues_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    survey_selected <- reactiveVal(FALSE)
    survey_key <- reactiveVal(NULL)   # anciennement survey_name
    show_info <- reactiveVal(FALSE)
    unit_selected <- reactiveVal(FALSE)
    selected_unit_id <- reactiveVal(NULL)
    pogues <- reactiveVal(NULL)
    db_path <- reactiveVal(NULL)
    enquete_id <- reactiveVal(NULL)
    current_module <- reactiveVal(NULL)
    env_vars <- reactiveValues()
    original_vars <- reactiveVal(list())   # valeurs d'origine (import) pour détecter les corrections
    unit_info_data <- reactiveVal(NULL)
    unit_overrides <- reactiveVal(list())
    unit_panel_refresh <- reactiveVal(0)
    n1_edit_enabled <- reactiveVal(FALSE)   # widget "Éditer les données N-1 ?" (Oui/Non), affiché sur chaque page concernée

    observe({
      for (sk in names(AVAILABLE_SURVEYS)) {
        btn_name <- paste0("select_", sk)
        if (!is.null(input[[btn_name]]) && input[[btn_name]] > 0) {
          survey_selected(TRUE)
          survey_key(sk)
          show_info(TRUE)
          break
        }
      }
    })
    observeEvent(input$btn_info_next, {
      show_info(FALSE)
    })
    observeEvent(input$btn_back_to_info, {
      show_info(TRUE)
    })

    units_r <- reactive({
      name <- survey_key()
      req(name)
      list_survey_units(name)
    })

    units_effective_r <- reactive({
      units <- units_r()
      req(units)
      apply_unit_overrides(units, unit_overrides())
    })

    observe({
      req(survey_selected(), !unit_selected(), !show_info())
      units <- units_effective_r()
      if (is.null(units) || nrow(units) == 0) {
        return()
      }
      lbls <- paste0(units$id, " - ", units$corporate_name, " (", units$city, ")")
      choices <- setNames(units$id, lbls)
      session$onFlushed(function() {
        updateSelectizeInput(session, "unit_selector",
          choices = choices,
          selected = character(0), server = TRUE
        )
      }, once = TRUE)
    })

    observeEvent(input$click_unit_row, {
      unit_id <- input$click_unit_row
      req(unit_id, nchar(unit_id) > 0)
      updateSelectizeInput(session, "unit_selector", selected = unit_id)
    })

    observeEvent(input$btn_select_unit, {
      unit_id <- input$unit_selector
      if (!is.null(unit_id) && unit_id != "") {
        selected_unit_id(unit_id)
        unit_selected(TRUE)
        name <- survey_key()
        ui <- get_unit_info_from_rem(name, unit_id)
        if (!is.null(ui) && nrow(ui) > 0) {
          ov <- unit_overrides()[[unit_id]]
          if (!is.null(ov)) for (f in names(ov)) if (f %in% names(ui)) ui[1, f] <- ov[[f]]
          unit_info_data(ui)
        }
      }
    })

    output$unit_details <- renderUI({
      uid <- input$unit_selector
      if (is.null(uid) || uid == "") {
        return(div("Selectionnez une unite"))
      }
      units <- units_effective_r()
      if (is.null(units)) {
        return(div(""))
      }
      ui <- units[units$id == uid, ]
      if (nrow(ui) == 0) {
        return(div(""))
      }
      tagList(
        div(
          style = "display: flex; gap: 20px; flex-wrap: wrap;",
          div(
            style = "flex: 1; min-width: 250px;",
            h5(icon("building"), " Identite"),
            p(paste0("SIRET : ", uid)),
            p(paste0("Raison sociale : ", ui$corporate_name[1])),
            p(paste0("APE : ", ui$ape[1]))
          ),
          div(
            style = "flex: 1; min-width: 250px;",
            h5(icon("map-marker"), " Adresse"),
            p(paste(ui$street_number[1], ui$street_type[1], ui$street_name[1])),
            if (nchar(ui$address_supplement[1]) > 0) p(ui$address_supplement[1]),
            p(paste(ui$zip_code[1], ui$city[1])),
            if (nchar(ui$cedex_name[1]) > 0) p(paste(ui$cedex_code[1], ui$cedex_name[1]))
          ),
          div(
            style = "flex: 1; min-width: 250px;",
            h5(icon("user"), " Contact"),
            p(paste0("Nom : ", ui$contact_name[1])),
            p(paste0("Tel : ", ui$contact_tel[1])),
            p(paste0("Email : ", ui$contact_email[1]))
          )
        )
      )
    })

    output$btn_download_csv <- downloadHandler(
      filename = function() {
        n <- survey_key()
        if (is.null(n)) "export.csv" else paste0("interrogations_", n, ".csv")
      },
      content = function(file) {
        n <- survey_key()
        if (is.null(n)) {
          return()
        }
        df <- export_interrogations_csv(n)
        if (!is.null(df)) write.csv2(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    observeEvent(input$btn_edit_unit_details, {
      if (isolate(unit_selected())) {
        ui <- isolate(unit_info_data())
        req(!is.null(ui), nrow(ui) > 0)
        modal_fields <- build_unit_edit_fields_ui(session$ns, ui, env_vars)
      } else {
        uid <- input$unit_selector
        if (is.null(uid) || uid == "") {
          showNotification("Selectionnez d'abord une unite dans la liste.", type = "warning")
          return()
        }
        units <- units_effective_r()
        ui <- units[units$id == uid, ]
        req(nrow(ui) == 1)
        modal_fields <- build_unit_edit_fields_ui(session$ns, ui)
      }
      showModal(modalDialog(
        title = paste0("Modifier les details - unite ", ui$id[1]),
        size = "l", easyClose = TRUE,
        modal_fields,
        footer = tagList(
          modalButton("Annuler"),
          actionButton(session$ns("btn_save_unit_modal"), "Enregistrer", class = "btn btn-primary")
        )
      ))
    })

    observeEvent(input$btn_save_unit_modal, {
      ov <- list()
      for (field in UNIT_EDIT_FIELDS) {
        val <- input[[paste0("modal_", field)]]
        if (!is.null(val)) ov[[field]] <- as.character(val)
      }
      if (isolate(unit_selected())) {
        p <- isolate(pogues())
        db <- isolate(db_path())
        eid <- isolate(enquete_id())
        req(p, db, eid)
        for (field in names(ov)) {
          vn <- paste0("UNIT_", toupper(field))
          env_vars[[vn]] <- ov[[field]]
          save_response(db, eid, p$questionnaire_id, vn, ov[[field]])
        }
        unit_panel_refresh(unit_panel_refresh() + 1)
      } else {
        uid <- input$unit_selector
        req(uid, uid != "")
        cur <- unit_overrides()
        cur[[uid]] <- ov
        unit_overrides(cur)
      }
      removeModal()
      showNotification("Details de l'unite mis a jour.", type = "message")
    })

    # Chargement questionnaire
    observe({
      name <- survey_key()
      uid <- selected_unit_id()
      if (is.null(name) || is.null(uid)) {
        return()
      }
      pogues_path <- find_pogues_file(name)
      if (is.null(pogues_path) || !file.exists(pogues_path)) {
        return()
      }
      p <- load_pogues(pogues_path)
      pogues(p)
      db <- paste0(tolower(p$name), "_questionnaire.db")
      db_path(db)
      init_db(db)
      
      # --- Rechercher un enquete_id existant pour cette paire (p$name, uid) ---
      # format attendu : <p$name>_<uid>_...
      con <- DBI::dbConnect(RSQLite::SQLite(), db)
      pref <- paste0(p$name, "_", uid, "_%")
      
      existing <- tryCatch({
        DBI::dbGetQuery(con,
          "SELECT id FROM enquetes WHERE id LIKE :pref ORDER BY updated_at DESC LIMIT 1",
          params = list(pref = pref))
      }, error = function(e) data.frame(id = character(0), stringsAsFactors = FALSE))
      
      DBI::dbDisconnect(con)
      
      if (nrow(existing) > 0 && nzchar(existing$id[1])) {
        eid <- existing$id[1]
      } else {
        eid <- paste0(p$name, "_", uid, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      }
      enquete_id(eid)
      create_enquete(db, eid, p$questionnaire_id, p$name)
      unit_data <- load_unit_data(name, uid)
      orig <- list()
      for (var_name in names(unit_data)) {
        if (var_name == "_N1_DATA_") {
          env_vars[[var_name]] <- unit_data[[var_name]]
          next
        }
        val <- unit_data[[var_name]]
        if (is.list(val)) {
          orig[[var_name]] <- val
        } else {
          orig[[var_name]] <- as.character(val)
        }
        if (is.list(val)) {
          env_vars[[var_name]] <- val
          for (i in seq_along(val)) {
            v <- val[[i]]
            if (length(v) == 1 && !is.null(v) && !is.na(v)) {
              save_response(db, eid, p$questionnaire_id, var_name, as.character(v), ligne = i, colonne = 1)
            }
          }
        } else if (length(val) == 1) {
          env_vars[[var_name]] <- val
          if (!is.na(val)) save_response(db, eid, p$questionnaire_id, var_name, as.character(val))
        } else {
          env_vars[[var_name]] <- val
        }
      }
      original_vars(orig)
      mods <- build_module_order(p, reactiveValuesToList(env_vars), list())
      if (length(mods) > 0) current_module(mods[1]) else current_module(NULL)
    })

    # Charger réponses existantes
    observe({
      p <- pogues()
      db <- db_path()
      eid <- enquete_id()
      if (is.null(p) || is.null(db) || is.null(eid)) {
        return()
      }
      er <- load_responses(db, eid)
      if (nrow(er) > 0) {
        vg <- er %>%
          group_by(variable_name) %>%
          summarise(valeurs = list(valeur), lignes = list(ligne), .groups = "keep")
        for (i in 1:nrow(vg)) {
          vn <- vg$variable_name[i]
          lignes <- unlist(vg$lignes[i])
          valeurs <- vg$valeurs[[i]]
          if (length(lignes) > 1) {
            vals <- list()
            for (j in seq_along(lignes)) {
              l <- lignes[j]
              while (length(vals) < l) vals[[length(vals) + 1]] <- NA
              vals[[l]] <- valeurs[[j]]
            }
            env_vars[[vn]] <- vals
          } else {
            env_vars[[vn]] <- as.character(valeurs[1])
          }
        }
      }
    })

    observeEvent(input$btn_change_survey, {
      survey_selected(FALSE)
      survey_key(NULL)
      unit_selected(FALSE)
      selected_unit_id(NULL)
      pogues(NULL)
      db_path(NULL)
      enquete_id(NULL)
      current_module(NULL)
      unit_info_data(NULL)
      for (nm in names(env_vars)) env_vars[[nm]] <- NULL
    })
    observeEvent(input$btn_change_unit, {
      unit_selected(FALSE)
      selected_unit_id(NULL)
      pogues(NULL)
      db_path(NULL)
      enquete_id(NULL)
      current_module(NULL)
      for (nm in names(env_vars)) env_vars[[nm]] <- NULL
    })

    # Navigation
    module_order <- reactive({
      p <- pogues()
      req(p)
      build_module_order(p, reactiveValuesToList(env_vars), list())
    })
    current_index <- reactive({
      req(current_module())
      order <- module_order()
      idx <- which(order == current_module())
      if (length(idx) == 0) 1 else idx
    })
    observeEvent(input$btn_next, {
      order <- module_order()
      idx <- current_index()
      if (idx < length(order)) current_module(order[idx + 1])
    })
    observeEvent(input$btn_prev, {
      order <- module_order()
      idx <- current_index()
      if (idx > 1) current_module(order[idx - 1])
    })
    observe({
      p <- pogues()
      req(p)
      for (mn in names(p$modules)) {
        bn <- paste0("nav_", mn)
        if (!is.null(input[[bn]]) && input[[bn]] > 0) current_module(mn)
      }
    })

    # ===========================================================================
    # SAUVEGARDE DIRECTE D'UN TABLEAU (bouton "Enregistrer les modifications")
    # ===========================================================================
    
    observeEvent(input$save_table_request, {
      req(input$save_table_request)
      q_name <- input$save_table_request$question
      if (is.null(q_name) || nchar(q_name) == 0) return()
      
      p <- isolate(pogues())
      db <- isolate(db_path())
      eid <- isolate(enquete_id())
      
      if (is.null(p) || is.null(db) || is.null(eid)) {
        showNotification("Contexte non disponible", type = "default")
        return()
      }
      
      pattern <- paste0("^tab_", q_name, "_(\\d+)_(\\d+)$")
      nb_saved <- 0
      
      for (input_name in names(input)) {
        if (grepl(pattern, input_name)) {
          parts <- stringr::str_match(input_name, pattern)
          row_idx <- as.numeric(parts[2])
          col_idx <- as.numeric(parts[3])
          new_value <- as.character(input[[input_name]])
          
          for (mn in names(p$modules)) {
            mod <- p$modules[[mn]]
            if (!is.null(mod) && q_name %in% names(mod$questions)) {
              q_tmp <- mod$questions[[q_name]]
              if (q_tmp$type == "TABLE") {
                vn <- get_table_var_name(q_tmp, row_idx, col_idx, p)
                save_response(db, eid, p$questionnaire_id, vn, new_value, ligne = row_idx, colonne = col_idx)
                
                cur <- isolate(env_vars[[vn]])
                if (is.null(cur) || !is.list(cur)) cur <- list()
                while (length(cur) < row_idx) cur[[length(cur) + 1]] <- NA
                cur[[row_idx]] <- new_value
                env_vars[[vn]] <- cur
                
                nb_saved <- nb_saved + 1
                break
              }
            }
          }
        }
      }
      
      showNotification(sprintf("Tableau '%s' : %d enregistr\u00e9e(s)", q_name, nb_saved), type = "default", duration = 3)
    })
    
    # ===========================================================================
    # ACTION SUR UNE CELLULE (validation / restauration)
    # ===========================================================================
    
    observeEvent(input$cell_action, {
      action <- input$cell_action
      req(action, action$action %in% c("validate", "reset"))
      
      var_name <- action$var
      new_value <- as.character(action$value %||% "")
      row_idx <- action$row
      col_idx <- action$col
      a <- action$action
      
      p <- isolate(pogues())
      db <- isolate(db_path())
      eid <- isolate(enquete_id())
      req(p, db, eid)
      
      # Si row/col sont fournis → cellule de tableau : stocker comme liste
      has_table_coords <- !is.null(row_idx) && !is.null(col_idx) &&
                          nchar(as.character(row_idx)) > 0 && as.numeric(row_idx) > 0
      
      if (has_table_coords) {
        cur <- isolate(env_vars[[var_name]])
        if (is.null(cur) || !is.list(cur)) cur <- list()
        r <- as.numeric(row_idx)
        while (length(cur) < r) cur[[length(cur) + 1]] <- NA
        cur[[r]] <- new_value
        env_vars[[var_name]] <- cur
        save_response(db, eid, p$questionnaire_id, var_name, new_value, ligne = r, colonne = as.numeric(col_idx))
      } else {
        # Cellule simple (non tableau)
        env_vars[[var_name]] <- new_value
        save_response(db, eid, p$questionnaire_id, var_name, new_value)
      }
    })
    
    # ===========================================================================
    # ACTION SUR UNE LIGNE (ajout / suppression)
    # ===========================================================================
    
    observeEvent(input$row_action, {
      action <- input$row_action
      req(action, action$action %in% c("add", "delete"))
      
      q_name <- action$question
      act <- action$action
      
      p <- isolate(pogues())
      req(p)
      
      rcv <- table_row_count_var(q_name)
      current_raw <- isolate(env_vars[[rcv]])
      
      # Convertir en numérique (toujours via caractère pour éviter le piège des facteurs)
      if (is.factor(current_raw)) current_raw <- as.character(current_raw)
      
      current_num <- suppressWarnings(as.numeric(current_raw))
      if (length(current_raw) == 0 || is.null(current_raw) || is.na(current_num) || current_raw == "") {
        q <- NULL
        for (mn in names(p$modules)) {
          mod <- p$modules[[mn]]
          if (!is.null(mod) && q_name %in% names(mod$questions)) {
            q <- mod$questions[[q_name]]
            break
          }
        }
        req(q)
        current_n <- get_table_base_n_rows(q, p, reactiveValuesToList(isolate(env_vars)))
      } else {
        current_n <- current_num
      }
      
      # Garantie que c'est un entier
      if (!is.numeric(current_n) || length(current_n) == 0 || is.na(current_n[1])) current_n <- 1
      
      if (act == "add") {
        new_n <- current_n + 1
        env_vars[[rcv]] <- as.character(new_n)
        db <- isolate(db_path())
        eid <- isolate(enquete_id())
        if (!is.null(db) && !is.null(eid)) {
          save_response(db, eid, p$questionnaire_id, rcv, as.character(new_n))
        }
      } else if (act == "delete") {
        row_to_delete <- action$row
        req(row_to_delete)
        new_n <- max(1, current_n - 1)
        env_vars[[rcv]] <- as.character(new_n)
        db <- isolate(db_path())
        eid <- isolate(enquete_id())
        if (!is.null(db) && !is.null(eid)) {
          save_response(db, eid, p$questionnaire_id, rcv, as.character(new_n))
        }
      }
    })
    
    # ===========================================================================
    # SAUVEGARDE DES MODIFICATIONS DE TABLEAUX
    # ===========================================================================

    observeEvent(input$table_modifications, {
      modifications <- input$table_modifications
      p <- pogues()
      db <- db_path()
      eid <- enquete_id()

      cat("\n=== SAUVEGARDE MODIFICATIONS TABLEAUX ===\n")
      cat("Modifications reçues:", length(modifications), "\n")

      if (is.null(modifications) || length(modifications) == 0) {
        cat("Aucune modification à enregistrer\n")
        showNotification("Aucune modification à enregistrer.", type = "warning")
        return()
      }

      if (is.null(p) || is.null(db) || is.null(eid)) {
        cat("Contexte manquant: p=", !is.null(p), "db=", !is.null(db), "eid=", !is.null(eid), "\n")
        showNotification("Erreur : contexte non disponible.", type = "error")
        return()
      }

      results <- list(success = list(), error = list())

      # Traiter chaque modification
      for (input_id in names(modifications)) {
        tryCatch(
          {
            new_value <- as.character(modifications[[input_id]])
            cat("Traitement:", input_id, "=", new_value, "\n")

            # Parser l'ID : format tab_QNAME_ROW_COL (les cellules N-1 utilisent
            # un qname préfixé "n1_QNAME" - cf. render_table_question)
            pattern <- "^tab_(.+)_(\\d+)_(\\d+)$"
            if (grepl(pattern, input_id)) {
              parts <- stringr::str_match(input_id, pattern)
              qname_raw <- parts[2]
              row_idx <- as.numeric(parts[3])
              col_idx <- as.numeric(parts[4])

              is_n1 <- grepl("^n1_", qname_raw)
              qname <- if (is_n1) sub("^n1_", "", qname_raw) else qname_raw

              cat("  Parsed: qname=", qname, "row=", row_idx, "col=", col_idx, "n1=", is_n1, "\n")

              # Retrouver le nom de variable
              found <- FALSE
              for (mn in names(p$modules)) {
                mod <- p$modules[[mn]]
                if (is.null(mod)) next
                if (!qname %in% names(mod$questions)) next

                q <- mod$questions[[qname]]
                if (q$type != "TABLE") next

                vn <- get_table_var_name(q, row_idx, col_idx, p)
                if (is_n1) vn <- n1_variable_name(vn)
                cat("  Variable trouvée:", vn, "\n")

                # Sauvegarder en base de données
                save_response(db, eid, p$questionnaire_id, vn, new_value, ligne = row_idx, colonne = col_idx)

                # Mettre à jour env_vars
                cur <- env_vars[[vn]]
                if (is.null(cur) || !is.list(cur)) cur <- list()
                while (length(cur) < row_idx) cur[[length(cur) + 1]] <- NA
                cur[[row_idx]] <- new_value
                env_vars[[vn]] <- cur

                results$success[[input_id]] <- TRUE
                found <- TRUE
                break
              }

              if (!found) {
                cat("  ERREUR: Question table non trouvée\n")
                results$error[[input_id]] <- "Question table non trouvée"
              }
            } else {
              cat("  ERREUR: ID ne correspond pas au pattern\n")
              results$error[[input_id]] <- "Format d'ID invalide"
            }
          },
          error = function(e) {
            cat("  EXCEPTION:", as.character(e$message), "\n")
            results$error[[input_id]] <<- as.character(e$message)
          }
        )
      }

      # Retourner le statut au JavaScript
      cat("Résultats: success=", length(results$success), "error=", length(results$error), "\n")
      shinyjs::runjs(sprintf("window.applySaveResults(%s);", jsonlite::toJSON(results)))

      # Message de confirmation
      nb_success <- length(results$success)
      nb_error <- length(results$error)
      if (nb_error == 0) {
        msg <- paste0("✓ ", nb_success, " modification(s) enregistrée(s).")
        showNotification(msg, type = "message")
        cat(msg, "\n")
      } else {
        msg <- paste0("⚠ ", nb_success, " enregistrée(s), ", nb_error, " erreur(s).")
        showNotification(msg, type = "warning")
        cat(msg, "\n")
      }
    })

    # Outputs
    output$unit_address_panel <- renderUI({
      unit_panel_refresh()
      ui <- unit_info_data()
      req(!is.null(ui), nrow(ui) > 0)
      div(
        class = "info-section",
        fluidRow(
          column(
            6,
            h5(icon("map-marker"), " Adresse de collecte"),
            p(paste(get_unit_field(ui, env_vars, "street_number"), get_unit_field(ui, env_vars, "street_type"), get_unit_field(ui, env_vars, "street_name"))),
            if (nchar(get_unit_field(ui, env_vars, "address_supplement")) > 0) p(get_unit_field(ui, env_vars, "address_supplement")),
            p(paste(get_unit_field(ui, env_vars, "zip_code"), get_unit_field(ui, env_vars, "city")))
          ),
          column(
            6,
            h5(icon("user"), " Coordonnees du correspondant"),
            p(paste0("Nom : ", get_unit_field(ui, env_vars, "contact_name"))),
            p(paste0("Tel : ", get_unit_field(ui, env_vars, "contact_tel"))),
            p(paste0("Email : ", get_unit_field(ui, env_vars, "contact_email")))
          )
        ),
        actionButton(session$ns("btn_edit_unit_details"), "Modifier l'adresse / le correspondant",
          icon = icon("pen"), class = "btn btn-default btn-sm"
        )
      )
    })
    output$module_selector_ui <- renderUI({
      p <- pogues()
      req(p)
      mods <- names(p$modules)
      mods <- mods[mods != "QUESTIONNAIRE_END"]
      labels <- sapply(mods, function(mn) {
        mi <- p$modules[[mn]]
        if (!is.null(mi)) mi$label %||% mn else mn
      })
      current <- current_module()
      selectInput(session$ns("module_selector"), NULL,
        choices = setNames(mods, labels),
        selected = if (!is.null(current) && current %in% mods) current else mods[1],
        width = "100%"
      )
    })
    
    observeEvent(input$module_selector, {
      req(input$module_selector)
      current_module(input$module_selector)
    })
    
    output$progress_bar <- renderUI({
      req(pogues(), current_module())
      order <- module_order()
      idx <- current_index()
      total <- length(order) - 1
      prog <- min(100, round((idx - 1) / max(total, 1) * 100))
      div(
        div(style = "display: flex; justify-content: space-between; font-size: 12px;", span("Debut"), span(ifelse(idx >= total, "Fin", paste0(prog, "%")))),
        div(class = "progress", div(class = "progress-bar", role = "progressbar", style = paste0("width: ", prog, "%;"), `aria-valuenow` = prog, `aria-valuemin` = 0, `aria-valuemax` = 100))
      )
    })
    output$nav_sidebar <- renderUI({
      p <- pogues()
      req(p, current_module())
      render_nav_sidebar(p, current_module())
    })
    output$module_indicator <- renderUI({
      p <- pogues()
      req(p, current_module())
      mod <- current_module()
      mi <- p$modules[[mod]]
      title <- if (!is.null(mi)) resolve_vtl(mi$label, reactiveValuesToList(env_vars)) else if (mod == "QUESTIONNAIRE_END") "Fin du questionnaire" else mod
      div(class = "module-header", fluidRow(column(12, h3(title))))
    })
    output$prev_button <- renderUI({
      req(current_module())
      if (current_index() > 1) actionButton(session$ns("btn_prev"), "◀ Precedent", class = "btn btn-default")
    })
    output$next_button <- renderUI({
      req(pogues(), current_module())
      order <- module_order()
      idx <- current_index()
      label <- if (idx < length(order)) "Suivant ▶" else "Terminer"
      actionButton(session$ns("btn_next"), label, class = "btn btn-primary")
    })
    output$main_content <- renderUI({
      p <- pogues()
      req(p, current_module())
      if (current_module() == "QUESTIONNAIRE_END") {
        render_fin_module(enquete_id())
      } else {
        evl <- reactiveValuesToList(env_vars)
        tagList(
          if (module_has_n1_data(current_module(), p, evl)) {
            div(
              class = "n1-edit-toggle", style = "margin-bottom: 15px;",
              shinyWidgets::radioGroupButtons(
                inputId = session$ns("toggle_edit_n1"),
                label = "Éditer les données N-1 ?",
                choices = c("Non" = "non", "Oui" = "oui"),
                selected = if (isTRUE(n1_edit_enabled())) "oui" else "non",
                status = "primary", size = "sm"
              )
            )
          },
          render_module(current_module(), p, evl, original_vars_list = isolate(original_vars()), n1_edit_enabled = isTRUE(n1_edit_enabled()))
        )
      }
    })

    observeEvent(input$toggle_edit_n1, {
      n1_edit_enabled(identical(input$toggle_edit_n1, "oui"))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_submit, {
      db <- db_path()
      eid <- enquete_id()
      if (is.null(db) || is.null(eid)) {
        return()
      }
      finalize_enquete(db, eid)
      showModal(modalDialog(title = "Questionnaire soumis", paste0("L'enquete ", eid, " a ete soumise avec succes."), easyClose = TRUE, footer = modalButton("Fermer")))
    })

    output$app_content <- renderUI({
      p <- pogues()
      if (!survey_selected()) {
        return(div(
          class = "welcome-container",
          h1("Questionnaire"),
          p("Selectionnez l'enquete a laquelle vous souhaitez repondre"),
          lapply(names(AVAILABLE_SURVEYS), function(sk) {
            s <- AVAILABLE_SURVEYS[[sk]]
            div(
              class = "survey-card", div(class = "icon", icon(s$icon)), h3(s$label), p(s$description), br(),
              actionButton(session$ns(paste0("select_", sk)), "Demarrer", class = "btn btn-primary btn-select")
            )
          })
        ))
      }
      if (survey_selected() && !unit_selected() && show_info()) {
        return(render_survey_info_fun(survey_key()))
      }
      if (survey_selected() && !unit_selected() && !show_info()) {
        return(render_unit_selection_fun(survey_key(), units_effective_r()))
      }
      if (unit_selected() && !is.null(p)) {
        return(fluidPage(
          div(
            style = "padding: 8px 0; border-bottom: 2px solid #2c3e50; margin-bottom: 10px;",
            fluidRow(
              column(6,
                h4(p$label, style = "color: #2c3e50; margin: 0; font-size: 16px;"),
                h6(paste0(enquete_id()), style = "color: #95a5a6; margin:0;")
              ),
              column(6, style = "text-align:right;",
                actionLink(session$ns("btn_change_unit"), "Changer d'unite", style = "font-size: 11px; text-decoration: underline; cursor: pointer; margin-right: 10px;"),
                actionLink(session$ns("btn_change_survey"), "Changer d'enquete", style = "font-size: 11px; text-decoration: underline; cursor: pointer;")
              )
            )
          ),
          uiOutput(session$ns("unit_address_panel")),
          # Navigation horizontale pleine largeur (remplace la sidebar gauche)
          fluidRow(
            column(10, uiOutput(session$ns("module_selector_ui"))),
            column(2, uiOutput(session$ns("progress_bar")))
          ),
          uiOutput(session$ns("module_indicator")),
          uiOutput(session$ns("main_content")),
          div(class = "nav-buttons", fluidRow(
            column(6, uiOutput(session$ns("prev_button"))),
            column(6, uiOutput(session$ns("next_button")), align = "right")
          ))
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
  context <- load_survey_context(survey_name)
  dates <- extract_survey_dates(context)
  meta <- extract_survey_metadata(context)
  if (is.null(dates) || is.null(meta)) {
    return(div(
      class = "unit-select-container", h2("Informations de l'enquete"), p("Aucune information contextuelle trouvee."),
      actionButton(ns("btn_info_next"), "Continuer →", class = "btn btn-primary btn-lg", width = "100%"), br(), br(),
      actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default")
    ))
  }
  date_row <- function(lbl, dv, ic = "calendar") {
    if (is.null(dv) || dv == "") {
      return(NULL)
    }
    tags$tr(tags$td(icon(ic)), tags$td(lbl, style = "font-weight: 500;"), tags$td(format_iso_date(dv), style = "font-weight: 600; color: #2c3e50;"))
  }
  div(
    class = "unit-select-container",
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
    actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default")
  )
}

render_unit_selection_fun <- function(survey_name, units) {
  ns <- getDefaultReactiveDomain()$ns
  if (is.null(units) || nrow(units) == 0) {
    return(div(
      class = "unit-select-container", h2("Selection de l'unite"), p("Aucune donnee d'unite trouvee."),
      actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default")
    ))
  }
  choices <- setNames(units$id, paste0(units$id, " - ", units$corporate_name, " (", units$city, ")"))
  tagList(
    div(
      class = "unit-select-container",
      h2(paste0("Selectionnez l'unite pour ", survey_name)),
      p(paste0(nrow(units), " unite(s) disponible(s).")),
      p(style = "font-size: 12px; color: #666;", icon("search"), " Tapez un SIRET, un nom d'entreprise ou une ville pour filtrer la liste."),
      selectizeInput(ns("unit_selector"), "Unite :",
        choices = NULL, multiple = FALSE, width = "100%",
        options = list(placeholder = "Rechercher par SIRET, raison sociale ou ville...", maxOptions = 1000)
      ),
      div(
        class = "info-section",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h5("Details de l'unite", style = "margin: 0;"),
          actionButton(ns("btn_edit_unit_details"), "Modifier", icon = icon("pen"), class = "btn btn-default btn-sm")
        ),
        uiOutput(ns("unit_details"))
      ),
      div(
        class = "unit-table",
        tags$table(
          class = "table table-condensed table-hover",
          tags$thead(tags$tr(tags$th("SIRET"), tags$th("Raison sociale"), tags$th("Ville"), tags$th("Code postal"), tags$th("APE"), tags$th("Statut"))),
          tags$tbody(lapply(seq_len(nrow(units)), function(i) {
            tags$tr(
              style = "cursor: pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'}); var t=this.closest('table'); if(t) t.querySelectorAll('tr').forEach(function(r){r.classList.remove('unit-highlight');}); this.classList.add('unit-highlight');",
                ns("click_unit_row"), units$id[i]
              ),
              tags$td(units$id[i]), tags$td(units$corporate_name[i]), tags$td(units$city[i]), tags$td(units$zip_code[i]),
              tags$td(units$ape[i]), tags$td(units$statut_label[i])
            )
          }))
        )
      )
    ),
    br(),
    actionButton(ns("btn_select_unit"), "Demarrer le questionnaire", class = "btn btn-primary btn-lg", width = "100%"), br(), br(),
    downloadButton(ns("btn_download_csv"), "Telecharger le CSV", class = "btn btn-success", width = "100%"), br(), br(),
    actionButton(ns("btn_change_survey"), "← Choisir une autre enquete", class = "btn btn-default"),
    actionButton(ns("btn_back_to_info"), "← Informations de l'enquete", class = "btn btn-default pull-left")
  )
}
