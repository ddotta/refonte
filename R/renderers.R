# ==============================================================================
# RENDERERS GÉNÉRIQUES POUR TOUS LES QUESTIONNAIRES
# ==============================================================================

#' Construit l'ordre de navigation des modules
build_module_order <- function(pogues, env_vars, collected_bools) {
  modules <- c()
  for (mod_name in names(pogues$modules)) {
    if (mod_name == "QUESTIONNAIRE_END") next
    modules <- c(modules, mod_name)
  }
  modules <- c(modules, "QUESTIONNAIRE_END")
  modules
}

#' Construit le menu de navigation latéral
render_nav_sidebar <- function(pogues, current_module) {
  tags$ul(
    class = "nav nav-pills nav-stacked",
    lapply(names(pogues$modules), function(mod_name) {
      mod <- pogues$modules[[mod_name]]
      if (mod_name == "QUESTIONNAIRE_END") {
        return(NULL)
      }
      active <- if (mod_name == current_module) "active" else ""
      tags$li(
        class = active,
        actionLink(paste0("nav_", mod_name),
          label = mod$label %||% mod_name,
          class = "nav-link"
        )
      )
    })
  )
}

#' Détermine le type d'input à utiliser pour une question
get_input_type <- function(q_type, var_info) {
  if (q_type == "SIMPLE") {
    if (!is.null(var_info) && var_info$type == "NUMERIC") {
      return("numeric")
    }
    if (!is.null(var_info) && var_info$type == "BOOLEAN") {
      return("checkbox")
    }
    if (!is.null(var_info) && var_info$type == "TEXT" &&
      !is.null(var_info$maxlength) && var_info$maxlength > 200) {
      return("textarea")
    }
    return("text")
  }
  if (q_type == "TEXT") {
    return("text")
  }
  if (q_type == "NUMERIC") {
    return("numeric")
  }
  if (q_type == "SINGLE_CHOICE") {
    return("radio")
  }
  if (q_type == "MULTIPLE_CHOICE") {
    return("checkbox")
  }
  if (q_type == "TABLE") {
    return("table")
  }
  if (q_type == "SUBMODULE") {
    return("submodule")
  }
  "text"
}

#' Récupère les choix depuis la CodeList référencée
get_choices_from_codelist <- function(pogues, q, env_vars_list) {
  choices <- NULL
  convert_codelist <- function(cl) {
    if (is.null(cl) || length(cl) == 0) {
      return(NULL)
    }
    vals <- names(cl)
    lbls <- unname(unlist(cl))
    lbls <- gsub('^"|"$', "", lbls)
    setNames(vals, lbls)
  }
  for (dim in q$dimensions) {
    if (!is.null(dim$code_list)) {
      choices <- convert_codelist(pogues$code_lists[[dim$code_list]])
      if (!is.null(choices)) break
    }
  }
  if (is.null(choices) && length(q$responses) > 0) {
    for (resp in q$responses) {
      if (!is.null(resp$CodeListReference)) {
        choices <- convert_codelist(pogues$code_lists[[resp$CodeListReference]])
        if (!is.null(choices)) break
      }
      if (!is.null(resp$Datatype$CodeListReference)) {
        choices <- convert_codelist(pogues$code_lists[[resp$Datatype$CodeListReference]])
        if (!is.null(choices)) break
      }
    }
  }
  if (is.null(choices)) {
    var_info <- pogues$variables[[q$name]]
    if (!is.null(var_info) && !is.na(var_info$code_list)) {
      choices <- convert_codelist(pogues$code_lists[[var_info$code_list]])
    }
  }
  choices
}

#' Rendu d'une question SIMPLE / TEXT / NUMERIC / DATE
render_simple_question <- function(q, pogues, env_vars_list, input_prefix = "") {
  q_name <- q$name
  q_label <- resolve_vtl(q$label, env_vars_list)
  var_info <- pogues$variables[[q_name]]
  input_type <- get_input_type(q$type, var_info)

  current_val <- env_vars_list[[q_name]]
  if (is.list(current_val)) current_val <- current_val[[1]]
  if (is.null(current_val) || is.na(current_val)) current_val <- ""

  input_id <- paste0(input_prefix, "q_", q_name)

  div(
    style = "margin-bottom: 20px;",
    div(class = "question-text", HTML(q_label)),
    if (input_type == "text") {
      textInput(input_id, NULL, value = current_val, width = "100%")
    } else if (input_type == "textarea") {
      textAreaInput(input_id, NULL, value = current_val, rows = 4, width = "100%")
    } else if (input_type == "numeric") {
      mini <- if (!is.null(var_info) && !is.na(var_info$min)) var_info$min else NA
      maxi <- if (!is.null(var_info) && !is.na(var_info$max)) var_info$max else NA
      num_val <- tryCatch(as.numeric(current_val), warning = function(w) NA)
      numericInput(input_id, NULL,
        value = if (!is.na(num_val)) num_val else NA,
        min = mini, max = maxi, step = 0.01, width = "200px"
      )
    } else if (input_type == "checkbox") {
      checkboxInput(input_id, NULL, value = isTRUE(as.logical(current_val)))
    } else {
      textInput(input_id, NULL, value = current_val, width = "100%")
    }
  )
}

#' Rendu d'une question SINGLE_CHOICE / MULTIPLE_CHOICE
render_choice_question <- function(q, pogues, env_vars_list, input_prefix = "") {
  q_name <- q$name
  q_label <- resolve_vtl(q$label, env_vars_list)
  input_id <- paste0(input_prefix, "q_", q_name)

  choices <- get_choices_from_codelist(pogues, q, env_vars_list)
  if (is.null(choices)) choices <- c("Oui" = "1", "Non" = "2")

  current_val <- env_vars_list[[q_name]]
  if (is.list(current_val)) current_val <- current_val[[1]]

  div(
    style = "margin-bottom: 20px;",
    div(class = "question-text", HTML(q_label)),
    if (q$type == "SINGLE_CHOICE" || q$type == "SINGLE_CHOICE") {
      radioButtons(input_id, NULL,
        choices = choices,
        selected = current_val %||% "", inline = TRUE
      )
    } else {
      tagList(lapply(names(choices), function(choice_val) {
        choice_label <- choices[[choice_val]]
        checked <- if (!is.null(current_val) && current_val == choice_val) TRUE else FALSE
        checkboxInput(paste0(input_id, "_", choice_val), choice_label, value = checked)
      }))
    }
  )
}

#' Récupère la valeur d'une variable (tableau REM indexé par ligne)
get_variable_value <- function(var_name, row_idx, env_vars_list) {
  val <- env_vars_list[[var_name]]
  if (is.null(val)) {
    return("")
  }
  if (is.list(val)) {
    if (row_idx <= length(val)) {
      v <- val[[row_idx]]
      if (is.null(v) || is.na(v)) {
        return("")
      }
      return(as.character(v))
    }
    return("")
  }
  if (is.na(val)) {
    return("")
  }
  return(as.character(val))
}

#' Nom de la variable interne utilisée pour stocker une correction utilisateur
#' apportée à une valeur N-1 (édition activée via le widget "Éditer les
#' données N-1 ?"). Distinct du nom de variable N pour ne jamais écraser la
#' donnée de l'année en cours, tout en étant sauvegardé de façon identique
#' (même mécanisme save_response / cell_action / table_modifications).
n1_variable_name <- function(var_name) paste0("N1__", var_name)

#' Récupère la valeur N-1 pour une variable et une ligne donnée
get_n1_value <- function(n1_data, var_name, row_idx) {
  if (is.null(n1_data)) {
    return(NULL)
  }
  val <- n1_data[[var_name]]
  if (is.null(val)) {
    return(NULL)
  }
  # Les données N-1 sont des vecteurs ou des listes (tableaux)
  if (is.list(val) || length(val) > 1) {
    if (row_idx <= length(val)) {
      v <- val[[row_idx]]
      if (is.null(v) || is.na(v)) {
        return(NULL)
      }
      return(v)
    }
    return(NULL)
  }
  # Scalaire : retourner uniquement pour la ligne 1
  if (row_idx == 1) {
    return(val)
  }
  return(NULL)
}

#' Formate une valeur numérique avec séparateurs de milliers
format_numeric <- function(val) {
  if (is.null(val) || is.na(val)) {
    return("")
  }
  num <- tryCatch(as.numeric(val), warning = function(w) NA)
  if (is.na(num)) {
    return(as.character(val))
  }
  format(num, big.mark = " ", scientific = FALSE, trim = TRUE)
}

#' Compare une valeur courante à sa valeur d'origine (import), pour savoir si
#' une cellule a été corrigée par l'utilisateur. Normalise les nombres
#' (espaces, virgule/point) pour éviter les faux positifs de formatage.
cell_value_differs <- function(current, original, is_numeric) {
  a <- if (is.null(current)) "" else as.character(current)
  b <- if (is.null(original)) "" else as.character(original)
  if (is.na(a)) a <- ""
  if (is.na(b)) b <- ""
  if (is_numeric) {
    na_ <- suppressWarnings(as.numeric(gsub(",", ".", gsub("[[:space:]]", "", a))))
    nb_ <- suppressWarnings(as.numeric(gsub(",", ".", gsub("[[:space:]]", "", b))))
    if (is.na(na_) && is.na(nb_)) {
      return(FALSE)
    }
    if (is.na(na_) || is.na(nb_)) {
      return(TRUE)
    }
    return(na_ != nb_)
  }
  trimws(a) != trimws(b)
}

# ==============================================================================
# RENDU DES QUESTIONS TABLE
# ==============================================================================

#' Extrait la dimension PRIMARY et les dimensions MEASURE d'une question TABLE
get_table_dims <- function(q) {
  primary_dim <- NULL
  measure_dims <- list()
  for (d in q$dimensions) {
    if (d$type == "PRIMARY") primary_dim <- d
    if (d$type == "MEASURE") measure_dims <- append(measure_dims, list(d))
  }
  list(primary_dim = primary_dim, measure_dims = measure_dims)
}

#' Nombre de lignes "calculé" (EXTERNAL > size > codelist > défaut), sans tenir
#' compte d'un éventuel ajout/suppression manuel de ligne par l'utilisateur.
get_table_base_n_rows <- function(q, pogues, env_vars_list) {
  dims <- get_table_dims(q)
  primary_dim <- dims$primary_dim
  n_rows <- 3
  size_var_name <- NULL
  if (!is.null(primary_dim$size)) size_var_name <- primary_dim$size
  if (is.null(size_var_name)) {
    ext_candidate <- paste0("NBLIGNES_TAB_", q$name)
    if (!is.null(env_vars_list[[ext_candidate]])) {
      size_var_name <- ext_candidate
    } else {
      calc_candidate <- paste0("CALC_NBLIGNES_TAB_", q$name)
      if (!is.null(env_vars_list[[calc_candidate]])) size_var_name <- calc_candidate
    }
  }
  if (!is.null(size_var_name)) {
    size_val <- env_vars_list[[size_var_name]]
    if (!is.null(size_val)) n_rows <- as.numeric(size_val) else n_rows <- as.numeric(size_var_name)
  } else if (!is.null(primary_dim$code_list)) {
    cl <- pogues$code_lists[[primary_dim$code_list]]
    if (!is.null(cl)) n_rows <- length(cl)
  }
  if (is.na(n_rows) || n_rows < 1) n_rows <- 1
  n_rows
}

#' Nom de la variable (interne, sauvegardée comme une réponse) qui stocke le
#' nombre de lignes effectif d'une question TABLE, après ajout/suppression
#' manuel par l'utilisateur via les boutons "Ajouter une ligne" / "Supprimer".
table_row_count_var <- function(q_name) paste0("_NROWS_", q_name)

#' Nombre de lignes effectif : la valeur "manuelle" (si l'utilisateur a ajouté
#' ou supprimé une ligne) prévaut sur le nombre calculé.
get_table_n_rows <- function(q, pogues, env_vars_list) {
  base_n <- get_table_base_n_rows(q, pogues, env_vars_list)
  override <- env_vars_list[[table_row_count_var(q$name)]]
  if (!is.null(override) && !is.na(suppressWarnings(as.numeric(override)))) {
    n <- as.numeric(override)
    if (n >= 1) {
      return(n)
    }
  }
  base_n
}

render_table_question <- function(q, pogues, env_vars_list, input_prefix = "", original_vars_list = list(),
                                   n1_edit_enabled = FALSE) {
  q_name <- q$name
  q_label <- resolve_vtl(q$label, env_vars_list)

  # Nom (avec namespace) de l'input caché utilisé pour faire remonter les
  # actions "valider" / "rétablir" faites sur une cellule (cf. editable-table.js).
  # Une seule action-input pour toute l'appli : le payload transporte var/ligne/colonne.
  domain <- getDefaultReactiveDomain()
  cell_action_input <- if (!is.null(domain) && !is.null(domain$ns)) domain$ns("cell_action") else "cell_action"
  row_action_input <- if (!is.null(domain) && !is.null(domain$ns)) domain$ns("row_action") else "row_action"

  dims <- get_table_dims(q)
  primary_dim <- dims$primary_dim
  measure_dims <- dims$measure_dims

  if (is.null(primary_dim) || length(measure_dims) == 0) {
    return(div("Question tableau mal configurée: ", q_name))
  }

  n_rows <- get_table_n_rows(q, pogues, env_vars_list)

  # Labels des colonnes
  col_labels <- sapply(measure_dims, function(d) {
    lbl <- d$label
    if (!is.null(lbl)) resolve_vtl(paste(lbl, collapse = ""), env_vars_list) else ""
  })

  # Labels des lignes
  row_labels <- NULL
  if (!is.null(primary_dim$code_list)) {
    cl <- pogues$code_lists[[primary_dim$code_list]]
    if (!is.null(cl)) row_labels <- unname(unlist(cl))
  }
  if (is.null(row_labels)) row_labels <- paste("Ligne", 1:n_rows)

  # Noms de variable par colonne
  get_var_name <- function(row_idx, col_idx) {
    key <- paste(row_idx, col_idx)
    if (!is.null(q$var_mapping) && length(q$var_mapping) > 0) {
      vn <- q$var_mapping[[key]]
      if (!is.null(vn)) {
        return(vn)
      }
    }
    if (col_idx <= length(measure_dims)) {
      dim <- measure_dims[[col_idx]]
      if (!is.null(dim$name)) {
        return(dim$name)
      }
    }
    paste0(q_name, col_idx)
  }

  col_var_names <- sapply(seq_along(measure_dims), function(col_idx) get_var_name(1, col_idx))

  # Données N-1
  n1_data <- env_vars_list[["_N1_DATA_"]]
  has_n1 <- !is.null(n1_data) && length(n1_data) > 0

  div(
    style = "margin-bottom: 30px; overflow-x: auto;",

    # Déclarations/help
    lapply(q$declarations, function(d) {
      txt <- resolve_vtl(d$text, env_vars_list)
      if (txt != "") div(class = "help-text", HTML(gsub("\r\n", "<br>", txt)))
    }),
    div(class = "question-text", q_label),
    tags$table(
      class = "table table-bordered production-table editable-table",
      tags$thead(tags$tr(
        lapply(col_labels, function(lbl) tags$th(HTML(lbl))),
        tags$th("")
      )),
      tags$tbody(lapply(1:n_rows, function(row_idx) {
        col_cells <- lapply(seq_along(col_labels), function(col_idx) {
            input_id <- paste0(input_prefix, "tab_", q_name, "_", row_idx, "_", col_idx)
            var_name <- get_var_name(row_idx, col_idx)
            var_info <- pogues$variables[[var_name]]
            current_val <- get_variable_value(var_name, row_idx, env_vars_list)

            is_numeric <- (!is.null(var_info) && var_info$type %in% c("NUMERIC", "INTEGER")) ||
              grepl("^[0-9]+(\\.[0-9]+)?$", current_val)

            # Valeur N-1 pour cette cellule
            n1_val <- NULL
            if (has_n1) n1_val <- get_n1_value(n1_data, var_name, row_idx)

            # Infos pour la cellule N-1 éditable (si le widget "Éditer les
            # données N-1 ?" est activé). Le fonctionnement est identique à
            # celui de la cellule N : nom de variable dédié (n1_variable_name),
            # valeur d'origine = donnée N-1 importée, badge/reset/validate.
            n1_var_name <- n1_variable_name(var_name)
            n1_original_val <- if (!is.null(n1_val)) as.character(n1_val) else ""
            n1_override <- if (has_n1) get_variable_value(n1_var_name, row_idx, env_vars_list) else ""
            n1_current_val <- if (nzchar(n1_override)) n1_override else n1_original_val
            n1_is_corrected <- FALSE
            try(
              {
                n1_is_corrected <- nchar(n1_original_val) > 0 &&
                  cell_value_differs(n1_current_val, n1_original_val, is_numeric)
              },
              silent = TRUE
            )
            n1_input_id <- paste0(input_prefix, "tabn1_", q_name, "_", row_idx, "_", col_idx)
            n1_indicators <- div(
              class = "cell-indicators",
              span(class = "modified-indicator", style = if (n1_is_corrected) "" else "display:none;", "C"),
              tags$a(
                href = "#", class = "reset-btn", style = if (n1_is_corrected) "" else "display:none;",
                title = "Rétablir la valeur N-1 initiale", tags$i(class = "fa fa-undo")
              ),
              tags$a(
                href = "#", class = "validate-btn", style = "display:none;",
                title = "Valider la modification N-1", tags$i(class = "fa fa-check")
              )
            )

            # Valeur d'origine (import), figée dès le chargement de l'unité :
            # sert à savoir si la cellule a été corrigée et à la restaurer.
            original_val <- get_variable_value(var_name, row_idx, original_vars_list)
            # calculer is_corrected en mode robuste pour éviter erreurs d'évaluation
            is_corrected <- FALSE
            try(
              {
                is_corrected <- nchar(original_val) > 0 && cell_value_differs(current_val, original_val, is_numeric)
              },
              silent = TRUE
            )

            # Indicateurs communs (numérique et caractère) : badge "C" + bouton
            # de restauration, affichés seulement si la cellule a déjà été
            # validée avec une valeur différente de l'import.
            indicators <- div(
              class = "cell-indicators",
              span(class = "modified-indicator", style = if (is_corrected) "" else "display:none;", "C"),
              tags$a(
                href = "#", class = "reset-btn", style = if (is_corrected) "" else "display:none;",
                title = "Rétablir la valeur initiale", tags$i(class = "fa fa-undo")
              ),
              tags$a(
                href = "#", class = "validate-btn", style = "display:none;",
                title = "Valider la modification", tags$i(class = "fa fa-check")
              )
            )

            if (is_numeric) {
              unit_label <- if (!is.null(var_info) && !is.na(var_info$unit)) {
                gsub('"', "", resolve_vtl(var_info$unit, env_vars_list))
              } else {
                ""
              }
              num_val <- tryCatch(as.numeric(gsub("[^0-9.,]", "", current_val)), warning = function(w) NA)
              raw_val <- if (!is.na(num_val)) as.character(num_val) else ""
              formatted_val <- if (!is.na(num_val)) format_numeric(num_val) else ""
              
              n1_num_val <- tryCatch(as.numeric(gsub("[^0-9.,]", "", n1_current_val)), warning = function(w) NA)
              n1_formatted_val <- if (!is.na(n1_num_val)) format_numeric(n1_num_val) else n1_current_val

              n1_td <- if (!has_n1) {
                NULL
              } else if (n1_edit_enabled) {
                tags$td(
                  class = paste("numeric-cell n1-cell", if (n1_is_corrected) "modified" else ""),
                  `data-original` = n1_original_val,
                  `data-saved` = n1_current_val,
                  `data-var` = n1_var_name,
                  `data-qname` = paste0("n1_", q_name),
                  `data-row` = row_idx,
                  `data-col` = col_idx,
                  `data-action-input` = cell_action_input,
                  div(
                    class = "cell-content",
                    textInput(n1_input_id, NULL, value = n1_formatted_val, width = "100%"),
                    n1_indicators
                  )
                )
              } else {
                tags$td(
                  class = "n1-value-cell",
                  if (!is.null(n1_val)) div(class = "n1-value", format_numeric(n1_val))
                )
              }

              list(
                main = tags$td(
                  class = paste("numeric-cell", if (is_corrected) "modified" else ""),
                  `data-original` = original_val,
                  `data-saved` = raw_val,
                  `data-var` = var_name,
                  `data-qname` = q_name,
                  `data-row` = row_idx,
                  `data-col` = col_idx,
                  `data-action-input` = cell_action_input,
                  div(
                    class = "cell-content",
                    textInput(input_id, NULL, value = formatted_val, width = "100%"),
                    indicators
                  ),
                  if (unit_label != "") div(class = "cell-unit", unit_label)
                ),
                n1 = n1_td
              )
            } else {
              n1_td <- if (!has_n1) {
                NULL
              } else if (n1_edit_enabled) {
                tags$td(
                  class = paste("text-cell n1-cell", if (n1_is_corrected) "modified" else ""),
                  `data-original` = n1_original_val,
                  `data-saved` = n1_current_val,
                  `data-var` = n1_var_name,
                  `data-qname` = paste0("n1_", q_name),
                  `data-row` = row_idx,
                  `data-col` = col_idx,
                  `data-action-input` = cell_action_input,
                  div(
                    class = "cell-content",
                    textInput(n1_input_id, NULL, value = n1_current_val, width = "150px"),
                    n1_indicators
                  )
                )
              } else {
                tags$td(
                  class = "n1-value-cell",
                  if (!is.null(n1_val)) div(class = "n1-value", as.character(n1_val))
                )
              }

              list(
                main = tags$td(
                  class = paste("text-cell", if (is_corrected) "modified" else ""),
                  `data-original` = original_val,
                  `data-saved` = current_val,
                  `data-var` = var_name,
                  `data-qname` = q_name,
                  `data-row` = row_idx,
                  `data-col` = col_idx,
                  `data-action-input` = cell_action_input,
                  div(
                    class = "cell-content",
                    textInput(input_id, NULL, value = current_val, width = "150px"),
                    indicators
                  )
                ),
                n1 = n1_td
              )
            }
          })

          # Ligne N : une <td> par colonne + la colonne d'actions (suppression
          # de ligne). Cette <td> d'actions couvre aussi la ligne N-1 juste en
          # dessous (rowspan) : les deux lignes forment une seule "ligne" de
          # données du point de vue de l'utilisateur.
          main_row <- tags$tr(
            lapply(col_cells, function(cc) cc$main),
            tags$td(
              class = "row-actions",
              rowspan = if (has_n1) "2" else NULL,
              if (n_rows > 1) {
                tags$a(
                  href = "#", class = "delete-row-btn", title = "Supprimer cette ligne",
                  onclick = sprintf(
                    "if(confirm('Supprimer cette ligne ?')){Shiny.setInputValue('%s',{question:'%s',action:'delete',row:%d,ts:Date.now()},{priority:'event'});} return false;",
                    row_action_input, q_name, row_idx
                  ),
                  tags$i(class = "fa fa-trash")
                )
              }
            )
          )

          # Ligne N-1 : affichée juste en dessous de la ligne N (pas sur la
          # même ligne de tableau), éditable ou en lecture seule selon le widget.
          n1_row <- if (has_n1) {
            tags$tr(
              class = "n1-row",
              lapply(col_cells, function(cc) cc$n1)
            )
          }

          tagList(main_row, n1_row)
      }))
    ),
    div(
      style = "margin-top: 8px;",
      tags$a(
        href = "#", class = "btn btn-default btn-sm add-row-btn",
        onclick = sprintf(
          "Shiny.setInputValue('%s',{question:'%s',action:'add',ts:Date.now()},{priority:'event'}); return false;",
          row_action_input, q_name
        ),
        tags$i(class = "fa fa-plus"), " Ajouter une ligne"
      ),
      HTML("&nbsp;"),
      tags$button(
        type = "button", class = "btn btn-success btn-sm save-table-btn",
        style = "font-weight: 600; padding: 6px 16px; font-size: 14px;",
        `data-input-id` = sprintf("%s", if (!is.null(domain$ns)) domain$ns("table_modifications") else "table_modifications"),
        tags$i(class = "fa fa-floppy-o"), " Enregistrer les modifications"
      )
    ),

    # Contrôles - filtrer les messages superflus (type "Merci de saisir")
    if (length(q$controls) > 0) {
      lapply(q$controls, function(ctl) {
        msg <- resolve_vtl(ctl$fail_message, env_vars_list)
        if (msg != "" && !grepl("Merci de saisir|merci de saisir", msg)) {
          css <- if (ctl$criticity == "ERROR") "control-error" else "control-warn"
          div(class = css, HTML(gsub("\r\n", "<br>", msg)))
        }
      })
    }
  )
}

#' Indique si le module courant contient au moins une question TABLE pour
#' laquelle des données N-1 sont disponibles (sert à n'afficher le widget
#' "Éditer les données N-1 ?" que sur les pages concernées).
module_has_n1_data <- function(module_name, pogues, env_vars_list) {
  n1_data <- env_vars_list[["_N1_DATA_"]]
  if (is.null(n1_data) || length(n1_data) == 0) {
    return(FALSE)
  }
  mod <- pogues$modules[[module_name]]
  if (is.null(mod)) {
    return(FALSE)
  }
  any(vapply(names(mod$questions), function(q_name) {
    q <- mod$questions[[q_name]]
    input_type <- get_input_type(q$type, pogues$variables[[q_name]])
    if (input_type != "table") {
      return(FALSE)
    }
    dims <- get_table_dims(q)
    if (is.null(dims$primary_dim) || length(dims$measure_dims) == 0) {
      return(FALSE)
    }
    any(vapply(seq_along(dims$measure_dims), function(col_idx) {
      dim <- dims$measure_dims[[col_idx]]
      vn <- if (!is.null(dim$name)) dim$name else paste0(q_name, col_idx)
      !is.null(n1_data[[vn]])
    }, logical(1)))
  }, logical(1)))
}

#' Rendu d'un module complet
render_module <- function(module_name, pogues, env_vars_list, input_prefix = "", original_vars_list = list(),
                           n1_edit_enabled = FALSE) {
  mod <- pogues$modules[[module_name]]
  if (is.null(mod)) {
    return(h3("Module non trouvé : ", module_name))
  }
  tagList(lapply(names(mod$questions), function(q_name) {
    q <- mod$questions[[q_name]]
    input_type <- get_input_type(q$type, pogues$variables[[q_name]])
    if (input_type == "table") {
      render_table_question(q, pogues, env_vars_list, input_prefix, original_vars_list, n1_edit_enabled)
    } else if (input_type %in% c("radio", "checkbox")) {
      render_choice_question(q, pogues, env_vars_list, input_prefix)
    } else if (input_type == "submodule") {
      NULL
    } else {
      render_simple_question(q, pogues, env_vars_list, input_prefix)
    }
  }))
}

#' Module FIN
render_fin_module <- function(eid) {
  fluidRow(column(12,
    align = "center", style = "padding: 50px 0;",
    div(style = "font-size: 48px;", icon("check-circle", class = "text-success")),
    h3("Questionnaire terminé", style = "color: #2c3e50;"),
    p("Merci d'avoir répondu à l'enquête.", style = "font-size: 16px; color: #555;"),
    p(paste0("Référence : ", eid), style = "color: #777;"), br(),
    actionButton("btn_submit", "Soumettre le questionnaire", class = "btn btn-success btn-lg")
  ))
}
