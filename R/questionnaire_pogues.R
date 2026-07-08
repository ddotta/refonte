# ... en-tête inchangé ...

# Chargement questionnaire
observe({
  name <- survey_name()
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
  # format précédemment utilisé : <p$name>_<uid>_<timestamp>
  # on cherche dans la table reponses pour voir si un enquete_id correspondant existe
  con <- dbConnect(RSQLite::SQLite(), db)
  pref <- paste0(p$name, "_", uid, "_%")
  existing <- tryCatch(
    {
      dbGetQuery(con, "SELECT DISTINCT enquete_id FROM reponses WHERE enquete_id LIKE :pref LIMIT 1",
        params = list(pref = pref)
      )
    },
    error = function(e) {
      data.frame(enquete_id = character(0), stringsAsFactors = FALSE)
    }
  )
  dbDisconnect(con)

  if (nrow(existing) > 0 && nzchar(existing$enquete_id[1])) {
    # réutiliser l'enquete existante pour récupérer les réponses sauvegardées
    eid <- existing$enquete_id[1]
  } else {
    # pas d'enquete existante : créer un nouvel id (avec timestamp)
    eid <- paste0(p$name, "_", uid, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  enquete_id(eid)
  create_enquete(db, eid, p$questionnaire_id, p$name)

  unit_data <- load_unit_data(name, uid)
  # Snapshot des valeurs d'origine AVANT toute sauvegarde/écrasement
  original_vars(unit_data)
  for (var_name in names(unit_data)) {
    if (var_name == "_N1_DATA_") {
      env_vars[[var_name]] <- unit_data[[var_name]]
      next
    }
    val <- unit_data[[var_name]]
    if (is.list(val)) {
      env_vars[[var_name]] <- val
      for (i in seq_along(val)) {
        v <- val[[i]]
        # v peut être NULL, NA, scalaire ou vecteur ; seul le cas scalaire non-NA est sauvegardé
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
  mods <- build_module_order(p, reactiveValuesToList(env_vars), list())
  if (length(mods) > 0) current_module(mods[1]) else current_module(NULL)
})
# ... reste du fichier inchangé ...
