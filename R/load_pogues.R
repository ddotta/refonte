# ==============================================================================
# CHARGEMENT DU FICHIER POGUES JSON
# ==============================================================================
# Charge et parse n'importe quel fichier Pogues JSON (PRIXGRUMES, EAL, etc.)

#' Détermine le chemin du fichier Pogues à utiliser
#' Détection auto : cherche dans ../<SURVEY>/insee/datacollection/
#' @param survey_name Nom de l'enquête (PrixGrumes, EAL, etc.)
find_pogues_file <- function(survey_name = NULL) {
  # Si un nom d'enquête est fourni
  if (!is.null(survey_name)) {
    base_path <- file.path("..", survey_name, "insee", "datacollection")
    if (dir.exists(base_path)) {
      files <- list.files(base_path, pattern = "\\.json$")
      pogues_files <- files[grepl("^pogues_", files)]
      if (length(pogues_files) > 0) {
        return(file.path(base_path, pogues_files[1]))
      }
    }
    # Fallback avec le pattern insee_<survey>
    base_path <- file.path("..", paste0("insee_", tolower(survey_name)), "datacollection")
    if (dir.exists(base_path)) {
      files <- list.files(base_path, pattern = "\\.json$")
      pogues_files <- files[grepl("^pogues_", files)]
      if (length(pogues_files) > 0) {
        return(file.path(base_path, pogues_files[1]))
      }
    }
  }
  
  # Par défaut : cherche un dossier contenant insee/datacollection/pogues_*.json
  parent <- ".."
  dirs <- list.files(parent, full.names = TRUE)
  for (d in dirs) {
    dc_path <- file.path(d, "insee", "datacollection")
    if (dir.exists(dc_path)) {
      files <- list.files(dc_path, pattern = "\\.json$")
      pogues_files <- files[grepl("^pogues_", files)]
      if (length(pogues_files) > 0) {
        return(file.path(dc_path, pogues_files[1]))
      }
    }
  }
  
  # Fallback (ancien pattern)
  dirs <- list.files(parent, pattern = "^insee_", full.names = TRUE)
  for (d in dirs) {
    dc_path <- file.path(d, "datacollection")
    if (dir.exists(dc_path)) {
      files <- list.files(dc_path, pattern = "\\.json$")
      pogues_files <- files[grepl("^pogues_", files)]
      if (length(pogues_files) > 0) {
        return(file.path(dc_path, pogues_files[1]))
      }
    }
  }
  
  # Fallback ultime
  warning("Aucun fichier Pogues trouvé, utilisation du chemin par défaut")
  file.path("..", "PrixGrumes", "insee", "datacollection", "pogues_mltnx5mv.json")
}

#' Charge et parse le fichier Pogues JSON
#' @param path Chemin vers le fichier JSON
#' @return Liste structurée contenant variables, codelists, modules, flowcontrol,
#'   ou NULL si le fichier est illisible/malformé (avec journalisation de l'erreur
#'   via `log_erreur()`, cf. R/error_handling.R)
load_pogues <- function(path = find_pogues_file()) {
  tryCatch(
    .load_pogues_impl(path),
    error = function(cond) {
      log_erreur(paste0("chargement du questionnaire Pogues '", path, "'"), cond)
      NULL
    }
  )
}

#' Implémentation interne de load_pogues() (non exportée, sans gestion d'erreur :
#' la gestion d'erreur est centralisée dans load_pogues() ci-dessus)
.load_pogues_impl <- function(path) {

  data <- fromJSON(path, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  
  # --- Variables ---
  vars_raw <- data$Variables$Variable
  variables <- list()
  for (v in vars_raw) {
    name <- v$Name
    dtype <- v$Datatype
    type_name <- if (!is.null(dtype$typeName)) dtype$typeName else "TEXT"
    unit <- if (!is.null(dtype$Unit)) dtype$Unit else NA
    min_val <- if (!is.null(dtype$Minimum)) dtype$Minimum else NA
    max_val <- if (!is.null(dtype$Maximum)) dtype$Maximum else NA
    decimals <- if (!is.null(dtype$Decimals)) dtype$Decimals else NA
    maxlength <- if (!is.null(dtype$MaxLength)) dtype$MaxLength else NA
    code_list_ref <- if (!is.null(v$CodeListReference)) v$CodeListReference else NA
    scope <- if (!is.null(v$Scope)) v$Scope else NA
    lbl <- if (!is.null(v$Label)) v$Label else name
    
    variables[[name]] <- list(
      id = v$id,
      name = name,
      type = type_name,
      label = lbl,
      unit = unit,
      min = min_val,
      max = max_val,
      decimals = decimals,
      maxlength = maxlength,
      code_list = code_list_ref,
      scope = scope
    )
  }
  
  # --- CodeLists ---
  cl_raw <- data$CodeLists$CodeList
  code_lists <- list()
  if (!is.null(cl_raw)) {
    for (cl in cl_raw) {
      cl_id <- cl$id
      codes_list <- list()
      if (!is.null(cl$Code)) {
        for (i in seq_along(cl$Code)) {
          c <- cl$Code[[i]]
          # Les codes peuvent utiliser "Value" ou "CodeValue" comme clé
          cv <- if (!is.null(c$Value)) c$Value else 
                if (!is.null(c$CodeValue)) c$CodeValue else as.character(i)
          clbl <- if (!is.null(c$Label) && length(c$Label) > 0) {
            lbl <- c$Label[[1]]
            # Nettoyer les guillemets autour des labels
            if (is.character(lbl)) lbl <- gsub('^"|"$', '', lbl)
            lbl
          } else cv
          codes_list[[cv]] <- clbl
        }
      }
      cl_name <- if (!is.null(cl$Name) && cl$Name != "") cl$Name else cl_id
      code_lists[[cl_id]] <- codes_list
      # Aussi indexer par nom si différent de l'id
      if (cl_name != cl_id) {
        code_lists[[cl_name]] <- codes_list
      }
    }
  }
  
  # --- Structure des modules (Child) ---
  modules <- list()
  for (child in data$Child) {
    if (child$genericName == "MODULE") {
      module_id <- child$id
      module_name <- child$Name
      module_label <- if (!is.null(child$Label) && length(child$Label) > 0) child$Label[[1]] else module_name
      
      questions <- list()
      if (!is.null(child$Child)) {
        for (q in child$Child) {
          q_name <- q$Name
          q_type <- if (!is.null(q$questionType)) q$questionType else 
                    if (!is.null(q$genericName)) q$genericName else "SIMPLE"
          q_label <- if (!is.null(q$Label) && length(q$Label) > 0) q$Label[[1]] else q_name
          
          # Déclarations (help text)
          declarations <- list()
          if (!is.null(q$Declaration) && length(q$Declaration) > 0) {
            for (d in q$Declaration) {
              declarations <- append(declarations, list(list(
                type = d$declarationType,
                position = d$position,
                text = d$Text
              )))
            }
          }
          
          # Contrôles
          controls <- list()
          if (!is.null(q$Control) && length(q$Control) > 0) {
            for (ctl in q$Control) {
              controls <- append(controls, list(list(
                id = ctl$id,
                criticity = ctl$criticity,
                scope = ctl$scope,
                description = ctl$Description,
                expression = ctl$Expression,
                fail_message = ctl$FailMessage
              )))
            }
          }
          
          # Response structure (pour les tableaux)
          response_struct <- q$ResponseStructure
          
          # Dimension du tableau
          dimensions <- list()
          if (!is.null(response_struct$Dimension)) {
            for (dim in response_struct$Dimension) {
              dim_info <- list(type = dim$dimensionType)
              if (!is.null(dim$Label)) {
                dim_info$label <- dim$Label[[1]]
              }
              if (!is.null(dim$CodeListReference)) {
                dim_info$code_list <- dim$CodeListReference
              }
              if (!is.null(dim$size)) {
                dim_info$size <- dim$size$value
              }
              dimensions <- append(dimensions, list(dim_info))
            }
          }
          
          # Mapping (pour lier variables aux cellules)
          mapping <- list()
          if (!is.null(response_struct$Mapping)) {
            mapping <- response_struct$Mapping
          }
          
          # Response
          responses <- list()
          if (!is.null(q$Response)) {
            responses <- q$Response
          }
          
          # Extraire les noms de variables collectées à partir des Response
          # Pour les questions TABLE, chaque Response correspond à une variable collectée
          # La CollectedVariableReference est un ID qu'il faut mapper au nom de variable
          collected_vars <- list()
          if (!is.null(q$Response)) {
            for (r in q$Response) {
              ref_id <- r$CollectedVariableReference
              if (!is.null(ref_id)) {
                # Chercher le nom de variable correspondant
                for (v_name in names(variables)) {
                  v <- variables[[v_name]]
                  if (!is.null(v) && v$id == ref_id) {
                    collected_vars[[r$id]] <- v_name
                    break
                  }
                }
                # Si pas trouvé, utiliser un nom basé sur l'ID de réponse
                if (is.null(collected_vars[[r$id]])) {
                  collected_vars[[r$id]] <- ref_id
                }
              }
            }
          }
          
          # Code filters
          code_filters <- list()
          if (!is.null(q$codeFilters)) {
            code_filters <- q$codeFilters
          }
          
          # Construire un mapping entre les cibles (MappingTarget) et les noms de variables collectées
          # Pour les tableaux, chaque mapping source (Response ID) correspond à une CollectedVariableReference
          # Ex: "1 1" → response_id "mm21i46a" → variable "VNB1"
          var_mapping <- list()
          if (length(mapping) > 0) {
            for (m_entry in mapping) {
              target <- m_entry$MappingTarget
              source <- m_entry$MappingSource
              var_name <- collected_vars[[source]]
              if (!is.null(var_name)) {
                var_mapping[[target]] <- var_name
              }
            }
          }
          
          questions[[q_name]] <- list(
            id = q$id,
            name = q_name,
            type = q_type,
            label = q_label,
            mandatory = if (!is.null(q$mandatory)) q$mandatory else FALSE,
            declarations = declarations,
            controls = controls,
            dimensions = dimensions,
            response_struct = response_struct,
            mapping = mapping,
            responses = responses,
            code_filters = code_filters,
            var_mapping = var_mapping
          )
        }
      }
      
      modules[[module_name]] <- list(
        id = module_id,
        name = module_name,
        label = module_label,
        questions = questions
      )
    }
  }
  
  # --- FlowControl global ---
  flow_control <- list()
  if (!is.null(data$FlowControl)) {
    for (fc in data$FlowControl) {
      flow_control <- append(flow_control, list(list(
        id = fc$id,
        description = fc$Description,
        expression = fc$Expression,
        if_true = fc$IfTrue
      )))
    }
  }
  
  # --- Iterations (boucles) ---
  iterations <- list()
  if (!is.null(data$Iterations$Iteration)) {
    for (it in data$Iterations$Iteration) {
      iterations[[it$id]] <- list(
        name = it$Name,
        members = it$MemberReference,
        iterable = it$IterableReference,
        filter = it$Filter
      )
    }
  }
  
  list(
    questionnaire_id = data$id,
    name = data$Name,
    label = data$Label[[1]],
    owner = data$owner,
    variables = variables,
    code_lists = code_lists,
    modules = modules,
    flow_control = flow_control,
    iterations = iterations
  )
}