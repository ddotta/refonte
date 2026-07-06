# ==============================================================================
# IMPORT DES DONNÉES D'ENQUÊTE
# ==============================================================================

library(tools)
library(jsonlite)
library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ==============================================================================
# MAPPE DES STATUTS D'INTERROGATION
# ==============================================================================
map_event_type <- function(event_type) {
  mapping <- c(
    "INITLA"     = "À expertiser", "PARTIELINT" = "En cours de saisie",
    "PARTIELPAP" = "En cours de saisie papier", "RECUPAP" = "Saisie papier récupérée",
    "APUR"       = "En cours d'apurement", "VALID" = "Validé",
    "EXPERT"     = "En cours d'expertise", "ONGEXPERT" = "En cours d'expertise en ligne",
    "HC"         = "Hors champ", "NPAI" = "NPAI", "REFUSAL" = "Refus", "UNUSABLE" = "Inexploitable"
  )
  if (is.null(event_type) || length(event_type) == 0) return(character(0))
  result <- ifelse(event_type %in% names(mapping), mapping[event_type], event_type)
  result[is.na(event_type) | event_type == "" | event_type == " "] <- "À expertiser"
  unname(result)
}

# ==============================================================================
# CHARGEMENT REM (interrogations.json) - DONNÉES COLLECTED + EXTERNAL
# ==============================================================================
load_unit_data_from_rem <- function(survey_name, unit_id) {
  if (is.null(survey_name) || is.null(unit_id)) return(list())
  path <- file.path("..", survey_name, "insee", "REM", "interrogations.json")
  if (!file.exists(path)) return(list())
  data <- fromJSON(path, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  result <- list()
  for (unit in data) {
    uid <- if (!is.null(unit$displayName)) unit$displayName else if (!is.null(unit$originId)) unit$originId else ""
    if (uid != unit_id) next
    if (is.null(unit$questionnaires) || length(unit$questionnaires) == 0) next
    for (q in unit$questionnaires) {
      if (is.null(q$questionningData)) next
      qd <- q$questionningData
      if (!is.null(qd$EXTERNAL)) {
        for (key in names(qd$EXTERNAL)) { val <- qd$EXTERNAL[[key]]; if (!is.null(val)) result[[key]] <- as.character(val) }
      }
      if (!is.null(qd$COLLECTED)) {
        for (key in names(qd$COLLECTED)) {
          item <- qd$COLLECTED[[key]]
          if (!is.null(item$COLLECTED)) {
            collected <- item$COLLECTED
            if (is.list(collected)) {
              vals <- list()
              for (i in seq_along(collected)) { val <- collected[[i]]; vals[[i]] <- if (!is.null(val)) as.character(val) else NA }
              result[[key]] <- vals
            } else if (length(collected) == 1) { result[[key]] <- as.character(collected) }
          } else if (!is.null(item) && !is.list(item)) { result[[key]] <- as.character(item) }
        }
      }
    }
    break
  }
  return(result)
}

# ==============================================================================
# CHARGEMENT N-1 (edited_previous_EAL.json)
# ==============================================================================
load_n1_data <- function(survey_name, unit_id) {
  if (is.null(survey_name) || is.null(unit_id)) return(list())
  path <- file.path("..", survey_name, "insee", "reprise", "historique_external", "edited_previous_EAL.json")
  if (!file.exists(path)) return(list())
  data <- fromJSON(path, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  previous <- data$editedPrevious
  if (is.null(previous) || length(previous) == 0) return(list())
  
  # Récupérer les départements VNB1 de l'unité courante
  # pour trouver l'entrée N-1 correspondante
  rem_path <- file.path("..", survey_name, "insee", "REM", "interrogations.json")
  if (!file.exists(rem_path)) return(list())
  rem_data <- fromJSON(rem_path, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  
  # Chercher les données VNB1 de l'unité courante
  current_vnb1 <- NULL
  for (unit in rem_data) {
    uid <- if (!is.null(unit$displayName)) unit$displayName else if (!is.null(unit$originId)) unit$originId else ""
    if (uid == unit_id) {
      if (!is.null(unit$questionnaires) && length(unit$questionnaires) > 0) {
        qd <- unit$questionnaires[[1]]$questionningData
        if (!is.null(qd$COLLECTED$VNB1$COLLECTED)) {
          current_vnb1 <- qd$COLLECTED$VNB1$COLLECTED
        }
      }
      break
    }
  }
  
  if (is.null(current_vnb1)) return(list())
  
  # Normaliser les départements pour la comparaison
  current_depts <- sort(unique(as.character(unlist(current_vnb1))))
  
  # Chercher dans les N-1 l'entrée qui a les mêmes départements VNB1
  best_match <- NULL
  best_match_score <- -1
  
  for (entry in previous) {
    if (!is.null(entry$VNB1)) {
      n1_vnb1 <- unlist(entry$VNB1)
      n1_depts <- sort(unique(as.character(n1_vnb1[!is.na(n1_vnb1)])))
      # Calculer le score de correspondance
      if (length(current_depts) == length(n1_depts)) {
        if (all(current_depts == n1_depts)) {
          # Correspondance exacte
          best_match <- entry
          best_match_score <- length(current_depts)
          break
        }
      }
    }
  }
  
  if (is.null(best_match)) {
    message("Aucune donnee N-1 trouvee pour ", unit_id)
    return(list())
  }
  
  n1 <- list()
  for (vname in names(best_match)) {
    if (vname == "interrogationId" || vname == "ANNEE_DONNEES" ||
        vname == "COM_GEST" || vname == "COM_GEST2" ||
        grepl("^FILTRE", vname) || vname == "ESTIM") next
    val <- best_match[[vname]]
    if (!is.null(val)) {
      n1[[vname]] <- val
    }
  }
  message("Donnees N-1 trouvees pour ", unit_id, " (", length(n1), " variables)")
  return(n1)
}

# ==============================================================================
# FONCTIONS CSV
# ==============================================================================
list_data_files <- function(survey_name) {
  if (is.null(survey_name) || survey_name == "") return(list())
  base <- if (survey_name == "PrixGrumes") file.path("..", "PrixGrumes", "data")
          else if (survey_name == "EAL") file.path("..", "EAL", "data") else return(list())
  if (!dir.exists(base)) return(list())
  data_dirs <- list.files(base, pattern = "^[0-9_]+$", full.names = TRUE)
  if (length(data_dirs) == 0) return(list())
  latest_dir <- sort(data_dirs, decreasing = TRUE)[1]
  csv_files <- list.files(latest_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) return(list())
  result <- list()
  for (f in csv_files) { name <- tools::file_path_sans_ext(basename(f)); result[[name]] <- f }
  result
}

load_csv_data <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  data <- read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", " ", "NA"))
  if (nrow(data) == 0) return(NULL)
  data <- data[, !grepl("_STATE$", names(data))]; data <- data[, !grepl("^FILTER_RESULT_", names(data))]
  meta_cols <- c("interrogationId", "validationDate", "questionnaireState")
  data <- data[, !(names(data) %in% meta_cols)]
  data
}

load_interrogation_statuses <- function(survey_name) {
  if (is.null(survey_name)) return(NULL)
  interog_file <- file.path("..", survey_name, paste0("interrogations_", survey_name, ".csv"))
  if (!file.exists(interog_file)) interog_file <- file.path("..", survey_name, "insee", "REM", "interrogations.csv")
  if (!file.exists(interog_file)) {
    parent <- file.path("..", survey_name); files <- list.files(parent, pattern = "^interrogations_.*\\.csv$", full.names = TRUE)
    if (length(files) > 0) interog_file <- files[1] else return(NULL)
  }
  data <- tryCatch({
    d <- read.csv(interog_file, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", " ", "NA"))
    if (ncol(d) < 2) d <- read.csv2(interog_file, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", " ", "NA"))
    d
  }, error = function(e) NULL)
  if (is.null(data) || nrow(data) == 0) return(NULL)
  names(data) <- gsub(" ", "", names(data))
  if (!"surveyUnitId" %in% names(data)) { if ("usualSurveyUnitId" %in% names(data)) names(data)[names(data) == "usualSurveyUnitId"] <- "surveyUnitId" else return(NULL) }
  if ("highestEventType" %in% names(data)) data$statut_label <- sapply(data$highestEventType, map_event_type)
  else { data$statut_label <- "À expertiser"; data$highestEventType <- NA }
  if (!"isOnProbation" %in% names(data)) data$isOnProbation <- FALSE
  data
}

get_unit_status <- function(statuses, unit_id) {
  if (is.null(statuses) || is.null(unit_id)) return(list(event_type = NA, statut_label = "À expertiser", isOnProbation = FALSE))
  row <- statuses[statuses$surveyUnitId == unit_id, ]
  if (nrow(row) == 0) return(list(event_type = NA, statut_label = "À expertiser", isOnProbation = FALSE))
  list(event_type = if (!is.null(row$highestEventType)) row$highestEventType[1] else NA,
       statut_label = if (!is.null(row$statut_label)) row$statut_label[1] else "À expertiser",
       isOnProbation = if (!is.null(row$isOnProbation)) isTRUE(row$isOnProbation[1]) else FALSE)
}

# ==============================================================================
# CHARGEMENT UNITÉS DEPUIS REM AVEC ADRESSE COMPLÈTE + CONTACT
# ==============================================================================
load_survey_units_from_rem <- function(survey_name) {
  if (is.null(survey_name)) return(NULL)
  path <- file.path("..", survey_name, "insee", "REM", "interrogations.json")
  if (!file.exists(path)) return(NULL)
  data <- fromJSON(path, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  statuses <- load_interrogation_statuses(survey_name)
  
  units <- data.frame(id = character(), statut = character(), statut_label = character(),
    is_on_probation = logical(), corporate_name = character(), ape = character(),
    street_number = character(), street_type = character(), street_name = character(),
    address_supplement = character(), city = character(), zip_code = character(),
    cedex_code = character(), cedex_name = character(), country = character(),
    email = character(), contact_name = character(), contact_tel = character(),
    contact_email = character(), contact_function = character(),
    stringsAsFactors = FALSE)
  
  for (unit in data) {
    unit_id <- if (!is.null(unit$displayName)) unit$displayName else if (!is.null(unit$originId)) unit$originId else ""
    corp_name <- if (!is.null(unit$corporateName)) unit$corporateName else ""
    ape_code <- if (!is.null(unit$ape)) unit$ape else ""
    
    addr <- unit$address
    street_number <- if (!is.null(addr$streetNumber)) addr$streetNumber else ""
    street_type <- if (!is.null(addr$streetType)) addr$streetType else ""
    street_name <- if (!is.null(addr$streetName)) addr$streetName else ""
    addr_supp <- if (!is.null(addr$addressSupplement)) addr$addressSupplement else ""
    city <- if (!is.null(addr$cityName)) addr$cityName else ""
    zip_code <- if (!is.null(addr$zipCode)) addr$zipCode else ""
    cedex_code <- if (!is.null(addr$cedexCode)) addr$cedexCode else ""
    cedex_name <- if (!is.null(addr$cedexName)) addr$cedexName else ""
    country <- if (!is.null(addr$countryName)) addr$countryName else "France"
    
    email <- ""; contact_name <- ""; contact_tel <- ""; contact_email <- ""; contact_function <- ""
    if (!is.null(unit$contacts) && length(unit$contacts) > 0) {
      c <- unit$contacts[[1]]
      contact_email <- if (!is.null(c$email)) c$email else ""
      first <- if (!is.null(c$firstName)) c$firstName else ""
      last <- if (!is.null(c$lastName)) c$lastName else ""
      contact_name <- trimws(paste(first, last))
      contact_function <- if (!is.null(c[["function"]])) c[["function"]] else ""
      if (!is.null(c$phoneNumbers) && length(c$phoneNumbers) > 0) {
        for (p in c$phoneNumbers) {
          if (!is.null(p$favorite) && p$favorite == TRUE && !is.null(p$number)) { contact_tel <- p$number; break }
        }
        if (contact_tel == "" && !is.null(c$phoneNumbers[[1]]$number)) contact_tel <- c$phoneNumbers[[1]]$number
      }
    }
    manager_email <- ""
    if (!is.null(unit$communicationPersos) && length(unit$communicationPersos) > 0) {
      for (cp in unit$communicationPersos) {
        if (!is.null(cp$extCommunicationData)) {
          for (ed in cp$extCommunicationData) {
            if (!is.null(ed$key) && ed$key == "managerEmail" && !is.null(ed$value)) manager_email <- ed$value
          }
        }
      }
    }
    if (manager_email != "") email <- manager_email
    if (contact_email == "") contact_email <- email
    
    unit_status <- get_unit_status(statuses, unit_id)
    if (nchar(unit_id) > 0) {
      units <- rbind(units, data.frame(id = unit_id, statut = unit_status$event_type %||% "A_SAISIR",
        statut_label = unit_status$statut_label, is_on_probation = unit_status$isOnProbation,
        corporate_name = corp_name, ape = ape_code,
        street_number = street_number, street_type = street_type, street_name = street_name,
        address_supplement = addr_supp, city = city, zip_code = zip_code,
        cedex_code = cedex_code, cedex_name = cedex_name, country = country,
        email = email, contact_name = contact_name, contact_tel = contact_tel,
        contact_email = contact_email, contact_function = contact_function,
        stringsAsFactors = FALSE))
    }
  }
  units <- units[!duplicated(units$id), ]; units <- units[order(units$id), ]; rownames(units) <- NULL
  units
}

list_survey_units <- function(survey_name) {
  rem_units <- load_survey_units_from_rem(survey_name)
  if (!is.null(rem_units) && nrow(rem_units) > 0) return(rem_units)
  files <- list_data_files(survey_name)
  if (length(files) == 0) return(NULL)
  racine_file <- NULL
  for (name in names(files)) { if (grepl("RACINE", name, ignore.case = TRUE)) { racine_file <- files[[name]]; break } }
  if (is.null(racine_file)) racine_file <- files[[1]]
  data <- read.csv2(racine_file, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", " ", "NA"))
  if (nrow(data) == 0) return(NULL)
  units <- data.frame(id = data$usualSurveyUnitId,
    statut = if (!is.null(data$questionnaireState)) data$questionnaireState else "INCONNU",
    statut_label = if (!is.null(data$questionnaireState)) data$questionnaireState else "Inconnu",
    is_on_probation = FALSE, corporate_name = "", ape = "",
    street_number = "", street_type = "", street_name = "", address_supplement = "",
    city = "", zip_code = "", cedex_code = "", cedex_name = "", country = "France",
    email = "", contact_name = "", contact_tel = "", contact_email = "", contact_function = "",
    stringsAsFactors = FALSE)
  units <- unique(units); units <- units[order(units$id), ]; rownames(units) <- NULL
  units
}

#' Charge les données d'une unité (REM + N-1)
load_unit_data <- function(survey_name, unit_id) {
  if (is.null(survey_name) || is.null(unit_id)) return(list())
  
  # Données courantes depuis REM
  result <- load_unit_data_from_rem(survey_name, unit_id)
  if (length(result) == 0) {
    # Fallback CSV
    files <- list_data_files(survey_name)
    if (length(files) == 0) return(list())
    result <- list()
    for (name in names(files)) {
      path <- files[[name]]; data <- load_csv_data(path)
      if (is.null(data)) next
      unit_data <- data[data$usualSurveyUnitId == unit_id, , drop = FALSE]
      if (nrow(unit_data) == 0) next
      has_module_col <- "Q_REGIONS_ESSENCES" %in% names(unit_data)
      if (has_module_col) {
        for (i in 1:nrow(unit_data)) { row <- unit_data[i, ]; mv <- as.character(row$Q_REGIONS_ESSENCES)
          suff <- if (!is.null(mv) && !is.na(mv)) gsub("-", "_", mv) else paste0("row_", i)
          for (col in names(row)) { if (col %in% c("usualSurveyUnitId", "Q_REGIONS_ESSENCES")) next; v <- as.character(row[[col]]); if (is.na(v) || v == "") next; result[[paste0(col, "_", suff)]] <- v }
        }
      } else {
        for (i in 1:nrow(unit_data)) { row <- unit_data[i, ]
          for (col in names(row)) { if (col == "usualSurveyUnitId") next; v <- as.character(row[[col]]); if (is.na(v) || v == "") next; result[[col]] <- v }
        }
      }
    }
  }
  message("Donnees chargees pour ", unit_id)
  
  # Charger les données N-1 et les stocker dans result
  n1_data <- load_n1_data(survey_name, unit_id)
  if (length(n1_data) > 0) {
    result[["_N1_DATA_"]] <- n1_data
    message("Donnees N-1 chargees pour ", unit_id, " (", length(n1_data), " variables)")
  }
  
  result
}

get_unit_info_from_rem <- function(survey_name, unit_id) {
  units <- load_survey_units_from_rem(survey_name)
  if (is.null(units)) return(NULL)
  units[units$id == unit_id, , drop = FALSE]
}

# ==============================================================================
# CONTEXTE REM
# ==============================================================================
load_survey_context <- function(survey_name) {
  if (is.null(survey_name)) return(NULL)
  context_path <- file.path("..", survey_name, "insee", "REM", "context.json")
  if (!file.exists(context_path)) return(NULL)
  fromJSON(context_path, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

extract_survey_dates <- function(context) {
  if (is.null(context)) return(NULL)
  partitions <- context$partitions; if (is.null(partitions) || length(partitions) == 0) return(NULL)
  p <- partitions[[1]]
  list(label = context$label, short_label = context$shortLabel,
    collection_start = if (!is.null(p$collectionStartDate)) p$collectionStartDate else NULL,
    collection_end = if (!is.null(p$collectionEndDate)) p$collectionEndDate else NULL,
    return_date = if (!is.null(p$returnDate)) p$returnDate else NULL,
    followup_letter1 = if (!is.null(p$followupLetter1Date)) p$followupLetter1Date else NULL,
    followup_letter2 = if (!is.null(p$followupLetter2Date)) p$followupLetter2Date else NULL,
    formal_notice = if (!is.null(p$formalNoticeDate)) p$formalNoticeDate else NULL,
    no_reply = if (!is.null(p$noReplyDate)) p$noReplyDate else NULL)
}

extract_survey_metadata <- function(context) {
  if (is.null(context)) return(NULL)
  m <- context$metadatas; if (is.null(m)) return(NULL)
  referents <- NULL
  if (!is.null(m$surveyReferents)) referents <- lapply(m$surveyReferents, function(r) paste0(r$firstName, " ", r$lastName, if (!is.null(r$telephone) && r$telephone != "") paste0(" - Tel: ", r$telephone) else ""))
  list(operation_label = if (!is.null(m$statisticalOperationSerieLabel)) m$statisticalOperationSerieLabel else NULL,
    operation_short_label = if (!is.null(m$statisticalOperationSerieShortLabel)) m$statisticalOperationSerieShortLabel else NULL,
    year = if (!is.null(m$year)) m$year else NULL, periodicity = if (!is.null(m$periodicity)) m$periodicity else NULL,
    short_objectives = if (!is.null(m$shortObjectives)) m$shortObjectives else NULL,
    visa_number = if (!is.null(m$visaNumber)) m$visaNumber else NULL, referents = referents)
}

format_iso_date <- function(iso_date) {
  if (is.null(iso_date) || iso_date == "") return("")
  date_part <- substr(iso_date, 1, 10)
  format(as.Date(date_part), "%d/%m/%Y")
}

export_interrogations_csv <- function(survey_name, output_path = NULL) {
  units <- list_survey_units(survey_name)
  if (is.null(units) || nrow(units) == 0) return(NULL)
  context <- load_survey_context(survey_name)
  partition_id <- ""
  if (!is.null(context$partitions) && length(context$partitions) > 0) partition_id <- context$partitions[[1]]$partitionId %||% ""
  export_df <- data.frame(partitioningId = partition_id, surveyUnitId = units$id,
    corporateName = units$corporate_name %||% "", ape = units$ape %||% "",
    streetNumber = units$street_number %||% "", streetType = units$street_type %||% "",
    streetName = units$street_name %||% "", addressSupplement = units$address_supplement %||% "",
    city = units$city %||% "", zipCode = units$zip_code %||% "",
    cedexCode = units$cedex_code %||% "", cedexName = units$cedex_name %||% "",
    country = units$country %||% "France",
    email = units$email %||% "",
    contactName = units$contact_name %||% "", contactTel = units$contact_tel %||% "",
    stringsAsFactors = FALSE)
  if (!is.null(output_path)) write.csv2(export_df, output_path, row.names = FALSE, fileEncoding = "UTF-8")
  export_df
}