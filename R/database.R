# ==============================================================================
# BASE DE DONNÉES UNIFIÉE - SUIVAL IAA (questionnaires + anomalies) + Pogues
# ==============================================================================
#
# Toutes les fonctions ci-dessous passent par with_db_connection() (cf.
# R/db_utils.R) : la connexion est systématiquement fermée, même en cas
# d'erreur, et toute erreur SQL est journalisée au lieu de faire planter
# l'application.

library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)

# ==============================================================================
# Initialisation de la base
# ==============================================================================

#' Initialise la base de données avec toutes les tables nécessaires
#' @param db_path Chemin vers le fichier SQLite
init_db <- function(db_path = cst_chemin_vers_dB) {
  with_db_connection(db_path = db_path, contexte = "initialisation de la base", valeur_par_defaut = NULL, fn = function(con) {
    dbExecute(con, "CREATE TABLE IF NOT EXISTS QUESTIONNAIRE (
      IDENTIFIANT_SUIVALIAA TEXT PRIMARY KEY,
      ENQUETE TEXT,
      CAMPAGNE TEXT,
      SIRET TEXT,
      RAISON_SOCIALE TEXT,
      GESTIONNAIRE_REF TEXT,
      PRIORITE_QUEST TEXT,
      DATE_MODIFICATION TEXT,
      DATE_VALIDATION TEXT,
      DATE_INTERNET TEXT,
      DATE_COURRIER TEXT,
      DATE_EXPORT_CAPI TEXT,
      DATE_MAJ_SUIVAL TEXT,
      COMMENT_QUEST TEXT,
      COMMENT_GEST TEXT,
      ETAT_QUEST INTEGER DEFAULT 0,
      NB_ANO_TOT INTEGER DEFAULT 0,
      NB_ANO_NON_TRAITEES INTEGER DEFAULT 0,
      NB_ANO_EN_COURS INTEGER DEFAULT 0,
      NB_ANO_EN_ATTENTE INTEGER DEFAULT 0,
      NB_ANO_CORRIGEES INTEGER DEFAULT 0,
      NB_ANO_FORCEES INTEGER DEFAULT 0
    )")

    dbExecute(con, "CREATE TABLE IF NOT EXISTS ANOMALIES (
      IDENTIFIANT_SUIVALIAA TEXT,
      CODE_ANOMALIE TEXT,
      LIB_ANOMALIE TEXT,
      ID_LIGNE_SR TEXT,
      LIGNE_ANOMALIE TEXT,
      TYPE_ANOMALIE TEXT,
      PRIORITE_ANOMALIE TEXT,
      ECRAN TEXT,
      VARIABLES TEXT,
      INFOS_COMP TEXT,
      VAGUE TEXT,
      DATE_ANOMALIE TEXT,
      ETAT_ANOMALIE INTEGER DEFAULT 0,
      COMMENT_VALIDATION TEXT,
      PRIMARY KEY (IDENTIFIANT_SUIVALIAA, CODE_ANOMALIE, ID_LIGNE_SR, LIGNE_ANOMALIE)
    )")

    dbExecute(con, "CREATE TABLE IF NOT EXISTS enquetes (
      id TEXT PRIMARY KEY,
      questionnaire_id TEXT NOT NULL,
      source_name TEXT,
      statut TEXT DEFAULT 'en_cours',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")

    dbExecute(con, "CREATE TABLE IF NOT EXISTS reponses (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      questionnaire_id TEXT NOT NULL,
      enquete_id TEXT NOT NULL,
      variable_name TEXT NOT NULL,
      valeur TEXT,
      ligne INTEGER DEFAULT 1,
      colonne INTEGER DEFAULT 1,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(questionnaire_id, enquete_id, variable_name, ligne, colonne)
    )")

    invisible(TRUE)
  })
}

# ==============================================================================
# FONCTIONS SUIVAL IAA - Questionnaires
# ==============================================================================

#' Récupère tous les questionnaires
interagir_dB_recuperer_tout_les_questionnaires <- function() {
  with_db_connection(
    contexte = "récupération des questionnaires",
    valeur_par_defaut = NULL,
    fn = function(con) dbGetQuery(con, "SELECT * FROM QUESTIONNAIRE")
  )
}

#' Récupère les anomalies pour un identifiant
#' @param str_identifiant IDENTIFIANT_SUIVALIAA
interagir_dB_recuperer_anomalies <- function(str_identifiant) {
  with_db_connection(
    contexte = "récupération des anomalies",
    valeur_par_defaut = NA,
    fn = function(con) {
      dbGetQuery(con, "SELECT * FROM ANOMALIES WHERE IDENTIFIANT_SUIVALIAA = :x",
                 params = list(x = str_identifiant))
    }
  )
}

#' Met à jour l'état d'une anomalie
interagir_dB_mise_a_jour_anomalies <- function(str_idendifiant, str_code_anomalie, str_id_ligne_sr, int_etat, str_comment) {
  with_db_connection(
    contexte = "mise à jour d'une anomalie",
    valeur_par_defaut = NA,
    fn = function(con) {
      if (is.na(str_id_ligne_sr)) {
        dbExecute(con,
          "UPDATE ANOMALIES SET ETAT_ANOMALIE = :a, COMMENT_VALIDATION = :b WHERE IDENTIFIANT_SUIVALIAA == :x AND CODE_ANOMALIE == :y",
          params = list(x = str_idendifiant, y = str_code_anomalie, a = int_etat, b = str_comment))
      } else {
        dbExecute(con,
          "UPDATE ANOMALIES SET ETAT_ANOMALIE = :a, COMMENT_VALIDATION = :b WHERE IDENTIFIANT_SUIVALIAA == :x AND CODE_ANOMALIE == :y AND ID_LIGNE_SR == :z",
          params = list(x = str_idendifiant, y = str_code_anomalie, z = str_id_ligne_sr, a = int_etat, b = str_comment))
      }
    }
  )
}

#' Met à jour un questionnaire
interagir_dB_mise_a_jour_questionnaire <- function(ligne_questionnaire) {
  with_db_connection(
    contexte = "mise à jour d'un questionnaire",
    valeur_par_defaut = NA,
    fn = function(con) {
      dbExecute(con,
        "UPDATE QUESTIONNAIRE SET ETAT_QUEST = :a, NB_ANO_TOT = :b, NB_ANO_CORRIGEES = :c, NB_ANO_FORCEES = :d, NB_ANO_EN_COURS = :e, NB_ANO_EN_ATTENTE = :f, NB_ANO_NON_TRAITEES = :g, DATE_MAJ_SUIVAL = :h
         WHERE IDENTIFIANT_SUIVALIAA == :x",
        params = list(
          x = ligne_questionnaire$IDENTIFIANT_SUIVALIAA,
          a = ligne_questionnaire$ETAT_QUEST,
          b = ligne_questionnaire$NB_ANO_TOT,
          c = ligne_questionnaire$NB_ANO_CORRIGEES,
          d = ligne_questionnaire$NB_ANO_FORCEES,
          e = ligne_questionnaire$NB_ANO_EN_COURS,
          f = ligne_questionnaire$NB_ANO_EN_ATTENTE,
          g = ligne_questionnaire$NB_ANO_NON_TRAITEES,
          h = format(Sys.Date(), "%d/%m/%Y")
        ))
    }
  )
}

#' Calcule les statistiques des anomalies par enquête/campagne/vague
interargir_dB_cherche_et_calcule_les_erreurs <- function(c_enquete, c_campagne, c_vague) {
  with_db_connection(
    contexte = "calcul des statistiques d'anomalies",
    valeur_par_defaut = NA,
    fn = function(con) {
      db_questionnaire <- dbGetQuery(con, "SELECT * FROM QUESTIONNAIRE")
      db_anomalies <- dbGetQuery(con, "SELECT * FROM ANOMALIES")

      get_questionnaire <- db_questionnaire %>%
        filter(ENQUETE %in% c_enquete | is.null(c_enquete)) %>%
        filter(CAMPAGNE %in% c_campagne | is.null(c_campagne)) %>%
        select(IDENTIFIANT_SUIVALIAA, ENQUETE, CAMPAGNE)

      anomalies <- db_anomalies %>%
        right_join(get_questionnaire, by = "IDENTIFIANT_SUIVALIAA")

      if (!is.null(c_vague)) {
        anomalies <- anomalies %>% filter(VAGUE %in% c_vague)
      }

      anomalies <- anomalies %>%
        group_by(ENQUETE, CAMPAGNE, VAGUE, ETAT_ANOMALIE) %>%
        summarise(NUMBER = n(), .groups = "keep") %>%
        ungroup() %>%
        mutate(ETAT_ANOMALIE = case_when(
          ETAT_ANOMALIE == 0 ~ "NB_ANO_NON_TRAITEES",
          ETAT_ANOMALIE == 1 ~ "NB_ANO_EN_COURS",
          ETAT_ANOMALIE == 2 ~ "NB_ANO_EN_ATTENTE",
          ETAT_ANOMALIE == 3 ~ "NB_ANO_CORRIGEES",
          ETAT_ANOMALIE == 4 ~ "NB_ANO_FORCEES"
        )) %>%
        pivot_wider(names_from = ETAT_ANOMALIE, values_from = NUMBER, values_fill = 0) %>%
        collect()

      for (col in c("NB_ANO_NON_TRAITEES", "NB_ANO_EN_COURS", "NB_ANO_EN_ATTENTE",
                    "NB_ANO_CORRIGEES", "NB_ANO_FORCEES")) {
        if (col %!in% colnames(anomalies)) {
          anomalies <- anomalies %>% mutate(!!col := 0)
        }
      }

      anomalies
    }
  )
}

#' Supprime un questionnaire et ses anomalies de la base
interagir_dB_supprimer_dans_questionnaire <- function(c_identifiant) {
  with_db_connection(
    contexte = "suppression d'un questionnaire",
    valeur_par_defaut = NA,
    fn = function(con) {
      dbExecute(con, "DELETE FROM QUESTIONNAIRE WHERE IDENTIFIANT_SUIVALIAA == :x",
                params = list(x = c_identifiant))
      dbExecute(con, "DELETE FROM ANOMALIES WHERE IDENTIFIANT_SUIVALIAA == :x",
                params = list(x = c_identifiant))
      1
    }
  )
}

#' Met à jour un questionnaire dans le dataframe global (fonction pure, pas d'accès disque)
mettreAjourQuestionnaire <- function(df_questionnaire, identifiant, etat_precedent, nouvel_etat, commentaire) {
  questionnaireToUpdate <- df_questionnaire %>% filter(IDENTIFIANT_SUIVALIAA == identifiant)
  questionnaireToUpdate <- questionnaireToUpdate %>%
    mutate(
      "{etat_precedent}" := !!sym(etat_precedent) - 1,
      "{nouvel_etat}" := !!sym(nouvel_etat) + 1,
      ETAT_QUEST = case_when(
        NB_ANO_TOT == NB_ANO_NON_TRAITEES ~ 0,
        NB_ANO_TOT == NB_ANO_CORRIGEES + NB_ANO_FORCEES ~ 2,
        TRUE ~ 1
      )
    )
  df_questionnaire %>% filter(IDENTIFIANT_SUIVALIAA != identifiant) %>% bind_rows(questionnaireToUpdate)
}

# ==============================================================================
# FONCTIONS POGUES - Questionnaire
# ==============================================================================

#' Crée une nouvelle enquête dans la base
create_enquete <- function(db, enquete_id, questionnaire_id, source_name) {
  with_db_connection(
    db_path = db,
    contexte = "création d'une enquête (Pogues)",
    valeur_par_defaut = NA,
    fn = function(con) {
      dbExecute(con,
        "INSERT INTO enquetes (id, questionnaire_id, source_name) VALUES (:id, :qid, :src)
         ON CONFLICT(id) DO UPDATE SET updated_at = CURRENT_TIMESTAMP",
        params = list(id = enquete_id, qid = questionnaire_id, src = source_name))
    }
  )
}

#' Sauvegarde une réponse
save_response <- function(db, enquete_id, questionnaire_id, variable_name, valeur, ligne = 1, colonne = 1) {
  with_db_connection(
    db_path = db,
    contexte = "sauvegarde d'une réponse (Pogues)",
    valeur_par_defaut = NA,
    fn = function(con) {
      dbExecute(con,
        "INSERT INTO reponses (questionnaire_id, enquete_id, variable_name, valeur, ligne, colonne)
         VALUES (:qid, :eid, :var, :val, :lig, :col)
         ON CONFLICT(questionnaire_id, enquete_id, variable_name, ligne, colonne)
         DO UPDATE SET valeur = excluded.valeur, updated_at = CURRENT_TIMESTAMP",
        params = list(qid = questionnaire_id, eid = enquete_id,
                      var = variable_name, val = as.character(valeur),
                      lig = as.integer(ligne), col = as.integer(colonne)))
    }
  )
}

#' Charge les réponses d'une enquête
load_responses <- function(db, enquete_id) {
  with_db_connection(
    db_path = db,
    contexte = "chargement des réponses (Pogues)",
    valeur_par_defaut = tibble(variable_name = character(), valeur = character(), ligne = integer(), colonne = integer()),
    fn = function(con) {
      dbGetQuery(con,
        "SELECT variable_name, valeur, ligne, colonne FROM reponses WHERE enquete_id = :eid",
        params = list(eid = enquete_id))
    }
  )
}

#' Finalise une enquête (passe à "termine")
finalize_enquete <- function(db, enquete_id) {
  with_db_connection(
    db_path = db,
    contexte = "finalisation d'une enquête (Pogues)",
    valeur_par_defaut = NA,
    fn = function(con) {
      dbExecute(con, "UPDATE enquetes SET statut = 'termine', updated_at = CURRENT_TIMESTAMP WHERE id = :eid",
                params = list(eid = enquete_id))
    }
  )
}

#' Construit la liste des anomalies archivées depuis les fichiers externes
#' @param chemin Dossier contenant les fichiers CSV archivés
construire_la_liste_des_anomalies_archivees <- function(chemin = "DonneesExternes") {
  tryCatch({
    liste_de_fichier <- list.files(path = chemin, full.names = TRUE)

    if (length(liste_de_fichier) == 0) {
      return(NULL)
    }

    lire_fichier_archive <- function(fichier) {
      tryCatch({
        if (str_detect(fichier, "ANOMALIES.csv")) {
          list(type = "anomalies", data = read_delim(fichier, delim = ";", show_col_types = FALSE,
            col_types = list(COMMENT_VALIDATION = col_character(), ID_LIGNE_SR = col_character())))
        } else {
          list(type = "questionnaire", data = read_delim(fichier, delim = ";", show_col_types = FALSE,
            col_types = list(SIRET = col_character())))
        }
      }, error = function(cond) {
        log_erreur(paste0("lecture du fichier archivé '", fichier, "'"), cond)
        NULL
      })
    }

    fichiers_lus <- Filter(Negate(is.null), lapply(liste_de_fichier, lire_fichier_archive))

    df_anomalies <- bind_rows(lapply(fichiers_lus, function(f) if (f$type == "anomalies") f$data))
    df_questionnaire <- bind_rows(lapply(fichiers_lus, function(f) if (f$type == "questionnaire") f$data))

    if (nrow(df_anomalies) == 0 && nrow(df_questionnaire) == 0) {
      return(NULL)
    }

    df_anomalies %>%
      right_join(df_questionnaire, by = "IDENTIFIANT_SUIVALIAA") %>%
      select(
        "ENQUETE", "CAMPAGNE", "SIRET", "RAISON_SOCIALE",
        "ETAT_QUEST", "PRIORITE_QUEST", "DATE_MODIFICATION", "DATE_VALIDATION",
        "DATE_INTERNET", "DATE_COURRIER", "NB_ANO_TOT", "NB_ANO_NON_TRAITEES",
        "NB_ANO_FORCEES", "NB_ANO_CORRIGEES", "NB_ANO_EN_ATTENTE",
        "GESTIONNAIRE_REF", "VAGUE", "DATE_ANOMALIE", "CODE_ANOMALIE",
        "LIB_ANOMALIE", "LIGNE_ANOMALIE", "TYPE_ANOMALIE", "PRIORITE_ANOMALIE",
        "INFOS_COMP", "ECRAN", "VARIABLES", "ETAT_ANOMALIE", "COMMENT_VALIDATION"
      ) %>%
      rename(
        CODE = CODE_ANOMALIE,
        LIGNE = LIGNE_ANOMALIE,
        LIBELLE = LIB_ANOMALIE,
        ETAT = ETAT_QUEST
      )
  }, error = function(cond) {
    log_erreur("construction de la liste des anomalies archivées", cond)
    NULL
  })
}
