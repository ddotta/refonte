# ==============================================================================
# ORDONNANCEUR (DAMAJQ) - Exécution des programmes de détection d'anomalies
# ==============================================================================
# Ce module exécute périodiquement les scripts R de détection d'anomalies
# pour chaque enquête active, et met à jour la base de données.

library(readr)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)

# ==============================================================================
# Gestion des logs
# ==============================================================================

#' Écrit un message dans le fichier de log
#' @param str_to_write Message à écrire
#' @param overwrite TRUE pour écraser le fichier, FALSE pour ajouter en tête
gestion_des_logs <- function(str_to_write, overwrite = FALSE){
  chemin <- cst_chemin_vers_fichier_log
  if(overwrite){
    write(str_to_write, chemin)
  }else{
    if(file.exists(chemin)){
      fichier_de_log <- readLines(chemin)
      new_logs <- append(str_to_write, fichier_de_log)
      write(new_logs, chemin)
    } else {
      write(str_to_write, chemin)
    }
  }
}

# ==============================================================================
# Exécution de la détection d'anomalies pour une enquête
# ==============================================================================

#' Exécute le programme de détection d'anomalies d'une enquête
#' @param line Ligne du fichier CSV (NOM_ENQUETE, CAMPAGNE, CHEMIN_ESPACE)
#' @return Liste contenant QUESTIONNAIRES et ANOMALIES, ou "ERREUR"
executerDetectionAnomalie <- function(line){
  print(paste("Execution pour l'enquete : ", line$NOM_ENQUETE))

  out <- tryCatch({
    if(!file.exists(line$CHEMIN_ESPACE)){
      stop("ERREUR")
    } else {
      source(line$CHEMIN_ESPACE, local = TRUE)
      listeData <- detection_anomalie()
    }
  },
  error = function(cond){
    message("Voici l'erreur")
    message(cond)
    return("ERREUR")
  })
  return(out)
}

# ==============================================================================
# Mise en base de données
# ==============================================================================

#' Met en base les questionnaires et anomalies
#' @param cst_nom_base Chemin de la base SQLite
#' @param listeData Liste contenant QUESTIONNAIRES et ANOMALIES
#' @param line Ligne du fichier CSV (NOM_ENQUETE, CAMPAGNE)
#' @return 1 si succès, 0 si échec
miseEnBaseDeDonnee <- function(cst_nom_base, listeData, line){
  out <- tryCatch({
    connection <- dbConnect(SQLite(), cst_nom_base)

    df_questionnaire <- listeData$QUESTIONNAIRES %>% mutate(
      DATE_MAJ_SUIVAL = format(Sys.Date(), "%d/%m/%Y"),
      ETAT_QUEST = 0,
      NB_ANO_TOT = 0,
      NB_ANO_CORRIGEES = 0,
      NB_ANO_FORCEES = 0,
      NB_ANO_EN_COURS = 0,
      NB_ANO_EN_ATTENTE = 0,
      NB_ANO_NON_TRAITEES = 0
    )

    df_anomalies <- listeData$ANOMALIES %>% mutate(
      DATE_ANOMALIE = format(Sys.Date(), "%d/%m/%Y"),
      ETAT_ANOMALIE = 0,
      COMMENT_VALIDATION = ""
    )

    # Mise à jour ou insertion des questionnaires
    if(dbExistsTable(connection, "QUESTIONNAIRE")){
      anciens_identifiants <- dbGetQuery(connection,
        'SELECT IDENTIFIANT_SUIVALIAA FROM QUESTIONNAIRE WHERE ENQUETE == :x AND CAMPAGNE == :y',
        params = list(x = line$NOM_ENQUETE, y = line$CAMPAGNE))

      df_questionnaire_maj <- df_questionnaire %>%
        select(IDENTIFIANT_SUIVALIAA, RAISON_SOCIALE, GESTIONNAIRE_REF,
               DATE_MODIFICATION, DATE_VALIDATION, DATE_COURRIER, DATE_INTERNET,
               COMMENT_QUEST, COMMENT_GEST, PRIORITE_QUEST) %>%
        filter(IDENTIFIANT_SUIVALIAA %in% anciens_identifiants$IDENTIFIANT_SUIVALIAA)

      if(nrow(df_questionnaire_maj) > 0){
        maj <- dbSendQuery(connection,
          "UPDATE QUESTIONNAIRE
           SET RAISON_SOCIALE = $RAISON_SOCIALE, GESTIONNAIRE_REF = $GESTIONNAIRE_REF,
               DATE_MODIFICATION = $DATE_MODIFICATION, DATE_VALIDATION = $DATE_VALIDATION,
               DATE_COURRIER = $DATE_COURRIER, DATE_INTERNET = $DATE_INTERNET,
               COMMENT_QUEST = $COMMENT_QUEST, COMMENT_GEST = $COMMENT_GEST,
               PRIORITE_QUEST = $PRIORITE_QUEST
           WHERE IDENTIFIANT_SUIVALIAA = $IDENTIFIANT_SUIVALIAA")
        dbBind(maj, df_questionnaire_maj)
        dbClearResult(maj)
      }

      df_questionnaire <- df_questionnaire %>%
        filter(IDENTIFIANT_SUIVALIAA %!in% anciens_identifiants$IDENTIFIANT_SUIVALIAA)

      if(nrow(df_questionnaire) > 0){
        dbWriteTable(connection, "QUESTIONNAIRE", df_questionnaire, overwrite = FALSE, append = TRUE)
      }
    } else {
      dbWriteTable(connection, "QUESTIONNAIRE", df_questionnaire, overwrite = TRUE, append = FALSE)
    }

    # Traitement des nouvelles anomalies
    if(dbExistsTable(connection, "ANOMALIES")){
      df_questionnaires_enquete <- dbGetQuery(connection,
        'SELECT * FROM QUESTIONNAIRE WHERE ENQUETE == :x AND CAMPAGNE == :y',
        params = list(x = line$NOM_ENQUETE, y = line$CAMPAGNE))

      if(nrow(df_questionnaires_enquete) > 0){
        df_anciennes_anomalies <- dbGetQuery(connection,
          'SELECT * FROM ANOMALIES WHERE IDENTIFIANT_SUIVALIAA IN (:x)',
          params = list(x = df_questionnaires_enquete$IDENTIFIANT_SUIVALIAA))

        dbExecute(connection,
          'DELETE FROM ANOMALIES WHERE IDENTIFIANT_SUIVALIAA IN (:x)',
          params = list(x = df_questionnaires_enquete$IDENTIFIANT_SUIVALIAA))

        df_anciennes_anomalies <- df_anciennes_anomalies %>%
          unite(UNIQUE_IDENTIFIER, c(IDENTIFIANT_SUIVALIAA, CODE_ANOMALIE, ID_LIGNE_SR, LIGNE_ANOMALIE), sep = "_", remove = FALSE)
        df_nouvelles_anomalies <- df_anomalies %>%
          unite(UNIQUE_IDENTIFIER, c(IDENTIFIANT_SUIVALIAA, CODE_ANOMALIE, ID_LIGNE_SR, LIGNE_ANOMALIE), sep = "_", remove = FALSE)

        df_anomalies_pas_encore_en_base <- anomalies_non_presentes(df_anciennes_anomalies, df_nouvelles_anomalies)
        df_anomalies_presentes_non_corrigees <- anomalies_presentes_et_non_corrigees(df_anciennes_anomalies, df_nouvelles_anomalies)
        df_anomalies_presentes_et_corrigees <- anomalies_presentes_et_corrigees(df_anciennes_anomalies, df_nouvelles_anomalies)
        df_anomalies_presentes_uniquement_en_base <- anomalies_presentes(df_anciennes_anomalies, df_nouvelles_anomalies)

        df_anomalies_a_pousser_en_base <- df_anomalies_pas_encore_en_base %>%
          bind_rows(df_anomalies_presentes_non_corrigees) %>%
          bind_rows(df_anomalies_presentes_et_corrigees) %>%
          bind_rows(df_anomalies_presentes_uniquement_en_base)

        if(nrow(df_anomalies_a_pousser_en_base) > 0){
          dbWriteTable(connection, "ANOMALIES", df_anomalies_a_pousser_en_base, append = TRUE)
        }
      }
    } else {
      dbWriteTable(connection, "ANOMALIES", df_anomalies, overwrite = TRUE)
    }

    dbDisconnect(connection)
    message("Sauvegarde Terminé !")
    return(1)
  },
  error = function(cond){
    message("Voici l'erreur")
    message(cond)
    return(0)
  })
  return(out)
}

# ==============================================================================
# Mise à jour des statistiques des questionnaires
# ==============================================================================

#' Met à jour les compteurs d'anomalies pour chaque questionnaire
#' @param cst_nom_base Chemin de la base SQLite
#' @param line Ligne du fichier CSV (NOM_ENQUETE, CAMPAGNE)
miseAJourDonneeEnBase <- function(cst_nom_base, line){
  connexion <- dbConnect(SQLite(), cst_nom_base)

  df_questionnaires_enquete <- dbGetQuery(connexion,
    'SELECT * FROM QUESTIONNAIRE WHERE ENQUETE == :x',
    params = list(x = line$NOM_ENQUETE))

  if(nrow(df_questionnaires_enquete) == 0){
    dbDisconnect(connexion)
    return()
  }

  dbExecute(connexion,
    'DELETE FROM QUESTIONNAIRE WHERE ENQUETE == :x',
    params = list(x = line$NOM_ENQUETE))

  df_anomalies_questionnaires <- dbGetQuery(connexion,
    'SELECT * FROM ANOMALIES WHERE IDENTIFIANT_SUIVALIAA IN (:x)',
    params = list(x = df_questionnaires_enquete$IDENTIFIANT_SUIVALIAA))

  df_nombre_anomalies_questionnaires <- statistiques_questionnaire(df_anomalies_questionnaires)

  df_questionnaires_enquete <- df_questionnaires_enquete %>%
    select(-c(NB_ANO_TOT, NB_ANO_NON_TRAITEES, NB_ANO_EN_COURS,
              NB_ANO_EN_ATTENTE, NB_ANO_CORRIGEES, NB_ANO_FORCEES, ETAT_QUEST)) %>%
    left_join(df_nombre_anomalies_questionnaires, by = "IDENTIFIANT_SUIVALIAA")

  dbWriteTable(connexion, "QUESTIONNAIRE", df_questionnaires_enquete, append = TRUE)
  dbDisconnect(connexion)
}

# ==============================================================================
# Fonction principale
# ==============================================================================

#' Lancement de l'ordonnanceur
#' @param appelDepuisLordonnanceur TRUE si appelé depuis l'ordonnanceur système, FALSE depuis l'appli
lancement_ordonnanceur <- function(appelDepuisLordonnanceur = TRUE){
  cst_nom_base <- cst_chemin_vers_dB
  chemin_vers_csv <- cst_chemin_vers_fichier_enquete

  print("Starting ---")
  if(!file.exists(chemin_vers_csv)){
    gestion_des_logs("Le fichier des enquêtes n'existe pas", TRUE)
    return()
  }

  contenu <- tryCatch({
    read.delim(chemin_vers_csv, sep = ";")
  }, error = function(e) NULL)

  if(!is.null(contenu) && nrow(contenu) != 0){
    df_dossier_enquete <- read_delim(chemin_vers_csv, delim = ";",
      escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE, lazy = FALSE)
    print(df_dossier_enquete)
    gestion_des_logs(paste(nrow(df_dossier_enquete), "enquête(s) à lire _ ", format(Sys.time(), "%a%H%P")), TRUE)
    print(nrow(df_dossier_enquete))

    for(n in 1:nrow(df_dossier_enquete)){
      line <- slice(df_dossier_enquete, n)
      list_data <- executerDetectionAnomalie(line)

      if(length(list_data) == 2){
        gestion_des_logs(paste(line$NOM_ENQUETE, "- Succès de l'éxécution des programmes de détection d'enquêtes"))

        res <- miseEnBaseDeDonnee(cst_nom_base, list_data, line)
        if(res){
          gestion_des_logs(paste(line$NOM_ENQUETE, "- Succès de la mise en base de donnée des nouveaux questionnaires"))
          miseAJourDonneeEnBase(cst_nom_base, line)
        } else {
          gestion_des_logs(paste(line$NOM_ENQUETE, "- Erreur lors de la l'écriture de base de donnée"))
        }
      } else {
        gestion_des_logs(paste(line$NOM_ENQUETE, "- Erreur lors de la récupération et de l'éxécution des programmes de détection d'enquêtes"))
      }
    }

    ecriture_des_donnees_en_sortie_du_programme(cst_nom_base)
  } else {
    print("Le dossier est vide")
    gestion_des_logs("Le fichier des enquêtes est vide, le DAMAJQ n'a pas été exécuté", TRUE)
  }
}

#' Écrit le contenu de la base dans des CSV de trace
ecriture_des_donnees_en_sortie_du_programme <- function(cst_nom_base){
  print("Ecriture des données en sortie du programme")
  dir.create("Logs/Donnees_traitees", showWarnings = FALSE, recursive = TRUE)

  connexion <- dbConnect(SQLite(), cst_nom_base)
  df_questionnaires <- dbGetQuery(connexion, 'SELECT * FROM QUESTIONNAIRE')
  df_anomalies <- dbGetQuery(connexion, 'SELECT * FROM ANOMALIES')
  dbDisconnect(connexion)

  write_csv2(df_questionnaires, "Logs/Donnees_traitees/QUESTIONNAIRES.csv")
  write_csv2(df_anomalies, "Logs/Donnees_traitees/ANOMALIES.csv")
}