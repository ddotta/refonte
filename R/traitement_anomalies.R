# ==============================================================================
# TRAITEMENT DES ANOMALIES - 4 cas de mise à jour
# ==============================================================================

library(dplyr)
library(tidyr)

# Ordre des colonnes pour les anomalies
ordre <- c("IDENTIFIANT_SUIVALIAA",
           "CODE_ANOMALIE",
           "LIB_ANOMALIE",
           "ID_LIGNE_SR",
           "LIGNE_ANOMALIE",
           "TYPE_ANOMALIE",
           "PRIORITE_ANOMALIE",
           "ECRAN",
           "VARIABLES",
           "INFOS_COMP",
           "VAGUE",
           "DATE_ANOMALIE",
           "ETAT_ANOMALIE",
           "COMMENT_VALIDATION"
)

#' Cas 1 : Nouvelle anomalie non présente dans SUIVAL-IAA
#' @param df_anomalies_in_suival Anomalies existantes en base
#' @param df_nouvelles_anomalies Nouvelles anomalies remontées
#' @return Dataframe des anomalies à ajouter
anomalies_non_presentes <- function(df_anomalies_in_suival, df_nouvelles_anomalies){
  anomalies_presentes_dans_SUIVAL <- df_anomalies_in_suival %>% pull(UNIQUE_IDENTIFIER)
  df_anomalies_not_in_suival <- df_nouvelles_anomalies %>%
    filter(UNIQUE_IDENTIFIER %!in% anomalies_presentes_dans_SUIVAL) %>%
    select(-UNIQUE_IDENTIFIER)
  return(df_anomalies_not_in_suival %>% select(ordre))
}

#' Cas 2 : Anomalie présente et non corrigée (état ≠ 3)
#' Mise à jour des métadonnées (libellé, type, priorité, écran, variables, etc.)
#' @param df_anomalies_in_suival Anomalies existantes en base
#' @param df_nouvelles_anomalies Nouvelles anomalies remontées
#' @return Dataframe des anomalies mises à jour
anomalies_presentes_et_non_corrigees <- function(df_anomalies_in_suival, df_nouvelles_anomalies){
  nouvelles_anomalies <- df_nouvelles_anomalies %>% pull(UNIQUE_IDENTIFIER)
  df_anomalies <- df_anomalies_in_suival %>%
    filter(UNIQUE_IDENTIFIER %in% nouvelles_anomalies,
           ETAT_ANOMALIE != 3) %>%
    select(UNIQUE_IDENTIFIER, DATE_ANOMALIE, ETAT_ANOMALIE, COMMENT_VALIDATION) %>%
    left_join(df_nouvelles_anomalies %>% select(-c(DATE_ANOMALIE, ETAT_ANOMALIE, COMMENT_VALIDATION)), by = "UNIQUE_IDENTIFIER") %>%
    select(-UNIQUE_IDENTIFIER)
  return(df_anomalies %>% select(ordre))
}

#' Cas 3 : Anomalie présente et corrigée (état = 3)
#' L'anomalie revient → remise à NON TRAITEE (0)
#' @param df_anomalies_in_suival Anomalies existantes en base
#' @param df_nouvelles_anomalies Nouvelles anomalies remontées
#' @return Dataframe des anomalies réactivées
anomalies_presentes_et_corrigees <- function(df_anomalies_in_suival, df_nouvelles_anomalies){
  nouvelles_anomalies <- df_nouvelles_anomalies %>% pull(UNIQUE_IDENTIFIER)
  df_anomalies <- df_anomalies_in_suival %>%
    filter(UNIQUE_IDENTIFIER %in% nouvelles_anomalies,
           ETAT_ANOMALIE == 3) %>%
    select(UNIQUE_IDENTIFIER, COMMENT_VALIDATION) %>%
    left_join(df_nouvelles_anomalies %>% select(-COMMENT_VALIDATION), by = "UNIQUE_IDENTIFIER") %>%
    mutate(ETAT_ANOMALIE = 0) %>%
    select(-UNIQUE_IDENTIFIER)
  return(df_anomalies %>% select(ordre))
}

#' Cas 4 : Anomalie disparue (uniquement en base, plus remontée)
#' Passage automatique à CORRIGEE (3)
#' @param df_anomalies_in_suival Anomalies existantes en base
#' @param df_nouvelles_anomalies Nouvelles anomalies remontées
#' @return Dataframe des anomalies corrigées automatiquement
anomalies_presentes <- function(df_anomalies_in_suival, df_nouvelles_anomalies){
  nouvelles_anomalies <- df_nouvelles_anomalies %>% pull(UNIQUE_IDENTIFIER)
  df_anomalies <- df_anomalies_in_suival %>%
    filter(UNIQUE_IDENTIFIER %!in% nouvelles_anomalies) %>%
    mutate(ETAT_ANOMALIE = 3) %>%
    select(-UNIQUE_IDENTIFIER)
  return(df_anomalies %>% select(ordre))
}

#' Calcule les statistiques d'un questionnaire (nombre d'anomalies par état)
#' @param df_anomalie_from_base Dataframe des anomalies
#' @return Dataframe avec les colonnes IDENTIFIANT_SUIVALIAA, NB_ANO_*, ETAT_QUEST
statistiques_questionnaire <- function(df_anomalie_from_base){
  if(nrow(df_anomalie_from_base) == 0){
    return(data.frame(
      IDENTIFIANT_SUIVALIAA = character(),
      NB_ANO_NON_TRAITEES = integer(),
      NB_ANO_EN_COURS = integer(),
      NB_ANO_EN_ATTENTE = integer(),
      NB_ANO_CORRIGEES = integer(),
      NB_ANO_FORCEES = integer(),
      NB_ANO_TOT = integer(),
      ETAT_QUEST = integer()
    ))
  }

  dataAnomalies <- df_anomalie_from_base %>%
    group_by(IDENTIFIANT_SUIVALIAA, ETAT_ANOMALIE) %>%
    summarise(NUMBER = n(), .groups = "keep") %>%
    ungroup() %>%
    mutate(ETAT_ANOMALIE = case_when(
      ETAT_ANOMALIE == 0 ~ "NB_ANO_NON_TRAITEES",
      ETAT_ANOMALIE == 1 ~ "NB_ANO_EN_COURS",
      ETAT_ANOMALIE == 2 ~ "NB_ANO_EN_ATTENTE",
      ETAT_ANOMALIE == 3 ~ "NB_ANO_CORRIGEES",
      ETAT_ANOMALIE == 4 ~ "NB_ANO_FORCEES"
    )) %>%
    pivot_wider(names_from = ETAT_ANOMALIE, values_from = NUMBER, values_fill = 0)

  # Ajout des colonnes manquantes
  for(col in c("NB_ANO_NON_TRAITEES", "NB_ANO_EN_COURS", "NB_ANO_EN_ATTENTE",
               "NB_ANO_CORRIGEES", "NB_ANO_FORCEES")){
    if(col %!in% colnames(dataAnomalies)){
      dataAnomalies <- dataAnomalies %>% mutate(!!col := 0)
    }
  }

  dataAnomalies <- dataAnomalies %>%
    mutate(
      NB_ANO_TOT = NB_ANO_NON_TRAITEES + NB_ANO_EN_COURS + NB_ANO_EN_ATTENTE +
                   NB_ANO_CORRIGEES + NB_ANO_FORCEES,
      ETAT_QUEST = case_when(
        NB_ANO_TOT == NB_ANO_NON_TRAITEES ~ 0,
        NB_ANO_TOT == NB_ANO_CORRIGEES + NB_ANO_FORCEES ~ 2,
        TRUE ~ 1
      )
    ) %>%
    select(IDENTIFIANT_SUIVALIAA, ETAT_QUEST, NB_ANO_TOT, NB_ANO_NON_TRAITEES,
           NB_ANO_EN_COURS, NB_ANO_EN_ATTENTE, NB_ANO_CORRIGEES, NB_ANO_FORCEES)

  return(dataAnomalies)
}