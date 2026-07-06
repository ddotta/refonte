# ==============================================================================
# GESTION DES FILTRES - Sauvegarde et restauration
# ==============================================================================

#' Sauvegarde l'état des filtres questionnaires
sauvegardeFiltreQuestionnaire <- function(df_input) {
  assign("df_inputQuestionnaire", 
         c(df_input[1], df_input[2], df_input[3], df_input[4], df_input[5],
           df_input[6], df_input[7], df_input[8], df_input[9], df_input[10],
           df_input[11], df_input[12], df_input[13]),
         envir = .GlobalEnv
  )
}

#' Sauvegarde l'état des filtres anomalies
sauvegardeFiltreAnomalie <- function(df_input) {
  assign("df_inputAnomalie", 
         c(df_input[1], df_input[2], df_input[3]),
         envir = .GlobalEnv
  )
}

#' Restaure l'état des filtres questionnaires
restaureFiltreQuestionnaire <- function(df_input){
  session <- getDefaultReactiveDomain()
  updateSelectizeInput(session, "filtreEnquete", selected = unlist(df_input[1]))
  updateSelectizeInput(session, "filtreCampagne", selected = unlist(df_input[2]))
  updateSelectizeInput(session, "filtreGestionnaire", selected = unlist(df_input[3]))
  updateSelectizeInput(session, "filtreEtat", selected = unlist(df_input[4]))
  updateSelectizeInput(session, "filtreSiret",
    choices = donnees_globales$df_questionnaire %>% pull("SIRET") %>% unique() %>% sort(),
    selected = unlist(df_input[5]), server = TRUE)
  updateSelectizeInput(session, "filtreRaison",
    choices = donnees_globales$df_questionnaire %>% pull("RAISON_SOCIALE") %>% unique() %>% sort(),
    selected = unlist(df_input[6]), server = TRUE)
  updateSelectizeInput(session, "filtrePriorite", selected = unlist(df_input[7]))
  updateDateRangeInput(session, "filtreModification", start = as.Date(df_input[[8]]), end = as.Date(df_input[[9]]))
  updateDateRangeInput(session, "filtreValidation", start = as.Date(df_input[[10]]), end = as.Date(df_input[[11]]))
  updateDateRangeInput(session, "filtreSaisie", start = as.Date(df_input[[12]]), end = as.Date(df_input[[13]]))
}

#' Restaure l'état des filtres anomalies
restaureFiltreAnomalie <- function(df_input){
  session <- getDefaultReactiveDomain()
  updateSelectizeInput(session, "selectizeEtat", selected = unlist(df_input[1]))
}