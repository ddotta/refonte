# ==============================================================================
# SUIVAL IAA - GLOBAL (initialisation)
# ==============================================================================

# Sauvegarde de l'environnement de départ
env_debut <- ls()

# Chargement des constantes
source("constantes.R", local = TRUE)

# Librairies
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(dbplyr)
library(tidyr)
library(tibble)
library(stringr)
library(readr)
library(xlsx)
library(lubridate)
library(janitor)
library(DBI)
library(RSQLite)
library(cli)
library(shinybusy)

# ==============================================================================
# Sources des fichiers R utilitaires
# ==============================================================================

# Modules pogues
source("R/vtl.R", local = TRUE)
source("R/load_pogues.R", local = TRUE)
source("R/import_data.R", local = TRUE)
# renderers.R expose des fonctions utilisées par d'autres modules => charger dans globalenv
source("R/renderers.R")

# Modules SUIVAL IAA
source("R/traitement_anomalies.R", local = TRUE)
source("R/ordonnanceur.R", local = TRUE)
source("R/gestion_filtre.R", local = TRUE)

# Base de données unifiée (après les constantes)
source("R/database.R", local = TRUE)

# ==============================================================================
# Sources des modules UI/Server
# ==============================================================================
# Header et Sidebar (définis dans app.R pour la clarté)
# Les modules fonctionnels sont chargés dans app.R via source()

# ==============================================================================
# Initialisation de la base de données
# ==============================================================================

# Créer la base avec toutes les tables si elle n'existe pas
dir.create("DonneesUtilisateurs", showWarnings = FALSE, recursive = TRUE)
dir.create("DonneesExternes", showWarnings = FALSE, recursive = TRUE)
dir.create("Logs", showWarnings = FALSE, recursive = TRUE)
init_db()

# ==============================================================================
# Chargement des données globales
# ==============================================================================

donnees_globales <- reactiveValues()

donnees_globales$df_questionnaire <- interagir_dB_recuperer_tout_les_questionnaires()
donnees_globales$df_anomalies_archivees <- construire_la_liste_des_anomalies_archivees()

# ==============================================================================
# Fonctions utilitaires
# ==============================================================================

#' Récupère le nom de l'utilisateur connecté
get_utilisateur <- function() {
  Sys.info()["user"]
}

#' Nettoie l'environnement à la fermeture
.onStop <- function() {
  cat(paste0("Fermeture de l'application ", cst_nom_application, "-R \n"))
  rm(list = ls(envir = .GlobalEnv)[!(ls(envir = .GlobalEnv) %in% env_debut)], envir = .GlobalEnv)
}