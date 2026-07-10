# ==============================================================================
# HELPER TESTTHAT - Charge uniquement les fonctions R nécessaires aux tests
# ==============================================================================
# Contrairement à global.R (qui initialise toute l'appli : connexion DB par
# défaut, dossiers Logs/DonneesUtilisateurs, sourcing des modules UI...), ce
# helper ne charge que les fonctions "métier" testables unitairement, pour que
# les tests soient rapides, isolés, et n'écrivent rien dans le projet réel.
# ==============================================================================

library(testthat)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(DBI)
library(RSQLite)
library(jsonlite)

# Racine du projet : les tests sont lancés depuis la racine (testthat::test_dir("tests/testthat")
# appelé depuis la racine du projet, ou via tests/testthat.R). On remonte de deux
# niveaux depuis tests/testthat/ si besoin, en se basant sur le répertoire de travail.
project_root <- if (file.exists("constantes.R")) {
  "."
} else if (file.exists(file.path("..", "..", "constantes.R"))) {
  file.path("..", "..")
} else {
  stop("Impossible de localiser la racine du projet (constantes.R introuvable). ",
       "Lancez les tests depuis la racine avec testthat::test_dir('tests/testthat').")
}

# On ne source que les fichiers "purs" utilisables hors contexte Shiny actif.
source(file.path(project_root, "constantes.R"), local = FALSE, chdir = TRUE)
source(file.path(project_root, "R", "error_handling.R"), local = FALSE)
source(file.path(project_root, "R", "db_utils.R"), local = FALSE)
source(file.path(project_root, "R", "database.R"), local = FALSE)
source(file.path(project_root, "R", "load_pogues.R"), local = FALSE)

#' Crée une base SQLite temporaire initialisée (tables vides) pour un test,
#' et retourne son chemin. Le fichier est automatiquement supprimé après le
#' test grâce à `withr::defer()` si le package `withr` est disponible, sinon
#' via `on.exit()` dans le test appelant.
creer_base_temporaire <- function() {
  chemin <- tempfile(fileext = ".sqlite")
  init_db(chemin)
  chemin
}
