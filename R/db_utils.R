# ==============================================================================
# UTILITAIRES BASE DE DONNÉES - Connexion sécurisée réutilisable
# ==============================================================================
#
# Avant : chaque fonction de R/database.R répétait le même motif
#   con <- dbConnect(...); ... ; dbDisconnect(con)
# avec deux problèmes :
#   1) code dupliqué ~15 fois,
#   2) si une erreur survient entre dbConnect() et dbDisconnect(), la connexion
#      n'est JAMAIS fermée (fuite de connexion), car dbDisconnect() n'était pas
#      dans un `on.exit()`.
#
# `with_db_connection()` corrige les deux : un seul point d'ouverture/fermeture
# de connexion, garanti même en cas d'erreur grâce à `on.exit(..., add = TRUE)`.
# ==============================================================================

#' Ouvre une connexion SQLite, exécute `fn(con)`, puis ferme la connexion
#' (même si `fn` lève une erreur), et journalise proprement les erreurs.
#'
#' @param fn Fonction prenant une connexion DBI en argument, ex: `function(con) dbGetQuery(con, "...")`
#' @param db_path Chemin vers le fichier SQLite (défaut : `cst_chemin_vers_dB`)
#' @param contexte Description utilisée pour le log en cas d'erreur
#' @param valeur_par_defaut Valeur renvoyée en cas d'erreur (NA par défaut, pour rester
#'   compatible avec le comportement historique des fonctions de ce projet)
#'
#' @return Le résultat de `fn(con)`, ou `valeur_par_defaut` en cas d'erreur
#'
#' @examples
#' \dontrun{
#' with_db_connection(function(con) dbGetQuery(con, "SELECT * FROM QUESTIONNAIRE"))
#' }
with_db_connection <- function(fn,
                                db_path = get0("cst_chemin_vers_dB", envir = .GlobalEnv, ifnotfound = "base_de_donnee_SUIVAL_IAA.sqlite"),
                                contexte = "accès base de données",
                                valeur_par_defaut = NA) {
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    fn(con)
  }, error = function(cond) {
    log_erreur(contexte, cond)
    valeur_par_defaut
  })
}
