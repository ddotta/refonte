# ==============================================================================
# GESTION DES ERREURS - Helpers génériques pour éviter les crashs et écrans gris
# ==============================================================================
#
# Principe : dans une appli Shiny classique, une erreur non interceptée dans un
# observeEvent/render*/reactive fait planter LA SESSION (écran gris "disconnected
# from server"). Les fonctions ci-dessous fournissent des enrobages tryCatch
# réutilisables pour :
#   - logger l'erreur (console + fichier de log),
#   - afficher une notification propre à l'utilisateur,
#   - retourner une valeur de repli au lieu de laisser l'erreur remonter.
#
# À utiliser en priorité autour de tout code qui touche : base de données,
# fichiers, API externes (SIRIUS, Pogues), parsing JSON, ou toute action
# déclenchée par un observeEvent (bouton, upload, etc.).
# ==============================================================================

#' Enregistre une erreur dans la console et (si possible) dans un fichier de log
#'
#' @param contexte Description courte du traitement en cours (ex: "chargement questionnaire")
#' @param cond Objet condition (error/warning) capturé par tryCatch
#' @return Invisible NULL
log_erreur <- function(contexte, cond) {
  horodatage <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message_erreur <- sprintf("[%s] Erreur dans '%s' : %s", horodatage, contexte, conditionMessage(cond))

  message(message_erreur)

  fichier_log <- tryCatch(get("cst_chemin_vers_fichier_log", envir = .GlobalEnv), error = function(e) NULL)
  if (!is.null(fichier_log)) {
    tryCatch({
      dir.create(dirname(fichier_log), showWarnings = FALSE, recursive = TRUE)
      cat(message_erreur, "\n", file = fichier_log, append = TRUE)
    }, error = function(e) invisible(NULL)) # on ne veut jamais planter à cause du logging lui-même
  }

  invisible(NULL)
}

#' Affiche une notification d'erreur à l'utilisateur (si une session Shiny existe)
#'
#' @param titre Titre affiché dans la notification
#' @param detail Détail optionnel (message technique)
notifier_erreur <- function(titre = "Une erreur est survenue", detail = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    shiny::showNotification(
      ui = if (is.null(detail)) titre else paste0(titre, " : ", detail),
      type = "error",
      duration = 8
    )
  }
  invisible(NULL)
}

#' Exécute une expression en capturant erreurs ET warnings, sans jamais laisser
#' planter l'application. C'est la fonction à utiliser pour enrober le contenu
#' d'un observeEvent, d'un render*, ou de toute fonction serveur "à risque".
#'
#' @param expr Expression à évaluer (utiliser `{ ... }` pour plusieurs lignes)
#' @param contexte Description courte utilisée pour le log et la notification
#' @param valeur_par_defaut Valeur renvoyée en cas d'erreur (NULL par défaut)
#' @param notifier Si TRUE (défaut), affiche une notification utilisateur en cas d'erreur
#'
#' @examples
#' \dontrun{
#' observeEvent(input$sauver, {
#'   executer_en_securite({
#'     interagir_dB_mise_a_jour_questionnaire(ligne)
#'   }, contexte = "sauvegarde questionnaire")
#' })
#' }
executer_en_securite <- function(expr, contexte = "opération", valeur_par_defaut = NULL, notifier = TRUE) {
  tryCatch({
    expr
  }, error = function(cond) {
    log_erreur(contexte, cond)
    if (notifier) notifier_erreur(paste0("Échec : ", contexte), conditionMessage(cond))
    valeur_par_defaut
  }, warning = function(cond) {
    log_erreur(paste0(contexte, " (warning)"), cond)
    valeur_par_defaut
  })
}

#' Variante de `executer_en_securite()` sous forme de fonction "wrapper" : au lieu
#' d'exécuter une expression, elle transforme une fonction en version sécurisée.
#' Pratique pour enrober directement les fonctions d'accès à la base de données.
#'
#' @param fn Fonction à sécuriser
#' @param contexte Description utilisée pour le log
#' @param valeur_par_defaut Valeur renvoyée en cas d'erreur
#'
#' @examples
#' \dontrun{
#' interagir_dB_recuperer_anomalies <- avec_gestion_erreur(
#'   .interagir_dB_recuperer_anomalies_impl,
#'   contexte = "récupération des anomalies",
#'   valeur_par_defaut = NA
#' )
#' }
avec_gestion_erreur <- function(fn, contexte = "opération", valeur_par_defaut = NULL) {
  function(...) {
    executer_en_securite(fn(...), contexte = contexte, valeur_par_defaut = valeur_par_defaut, notifier = TRUE)
  }
}

#' Installe un gestionnaire d'erreur global pour la SESSION Shiny en cours.
#'
#' IMPORTANT : `onUnhandledError()` ne fait que journaliser l'erreur et nettoyer
#' proprement les ressources ; il n'empêche PAS la fermeture de la session (donc
#' pas l'écran gris "Disconnected from server"). La vraie protection contre les
#' écrans gris consiste à enrober chaque observeEvent/observe "à risque" avec
#' `executer_en_securite()` plus haut dans ce fichier, pour que l'erreur soit
#' interceptée AVANT de remonter jusqu'à Shiny.
#'
#' Cette fonction ne fait donc que :
#'  - garantir qu'aucune erreur non prévue n'affiche de détails techniques
#'    (chemins de fichiers, requêtes SQL...) côté utilisateur,
#'  - journaliser malgré tout les erreurs non anticipées pour pouvoir les
#'    corriger a posteriori.
#'
#' À appeler en tout début de la fonction server(), pour chaque session.
installer_gestion_erreur_session <- function(session = shiny::getDefaultReactiveDomain()) {
  # Empêche shiny d'afficher aux utilisateurs la stack trace brute des erreurs R
  options(shiny.sanitize.errors = TRUE)

  shiny::onUnhandledError(function(cond) {
    log_erreur("erreur non gerée (session)", cond)
  })

  invisible(NULL)
}
