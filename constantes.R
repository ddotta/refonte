##########
# SUIVAL IAA + Questionnaire Pogues - Constantes fusionnées
##########

# Nom de l'application
cst_nom_application <- "SUIVAL-IAA"

print(cst_nom_application)

##########
# Chemins base de données
##########

cst_chemin_vers_dB <- "base_de_donnee_SUIVAL_IAA.sqlite"

##########
# Fichiers de référence SUIVAL
##########

cst_chemin_vers_fichier_enquete <- "SUIVAL_IAA_REF_ENQUETE_CAMPAGNE.csv"

cst_chemin_vers_fichier_nom_enquete <- "NOM_DES_ENQUETES.csv"

cst_chemin_sauvegarde_donnees <- "DonneesUtilisateurs/"

##########
# Logs
##########

cst_chemin_logs_donnees_traitees <- "Logs/Donnees_traitees"
cst_chemin_vers_fichier_log <- "Logs/logs_ordonnanceur.txt"

##########
# Racine du projet Cerise
##########

cst_chemin_root <- "/var/data/nfs/CERISE/"

##########
# Mapping des états d'anomalies
##########

cst_mapping_etat <- tibble(
  ETAT_LETTRE  = c("Non traitée", "En cours de traitement", "Corrigée dans l'enquête", "Corrigée", "Forcée"),
  ETAT_CHIFFRE = c(0, 1, 2, 3, 4),
  ETAT_COLONNE = c("NB_ANO_NON_TRAITEES", "NB_ANO_EN_COURS", "NB_ANO_EN_ATTENTE", "NB_ANO_CORRIGEES", "NB_ANO_FORCEES")
)

##########
# Traductions DataTable (français)
##########

fr <- list(
  sProcessing = "Traitement en cours...", sSearch = "Rechercher&nbsp;:",
  sLengthMenu = "Afficher _MENU_ &eacute;l&eacute;ments",
  sInfo = "Affichage de l'&eacute;l&eacute;ment _START_ &agrave; _END_ sur _TOTAL_ &eacute;l&eacute;ments",
  sInfoEmpty = "Affichage de l'&eacute;l&eacute;ment 0 &agrave; 0 sur 0 &eacute;l&eacute;ment",
  sInfoFiltered = "(filtr&eacute; de _MAX_ &eacute;l&eacute;ments au total)",
  sInfoPostFix = "", sLoadingRecords = "Chargement en cours...",
  sZeroRecords = "Aucun &eacute;l&eacute;ment &agrave; afficher",
  sEmptyTable = "Aucune donn&eacute;e disponible dans le tableau",
  oPaginate = list(
    sFirst = "Premier", sPrevious = "Pr&eacute;c&eacute;dent",
    sNext = "Suivant", sLast = "Dernier"
  ),
  oAria = list(
    sSortAscending = ": activer pour trier la colonne par ordre croissant",
    sSortDescending = ": activer pour trier la colonne par ordre d&eacute;croissant"
  )
)

##########
# Aide - Assistance BMIS
##########

cst_objet_mail <- paste0("[CERISE] demande assistance : ", cst_nom_application)
cst_mail <- paste0("window.open('mailto:contact-tech.cassis.ssp.sg@agriculture.gouv.fr?subject=", cst_objet_mail, "' , '_blank')")

##########
# Commentaires contextuels
##########

cst_commentaire_integration <- "L'enquête, la campagne (année) et le programme R de détection des anomalies sont obligatoires."
cst_commentaire_suivi_traitement <- "Sélectionnez le niveau de rapportage : questionnaires (au sens SUIVAL IAA = couple enquête-campagne) ou anomalies"
cst_commentaire_export_csv <-"Sélectionnez le type d'export : questionnaires (au sens SUIVAL IAA = couple enquête-campagne) ou anomalies"
cst_commentaire_suivi_questionnaire <- "Tableau de suivi à destination du responsable d'enquête : croisement de la 'Priorité' et de l'état du questionnaire ; Affichage des totaux lignes et colonnes et des % correspondants"
cst_commentaire_archivage <- "Sélectionnez une enquête et une campagne (année). Attention, l'archivage est définitif. La campagne ne sera plus active dans SUIVAL-IAA"

##########
# Configuration Questionnaire Pogues
##########

# Liste des enquêtes disponibles pour le questionnaire
AVAILABLE_SURVEYS <- list(
  "PrixGrumes" = list(
    id = "PrixGrumes",
    label = "Prix des Grumes",
    description = "Enquête sur les prix des grumes et produits forestiers",
    icon = "tree"
  ),
  "EAL" = list(
    id = "EAL",
    label = "EAL",
    description = "Enquête sur les achats des ménages en ligne",
    icon = "shopping-cart"
  )
)

# Variable d'environnement pour forcer une enquête
FORCED_SURVEY <- Sys.getenv("SURVEY", unset = NA)
if (!is.na(FORCED_SURVEY) && nchar(FORCED_SURVEY) > 0) {
  FORCED_SURVEY <- FORCED_SURVEY
} else {
  FORCED_SURVEY <- NULL
}

##########
# Définition de l'opérateur Not In
##########

`%!in%` <- function(x,y)!('%in%'(x,y))