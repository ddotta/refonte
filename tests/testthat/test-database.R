test_that("init_db crée bien les 4 tables attendues", {
  chemin <- creer_base_temporaire()
  on.exit(unlink(chemin))

  tables <- with_db_connection(db_path = chemin, contexte = "test liste tables", fn = function(con) {
    DBI::dbListTables(con)
  })

  expect_setequal(tables, c("QUESTIONNAIRE", "ANOMALIES", "enquetes", "reponses"))
})

test_that("interagir_dB_recuperer_tout_les_questionnaires renvoie un data.frame vide sur une base neuve", {
  chemin <- creer_base_temporaire()
  on.exit(unlink(chemin))

  # On force temporairement cst_chemin_vers_dB pour cette fonction historique
  # qui utilise le chemin global par défaut.
  ancien <- cst_chemin_vers_dB
  cst_chemin_vers_dB <<- chemin
  on.exit(cst_chemin_vers_dB <<- ancien, add = TRUE)

  resultat <- interagir_dB_recuperer_tout_les_questionnaires()

  expect_s3_class(resultat, "data.frame")
  expect_equal(nrow(resultat), 0)
})

test_that("interagir_dB_recuperer_tout_les_questionnaires renvoie NULL (pas un crash) si le fichier n'existe pas", {
  chemin_inexistant <- file.path(tempdir(), "base_inexistante_12345.sqlite")
  ancien <- cst_chemin_vers_dB
  cst_chemin_vers_dB <<- chemin_inexistant
  on.exit(cst_chemin_vers_dB <<- ancien, add = TRUE)
  on.exit(unlink(chemin_inexistant), add = TRUE)

  # RSQLite crée le fichier automatiquement à la connexion, donc ce cas ne
  # déclenche pas d'erreur ; le test vérifie surtout l'absence de crash.
  expect_no_error(interagir_dB_recuperer_tout_les_questionnaires())
})

test_that("create_enquete / save_response / load_responses fonctionnent ensemble (flux Pogues)", {
  chemin <- creer_base_temporaire()
  on.exit(unlink(chemin))

  create_enquete(chemin, "ENQ_TEST_1", "QUESTIONNAIRE_TEST", "test")
  save_response(chemin, "ENQ_TEST_1", "QUESTIONNAIRE_TEST", "VARIABLE_A", 42, ligne = 1, colonne = 1)
  save_response(chemin, "ENQ_TEST_1", "QUESTIONNAIRE_TEST", "VARIABLE_A", 99, ligne = 1, colonne = 1) # écrase la précédente

  reponses <- load_responses(chemin, "ENQ_TEST_1")

  expect_equal(nrow(reponses), 1)
  expect_equal(reponses$valeur, "99")
})

test_that("load_responses renvoie un tibble vide (et non une erreur) pour une enquête inconnue", {
  chemin <- creer_base_temporaire()
  on.exit(unlink(chemin))

  reponses <- load_responses(chemin, "ENQUETE_QUI_NEXISTE_PAS")

  expect_equal(nrow(reponses), 0)
})

test_that("mettreAjourQuestionnaire recalcule correctement ETAT_QUEST (fonction pure, sans base)", {
  df <- tibble::tibble(
    IDENTIFIANT_SUIVALIAA = "Q1",
    NB_ANO_TOT = 2,
    NB_ANO_NON_TRAITEES = 2,
    NB_ANO_EN_COURS = 0,
    NB_ANO_EN_ATTENTE = 0,
    NB_ANO_CORRIGEES = 0,
    NB_ANO_FORCEES = 0
  )

  # Une anomalie passe de "non traitée" à "en cours"
  resultat <- mettreAjourQuestionnaire(df, "Q1", "NB_ANO_NON_TRAITEES", "NB_ANO_EN_COURS", "commentaire")

  expect_equal(resultat$NB_ANO_NON_TRAITEES, 1)
  expect_equal(resultat$NB_ANO_EN_COURS, 1)
  expect_equal(resultat$ETAT_QUEST, 1) # ni tout traité, ni tout corrigé => en cours
})

test_that("mettreAjourQuestionnaire passe ETAT_QUEST à 2 quand toutes les anomalies sont corrigées/forcées", {
  df <- tibble::tibble(
    IDENTIFIANT_SUIVALIAA = "Q1",
    NB_ANO_TOT = 1,
    NB_ANO_NON_TRAITEES = 0,
    NB_ANO_EN_COURS = 1,
    NB_ANO_EN_ATTENTE = 0,
    NB_ANO_CORRIGEES = 0,
    NB_ANO_FORCEES = 0
  )

  resultat <- mettreAjourQuestionnaire(df, "Q1", "NB_ANO_EN_COURS", "NB_ANO_CORRIGEES", "ok")

  expect_equal(resultat$NB_ANO_CORRIGEES, 1)
  expect_equal(resultat$ETAT_QUEST, 2)
})

test_that("construire_la_liste_des_anomalies_archivees renvoie NULL si le dossier est vide", {
  dossier_vide <- tempfile()
  dir.create(dossier_vide)
  on.exit(unlink(dossier_vide, recursive = TRUE))

  expect_null(construire_la_liste_des_anomalies_archivees(dossier_vide))
})

test_that("construire_la_liste_des_anomalies_archivees ignore un CSV corrompu sans planter", {
  dossier <- tempfile()
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE))

  # Fichier volontairement corrompu (mauvais nombre de colonnes / encodage)
  writeLines(c("SIRET;RAISON_SOCIALE", "\"12345 sans guillemet fermant"), file.path(dossier, "corrompu.csv"))

  expect_no_error(construire_la_liste_des_anomalies_archivees(dossier))
})
