test_that("with_db_connection exécute la requête et ferme la connexion", {
  chemin <- tempfile(fileext = ".sqlite")
  on.exit(unlink(chemin))

  resultat <- with_db_connection(db_path = chemin, contexte = "test création table", fn = function(con) {
    DBI::dbExecute(con, "CREATE TABLE t (id INTEGER)")
    DBI::dbExecute(con, "INSERT INTO t (id) VALUES (1), (2), (3)")
    DBI::dbGetQuery(con, "SELECT * FROM t")
  })

  expect_equal(nrow(resultat), 3)
  expect_equal(resultat$id, c(1, 2, 3))
})

test_that("with_db_connection renvoie la valeur par défaut en cas d'erreur SQL, sans planter", {
  chemin <- tempfile(fileext = ".sqlite")
  on.exit(unlink(chemin))

  resultat <- with_db_connection(
    db_path = chemin,
    contexte = "test requête invalide",
    valeur_par_defaut = "ERREUR_GEREE",
    fn = function(con) {
      DBI::dbGetQuery(con, "SELECT * FROM une_table_qui_nexiste_pas")
    }
  )

  expect_equal(resultat, "ERREUR_GEREE")
})

test_that("with_db_connection ne laisse pas de connexion ouverte après une erreur", {
  # Reproduit le bug historique : avant le refactoring, dbDisconnect() n'était
  # jamais appelé si une erreur survenait entre dbConnect() et dbDisconnect(),
  # ce qui provoquait une fuite de connexions SQLite.
  chemin <- tempfile(fileext = ".sqlite")
  on.exit(unlink(chemin))

  for (i in 1:5) {
    with_db_connection(db_path = chemin, contexte = "test fuite connexion", fn = function(con) {
      stop("erreur volontaire")
    })
  }

  # Si des connexions avaient fui, ce nouvel appel échouerait ou la base
  # serait verrouillée ; ici il doit fonctionner normalement.
  resultat <- with_db_connection(db_path = chemin, contexte = "vérification post-fuite", fn = function(con) {
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS t (id INTEGER)")
    DBI::dbExecute(con, "INSERT INTO t (id) VALUES (42)")
    DBI::dbGetQuery(con, "SELECT * FROM t")
  })

  expect_equal(resultat$id, 42)
})
