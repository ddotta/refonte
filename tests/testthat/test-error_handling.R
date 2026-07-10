test_that("executer_en_securite renvoie le résultat normal quand tout va bien", {
  resultat <- executer_en_securite({
    1 + 1
  }, contexte = "test addition", notifier = FALSE)

  expect_equal(resultat, 2)
})

test_that("executer_en_securite intercepte une erreur et renvoie la valeur par défaut", {
  resultat <- executer_en_securite({
    stop("erreur volontaire pour le test")
  }, contexte = "test erreur", valeur_par_defaut = "repli", notifier = FALSE)

  expect_equal(resultat, "repli")
})

test_that("executer_en_securite n'interrompt jamais l'exécution (pas de crash)", {
  # Ce test vérifie le comportement central demandé : une erreur ne doit
  # jamais remonter en dehors de executer_en_securite.
  expect_no_error(
    executer_en_securite(stop("boom"), contexte = "test", notifier = FALSE)
  )
})

test_that("executer_en_securite intercepte aussi les warnings", {
  resultat <- executer_en_securite({
    warning("attention")
    "jamais atteint si le warning interrompt"
  }, contexte = "test warning", valeur_par_defaut = "repli_warning", notifier = FALSE)

  expect_equal(resultat, "repli_warning")
})

test_that("avec_gestion_erreur sécurise une fonction existante", {
  fonction_risquee <- function(x) {
    if (x < 0) stop("valeur négative interdite")
    sqrt(x)
  }
  fonction_securisee <- avec_gestion_erreur(fonction_risquee, contexte = "test sqrt", valeur_par_defaut = NA)

  expect_equal(fonction_securisee(4), 2)
  expect_true(is.na(fonction_securisee(-1)))
})

test_that("log_erreur ne lève jamais d'erreur, même sans session ni fichier de log", {
  cond <- simpleError("condition de test")
  expect_no_error(log_erreur("contexte de test", cond))
})

test_that("notifier_erreur ne lève pas d'erreur en dehors d'une session Shiny", {
  expect_no_error(notifier_erreur("titre de test", "detail de test"))
})
