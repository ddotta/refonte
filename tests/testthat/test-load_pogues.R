test_that("load_pogues renvoie NULL (et ne plante pas) sur un JSON invalide", {
  fichier_invalide <- tempfile(fileext = ".json")
  writeLines("{ ceci n'est pas du JSON valide !!", fichier_invalide)
  on.exit(unlink(fichier_invalide))

  expect_no_error({
    resultat <- load_pogues(fichier_invalide)
  })
  expect_null(load_pogues(fichier_invalide))
})

test_that("load_pogues renvoie NULL si le fichier n'existe pas", {
  expect_null(load_pogues(file.path(tempdir(), "fichier_pogues_inexistant_12345.json")))
})

test_that("load_pogues parse correctement un questionnaire Pogues minimal valide", {
  fichier <- tempfile(fileext = ".json")
  on.exit(unlink(fichier))

  jsonlite::write_json(list(
    id = "QUEST_TEST",
    Name = "TestSurvey",
    Label = list("Enquête de test"),
    owner = "SSM",
    Variables = list(Variable = list(
      list(id = "v1", Name = "VAR_A", Datatype = list(typeName = "NUMERIC"), Label = list("Variable A"))
    )),
    Child = list(
      list(
        genericName = "MODULE", id = "m1", Name = "MODULE_1", Label = list("Module 1"),
        Child = list(
          list(id = "q1", Name = "QUESTION_1", genericName = "SIMPLE", Label = list("Question 1"))
        )
      )
    )
  ), path = fichier, auto_unbox = TRUE)

  p <- load_pogues(fichier)

  expect_false(is.null(p))
  expect_equal(p$questionnaire_id, "QUEST_TEST")
  expect_equal(p$name, "TestSurvey")
  expect_true("VAR_A" %in% names(p$variables))
  expect_true("MODULE_1" %in% names(p$modules))
})

test_that("find_pogues_file renvoie un chemin par défaut sans planter quand rien n'est trouvé", {
  ancien_wd <- getwd()
  dossier_isole <- tempfile()
  dir.create(dossier_isole)
  setwd(dossier_isole)
  on.exit(setwd(ancien_wd))

  expect_no_error(suppressWarnings(find_pogues_file("EnqueteInexistante")))
})
