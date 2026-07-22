test_that("resolve_vtl remplace une variable simple $VAR$", {
  expect_equal(resolve_vtl("Campagne $ANNEE$", list(ANNEE = "2026")), "Campagne 2026")
})

test_that("resolve_vtl supprime une variable non résolue (absente de env)", {
  expect_equal(resolve_vtl("Valeur : $INCONNUE$", list()), "Valeur :")
})

test_that("resolve_vtl gère la concaténation VTL '||' avec une variable (cas PRIXGRUMES)", {
  expr <- '"Ventes hêtres - " || $REGIONS_EXT$'
  resultat <- resolve_vtl(expr, list(REGIONS_EXT = "Grand Est"))
  expect_equal(resultat, "Ventes hêtres - Grand Est")
})

test_that("resolve_vtl reste propre si la variable de concaténation est absente", {
  expr <- '"Ventes hêtres - " || $REGIONS_EXT$'
  resultat <- resolve_vtl(expr, list())
  expect_equal(resultat, "Ventes hêtres -")
})

test_that("resolve_vtl nettoie les structures if-then-else simples et complexes", {
  # Simple
  expr1 <- 'if ($SEXE$ = "1") then "Homme" else "Femme"'
  expect_equal(resolve_vtl(expr1, list(SEXE = "1")), "Homme")
  expect_equal(resolve_vtl(expr1, list(SEXE = "2")), "Femme")

  # Complexe (cas EAL avec cast)
  expr2 <- '(if ($ACT_VNB_PREC$="true") then ""||cast($CALC_CONSIGNE_COLL$,string)||"\n" else "Veuillez ajouter les départements dans le tableau.\n" )'
  # Quand la condition est fausse / absente (le cast vide de CALC_CONSIGNE_COLL s'efface d'abord)
  expect_equal(resolve_vtl(expr2, list()), "Veuillez ajouter les départements dans le tableau.")
})

test_that("resolve_vtl nettoie les casts vides sans laisser de résidu de type ,string", {
  expr <- 'cast($TOTCOLL_VNB$,string)'
  expect_equal(resolve_vtl(expr, list()), "")
})
