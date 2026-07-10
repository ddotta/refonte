test_that("%!in% renvoie l'inverse de %in%", {
  expect_true(1 %!in% c(2, 3, 4))
  expect_false(2 %!in% c(2, 3, 4))
  expect_true("z" %!in% letters[1:5])
  expect_false("a" %!in% letters[1:5])
})

test_that("%!in% gère les vecteurs vides et les NA sans erreur", {
  expect_true(1 %!in% c())
  expect_true(NA %!in% c(1, 2, 3))
})

test_that("cst_mapping_etat couvre bien les 5 états d'anomalie (0 à 4)", {
  expect_equal(nrow(cst_mapping_etat), 5)
  expect_setequal(cst_mapping_etat$ETAT_CHIFFRE, 0:4)
  expect_setequal(
    cst_mapping_etat$ETAT_COLONNE,
    c("NB_ANO_NON_TRAITEES", "NB_ANO_EN_COURS", "NB_ANO_EN_ATTENTE", "NB_ANO_CORRIGEES", "NB_ANO_FORCEES")
  )
})
