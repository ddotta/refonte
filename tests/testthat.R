# Point d'entrée pour `R CMD check` / testthat::test_dir().
# Lancer les tests depuis la racine du projet avec :
#   testthat::test_dir("tests/testthat")
# ou, si le package testthat est installé :
#   Rscript tests/testthat.R

library(testthat)

test_dir(file.path("tests", "testthat"), reporter = "summary")
