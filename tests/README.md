# Tests automatisés

Ce dossier contient des tests [testthat](https://testthat.r-lib.org/) pour les
fonctions "métier" du projet (base de données, chargement Pogues, gestion des
erreurs). Ils ne testent **pas** l'UI Shiny elle-même (pas de `shinytest2` ici) :
l'objectif est de sécuriser en priorité les fonctions les plus à risque de crash
(accès disque, base de données, parsing JSON).

## Lancer les tests

Depuis la racine du projet, dans une session R (RStudio / Posit Workbench / CERISE) :

```r
testthat::test_dir("tests/testthat")
```

ou en ligne de commande :

```bash
Rscript tests/testthat.R
```

## Packages nécessaires

```r
install.packages(c("testthat", "DBI", "RSQLite", "jsonlite", "dplyr", "tidyr", "tibble", "stringr"))
```

(Ce sont les mêmes packages que ceux déjà utilisés par l'application, cf. `global.R`.)

## Organisation

| Fichier                     | Ce qui est testé                                                                 |
|------------------------------|-----------------------------------------------------------------------------------|
| `helper-setup.R`             | Charge uniquement les fonctions nécessaires, sans lancer toute l'appli Shiny       |
| `test-constantes.R`          | L'opérateur `%!in%` et la cohérence de `cst_mapping_etat`                          |
| `test-error_handling.R`      | `executer_en_securite()`, `avec_gestion_erreur()`, `log_erreur()`, `notifier_erreur()` |
| `test-db_utils.R`            | `with_db_connection()` : exécution normale, erreur SQL gérée, absence de fuite de connexion |
| `test-database.R`            | Fonctions de `R/database.R` : init, CRUD questionnaires/anomalies, flux Pogues, fonction pure `mettreAjourQuestionnaire()`, résilience de `construire_la_liste_des_anomalies_archivees()` face à un CSV corrompu |
| `test-load_pogues.R`         | `load_pogues()` face à un JSON invalide, un fichier manquant, et un questionnaire minimal valide |

## Important : environnement d'exécution

Ces tests ont été rédigés et relus attentivement (vérification manuelle de la
syntaxe R et de la logique), mais **n'ont pas pu être exécutés dans cet
environnement** : le bac à sable utilisé pour préparer cette livraison n'a pas
d'installation R ni d'accès au CRAN. Merci de les lancer une première fois sur
CERISE/Posit Workbench avant de les intégrer à votre CI/CD GitLab, et de me
signaler tout échec éventuel pour correction.

## Étendre la couverture de tests

Les fichiers non couverts ici (modules Shiny sous `modules/`, `R/renderers.R`,
`R/vtl.R`, `R/import_data.R`, `R/ordonnanceur.R`, `R/traitement_anomalies.R`)
contiennent surtout de la logique UI/réactive difficile à tester unitairement
sans `shinytest2`. Pour les couvrir :
- extraire les fonctions "pures" (qui ne dépendent pas de `input`/`session`)
  dans des fichiers séparés, comme cela a déjà été fait pour
  `mettreAjourQuestionnaire()` ;
- utiliser [`shinytest2`](https://rstudio.github.io/shinytest2/) pour des tests
  de bout en bout sur les parcours utilisateurs critiques (chargement d'un
  questionnaire, sauvegarde d'une anomalie, export CSV).
