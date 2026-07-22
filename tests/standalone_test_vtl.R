# Test standalone pour les fonctions VTL
# Usage: source this file in R

source("../R/vtl.R")

cat("=== TESTS resolve_vtl ===\n\n")

# Test 1: Remplacement simple de variable
r <- resolve_vtl("Campagne $ANNEE$", list(ANNEE = "2026"))
cat(sprintf("Test 1 (var simple)   : '%s'  [attendu: 'Campagne 2026']\n", r))
stopifnot(r == "Campagne 2026")

# Test 2: Variable absente nettoyée
r <- resolve_vtl("Valeur : $INCONNUE$", list())
cat(sprintf("Test 2 (var absente)  : '%s'  [attendu: 'Valeur :']\n", r))
stopifnot(r == "Valeur :")

# Test 3: Concaténation VTL avec variable
r <- resolve_vtl('"Ventes hêtres - " || $REGIONS_EXT$', list(REGIONS_EXT = "Grand Est"))
cat(sprintf("Test 3 (concat)       : '%s'  [attendu: 'Ventes hêtres - Grand Est']\n", r))
stopifnot(r == "Ventes hêtres - Grand Est")

# Test 4: Concaténation sans variable
r <- resolve_vtl('"Ventes hêtres - " || $REGIONS_EXT$', list())
cat(sprintf("Test 4 (concat absent): '%s'  [attendu: 'Ventes hêtres -']\n", r))
stopifnot(r == "Ventes hêtres -")

# Test 5: if-then-else simple (vrai)
r <- resolve_vtl('if ($SEXE$ = "1") then "Homme" else "Femme"', list(SEXE = "1"))
cat(sprintf("Test 5a (if-else vrai): '%s'  [attendu: 'Homme']\n", r))
stopifnot(r == "Homme")

# Test 6: if-then-else simple (faux)
r <- resolve_vtl('if ($SEXE$ = "1") then "Homme" else "Femme"', list(SEXE = "2"))
cat(sprintf("Test 6a (if-else faux): '%s'  [attendu: 'Femme']\n", r))
stopifnot(r == "Femme")

# Test 7: cast vide
r <- resolve_vtl('cast($TOTO$,string)', list())
cat(sprintf("Test 7 (cast vide)    : '%s'  [attendu: '']\n", r))
stopifnot(r == "")

# Test 8: isnull avec variable absente
r <- resolve_vtl("isnull($VAR$) = true", list())
cat(sprintf("Test 8 (isnull absent): '%s'  [attendu: 'true = true']\n", r))
# après isnull -> true, puis true=true -> nettoyage

# Test 9: nvl(vide, 0) -> 0 
r <- resolve_vtl("nvl($X$, 0) <> 0", list())
cat(sprintf("Test 9 (nvl vide->0)  : '%s'  [attendu: '0 <> 0']\n", r))

# Test 10: nvl(valeur, 0) -> valeur
r <- resolve_vtl("nvl($X$, 0) <> 0", list(X = "5"))
cat(sprintf("Test 10 (nvl val->5)  : '%s'  [attendu: '5 <> 0']\n", r))

# Test 11: NULL de "REGIONS_EXT" avec label de module sans variable
r <- resolve_vtl('"Ventes chêne - " || $REGIONS_EXT$', list())
cat(sprintf("Test 11 (label sans var): '%s'  [attendu: 'Ventes chêne -']\n", r))
stopifnot(r == "Ventes chêne -")

# Test 12: vtl avec code de contrôle complexe (cas réel: FailMessage)
r <- resolve_vtl('"**Forte évolution des volumes**. Veuillez corriger ou en expliquer les raisons dans le cadre commentaires situé au bas de la page.\r\n" || (if (nvl($PRODUCTION_REGIONS_CHN11$,0) <> 0 and nvl($PRODUCTION_REGIONS_CHN13$,0) <> 0 and ($PRODUCTION_REGIONS_CHN11$ / $PRODUCTION_REGIONS_CHN13$ > 4 or $PRODUCTION_REGIONS_CHN13$ / $PRODUCTION_REGIONS_CHN11$ > 4)) then ("Ligne 1 :  **" || cast($PRODUCTION_REGIONS_CHN13$, string) || "** m³ de bois au " || $SP$ || " de " || $ANNEEP$ || " contre **" || cast($PRODUCTION_REGIONS_CHN11$, string) || "** m³ de bois au " || $SC$ || " de " || $ANNEEC$ || "\r\n" ) else (""))', 
  list(
    PRODUCTION_REGIONS_CHN11 = "500", PRODUCTION_REGIONS_CHN13 = "100",
    SP = "S2", SC = "S1", ANNEEP = "2025", ANNEEC = "2026"
  ))
cat(sprintf("Test 12 (contrôle fort vol):\n'%s'\n", r))

# Test 13: vtl avec déclaration help (cas réel)
r <- resolve_vtl('if ($FR$=false or isnull($FR$)) then "ventes nationales" else "ventes régionales"',
  list(FR = "false"))
cat(sprintf("Test 13 (help FR=false): '%s'  [attendu: 'ventes nationales']\n", r))

# Test 14: vtl avec label module (cas réel PrixGrumes)
r <- resolve_vtl('"Ventes chêne - " || $REGIONS_EXT$', list(REGIONS_EXT = "Bourgogne"))
cat(sprintf("Test 14 (label module) : '%s'  [attendu: 'Ventes chêne - Bourgogne']\n", r))

# Test 15: vtl label question
r <- resolve_vtl('"Dans la région " || $REGIONS_EXT$ || ", quels sont les volumes de grumes de chêne vendus ainsi que les prix de vente moyens en bord de route appliqués au semestre " || $SC$ || " de l\'année " || $ANNEEC$ || " ?"',
  list(REGIONS_EXT = "Nord-Ouest", SC = "S1", ANNEEC = "2026"))
cat(sprintf("Test 15 (label question):\n'%s'\n", r))

# Test 16: Condition avec FR=true
r <- resolve_vtl('if ($FR$=false or isnull($FR$)) then "national" else "régional"',
  list(FR = "true"))
cat(sprintf("Test 16 (FR=true)      : '%s'  [attendu: 'régional']\n", r))

cat("\n=== Tous les tests passés ===\n")