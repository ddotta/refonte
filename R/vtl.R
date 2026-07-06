# ==============================================================================
# RÉSOLUTION D'EXPRESSIONS VTL
# ==============================================================================

#' Résout les références VTL : remplace $VAR$, nettoie les expressions VTL
#' Version robuste : boucle simple avec gsub fixed=TRUE pour éviter les problèmes de regex
#' @param expr Expression VTL à résoudre
#' @param env Liste nommée des valeurs des variables
#' @return Chaîne résolue
resolve_vtl <- function(expr, env) {
  if (is.null(expr) || is.na(expr) || expr == "") return("")
  if (length(expr) > 1) expr <- expr[1]
  result <- as.character(expr)
  if (is.na(result) || result == "") return("")

  # =========================================================================
  # ÉTAPE 1 : Remplacer $VAR$ par leur valeur
  # =========================================================================
  # On cherche les motifs $LETTRES_CHIFFRES$ avec stringr::str_match_all
  for (iter in 1:10) {
    m <- tryCatch(str_match_all(result, "\\$([A-Z_][A-Z0-9_]*)\\$")[[1]], error = function(e) matrix(nrow = 0, ncol = 2))
    if (!is.matrix(m) || nrow(m) == 0) break
    
    any_replaced <- FALSE
    for (i in 1:nrow(m)) {
      varname <- m[i, 2]
      pattern_full <- m[i, 1]  # ex: "$ANNEE_N1$"
      
      val <- env[[varname]]
      if (!is.null(val)) {
        if (is.list(val)) val <- val[[1]]
        if (!is.null(val) && !is.na(val)) {
          replacement <- as.character(val)
          if (nchar(replacement) > 0) {
            result <- gsub(pattern_full, replacement, result, fixed = TRUE)
          } else {
            result <- gsub(pattern_full, "", result, fixed = TRUE)
          }
        } else {
          result <- gsub(pattern_full, "", result, fixed = TRUE)
        }
      } else {
        result <- gsub(pattern_full, "", result, fixed = TRUE)
      }
      any_replaced <- TRUE
    }
    if (!any_replaced) break
  }
  
  # Nettoyer les $VAR$ non résolus restants
  result <- gsub("\\$[A-Z_][A-Z0-9_]*\\$", "", result, perl = TRUE)
  
  # =========================================================================
  # ÉTAPE 2 : Nettoyer les structures if-then-else
  # =========================================================================
  # Pattern 1: (if (cond) then "texte1" else "texte2") -> texte1
  result <- gsub("\\(\\s*if\\s*\\([^)]*\\)\\s*then\\s*\"([^\"]*)\"\\s*else\\s*\"([^\"]*)\"\\s*\\)", "\\1", result, perl = TRUE)
  # Pattern 2: if (cond) then "texte1" else "texte2" -> texte1  
  result <- gsub("if\\s*\\([^)]*\\)\\s*then\\s*\"([^\"]*)\"\\s*else\\s*\"([^\"]*)\"", "\\1", result, perl = TRUE)
  # Pattern 3: if (cond) then texte (sans guillemets) - supprimer if/else
  result <- gsub("if\\s*\\([^)]*\\)\\s*then\\s*", "", result, perl = TRUE)
  result <- gsub("\\s*else\\s*[^)]*$", "", result)
  result <- gsub("\\s*else\\s*$", "", result)
  
  # =========================================================================
  # ÉTAPE 3 : cast(expr, type) -> expr
  # =========================================================================
  result <- gsub("cast\\(([^,]+),\\s*[A-Za-z]+\\)", "\\1", result, perl = TRUE)
  result <- gsub("cast\\(([^)]*)\\)", "\\1", result)
  
  # =========================================================================
  # ÉTAPE 4 : Fonctions VTL diverses
  # =========================================================================
  result <- gsub("normalize-whitespace\\(([^)]*)\\)", "\\1", result)
  result <- gsub("number\\(([^)]*)\\)", "\\1", result)
  result <- gsub("string\\(([^)]*)\\)", "\\1", result)
  result <- gsub("boolean\\(([^)]*)\\)", "\\1", result)
  result <- gsub("contains\\(([^)]*)\\)", "\\1", result)
  result <- gsub("not\\s+", "", result)
  result <- gsub("sum\\(([^)]*)\\)", "\\1", result)
  result <- gsub("left_join\\(([^,]+),([^)]+)\\)", "\\1", result)
  result <- gsub("isnull\\(([^)]+)\\)", "", result)
  result <- gsub("nvl\\(([^,]+),([^)]+)\\)", "\\2", result)
  
  # =========================================================================
  # ÉTAPE 5 : Concaténation ||
  # =========================================================================
  result <- gsub("\\s*\\|\\|\\s*", "", result)
  
  # =========================================================================
  # ÉTAPE 6 : Conditions résiduelles
  # =========================================================================
  result <- gsub('"="', "=", result, fixed = TRUE)
  result <- gsub('"true"', "", result, fixed = TRUE)
  result <- gsub('"false"', "", result, fixed = TRUE)
  result <- gsub("=true", "", result, fixed = TRUE)
  result <- gsub("=false", "", result, fixed = TRUE)
  result <- gsub("==", "", result, fixed = TRUE)
  result <- gsub("\\btrue\\b", "", result)
  result <- gsub("\\bfalse\\b", "", result)
  
  # =========================================================================
  # ÉTAPE 7 : Guillemets et sauts de ligne
  # =========================================================================
  result <- gsub('"', "", result, fixed = TRUE)
  result <- gsub("\\\\r\\\\n", "\n", result, fixed = TRUE)
  result <- gsub("\\\\n", "\n", result, fixed = TRUE)
  result <- gsub("\\\\r", "\n", result, fixed = TRUE)
  result <- gsub("\r\n", "\n", result, fixed = TRUE)
  
  # =========================================================================
  # ÉTAPE 8 : Nettoyage final
  # =========================================================================
  result <- gsub("^\\s*\\(\\s*", "", result)
  result <- gsub("\\s*\\)\\s*$", "", result)
  result <- gsub("\\s*\\(\\s*\\)", "", result)
  while (grepl("  ", result, fixed = TRUE)) result <- gsub("  ", " ", result, fixed = TRUE)
  result <- trimws(result)
  result <- gsub("\n\\s*\n", "\n", result)
  
  return(result)
}

#' Helper: opérateur null-coalescing
`%||%` <- function(a, b) {
  if (length(a) > 1) return(a)
  if (is.null(a) || isTRUE(is.na(a)) || (is.character(a) && a == "")) b else a
}