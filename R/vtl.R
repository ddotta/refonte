# ==============================================================================
# RÉSOLUTION D'EXPRESSIONS VTL
# ==============================================================================

#' Résout les références VTL simples (variables $VAR$ et concaténation ||)
#' et nettoie les expressions complexes (if-then-else, cast, number, etc.)
#' @param expr Expression VTL à résoudre
#' @param env Liste nommée des valeurs des variables
#' @return Chaîne résolue
resolve_vtl <- function(expr, env) {
  if (is.null(expr) || is.na(expr) || expr == "") return("")
  
  result <- expr
  
  # Gérer le cas où expr est un vecteur : prendre le premier élément
  if (length(result) > 1) {
    result <- result[1]
  }
  
  result <- as.character(result)
  
  # ==============================================================
  # ÉTAPE 1 : Remplacer les références $VAR$ par leur valeur
  # ==============================================================
  matches <- str_match_all(result, "\\$([A-Z_]+[A-Z0-9_]*)\\$")[[1]]
  if (nrow(matches) > 0) {
    for (i in 1:nrow(matches)) {
      var_name <- matches[i, 2]
      val <- env[[var_name]]
      if (!is.null(val)) {
        if (is.list(val)) {
          val <- val[[1]]
        }
        if (!is.null(val) && !is.na(val)) {
          result <- gsub(paste0("\\$", var_name, "\\$"), as.character(val), result, fixed = TRUE)
        }
      }
    }
  }
  
  # ==============================================================
  # ÉTAPE 2 : Nettoyer toutes les fonctions VTL
  # ==============================================================
  
  # cast(expr, type) → expr (garder le contenu entre cast( et ,type)
  result <- gsub("cast\\(([^)]+),\\s*[a-zA-Z]+\\)", "\\1", result)
  
  # if (condition) then text1 else text2 → text1 (conserver l'alternative then)
  result <- gsub("if\\s*\\([^)]*\\)\\s*then\\s*", "", result, perl = TRUE)
  
  # Supprimer le "else ..." de fin
  result <- gsub("\\s*else\\s*(\"[^\"]*\"|[^)]*)$", "", result)
  result <- gsub("\\s*else\\s*\\)", ")", result)
  result <- gsub("\\s*else\\s*$", "", result)
  
  # number() → contenu
  result <- gsub("number\\(([^)]*)\\)", "\\1", result)
  # string() → contenu
  result <- gsub("string\\(([^)]*)\\)", "\\1", result)
  # boolean() → contenu
  result <- gsub("boolean\\(([^)]*)\\)", "\\1", result)
  # normalize-whitespace() → contenu
  result <- gsub("normalize-whitespace\\(([^)]*)\\)", "\\1", result)
  # contains() → contenu
  result <- gsub("contains\\(([^)]*)\\)", "\\1", result)
  # nvl() → remplacer par la valeur par défaut
  result <- gsub("nvl\\(([^,]+),([^)]+)\\)", "\\2", result)
  # not → supprimer
  result <- gsub("not\\s+", "", result)
  # sum() → contenu
  result <- gsub("sum\\(([^)]*)\\)", "\\1", result)
  # left_join() → premier argument
  result <- gsub("left_join\\(([^,]+),([^)]+)\\)", "\\1", result)
  # isnull() → false
  result <- gsub("isnull\\(([^)]+)\\)", "false", result)
  
  # ==============================================================
  # ÉTAPE 3 : Nettoyer l'opérateur || (concaténation)
  # ==============================================================
  result <- gsub('\\s*\\|\\|\\s*', '', result)
  
  # ==============================================================
  # ÉTAPE 4 : Nettoyer les guillemets
  # ==============================================================
  result <- gsub('^"|"$', '', result)
  result <- gsub('""', '', result)
  result <- gsub('\\\\r\\\\n', '\n', result)
  result <- gsub('\\\\n', '\n', result)
  result <- gsub('\\\\r', '\n', result)
  
  # Supprimer les guillemets doubles restants
  result <- gsub('"', '', result)
  
  # Nettoyer les espaces multiples
  result <- gsub('\\s+', ' ', result)
  result <- trimws(result)
  
  # Nettoyer les ==
  result <- gsub('==', '', result)
  # Nettoyer true/false
  result <- gsub('\\btrue\\b|\\bfalse\\b', '', result)
  
  result <- trimws(gsub('\\s+', ' ', result))
  
  result
}

#' Helper: opérateur null-coalescing
`%||%` <- function(a, b) {
  if (length(a) > 1) return(a)
  if (is.null(a) || isTRUE(is.na(a)) || (is.character(a) && a == "")) b else a
}