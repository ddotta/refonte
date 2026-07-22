# ==============================================================================
# RÉSOLUTION D'EXPRESSIONS VTL
# ==============================================================================

#' Résout les références VTL : remplace $VAR$, évalue if/then/else, nettoie les expressions VTL
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
  for (iter in 1:10) {
    m <- tryCatch(str_match_all(result, "\\$([A-Z_][A-Z0-9_]*)\\$")[[1]], error = function(e) matrix(nrow = 0, ncol = 2))
    if (!is.matrix(m) || nrow(m) == 0) break

    for (i in 1:nrow(m)) {
      varname <- m[i, 2]
      pattern_full <- m[i, 1]

      val <- env[[varname]]
      if (!is.null(val)) {
        if (is.list(val)) val <- val[[1]]
        if (length(val) > 1) val <- val[[1]]
      }
      if (!is.null(val) && length(val) == 1 && !is.na(val)) {
        replacement <- as.character(val)
        if (nzchar(replacement)) {
          result <- gsub(pattern_full, replacement, result, fixed = TRUE)
        } else {
          result <- gsub(pattern_full, "", result, fixed = TRUE)
        }
      } else {
        result <- gsub(pattern_full, "", result, fixed = TRUE)
      }
    }
  }
  # Nettoyer les $VAR$ non résolus restants
  result <- gsub("\\$[A-Z_][A-Z0-9_]*\\$", "", result, perl = TRUE)

  # =========================================================================
  # ÉTAPE 2 : Résoudre les fonctions VTL (isnull, nvl, not, etc.)
  # AVANT if/then/else pour simplifier les conditions
  # =========================================================================

  # 2a. isnull() vide -> "true" (la variable absente n'a pas de valeur)
  result <- gsub("isnull\\(\\s*\\)", "true", result, perl = TRUE)
  # 2b. isnull(0) -> "false" (zéro est une valeur)
  result <- gsub("isnull\\(\\s*\"?0\"?\\s*\\)", "false", result, perl = TRUE)
  # 2c. isnull(nombre) -> "false" pour tout nombre non nul
  # On itère pour traiter les isnull restants
  for (i in 1:20) {
    prev <- result
    # isnull(valeur) -> false quand il y a une valeur (non vide)
    result <- gsub("isnull\\(\\s*(\\S+)\\s*\\)", "false", result, perl = TRUE)
    result <- gsub("isnull\\(\\s*\\)", "true", result, perl = TRUE)
    if (identical(prev, result)) break
  }

  # 2d. nvl(vide, default) -> default
  result <- gsub("nvl\\(\\s*,\\s*([^)]*?)\\s*\\)", "\\1", result, perl = TRUE)
  # 2e. nvl(expr, default) -> expr (itération pour traiter les imbrications)
  for (i in 1:20) {
    prev <- result
    result <- gsub("nvl\\(\\s*([^,]+)\\s*,\\s*([^)]*?)\\s*\\)", "\\1", result, perl = TRUE)
    result <- gsub("nvl\\(\\s*,\\s*([^)]*?)\\s*\\)", "\\1", result, perl = TRUE)
    if (identical(prev, result)) break
  }

  # 2f. not(true) -> false, not(false) -> true
  result <- gsub("not\\s*\\(\\s*true\\s*\\)", "false", result, perl = TRUE)
  result <- gsub("not\\s*\\(\\s*false\\s*\\)", "true", result, perl = TRUE)
  result <- gsub("\\bnot\\s+true\\b", "false", result, perl = TRUE)
  result <- gsub("\\bnot\\s+false\\b", "true", result, perl = TRUE)

  # 2g. cast(,"type") ou cast( ,"type") vides -> ""
  result <- gsub("cast\\(\\s*,\\s*\"[A-Za-z]+\"\\s*\\)", "", result, perl = TRUE)
  result <- gsub("cast\\(\\s*,\\s*[A-Za-z]+\\s*\\)", "", result, perl = TRUE)
  result <- gsub("cast\\(\\s*\\)", "", result, perl = TRUE)
  # 2h. cast(expr, type) -> expr
  result <- gsub("cast\\(\\s*([^,]+)\\s*,\\s*(?:\"[A-Za-z]+\"|[A-Za-z]+)\\s*\\)", "\\1", result, perl = TRUE)

  # 2i. Autres fonctions VTL
  result <- gsub("normalize-whitespace\\(([^)]*)\\)", "\\1", result, perl = TRUE)
  result <- gsub("number\\(([^)]*)\\)", "\\1", result, perl = TRUE)
  result <- gsub("string\\(([^)]*)\\)", "\\1", result, perl = TRUE)
  result <- gsub("boolean\\(([^)]*)\\)", "\\1", result, perl = TRUE)
  result <- gsub("contains\\(([^)]*)\\)", "\\1", result, perl = TRUE)
  result <- gsub("sum\\(([^)]*)\\)", "\\1", result, perl = TRUE)
  result <- gsub("left_join\\(([^,]+),([^)]+)\\)", "\\1", result, perl = TRUE)

  # =========================================================================
  # ÉTAPE 3 : Évaluer les structures if/then/else
  # =========================================================================
  result <- .evaluate_vtl_conditions(result)

  # =========================================================================
  # ÉTAPE 4 : Concaténation ||
  # =========================================================================
  result <- gsub("\\s*\\|\\|\\s*", "", result, perl = TRUE)

  # =========================================================================
  # ÉTAPE 5 : Nettoyer les résidus
  # =========================================================================
  result <- gsub('"="', "=", result, fixed = TRUE)
  result <- gsub('"true"', "true", result, fixed = TRUE)
  result <- gsub('"false"', "false", result, fixed = TRUE)
  result <- gsub("=true", "", result, fixed = TRUE)
  result <- gsub("=false", "", result, fixed = TRUE)
  result <- gsub("==true", "", result, fixed = TRUE)
  result <- gsub("==false", "", result, fixed = TRUE)
  result <- gsub("\\btrue\\b", "", result, perl = TRUE)
  result <- gsub("\\bfalse\\b", "", result, perl = TRUE)
  result <- gsub(",string", "", result, fixed = TRUE)
  result <- gsub(",integer", "", result, fixed = TRUE)
  result <- gsub(",number", "", result, fixed = TRUE)
  result <- gsub("\\band\\b", "", result, perl = TRUE)
  result <- gsub("\\bor\\b", "", result, perl = TRUE)
  result <- gsub("<>", "", result, fixed = TRUE)
  result <- gsub("\\s*\\(\\s*\\)", "", result, perl = TRUE)

  # =========================================================================
  # ÉTAPE 6 : Markdown simple (**text** -> <strong>)
  # =========================================================================
  result <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", result, perl = TRUE)

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
  result <- gsub("^\\s*\\(\\s*", "", result, perl = TRUE)
  result <- gsub("\\s*\\)\\s*$", "", result, perl = TRUE)
  result <- gsub("\\s*\\(\\s*\\)", "", result, perl = TRUE)
  while (grepl("  ", result, fixed = TRUE)) result <- gsub("  ", " ", result, fixed = TRUE)
  result <- trimws(result)
  result <- gsub("\n\\s*\n", "\n", result, perl = TRUE)

  return(result)
}

# ------------------------------------------------------------------------------
# Helper interne : évalue récursivement les structures if-then-else VTL
# après nettoyage des fonctions (isnull, nvl, cast, etc.).
# Utilise une approche par extraction manuelle des blocs if/then/else
# plutôt que des appels gsub avec fonction de remplacement (qui ne passent
# pas les groupes de capture individuels en R de base).
# ------------------------------------------------------------------------------
.evaluate_vtl_conditions <- function(txt) {
  max_iter <- 100
  iter <- 0
  prev <- txt

  while (iter < max_iter) {
    iter <- iter + 1

    # -------------------------------------------------------------------
    # Pattern : (if (cond) then "texte1" else "texte2")
    # On cherche la première occurrence, on l'évalue, on remplace,
    # et on recommence.
    # -------------------------------------------------------------------
    res <- .extract_and_eval_first_if(txt)
    if (is.null(res)) break
    txt <- res
    if (identical(txt, prev)) break
    prev <- txt
  }

  # Supprimer les mots-clés résiduels if/then/else
  txt <- gsub("\\bif\\b", "", txt, perl = TRUE)
  txt <- gsub("\\bthen\\b", "", txt, perl = TRUE)
  txt <- gsub("\\belse\\b", "", txt, perl = TRUE)

  return(txt)
}

# ------------------------------------------------------------------------------
# Extrait et évalue le premier bloc if/then/else trouvé dans la chaîne.
# Retourne la chaîne modifiée, ou NULL si aucun bloc trouvé.
# ------------------------------------------------------------------------------
.extract_and_eval_first_if <- function(txt) {
  # Pattern complet pour if/then/else avec guillemets autour des branches
  pattern <- paste0(
    "\\(\\s*if\\s*\\(([^()]*)\\)\\s*",   # (if (cond)
    "then\\s*\"([^\"]*)\"\\s*",           # then "texte1"
    "else\\s*\"([^\"]*)\"\\s*\\)"         # else "texte2")
  )

  m <- regexpr(pattern, txt, perl = TRUE)
  if (m == -1 || length(m) == 0) {
    # Essayer sans parenthèses externes
    pattern <- paste0(
      "if\\s*\\(([^()]*)\\)\\s*",          # if (cond)
      "then\\s*\"([^\"]*)\"\\s*",          # then "texte1"
      "else\\s*\"([^\"]*)\""               # else "texte2"
    )
    m <- regexpr(pattern, txt, perl = TRUE)
    if (m == -1 || length(m) == 0) {
      # Essayer sans guillemets (avec parenthèses externes)
      pattern <- paste0(
        "\\(\\s*if\\s*\\(([^()]*)\\)\\s*",  # (if (cond)
        "then\\s*(.*?)\\s*",                # then texte
        "else\\s*(.*?)\\s*\\)"              # else texte)
      )
      m <- regexpr(pattern, txt, perl = TRUE)
      if (m == -1 || length(m) == 0) {
        # Essayer sans guillemets ni parenthèses externes
        pattern <- paste0(
          "if\\s*\\(([^()]*)\\)\\s*",       # if (cond)
          "then\\s*(.*?)\\s*",               # then texte
          "else\\s*(.*?)$"                   # else texte (jusqu'à fin)
        )
        m <- regexpr(pattern, txt, perl = TRUE)
        if (m == -1 || length(m) == 0) {
          # Essayer if-then sans else
          pattern <- paste0(
            "if\\s*\\(([^()]*)\\)\\s*",      # if (cond)
            "then\\s*(.*?)\\s*(?:\\(|$)"     # then texte (arrêt avant '(' ou fin)
          )
          m <- regexpr(pattern, txt, perl = TRUE)
          if (m == -1 || length(m) == 0) {
            return(NULL)
          }
        }
      }
    }
  }

  # Extraire les captures
  caps <- attr(m, "capture.start", exact = TRUE)
  cap_len <- attr(m, "capture.length", exact = TRUE)

  n_caps <- nrow(caps)
  cond <- if (n_caps >= 1 && cap_len[1, 1] > 0) substr(txt, caps[1, 1], caps[1, 1] + cap_len[1, 1] - 1) else ""
  txt_then <- if (n_caps >= 2 && cap_len[1, 2] > 0) substr(txt, caps[1, 2], caps[1, 2] + cap_len[1, 2] - 1) else ""
  txt_else <- if (n_caps >= 3 && cap_len[1, 3] > 0) substr(txt, caps[1, 3], caps[1, 3] + cap_len[1, 3] - 1) else ""

  # Évaluer la condition et choisir la branche
  replacement <- if (.vtl_cond_is_true(cond)) txt_then else txt_else

  # Remplacer le bloc complet
  full_len <- attr(m, "match.length", exact = TRUE)
  full_start <- m[1]
  paste0(
    if (full_start > 1) substr(txt, 1, full_start - 1) else "",
    replacement,
    substr(txt, full_start + full_len, nchar(txt))
  )
}

# ------------------------------------------------------------------------------
# Helper : évalue une condition VTL simplifiée (chaîne de caractères)
# Retourne TRUE si la condition est vraie, FALSE sinon
# ------------------------------------------------------------------------------
.vtl_cond_is_true <- function(cond_str) {
  cond <- trimws(cond_str)
  if (cond == "" || cond == "true" || cond == "1") return(TRUE)
  if (cond == "false" || cond == "0") return(FALSE)

  # Comparaisons : val1 op val2
  num_ops <- c(">=", "<=", "!=", "<>", "=", ">", "<")
  for (op in num_ops) {
    if (grepl(op, cond, fixed = TRUE)) {
      parts <- strsplit(cond, op, fixed = TRUE)[[1]]
      if (length(parts) == 2) {
      v1 <- trimws(gsub('"', '', parts[1]))
      v2 <- trimws(gsub('"', '', parts[2]))
      # Comparaison numérique
        n1 <- suppressWarnings(as.numeric(v1))
        n2 <- suppressWarnings(as.numeric(v2))
        if (!is.na(n1) && !is.na(n2)) {
          if (op == "=" || op == "==") return(n1 == n2)
          if (op == "!=" || op == "<>") return(n1 != n2)
          if (op == ">") return(n1 > n2)
          if (op == "<") return(n1 < n2)
          if (op == ">=") return(n1 >= n2)
          if (op == "<=") return(n1 <= n2)
        }
        # Comparaison de chaînes
        if (op == "=" || op == "==") return(v1 == v2)
        if (op == "!=" || op == "<>") return(v1 != v2)
      }
    }
  }

  # Par défaut : chaîne non vide = true
  return(nchar(cond) > 0)
}

#' Helper: opérateur null-coalescing
`%||%` <- function(a, b) {
  if (length(a) > 1) return(a)
  if (is.null(a) || isTRUE(is.na(a)) || (is.character(a) && a == "")) b else a
}