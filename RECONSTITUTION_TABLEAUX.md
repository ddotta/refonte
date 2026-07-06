# Reconstitution des tableaux du questionnaire Pogues

## Source des données

Les tableaux affichés dans le questionnaire Pogues sont reconstitués à partir de **deux sources de données** principales :

1. **Le fichier JSON du questionnaire Pogues** (`pogues_*.json`) : définit la structure du questionnaire (variables, modules, dimensions des tableaux, libellés)
2. **Le fichier `interrogations.json`** (REM) : contient les données collectées (COLLECTED) et les paramètres (EXTERNAL) pour chaque unité enquêtée

## Le fichier `interrogations.json`

Ce fichier au format JSON REM (Répertoire des Enquêtes et des Métadonnées) liste toutes les unités enquêtées avec leurs données. Pour chaque unité, on trouve :

- **`EXTERNAL`** : variables de paramétrage qui définissent la structure attendue (ex: `NBLIGNES_TAB_VNB` = nombre de lignes du tableau "Vache Non Bio")
- **`COLLECTED`** : les valeurs collectées sous forme de tableaux JSON (tableau à une dimension)

### Exemple de structure

```json
{
  "displayName": "31460305100215",
  "corporateName": "NOVANDIE",
  "questionnaires": [{
    "questionnaireModelId": "EAL2026X01",
    "questionningData": {
      "EXTERNAL": {
        "NBLIGNES_TAB_VNB": "2",
        "NBLIGNES_TAB_VB": "0",
        ...
      },
      "COLLECTED": {
        "VNB1": { "COLLECTED": ["62", "80"] },
        "VNB2": { "COLLECTED": [87186211, 4165191] },
        "VNB3": { "COLLECTED": [138, 5] },
        "VNB4": { "COLLECTED": ["87 186 211 litres (12 mois déclaré(s))", "4 165 191 litres (12 mois déclaré(s))"] },
        "CALC_DEP_VNB": { "COLLECTED": ["PAS-DE-CALAIS (62)", "SOMME (80)"] }
      }
    }
  }]
}
```

## Mécanisme de reconstitution des tableaux

### 1. Rôle du DDI (Data Documentation Initiative)

Le fichier Pogues JSON est un export au format **DDI** (Data Documentation Initiative) simplifié. Il contient :

- **`variables`** : la définition de chaque variable avec son nom, son type, sa codelist
- **`codelists`** : les listes de codes (ex: liste des départements)
- **`modules`****: les regroupements logiques de questions
- **`flowControl`** : les règles de filtrage et de calcul

C'est **ce fichier Pogues qui fait le lien** entre le nom logique des variables dans `interrogations.json` (ex: `VNB1`) et la structure d'affichage du tableau.

### 2. Rôle de `load_pogues.R`

La fonction `load_pogues()` parse le JSON Pogues et en extrait :

1. **Les variables** : dictionnaire nom → type
2. **Les modules** : regroupements de questions
3. **Les questions** : avec leur type (SIMPLE, CHOICE, TABLE)
4. **Pour les TABLE** : les dimensions (PRIMARY = lignes, MEASURE = colonnes)

```r
# Exemple de structure extraite par load_pogues pour une question TABLE
question = list(
  name = "VNB",
  type = "TABLE",
  label = "Vaches non biologiques - Fonds de collecte",
  dimensions = list(
    list(name = "Département", type = "PRIMARY", size = "NBLIGNES_TAB_VNB"),
    list(name = "Nb producteurs", type = "MEASURE"),
    list(name = "Collecte totale", type = "MEASURE"),
    list(name = "Nb moyen producteurs", type = "MEASURE"),
    list(name = "Collecte en clair", type = "MEASURE")
  ),
  variables = list(
    primary = "VNB1",      # Département
    measures = c("VNB2", "VNB3", "VNB4", "VNB4")  # Nb producteurs, Collecte, etc.
  )
)
```

### 3. Rôle de l'EXTERNAL : taille dynamique des tableaux

Les variables `EXTERNAL` (`NBLIGNES_TAB_*`) définissent le nombre de lignes de chaque tableau **par unité**. Ce nombre est issu du paramétrage de l'enquête (ex: dans une application de gestion des collectes).

```r
# Exemple : chargement et interprétation
nblignes <- as.numeric(EXTERNAL$NBLIGNES_TAB_VNB)  # => 2 lignes pour NOVANDIE
```

C'est ce qui explique pourquoi :
- **NOVANDIE** : `NBLIGNES_TAB_VNB = 2` → 2 lignes (62, 80)
- **ISIGNY STE MERE** : `NBLIGNES_TAB_VNB = 3` → 3 lignes (14, 50, 61)
- **LACTALIS** : `NBLIGNES_TAB_VNB = 59` → 59 lignes

### 4. Rôle des variables collectées (COLLECTED)

Les variables COLLECTED sont nommées selon la convention :

```
<TABLE><NUM_LIGNE>
```

Où :
- `<TABLE>` est le préfixe du tableau (ex: `VNB`, `VB`, `VRAC`, `FINIS_LAITFER`, `PRODBIO`, etc.)
- `<NUM_LIGNE>` est le numéro de la colonne (1 = départements, 2 = collecte totale, 3 = nb producteurs, 4 = collecte en clair, etc.)

### 5. Rôle de `renderers.R`

La fonction `render_table_question()` dans `renderers.R` reconstruit le tableau HTML à partir de :

1. **La structure du Pogues** : dimensions du tableau, libellés des colonnes
2. **La variable EXTERNAL** : nombre de lignes
3. **Les variables COLLECTED** : valeurs numériques/texte par ligne et colonne
4. **Les libellés** via `resolve_vtl()` : résolution des expressions VTL

```r
# Logique simplifiée de reconstruction
render_table_question <- function(q, values) {
  n_rows <- as.numeric(values$EXTERNAL[[paste0("NBLIGNES_TAB_", q$name)]])
  # Pour chaque ligne, pour chaque colonne
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      var_name <- paste0(q$variables$measures[j], i)
      value <- values$COLLECTED[[var_name]]
      # Afficher la cellule
    }
  }
}
```

## Exemple concret : Tableau VNB (Vaches Non Bio)

### Données pour NOVANDIE (314603051)

```json
{
  "EXTERNAL": { "NBLIGNES_TAB_VNB": "2" },
  "COLLECTED": {
    "VNB1": ["62", "80"],          // Colonne 1 : Départements
    "VNB2": [87186211, 4165191],   // Colonne 2 : Collecte en litres
    "VNB3": [138, 5],              // Colonne 3 : Nb producteurs
    "VNB4": ["87 186 211 litres (12 mois)", "4 165 191 litres (12 mois)"],  // Colonne 4 : Libellé collecte
    "CALC_DEP_VNB": ["PAS-DE-CALAIS (62)", "SOMME (80)"]  // Département calculé
  }
}
```

### Rendu dans l'application

| Département | Collecte totale (litres) | Nb producteurs | Collecte |
|---|---|---|---|
| PAS-DE-CALAIS (62) | 87 186 211 | 138 | 87 186 211 litres (12 mois) |
| SOMME (80) | 4 165 191 | 5 | 4 165 191 litres (12 mois) |

### Données pour GROUPE LACTALIS (59 lignes)

```json
{
  "EXTERNAL": { "NBLIGNES_TAB_VNB": "59" },
  "COLLECTED": {
    "VNB1": ["01", "02", "08", "09", "10", "11", "12", "14", ...],  // 59 départements
    "VNB2": [5076946, 13412649, 33443916, 6606180, ...],            // 59 valeurs
    "VNB3": [11, 21, 63, 12, ...],                                   // 59 valeurs
    "VNB4": ["5 076 946 litres (12 mois)", "13 412 649 litres (12 mois)", ...],  // 59 valeurs
    "CALC_DEP_VNB": ["AIN (01)", "AISNE (02)", "ARDENNES (08)", ...]  // 59 départements
  }
}
```

## Autres exemples de tableaux

### Tableau VRAC (Vrac)

```json
{
  "EXTERNAL": { "NBLIGNES_TAB_VRAC": "6" },
  "COLLECTED": {
    "VRAC1": ["162000", "187010", "187100", "197310", "200000", "220000"],
    "SECHVRAC11": 21834677,   // Variable unique (pas un tableau à plusieurs lignes)
    ...
  }
}
```

Ici le tableau VRAC a 1 seule colonne de saisie (VRAC1) et un total calculé (SECHVRAC11).

### Tableau PRODBIO (Produits Bio)

```json
{
  "EXTERNAL": { "NBLIGNES_TAB_PRODBIO": "3" },
  "COLLECTED": {
    "PRODBIO1": ["AB5110", "AB5141", "AB5200"],   // 3 produits bio
    "PRODBIO3": [null, null, "1 474 595 kg (7 mois déclaré(s))"]
  }
}
```

## Variables calculées (préfixe `CALC_`)

Certaines variables commençant par `CALC_` sont des **variables calculées** :

| Variable | Signification | Données NOVANDIE |
|---|---|---|
| `CALC_DEP_VNB` | Libellé département VNB | `["PAS-DE-CALAIS (62)", "SOMME (80)"]` |
| `CALC_DEP_VB` | Libellé département VB | `[null]` |
| `CALC_UNITE_PRODBIO` | Unité des produits bio | `["KG", "KG", "KG"]` |

Ces variables sont produites par le système de collecte (Capibara/Eno) à partir des données saisies.

## Flux de bout en bout

```
1. interrogations.json (REM)
   ├── EXTERNAL         → nombre de lignes des tableaux
   └── COLLECTED        → valeurs collectées par variable

2. pogues_*.json (DDI)
   ├── dimensions        → structure du tableau (lignes × colonnes)
   ├── variables         → noms logiques (VNB1, VNB2, ...)
   └── codelists         → libellés des codes

3. load_pogues.R         → parse le DDI
   ├── build_module_order() → ordre des modules
   └── extrait dimensions   → PRIMARY = lignes, MEASURE = colonnes

4. import_data.R         → charge les données REM
   ├── load_unit_data()     → filtre les données d'une unité
   └── extrait EXTERNAL + COLLECTED

5. renderers.R           → rendu UI
   ├── render_table_question() → génère le tableau HTML
   └── resolve_vtl()          → résout les expressions VTL

6. Base SQLite            → persistance
   ├── reponses             → sauvegarde des réponses modifiées
   └── enquetes             → session d'enquête
```

## Résumé

Les tableaux ne sont pas reconstitués à partir d'un fichier DDI XML traditionnel, mais à partir du **JSON Pogues** qui est un format DDI simplifié (export Pogues → Eno → JSON). Ce JSON contient :

- La **structure** du questionnaire (modules, questions, dimensions)
- Les **variables** avec leurs noms logiques
- Les **codelists** pour les libellés

Les **données** proviennent du fichier `interrogations.json` (REM) qui contient les valeurs collectées par unité enquêtée, organisées par nom de variable.

La **taille dynamique** des tableaux (chaque unité peut avoir un nombre différent de lignes) est déterminée par les variables **EXTERNAL** (`NBLIGNES_TAB_*`), ce qui permet une structure adaptée à chaque unité enquêtée.