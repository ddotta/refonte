# Documentation des flux de données — Application Questionnaire Pogues

## 1. Vue d'ensemble

L'application Shiny "Questionnaire Pogues" permet aux gestionnaires de consulter et modifier les données d'enquête (Prix Grumes, EAL, etc.). Elle s'appuie sur deux sources de données principales :

| Source | Format | Rôle |
|--------|--------|------|
| Fichiers REM (Répertoire des Enquêtes et Métadonnées) | JSON / CSV | Données d'initialisation : unités, réponses collectées, statuts, métadonnées |
| Base SQLite locale | `.db` | Persistance des modifications saisies par les gestionnaires via l'IHM |

Le schéma ci-dessous résume l'architecture globale des flux :

```
┌─────────────────────────────────────────────────────────────────────┐
│                    SOURCES DE DONNÉES (REM)                          │
│                                                                      │
│  ../<Enquete>/insee/REM/                                             │
│  ├── interrogations.json    ← Unités + Réponses (COLLECTED/EXTERNAL) │
│  ├── context.json           ← Métadonnées enquête                    │
│  └── interrogations.csv     ← Statuts d'interrogation                │
│                                                                      │
│  ../<Enquete>/insee/reprise/historique_external/                     │
│  └── edited_previous_EAL.json ← Données N-1                          │
│                                                                      │
│  ../<Enquete>/insee/datacollection/                                  │
│  └── pogues_*.json          ← Structure du questionnaire             │
└───────────────────────┬─────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    APPLICATION SHINY                                  │
│                                                                      │
│  ┌─────────────┐    ┌──────────────────┐    ┌───────────────────┐   │
│  │ import_data.R│    │questionnaire_pogues│   │  renderers.R      │   │
│  │ (chargement) │    │   _module.R/server │   │  (affichage)      │   │
│  └──────┬───────┘    └────────┬─────────┘    └───────────────────┘   │
│         │                     │                                       │
│         │    ┌────────────────▼──────────┐                           │
│         │    │  env_vars (reactiveValues) │  ← Mémoire volatile       │
│         │    │  - Variables N (COLLECTED) │                           │
│         │    │  - Variables N1__ (N-1)    │                           │
│         │    │  - UNIT_* (détails unité)  │                           │
│         │    └────────────────┬──────────┘                           │
│         │                     │                                       │
│         │    ┌────────────────▼──────────┐                           │
│         └────►     database.R             │                           │
│              │  SQLite (persistance)      │                           │
│              │  - enquetes                │                           │
│              │  - reponses                │                           │
│              └────────────────────────────┘                           │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 2. Fichiers sources détaillés

### 2.1 `interrogations.json` — Données REM (initialisation, prioritaire)

**Chemin** : `../<Enquete>/insee/REM/interrogations.json`

**Rôle** : Source principale des données d'unité et des réponses collectées.

**Structure** :

```json
[
  {
    "displayName": "30093493200017",     // SIRET
    "originId": "...",
    "corporateName": "ENTREPRISE EXEMPLE",
    "ape": "02.20Z",
    "address": {
      "streetNumber": "1",
      "streetType": "RUE",
      "streetName": "DE LA FORET",
      "addressSupplement": "",
      "cityName": "PARIS",
      "zipCode": "75001",
      "cedexCode": "",
      "cedexName": "",
      "countryName": "France"
    },
    "contacts": [{ "firstName": "Jean", "lastName": "Dupont", "email": "...", "phoneNumbers": [...] }],
    "questionnaires": [{
      "questionningData": {
        "EXTERNAL": {                         // Variables calculées / externes
          "NBLIGNES_TAB_QUESTION1": "5",      // Ex: nombre de lignes d'un tableau
          "ANNEE_REFERENCE": "2025"
        },
        "COLLECTED": {                        // Réponses saisies par l'enquêté
          "VNB1": { "COLLECTED": ["75", "75", "75", "75"] },
          "PRODUCTION": { "COLLECTED": ["1200", "3400", "2100", "800"] },
          "SP": { "COLLECTED": "S2" }
        }
      }
    }]
  }
]
```

**Utilisation dans l'application** :

| Fonction | Fichier | Ce qui est extrait |
|----------|---------|-------------------|
| `load_survey_units_from_rem()` | `import_data.R` | Liste des unités (SIRET, raison sociale, adresse, contact, APE) |
| `load_unit_data_from_rem()` | `import_data.R` | Variables EXTERNAL (calculées) et COLLECTED (réponses), stockées dans `env_vars` |
| `get_unit_info_from_rem()` | `import_data.R` | Détails d'une unité spécifique pour le panneau d'information |

**Mapping des données vers `env_vars`** :

```
EXTERNAL → env_vars[[NOM_VARIABLE]]          ← scalaire (ex: "5")
COLLECTED (scalaire) → env_vars[[NOM_VARIABLE]] ← caractère (ex: "S2")
COLLECTED (liste) → env_vars[[NOM_VARIABLE]]    ← liste indexée (ex: list("1200", "3400", ...))
```

### 2.2 `context.json` — Métadonnées de l'enquête

**Chemin** : `../<Enquete>/insee/REM/context.json`

**Rôle** : Informations contextuelles affichées à l'écran d'accueil (objectifs, calendrier, contacts).

**Utilisation** :
- `load_survey_context()` → parse le JSON
- `extract_survey_dates()` → dates de collecte, relances, mise en demeure
- `extract_survey_metadata()` → code enquête, année, référents

### 2.3 `interrogations.csv` — Statuts d'interrogation

**Chemin** : `../<Enquete>/interrogations_<Enquete>.csv`

**Rôle** : Statuts des unités (À expertiser, En cours, Validé, etc.).

**Colonnes principales** :

| Colonne | Description |
|---------|-------------|
| `surveyUnitId` | SIRET |
| `highestEventType` | Code statut REM (INITLA, PARTIELINT, VALID, HC…) |
| `highestEventDate` | Date du dernier événement |
| `isOnProbation` | Unité en probation ? |

**Mappe des statuts** (`map_event_type()` dans `import_data.R`) :

| Groupe UI  | Codes inclus | Signification                    |
|------------|--------------|----------------------------------|
| Non reçues | INITLA       | En attente de réponse            |
| Non reçues | PARTIELINT   | Démarrée internet                |
| Non reçues | PARTIELPAP   | Démarrée papier                  |
| Reçues     | HC           | Hors champ                       |
| Reçues     | REFUSAL      | Refus de réponse                 |
| Reçues     | WASTE        | Déchet                           |
| Reçues     | RECUPAP      | Questionnaire papier réceptionné |
| Reçues     | VALINT       | Validée internet                 |
| Reçues     | VALPAP       | Validée papier                   |
| En reprise | EXPERT       | À expertiser                     |
| En reprise | ONGEXPERT    | En cours d'expertise             |
| En reprise | VALID        | Score qualité maximum            |
| En reprise | ENDEXPERT    | Expertise terminée               |
| En reprise | NOQUAL       | Pas de score calculé             |

### 2.4 `pogues_*.json` — Structure du questionnaire

**Chemin** : `../<Enquete>/insee/datacollection/pogues_*.json`

**Rôle** : Définition de la structure du questionnaire généré par Pogues.

**Contenu** :
- `Variables` : liste des variables avec leur type (TEXT, NUMERIC, BOOLEAN), unité, bornes min/max
- `CodeLists` : listes de codes (ex: départements, essences)
- `Child` : arborescence des modules et questions (SIMPLE, SINGLE_CHOICE, MULTIPLE_CHOICE, TABLE)
- `FlowControl` : règles de filtrage conditionnel
- `ResponseStructure` : dimensions des tableaux (PRIMARY, MEASURE), mappings cellule→variable

**Utilisation** : `load_pogues()` (dans `load_pogues.R`) parse le JSON et produit une liste R structurée utilisée par `renderers.R` pour générer l'interface.

**Mapping des cellules de tableau** :

Chaque question de type TABLE définit un mapping entre les coordonnées (ligne, colonne) et les noms de variables collectées. Exemple :

```
MappingTarget "1 1" → MappingSource "mm21i46a" → CollectedVariableReference → variable "VNB1"
```

Ce mapping est stocké dans `q$var_mapping` et utilisé par `get_table_var_name()` pour retrouver le nom de variable à partir de (row, col).

### 2.5 Exports quotidiens Kraftwerk — Données saisies par les enquêtés (fallback)

**Chemin** : `../<Enquete>/data/<YYYY_MM_DD_HH_MM_SS>/`

*Exemple* : `../EAL/data/2026_06_05_07_01_38/`

**Rôle** : Exports quotidiens des réponses saisies par les enquêtés sur la plateforme Kraftwerk. Ces fichiers sont utilisés comme source alternative lorsque les données REM (`interrogations.json`) ne sont pas disponibles pour une unité.

**Structure du répertoire quotidien** :

Chaque export contient un script SAS `import.sas` qui référence un ensemble de fichiers CSV (un par module/questionnaire) :

```
2026_06_05_07_01_38/
├── import.sas                        ← Script d'import SAS (documentation du schéma)
├── EAL2026X01_RACINE.csv             ← Table racine (métadonnées, variables calculées, totaux)
├── EAL2026X01_VNB.csv                ← Module "Vaches nourrices brebis"
├── EAL2026X01_VB.csv                 ← Module "Vaches brebis"
├── EAL2026X01_CB.csv                 ← Module "Chèvres brebis"
├── EAL2026X01_CNB.csv               ← Module "Chèvres nourrices brebis"
├── EAL2026X01_BB.csv                 ← Module "Brebis brebis"
├── EAL2026X01_BNB.csv               ← Module "Brebis nourrices brebis"
├── EAL2026X01_FINIS_LAITCOND.csv    ← Module "Produits finis - Lait conditionné"
├── EAL2026X01_FINIS_LAITFER.csv     ← Module "Produits finis - Lait fermenté"
├── EAL2026X01_FINIS_CREME.csv       ← Module "Produits finis - Crème"
├── EAL2026X01_VRAC.csv              ← Module "Vrac"
├── EAL2026X01_PRODBIO.csv           ← Module "Produits bio"
├── EAL2026X01_NBLIGNES.csv          ← (si présent) Nombre de lignes par tableau
└── ...
```

**Format des CSV Kraftwerk** :

- Séparateur : point-virgule (`;`)
- Encodage : UTF-8
- Chaque ligne correspond à une unité enquêtée (identifiée par `usualSurveyUnitId`)
- Colonnes systématiques : `interrogationId`, `usualSurveyUnitId`
- Colonnes de données : variables du questionnaire (ex: `VNB1`, `VNB2`, `PRODUCTION`, etc.)
- Colonnes `_STATE` : statut de collecte de chaque variable (filtrées au chargement)
- Colonnes `FILTER_RESULT_*` : résultats des filtres (filtrées au chargement)

**Utilisation dans l'application** :

| Fonction | Fichier | Ce qui est extrait |
|----------|---------|-------------------|
| `list_data_files()` | `import_data.R` | Liste les répertoires d'export, sélectionne le plus récent |
| `load_csv_data()` | `import_data.R` | Charge un CSV en filtrant les colonnes `_STATE` et `FILTER_RESULT_*` |
| `load_unit_data()` | `import_data.R` | Extrait les données d'une unité spécifique depuis les CSV |

**Mécanisme de sélection du répertoire** :

```r
# Dans list_data_files() :
data_dirs <- list.files(base, pattern = "^[0-9_]+$", full.names = TRUE)
latest_dir <- sort(data_dirs, decreasing = TRUE)[1]  # ← le plus récent
```

L'application sélectionne automatiquement le répertoire d'export le plus récent (tri alphabétique décroissant sur `YYYY_MM_DD_HH_MM_SS`).

**Priorité REM vs Kraftwerk** :

```
1. interrogations.json (REM)     ← Prioritaire : données officielles du référentiel
2. Exports Kraftwerk (CSV)       ← Fallback : utilisé uniquement si REM indisponible
```

Le fallback CSV est déclenché dans `load_unit_data()` lorsque `load_unit_data_from_rem()` retourne une liste vide (unité non trouvée dans REM, ou fichier REM inexistant).

**Traitement des variables calculées** :

Les colonnes préfixées `CALC_` (ex: `CALC_NBLIGNES_TAB_VNB`, `CALC_AFF_VNB_PREC`) dans `RACINE.csv` contiennent des variables externes/calculées qui sont chargées comme des variables EXTERNAL (équivalentes à celles de `interrogations.json` → `questionningData.EXTERNAL`).

**Colonnes ignorées au chargement** :

| Pattern | Raison |
|---------|--------|
| `*_STATE` | Statut de collecte (interne Kraftwerk) |
| `FILTER_RESULT_*` | Résultat de filtre (interne Kraftwerk) |
| `interrogationId` | UUID technique |
| `validationDate` | Date de validation |
| `questionnaireState` | État du questionnaire |

### 2.6 `edited_previous_EAL.json` — Données N-1

**Chemin** : `../<Enquete>/insee/reprise/historique_external/edited_previous_EAL.json`

**Rôle** : Données de l'année précédente (N-1), utilisées pour comparaison et correction.

**Structure** :

```json
{
  "editedPrevious": [
    {
      "interrogationId": "...",
      "ANNEE_DONNEES": "2024",
      "VNB1": ["75", "75", "75"],
      "PRODUCTION": ["1100", "3200", "2000"],
      ...
    }
  ]
}
```

**Appariement avec l'unité courante** :

La fonction `load_n1_data()` (dans `import_data.R`) identifie l'entrée N-1 correspondante en comparant les départements `VNB1` de l'unité courante avec ceux des entrées N-1. Si les listes de départements correspondent exactement, les données N-1 sont chargées.

**Stockage** : Les données N-1 sont stockées dans `env_vars[["_N1_DATA_"]]` sous forme de liste associative (`nom_variable → valeur`).

---

## 3. Base de données SQLite — Persistance des modifications

### 3.1 Architecture

Deux bases SQLite distinctes sont créées automatiquement, une par enquête :

| Base | Chemin |
|------|--------|
| Prix Grumes | `prixgrumes_questionnaire.db` |
| EAL | `eal_questionnaire.db` |

Chaque base contient deux tables :

```
┌─────────────────────────────────┐     ┌─────────────────────────────────┐
│           enquetes               │     │           reponses              │
├─────────────────────────────────┤     ├─────────────────────────────────┤
│ id (PK)          TEXT            │──┐  │ id (PK)          INTEGER        │
│ questionnaire_id TEXT            │  │  │ questionnaire_id TEXT (FK)      │
│ source_name      TEXT            │  │  │ enquete_id       TEXT (FK)      │◄─┐
│ statut           TEXT            │  │  │ variable_name    TEXT            │  │
│ created_at       TIMESTAMP       │  │  │ valeur           TEXT            │  │
│ updated_at       TIMESTAMP       │  │  │ ligne            INTEGER         │  │
└─────────────────────────────────┘  │  │ colonne          INTEGER         │  │
                                     │  │ updated_at       TIMESTAMP       │  │
                                     │  └─────────────────────────────────┘  │
                                     └───────────────────────────────────────┘
                                        UNIQUE(questionnaire_id, enquete_id,
                                               variable_name, ligne, colonne)
```

### 3.2 Convention de nommage

**`enquetes.id`** : Format `<NomEnquete>_<SIRET>_<Timestamp>`

Exemple : `PRIXGRUMES_30093493200017_20260709_143000`

- Si une enquête existe déjà pour la même paire (enquête, SIRET), elle est **réutilisée** (les réponses précédentes sont rechargées).
- Sinon, un nouvel ID est créé avec le timestamp courant.

### 3.3 Variables stockées

Le préfixe du `variable_name` dans la table `reponses` indique la nature de la donnée :

| Préfixe | Exemple | Signification |
|---------|---------|---------------|
| *(aucun)* | `PRODUCTION_FR_CHN13` | Variable N standard (collectée) |
| `N1__` | `N1__PRODUCTION_FR_CHN13` | Correction N-1 saisie par le gestionnaire |
| `UNIT_` | `UNIT_STREET_NAME` | Détail d'unité modifié (adresse, contact) |
| `_NROWS_` | `_NROWS_QUESTION1` | Nombre de lignes d'un tableau (après ajout/suppression) |
| `_N1EDIT_` | `_N1EDIT_QUESTION1` | État du toggle d'édition N-1 (0 ou 1) |

---

## 4. Flux de données complet

### 4.1 Phase 1 : Démarrage de l'enquête

```
1. Utilisateur sélectionne une enquête (ex: "EAL")
2. Chargement du contexte : context.json → métadonnées affichées
3. Chargement des unités : interrogations.json → liste des SIRET avec statuts
4. L'utilisateur sélectionne une unité
```

### 4.2 Phase 2 : Initialisation des données

```
5. Chargement du questionnaire : pogues_*.json → structure (modules, variables)
6. Création/ouverture de la base SQLite : eal_questionnaire.db
7. Recherche d'un enquete_id existant (même enquête + même SIRET)
   → Si trouvé : réutilisation de l'ID, les réponses précédentes seront rechargées
   → Sinon : création d'un nouvel ID avec timestamp
8. Chargement des données unité depuis interrogations.json (initialisation REM) :
   ├── EXTERNAL → env_vars[NOM_VARIABLE]
   └── COLLECTED → env_vars[NOM_VARIABLE]
9. Chargement et fusion des données Kraftwerk (export quotidien le plus récent) :
   ├── Kraftwerk écrase REM pour les variables présentes (données plus fraîches)
   └── load_kraftwerk_unit_data() parcourt tous les CSV du répertoire data/ le plus récent
10. Sauvegarde initiale dans reponses avec INSERT OR IGNORE
    (les données source REM+Kraftwerk sont persistées SANS écraser les modifications
     gestionnaire déjà existantes en base)
11. Chargement des réponses existantes depuis SQLite → écrase env_vars
    (les modifications précédentes du gestionnaire priment sur REM et Kraftwerk)
12. Chargement des données N-1 depuis edited_previous_EAL.json
    → env_vars[["_N1_DATA_"]] ← liste des variables N-1
```

**Règle de priorité** : Les données chargées depuis SQLite (étape 11) écrasent celles des sources (étapes 8-9). Ainsi, les corrections saisies par le gestionnaire lors d'une session précédente sont conservées, même après mise à jour quotidienne des fichiers Kraftwerk. Les nouvelles données Kraftwerk (non encore modifiées par le gestionnaire) sont ajoutées via `save_response_if_missing()`.

### 4.3 Phase 3 : Affichage du questionnaire

```
12. render_module() parcourt les questions du module courant
13. Pour chaque question TABLE :
    ├── Lecture des valeurs N dans env_vars[[var_name]][[row_idx]]
    ├── Comparaison avec original_vars (détection des corrections "C")
    ├── Lecture des valeurs N-1 dans env_vars[["_N1_DATA_"]][[var_name]]
    └── Si N1__var_name existe dans env_vars → la correction est affichée
14. Rendu HTML avec attributs data-* pour editable-table.js
```

### 4.4 Phase 4 : Modification par le gestionnaire

#### 4.4.1 Édition d'une cellule N ou N-1 (validation unitaire)

```
15. L'utilisateur modifie une valeur dans un <input> → le JS détecte le changement
16. Le bouton "Valider" (✔) apparaît
17. Clic sur Valider → Shiny.setInputValue("cell_action", {var, value, row, col, action:"validate"})
18. Handler serveur cell_action :
    ├── Nettoyage de la valeur (suppression des espaces pour les numériques)
    ├── Mise à jour de env_vars[[var_name]][[row_idx]] ← nouvelle valeur
    └── save_response() → INSERT OR UPDATE dans la table reponses
```

#### 4.4.2 Sauvegarde en bloc ("Enregistrer les modifications")

```
19. Clic sur "Enregistrer les modifications" → le JS collecte toutes les cellules
    dont data-saved ≠ data-original
20. Pour chaque cellule modifiée, une clé est générée :
    ├── Cellule N   : "tab_<qname>_<row>_<col>"
    └── Cellule N-1 : "tab_n1_<qname>_<row>_<col>" (qname préfixé "n1_")
21. Envoi au serveur via Shiny.setInputValue("table_modifications", {...})
22. Handler table_modifications :
    ├── Parse la clé → extrait qname, row, col, is_n1
    ├── Retrouve le nom de variable via get_table_var_name()
    ├── Si N-1 → préfixe avec n1_variable_name() → "N1__VAR"
    ├── save_response() → persistance SQLite
    └── Mise à jour env_vars
23. Le serveur exécute shinyjs::runjs("window.applySaveResults(...)")
    → Le JS met à jour data-original ← data-saved pour chaque cellule sauvée
```

#### 4.4.3 Édition des détails d'unité (adresse, contact)

```
24. Clic sur "Modifier l'adresse / le correspondant" → modal avec formulaire
25. Les champs sont pré-remplis avec les valeurs UNIT_* de env_vars
    (ou les valeurs REM si pas de modification antérieure)
26. Clic sur "Enregistrer" → pour chaque champ modifié :
    ├── env_vars[["UNIT_STREET_NAME"]] ← nouvelle valeur
    └── save_response() avec variable_name = "UNIT_STREET_NAME"
```

#### 4.4.4 Ajout/Suppression de ligne dans un tableau

```
27. Clic sur "+ Ajouter une ligne" → Shiny.setInputValue("row_action", {action:"add", question:"..."})
28. Handler row_action :
    ├── Lit _NROWS_<qname> (nombre actuel de lignes)
    ├── Incrémente ou décrémente
    └── save_response() → persiste la nouvelle valeur de _NROWS_
```

### 4.5 Phase 5 : Réouverture de l'application

```
29. L'utilisateur relance l'application et sélectionne la même unité
30. L'enquete_id existant est retrouvé (même enquête + même SIRET)
31. Les réponses sont rechargées depuis SQLite → env_vars restauré
32. Les cellules précédemment modifiées apparaissent avec :
    ├── Fond jaune (corrigé)
    ├── Badge "C" (corrigé)
    └── Bouton "↩" (restaurer la valeur initiale)
33. Les données N-1 modifiées (N1__*) sont également rechargées
34. En mode lecture seule (widget OFF), les valeurs modifiées N-1
    sont affichées (pas seulement les valeurs brutes importées)
```

---

## 5. Schéma récapitulatif des flux

```
                      ┌──────────────────────┐
                      │   interrogations.json │
                      │   (REM - source)      │
                      └──────────┬───────────┘
                                 │
                    ┌────────────▼────────────┐
                    │   env_vars (mémoire)     │
                    │   - Variables N          │
                    │   - _N1_DATA_            │
                    │   - N1__* (corrections)  │
                    │   - UNIT_* (détails)     │
                    └────────────┬────────────┘
                                 │
              ┌──────────────────┼──────────────────┐
              │                  │                  │
     ┌────────▼────────┐ ┌──────▼──────┐ ┌────────▼────────┐
     │  Lecture seule   │ │  Édition    │ │  Édition N-1    │
     │  (affichage)     │ │  cellule    │ │  (widget ON)    │
     │                  │ │  N          │ │                 │
     └─────────────────┘ └──────┬──────┘ └────────┬────────┘
                                │                  │
                                └────────┬─────────┘
                                         │
                              ┌──────────▼──────────┐
                              │   SQLite             │
                              │   reponses            │
                              │   - variable_name    │
                              │   - valeur           │
                              │   - ligne/colonne    │
                              └──────────────────────┘
```

---

## 6. Implémentation technique

### 6.1 Fichiers clés

| Fichier | Rôle |
|---------|------|
| `R/import_data.R` | Chargement des données REM (JSON, CSV), données N-1, statuts |
| `R/load_pogues.R` | Parsing du questionnaire Pogues JSON |
| `R/database.R` | Gestion SQLite : `init_db()`, `save_response()`, `load_responses()` |
| `R/renderers.R` | Rendu UI des questions, tableaux, cellules N et N-1 |
| `modules/questionnaire_pogues.R` | Module Shiny principal : serveur, navigation, handlers |
| `www/editable-table.js` | Gestion JS des cellules éditables (validate/reset/bulk save) |

### 6.2 Fonctions principales

| Fonction | Fichier | Rôle |
|----------|---------|------|
| `load_unit_data()` | `import_data.R` | Charge données REM + N-1 pour une unité |
| `save_response()` | `database.R` | Insère ou met à jour une réponse dans SQLite |
| `load_responses()` | `database.R` | Charge toutes les réponses d'une enquête |
| `get_variable_value()` | `renderers.R` | Lit une valeur dans `env_vars` (gère liste/scalaire) |
| `n1_variable_name()` | `renderers.R` | Génère le nom de variable N-1 (`N1__` + var) |
| `cell_value_differs()` | `renderers.R` | Compare deux valeurs (détection de correction) |
| `format_numeric()` | `renderers.R` | Formate un nombre avec séparateurs de milliers |

---

*Document généré le 9 juillet 2026 — Application Questionnaire Pogues v2*