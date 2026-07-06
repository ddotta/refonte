# Cahier des charges — Application de consultation des questionnaires Pogues

## 1. Présentation

### 1.1 Contexte
L'application permet de visualiser et pré-remplir des questionnaires d'enquête
(ELP, EAL) conçus avec Pogues, à partir des fichiers d'export et des données
REM (Répertoire des Enquêtes et des Métadonnées).

### 1.2 Objectifs
- Offrir une interface de consultation des questionnaires avec leurs données
- Permettre la sélection d'une unité enquêtée avec affichage de son statut
- Pré-remplir automatiquement les champs du questionnaire avec les données
  collectées
- Exporter la liste des interrogations et leurs statuts en CSV

## 2. Périmètre fonctionnel

### 2.1 Écran de sélection de l'enquête (Étape 1)
- Afficher une page d'accueil avec les enquêtes disponibles
- Chaque enquête est représentée par une carte (icône, nom, description)
- Les enquêtes disponibles sont configurées dans `AVAILABLE_SURVEYS`

**Maquette fonctionnelle :**
```
┌─────────────────────────────────────────┐
│         [Titre: "Questionnaire"]         │
│  Sélectionnez l'enquête à laquelle       │
│  vous souhaitez répondre                 │
│                                          │
│  ┌──────────────┐  ┌──────────────┐     │
│  │  🌲          │  │  🛒          │     │
│  │ Prix Grumes  │  │    EAL       │     │
│  │ Enquête sur  │  │ Enquête sur  │     │
│  │ les prix des │  │ les achats   │     │
│  │ grumes...    │  │ en ligne...  │     │
│  │              │  │              │     │
│  │ [Démarrer]   │  │  [Démarrer]  │     │
│  └──────────────┘  └──────────────┘     │
└─────────────────────────────────────────┘
```

### 2.2 Écran d'informations de l'enquête (Étape 2)
- Afficher les objectifs de l'enquête (depuis `context.json`)
- Afficher les références : code enquête, année, périodicité, visa CNIS
- Afficher le calendrier complet : début collecte, retour, relances, mise en
  demeure, fin de collecte
- Afficher le contact référent
- Bouton "Continuer →" pour passer à l'étape suivante

### 2.3 Écran de sélection de l'unité (Étape 3)
- Charger la liste complète des unités depuis `interrogations.json` (REM)
- Pour chaque unité, afficher :
  - Identifiant SIRET
  - Raison sociale
  - Ville
  - Statut d'interrogation (libellé français)
  - Colonne probation (si applicable)
- **Mappe des statuts** :
  - `INITLA` → "À expertiser"
  - `EXPERT` → "En cours d'expertise"
  - `ONGEXPERT` → "En cours d'expertise en ligne"
  - `PARTIELINT` → "En cours de saisie"
  - `PARTIELPAP` → "En cours de saisie papier"
  - `RECUPAP` → "Saisie papier récupérée"
  - `APUR` → "En cours d'apurement"
  - `VALID` → "Validé"
  - `HC` → "Hors champ"
  - `REFUSAL` → "Refus"
- Légende colorée affichée au-dessus du tableau
- Sélection d'une unité via `selectInput` avec nom d'entreprise
- Panneau de détails (raison sociale, ville, email, contact)
- Limite d'affichage à 20 lignes avec compteur
- Boutons d'export CSV (sur disque et téléchargement)

### 2.4 Questionnaire (Étape 4)
- Charger le Pogues JSON (`pogues_*.json`)
- Charger les données collectées (`*RACINE.csv` + modules)
- Pré-remplir les champs du questionnaire
- Navigation par module (sidebar + boutons Précédent/Suivant)
- Barre de progression
- Sauvegarde automatique en base SQLite
- Écran de fin avec soumission

## 3. Architecture technique

### 3.1 Stack
- **Langage** : R 4.x
- **Framework** : Shiny (shinythemes)
- **Base de données** : SQLite
- **Parsing JSON** : jsonlite
- **Parsing XML** : xml2
- **Manipulation données** : dplyr, tidyr

### 3.2 Modules R
| Module | Fichier | Responsabilité |
|--------|---------|---------------|
| `app.R` | Entrée principale | UI, serveur, navigation |
| `R/vtl.R` | Résolution VTL | Interpolation de variables |
| `R/load_pogues.R` | Chargement Pogues | Parse du JSON questionnaire |
| `R/database.R` | Persistance | SQLite, CRUD réponses |
| `R/renderers.R` | Rendu UI | Composants de formulaire |
| `R/import_data.R` | Import données | CSV, JSON, statuts |

### 3.3 Structure des données

**interrogations.csv :**
| Colonne | Type | Description |
|---------|------|-------------|
| `partitioningId` | string | Identifiant de partition |
| `surveyUnitId` | string | SIRET de l'unité |
| `interrogationId` | string | UUID de l'interrogation |
| `highestEventType` | string | Dernier événement REM |
| `highestEventDate` | datetime | Date du dernier événement |
| `isOnProbation` | boolean | Unité en probation ? |

**Base SQLite - table `reponses` :**
| Colonne | Type | Description |
|---------|------|-------------|
| `id` | INTEGER PK | Auto-incrément |
| `questionnaire_id` | TEXT | ID questionnaire Pogues |
| `enquete_id` | TEXT | ID de l'enquête en cours |
| `variable_name` | TEXT | Nom de la variable |
| `valeur` | TEXT | Valeur saisie |
| `ligne` | INTEGER | Ligne (pour tableaux) |
| `colonne` | INTEGER | Colonne (pour tableaux) |

## 4. Contraintes techniques

### 4.1 Dépendances
- R ≥ 4.0
- Packages : shiny, shinythemes, RSQLite, dplyr, tidyr, jsonlite, xml2, stringr, tools

### 4.2 Chemins
L'application doit être lancée depuis le dossier racine du projet :
```r
setwd("c:/Users/damien.dotta/DEMESIS/Filière_enquete/REFONTE")
shiny::runApp("shiny_app")
```

### 4.3 Configuration
- Variable d'environnement `SURVEY` : force une enquête (ex: `SURVEY=PrixGrumes`)
- Liste des enquêtes : configurée dans `AVAILABLE_SURVEYS` (app.R)

## 5. Évolutions possibles

- Interface administrateur pour la gestion des statuts
- Modification en ligne des statuts avec sauvegarde dans le CSV
- Statistiques de collecte (taux de retour, délais)
- Authentification (Shiny auth ou SSO)
- Déploiement serveur (Shiny Server, RStudio Connect)
- Enrichissement des données EXTERNAL (pré-remplissage avec données externes)