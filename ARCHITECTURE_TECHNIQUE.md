# Architecture technique — Application fusionnée SUIVAL IAA + Questionnaire Pogues

## Présentation

Application Shiny R fusionnant deux applications :
1. **SUIVAL IAA** — Suivi et traitement des anomalies de collecte des enquêtes IAA
2. **Questionnaire dynamique Pogues** — Saisie de questionnaires basés sur Pogues avec pré-remplissage

## Arborescence

```
shiny_app/
├── app.R                           # Point d'entrée (dashboard Shiny)
├── global.R                        # Initialisation : librairies, constantes, sources
├── constantes.R                    # Constantes fusionnées (SUIVAL + Pogues)
├── shiny_app.Rproj                 # Fichier projet RStudio
│
├── R/                              # Fonctions utilitaires partagées
│   ├── vtl.R                       # Résolution d'expressions VTL
│   ├── load_pogues.R               # Chargement du JSON Pogues
│   ├── database.R                  # DB unifiée (QUESTIONNAIRE, ANOMALIES, enquetes, reponses)
│   ├── renderers.R                 # Rendu des composants UI du questionnaire
│   ├── import_data.R               # Import CSV/JSON + mappe statuts
│   ├── ordonnanceur.R              # DAMAJQ : exécution scripts détection anomalies
│   ├── traitement_anomalies.R      # 4 cas de mise à jour des anomalies
│   └── gestion_filtre.R            # Sauvegarde/restauration des filtres
│
├── modules/                        # Modules fonctionnels (1 fichier = UI + Server)
│   ├── header.R                    # En-tête du dashboard
│   ├── sidebar.R                   # Barre de navigation
│   ├── recherche_traitement.R      # Recherche et traitement des anomalies
│   ├── suivi_traitements.R         # Suivi des traitements (tableaux + graphiques)
│   ├── exporter_csv.R              # Export CSV/Excel
│   ├── suivi_questionnaires.R      # Suivi questionnaires (responsable enquête)
│   ├── anomalies_archivees.R       # Visualisation anomalies archivées
│   ├── integration.R               # Intégration nouvelle collecte
│   ├── archivage.R                 # Archivage campagne
│   └── questionnaire_pogues.R      # Saisie questionnaire dynamique Pogues
│
├── www/                            # Ressources statiques
├── DonneesUtilisateurs/            # Exports utilisateurs
├── DonneesExternes/                # Anomalies archivées (CSV)
├── Logs/                           # Logs ordonnanceur
└── *.db                            # Bases SQLite
```

## Navigation

```
┌─────────────────────────────────────────────────────────┐
│                    SUIVAL-IAA (Dashboard)                │
├─────────────┬───────────┬───────────┬─────────┬─────────┤
│  Anomalies  │   Suivi   │Historique │ Gestion │ Question│
│  ─────────  │  ───────  │ ────────  │ ─────── │ ─────── │
│ Recherche & │ Suivi     │ Anomalies │ Intégra │ Saisie  │
│ traitement  │ traitements│ archivées│ tion    │ question│
│             │ Export CSV│           │ Archiva │ naire   │
│             │ Suivi     │           │ ge      │ Pogues  │
│             │ questions │           │         │         │
└─────────────┴───────────┴───────────┴─────────┴─────────┘
```

## Modèle de données unifié

La base SQLite unique `base_de_donnee_SUIVAL_IAA.sqlite` contient 4 tables :

| Table | Source | Description |
|---|---|---|
| `QUESTIONNAIRE` | SUIVAL | Questionnaires avec compteurs d'anomalies |
| `ANOMALIES` | SUIVAL | Anomalies liées aux questionnaires |
| `enquetes` | Pogues | Sessions d'enquête questionnaire |
| `reponses` | Pogues | Réponses aux questions |

## Flux de données

```
┌──────────────┐     ┌──────────┐     ┌──────────────────────┐
│  Programmes  │ ──▶ │ORDONNAN- │ ──▶ │  Base SQLite unifiée │
│  détection R │     │CEUR      │     │  ─────────────────── │
│  (EAPC, QVOL)│     │(damajq.R)│     │  QUESTIONNAIRE       │
└──────────────┘     └──────────┘     │  ANOMALIES           │
                                      │  enquetes            │
┌──────────────┐     ┌──────────┐     │  reponses            │
│  Fichiers    │ ──▶ │  SHINY   │ ──▶ │                      │
│  Pogues JSON │     │   APP    │     └──────────────────────┘
│  Données     │     │(modules/)│              │
│  REM/CSV     │     └──────────┘              ▼
│              │                          Interface Shiny
└──────────────┘                     (dashboard + modals)
```

## Dépendances R

- shiny, shinydashboard, shinyjs, shinyWidgets, shinyFiles, shinybusy
- DT (DataTables)
- DBI, RSQLite
- dplyr, dbplyr, tidyr, tibble, stringr, lubridate
- readr, xlsx (import/export)
- janitor (tableaux croisés)
- ggplot2, ggrepel (graphiques)
- jsonlite, xml2 (Pogues)
- mailR (notifications email)
- cli