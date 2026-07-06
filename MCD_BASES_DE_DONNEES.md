# Modèle Conceptuel de Données (MCD) — Bases SQLite

## Introduction

Les bases de données SQLite (`prixgrumes_questionnaire.db`, `eal_questionnaire.db`)
sont générées automatiquement par l'application Shiny lors de la saisie des
questionnaires. Elles assurent la persistance des réponses.

## Entités

### 1. `enquetes` — Session d'enquête

| Colonne | Type | Contrainte | Description |
|---------|------|-----------|-------------|
| `id` | TEXT | PRIMARY KEY | Identifiant unique de l'enquête (format : `<Nom>_<SIRET>_<Timestamp>`) |
| `questionnaire_id` | TEXT | NOT NULL | Identifiant du questionnaire Pogues (UUID) |
| `source_name` | TEXT | | Nom de l'enquête source (ex: "PRIXGRUMES", "EAL") |
| `statut` | TEXT | DEFAULT 'en_cours' | Statut de l'enquête : `en_cours`, `termine` |
| `created_at` | TIMESTAMP | DEFAULT CURRENT_TIMESTAMP | Date de création |
| `updated_at` | TIMESTAMP | DEFAULT CURRENT_TIMESTAMP | Date de dernière modification |

**Clés :**
- PK : `(id)`

### 2. `reponses` — Valeurs des variables collectées

| Colonne | Type | Contrainte | Description |
|---------|------|-----------|-------------|
| `id` | INTEGER | PRIMARY KEY AUTOINCREMENT | Identifiant technique |
| `questionnaire_id` | TEXT | NOT NULL | Identifiant du questionnaire Pogues (UUID) |
| `enquete_id` | TEXT | NOT NULL | Référence vers `enquetes.id` |
| `variable_name` | TEXT | NOT NULL | Nom de la variable Pogues (ex: `PRODUCTION_FR_CHN13`) |
| `valeur` | TEXT | | Valeur saisie (toujours stockée en texte) |
| `ligne` | INTEGER | DEFAULT 1 | Numéro de ligne (pour les questions TABLE) |
| `colonne` | INTEGER | DEFAULT 1 | Numéro de colonne (pour les questions TABLE) |
| `updated_at` | TIMESTAMP | DEFAULT CURRENT_TIMESTAMP | Date de dernière modification |

**Clés :**
- PK : `(id)`
- UK : `(questionnaire_id, enquete_id, variable_name, ligne, colonne)`
- FK : `(enquete_id)` → `enquetes(id)`

## Relations

```
┌──────────────────────────────────────────────────────────┐
│                     enquetes                              │
├──────────────────────────────────────────────────────────┤
│ id (PK) : TEXT                                            │
│ questionnaire_id : TEXT                                   │──┐
│ source_name : TEXT                                        │  │
│ statut : TEXT                                             │  │
│ created_at : TIMESTAMP                                    │  │
│ updated_at : TIMESTAMP                                    │  │
└──────────────────────────────────────────────────────────┘  │
                                                              │ 1
                                                              │
                                                              │
                                                              │ N
┌──────────────────────────────────────────────────────────┐  │
│                     reponses                              │  │
├──────────────────────────────────────────────────────────┤  │
│ id (PK) : INTEGER                                         │  │
│ questionnaire_id : TEXT (FK)                              │◄─┘
│ enquete_id : TEXT (FK)                                    │
│ variable_name : TEXT                                      │
│ valeur : TEXT                                             │
│ ligne : INTEGER                                           │
│ colonne : INTEGER                                         │
│ updated_at : TIMESTAMP                                    │
├──────────────────────────────────────────────────────────┤
│ UK : (questionnaire_id, enquete_id,                       │
│       variable_name, ligne, colonne)                      │
└──────────────────────────────────────────────────────────┘
```

**Règle de gestion** : Une enquête possède 0 à N réponses.
Chaque réponse est unique par couple (questionnaire, enquête, variable, ligne, colonne).

## Contraintes d'intégrité

1. **Unicité** : Une variable ne peut avoir qu'une seule valeur pour un
   couple (questionnaire_id, enquete_id, ligne, colonne). La mise à jour
   écrase la valeur précédente (`ON CONFLICT ... DO UPDATE SET`).

2. **Stockage** : Toutes les valeurs sont stockées en texte (`TEXT`). La
   conversion numérique se fait à l'affichage.

3. **Absence de valeur** : Si une variable n'a pas été collectée, aucune
   ligne n'est insérée dans `reponses`.

## Exemples de données

### Table `enquetes`

| id | questionnaire_id | source_name | statut | created_at |
|----|-----------------|-------------|--------|------------|
| PRIXGRUMES_30093493200017_20260625_150000 | mltnx5mv | PRIXGRUMES | en_cours | 2026-06-25 15:00:00 |

### Table `reponses`

| questionnaire_id | enquete_id | variable_name | valeur | ligne | colonne |
|-----------------|-----------|---------------|--------|-------|---------|
| mltnx5mv | PRIXGRUMES_30093493200017_20260625_150000 | SP | S2 | 1 | 1 |
| mltnx5mv | PRIXGRUMES_30093493200017_20260625_150000 | PRODUCTION_FR_CHN13 | 1339.0 | 1 | 1 |
| mltnx5mv | PRIXGRUMES_30093493200017_20260625_150000 | PRODUCTION_FR_CHN23 | 866.0 | 1 | 1 |

## Schéma SQL de création

```sql
CREATE TABLE IF NOT EXISTS enquetes (
    id TEXT PRIMARY KEY,
    questionnaire_id TEXT NOT NULL,
    source_name TEXT,
    statut TEXT DEFAULT 'en_cours',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS reponses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    questionnaire_id TEXT NOT NULL,
    enquete_id TEXT NOT NULL,
    variable_name TEXT NOT NULL,
    valeur TEXT,
    ligne INTEGER DEFAULT 1,
    colonne INTEGER DEFAULT 1,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(questionnaire_id, enquete_id, variable_name, ligne, colonne)
);