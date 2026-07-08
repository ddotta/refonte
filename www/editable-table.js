// ==============================================================================
// GESTION DES TABLEAUX ÉDITABLES AVEC AFFICHAGE FORMATÉ
// ==============================================================================
// Fonctionnalités :
// - Affichage formaté des valeurs numériques (séparateur de milliers)
// - Édition directe dans le champ (numérique ou caractère)
// - Détection d'une saisie non enregistrée -> bouton "Valider"
// - Validation explicite : envoie la valeur au serveur pour sauvegarde en base
// - Une fois validée, si la valeur diffère de la valeur d'origine (import) :
//   badge "C" (corrigé) + bouton de restauration de la valeur initiale
// - L'état "corrigé" est calculé côté serveur à chaque rendu (data-original /
//   data-saved), donc il reste visible si on change de module et qu'on revient
// ==============================================================================

(function() {
  'use strict';

  console.debug('[editable-table] script chargé (version avec bouton Valider + ajout/suppression de lignes)');

  // Formate un nombre avec séparateur de milliers (espace)
  function formatNumber(str) {
    if (str === null || str === undefined || str === '') return '';
    var cleaned = String(str).replace(/\s/g, '').replace(',', '.');
    var num = parseFloat(cleaned);
    if (isNaN(num)) return String(str);
    return num.toLocaleString('fr-FR', {
      minimumFractionDigits: 0,
      maximumFractionDigits: 10,
      useGrouping: true
    }).replace(/\u202f/g, ' ').replace(/,/g, ',');
  }

  // Supprime les séparateurs de milliers pour obtenir la valeur brute
  function unformatNumber(str) {
    if (str === null || str === undefined) return '';
    return String(str).replace(/\s/g, '').replace(',', '.');
  }

  // Normalise une valeur pour comparaison (numérique ou caractère)
  function normalize(str, isNumeric) {
    if (isNumeric) return unformatNumber(str);
    return (str === null || str === undefined) ? '' : String(str).trim();
  }

  // Initialise une cellule éditable (numérique ou caractère)
  function initEditableCell(cell) {
    var input = cell.querySelector('input[type="text"]');
    if (!input) return;

    var indicatorEl = cell.querySelector('.modified-indicator');
    var resetBtn = cell.querySelector('.reset-btn');
    var validateBtn = cell.querySelector('.validate-btn');
    var isNumeric = cell.classList.contains('numeric-cell');

    var originalValue = cell.getAttribute('data-original') || '';
    // savedValue = valeur actuellement enregistrée en base (mise à jour
    // localement après un clic sur Valider/Rétablir, sans attendre un re-rendu)
    var savedValue = cell.getAttribute('data-saved') || '';
    var varName = cell.getAttribute('data-var');
    var rowIdx = cell.getAttribute('data-row');
    var colIdx = cell.getAttribute('data-col');
    var actionInputId = cell.getAttribute('data-action-input');

    function isPending() {
      return normalize(input.value, isNumeric) !== normalize(savedValue, isNumeric);
    }
    function isCorrected() {
      return normalize(savedValue, isNumeric) !== normalize(originalValue, isNumeric);
    }

    // Met à jour l'affichage des indicateurs : bouton Valider si saisie non
    // enregistrée ; sinon badge "C" + bouton Rétablir si la valeur enregistrée
    // diffère de la valeur d'origine.
    function updateIndicators() {
      var pending = isPending();
      var corrected = isCorrected();

      if (validateBtn) validateBtn.style.display = pending ? 'inline-block' : 'none';
      if (indicatorEl) indicatorEl.style.display = (corrected && !pending) ? 'inline' : 'none';
      if (resetBtn) resetBtn.style.display = (corrected && !pending) ? 'inline-block' : 'none';

      cell.classList.toggle('modified', corrected && !pending);
      cell.classList.toggle('pending-edit', pending);
    }

    // Formate la valeur avec séparateurs de milliers (uniquement numérique)
    function formatDisplay() {
      if (!isNumeric) return;
      var cleaned = unformatNumber(input.value);
      var num = parseFloat(cleaned);
      if (!isNaN(num) && cleaned !== '') {
        input.value = cleaned;
      } else if (cleaned === '') {
        input.value = '';
      }
    }

    // Envoie une action ("validate" ou "reset") au serveur Shiny
    function sendAction(action, value) {
      if (!actionInputId || typeof Shiny === 'undefined') return;
      Shiny.setInputValue(actionInputId, {
        var: varName, row: rowIdx, col: colIdx,
        action: action, value: value, ts: Date.now()
      }, { priority: 'event' });
    }

    // Clic sur "Valider" : envoie la valeur saisie, elle devient la nouvelle
    // valeur enregistrée
    if (validateBtn) {
      validateBtn.addEventListener('click', function(e) {
        e.stopPropagation();
        e.preventDefault();
        formatDisplay();
        savedValue = input.value;
        cell.setAttribute('data-saved', savedValue);
        sendAction('validate', savedValue);
        updateIndicators();
      });
    }

    // Clic sur le bouton reset : restaurer la valeur initiale et l'enregistrer
    if (resetBtn) {
      resetBtn.addEventListener('click', function(e) {
        e.stopPropagation();
        e.preventDefault();
        input.value = originalValue;
        savedValue = originalValue;
        cell.setAttribute('data-saved', savedValue);
        sendAction('reset', originalValue);
        updateIndicators();
      });
    }

    // Détection de saisie en temps réel (fait apparaître/disparaître "Valider")
    input.addEventListener('input', function() {
      updateIndicators();
    });

    // Perte de focus : nettoyer l'affichage numérique
    input.addEventListener('blur', function() {
      formatDisplay();
      updateIndicators();
    });

    // Validation au clavier (Entrée) sans quitter le champ
    input.addEventListener('keydown', function(e) {
      if (e.key === 'Enter' && validateBtn && isPending()) {
        e.preventDefault();
        validateBtn.click();
      }
    });

    // Initialisation
    updateIndicators();
  }

  // Initialise toutes les cellules éditables pas encore initialisées
  function init() {
    var cells = document.querySelectorAll('.editable-table td[data-var]:not(.initialized)');
    if (cells.length > 0) {
      console.debug('[editable-table] initialisation de', cells.length, 'cellule(s)');
    }
    cells.forEach(function(cell) {
      try {
        initEditableCell(cell);
      } catch (err) {
        console.error('[editable-table] erreur d\'initialisation sur une cellule', err, cell);
      }
      cell.classList.add('initialized');
    });
  }

  // Observer les mutations du DOM (tableaux chargés/rechargés dynamiquement,
  // par ex. en changeant de module puis en revenant)
  var observer = new MutationObserver(function() {
    init();
  });

  observer.observe(document.body, {
    childList: true,
    subtree: true
  });

  // Initialisation initiale
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

})();
