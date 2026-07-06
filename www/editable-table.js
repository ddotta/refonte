// ==============================================================================
// GESTION DES TABLEAUX ÉDITABLES AVEC AFFICHAGE FORMATÉ
// ==============================================================================
// Fonctionnalités :
// - Affichage formaté des valeurs numériques (séparateur de milliers)
// - Édition directe dans le champ
// - Détection de modification avec indicateur "C"
// - Bouton de restauration de la valeur initiale
// ==============================================================================

(function() {
  'use strict';

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

  // Initialise une cellule numérique éditable
  function initNumericCell(cell) {
    var input = cell.querySelector('input[type="text"]');
    var indicatorEl = cell.querySelector('.modified-indicator');
    var resetBtn = cell.querySelector('.reset-btn');

    if (!input) return;

    var originalValue = cell.getAttribute('data-original') || '';

    // Met à jour l'affichage des indicateurs
    function updateIndicators() {
      var rawVal = input.value;
      var isModified = false;
      if (originalValue !== '') {
        var currentCleaned = unformatNumber(rawVal);
        var originalCleaned = unformatNumber(originalValue);
        isModified = (currentCleaned !== originalCleaned);
      }

      if (indicatorEl) {
        indicatorEl.style.display = isModified ? 'inline' : 'none';
      }
      if (resetBtn) {
        resetBtn.style.display = isModified ? 'inline-block' : 'none';
      }

      if (isModified) {
        cell.classList.add('modified');
      } else {
        cell.classList.remove('modified');
      }
    }

    // Formate la valeur avec séparateurs de milliers
    function formatDisplay() {
      var rawVal = input.value;
      var cleaned = unformatNumber(rawVal);
      var num = parseFloat(cleaned);
      if (!isNaN(num) && cleaned !== '') {
        // On garde la valeur nettoyée (sans formatage) dans l'input pour Shiny,
        // mais on affiche la version formatée
        input.value = cleaned;
        // Le formatage visuel se fait via le style text-align:right et le poids
      } else if (cleaned === '') {
        input.value = '';
      }
    }

    // Clic sur le bouton reset : restaurer la valeur initiale
    if (resetBtn) {
      resetBtn.addEventListener('click', function(e) {
        e.stopPropagation();
        e.preventDefault();
        input.value = originalValue;
        var event = new Event('change', { bubbles: true });
        input.dispatchEvent(event);
        updateIndicators();
      });
    }

    // Perte de focus : nettoyer et mettre à jour
    input.addEventListener('blur', function() {
      formatDisplay();
      updateIndicators();
    });

    // Détection de changement en temps réel
    input.addEventListener('input', function() {
      updateIndicators();
    });

    input.addEventListener('change', function() {
      formatDisplay();
      updateIndicators();
    });

    // Initialisation
    updateIndicators();
  }

  // Initialisation au chargement
  function init() {
    var cells = document.querySelectorAll('.editable-table .numeric-cell');
    cells.forEach(function(cell) {
      if (!cell.classList.contains('initialized')) {
        initNumericCell(cell);
        cell.classList.add('initialized');
      }
    });
  }

  // Observer les mutations du DOM (pour les tableaux chargés dynamiquement)
  var observer = new MutationObserver(function() {
    var cells = document.querySelectorAll('.editable-table .numeric-cell:not(.initialized)');
    if (cells.length > 0) {
      cells.forEach(function(cell) {
        initNumericCell(cell);
        cell.classList.add('initialized');
      });
    }
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