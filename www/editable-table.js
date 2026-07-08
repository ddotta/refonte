// ==============================================================================
// GESTION DES TABLEAUX EDITABLES - VERSION SIMPLIFIEE
// ==============================================================================
// Fonctionnalites :
// - Affichage formaté des valeurs numeriques (separateur de milliers)
// - Edition directe dans le champ (numerique ou caractere)
// - Detection d'une saisie non enregistrée -> bouton "Valider"
// - Validation explicite : envoie la valeur au serveur pour sauvegarde en base
// - Une fois validee, si la valeur differe de la valeur d'origine (import) :
//   badge "C" (corrige) + bouton de restauration de la valeur initiale
//
// Approche : délégation d'evenements (un seul listener global par type
// d'evenement) pour plus de robustesse face aux re-rendus DOM de Shiny.
// ==============================================================================
(function() {
  'use strict';

  console.log('[editable-table] script charge (version simplifiee)');

  // ===== Utilitaires =====

  function normalize(str, isNumeric) {
    if (str === null || str === undefined) return '';
    if (isNumeric) return String(str).replace(/\s/g, '').replace(',', '.');
    return String(str).trim();
  }

  function cleanNumericInput(input) {
    if (!input) return;
    var val = input.value.replace(/\s/g, '').replace(',', '.');
    var num = parseFloat(val);
    if (!isNaN(num) && val !== '') {
      input.value = String(num);
    } else if (val === '') {
      input.value = '';
    }
  }

  // ===== Mise à jour visuelle d'une cellule =====

  function updateCellState(cell) {
    var input = cell.querySelector('input[type="text"]');
    if (!input) return;

    var validateBtn = cell.querySelector('.validate-btn');
    var indicatorEl = cell.querySelector('.modified-indicator');
    var resetBtn    = cell.querySelector('.reset-btn');
    var savedValue  = cell.getAttribute('data-saved')   || '';
    var originalValue = cell.getAttribute('data-original') || '';
    var isNumeric   = cell.classList.contains('numeric-cell');

    var currentVal  = input.value;
    var pending     = normalize(currentVal, isNumeric) !== normalize(savedValue, isNumeric);
    var corrected   = normalize(savedValue, isNumeric) !== normalize(originalValue, isNumeric);

    // Bouton Valider  : visible si saisie non enregistree
    if (validateBtn) validateBtn.style.display = pending   ? 'inline-flex' : 'none';
    // Badge "C"       : visible si corrigé et pas en cours de saisie
    if (indicatorEl) indicatorEl.style.display = (corrected && !pending) ? 'inline'      : 'none';
    // Bouton Retablir : visible si corrigé et pas en cours de saisie
    if (resetBtn)    resetBtn.style.display    = (corrected && !pending) ? 'inline-flex' : 'none';

    cell.classList.toggle('modified',     corrected && !pending);
    cell.classList.toggle('pending-edit', pending);
  }

  function initAllCells() {
    document.querySelectorAll('td[data-var]').forEach(function(cell) {
      updateCellState(cell);
    });
  }

  // ===== Envoi d'action au serveur Shiny =====

  function sendAction(cell, action, value) {
    var actionInputId = cell.getAttribute('data-action-input');
    var varName       = cell.getAttribute('data-var');
    var rowIdx        = cell.getAttribute('data-row');
    var colIdx        = cell.getAttribute('data-col');

    if (!actionInputId || typeof Shiny === 'undefined') {
      console.warn('[editable-table] Shiny indisponible ou action-input manquant');
      return;
    }

    Shiny.setInputValue(actionInputId, {
      var:    varName,
      row:    rowIdx,
      col:    colIdx,
      action: action,
      value:  value,
      ts:     Date.now()
    }, { priority: 'event' });

    console.log('[editable-table] action envoyee :', action, varName, '=', value);
  }

  function handleValidate(cell) {
    var input = cell.querySelector('input[type="text"]');
    if (!input) return;

    // Nettoyer la valeur numerique avant sauvegarde
    if (cell.classList.contains('numeric-cell')) {
      cleanNumericInput(input);
    }

    var valueToSave = input.value;
    cell.setAttribute('data-saved', valueToSave);

    sendAction(cell, 'validate', valueToSave);
    updateCellState(cell);
  }

  function handleReset(cell) {
    var input         = cell.querySelector('input[type="text"]');
    var originalValue = cell.getAttribute('data-original') || '';

    if (input) input.value = originalValue;
    cell.setAttribute('data-saved', originalValue);

    sendAction(cell, 'reset', originalValue);
    updateCellState(cell);
  }

  // ===== Écouteurs d'evenements (délégation) =====

  // Clic sur les boutons Valider / Retablir
  document.addEventListener('click', function(e) {
    var validateBtn = e.target.closest('.validate-btn');
    if (validateBtn) {
      e.preventDefault();
      e.stopPropagation();
      var cell = validateBtn.closest('td[data-var]');
      if (cell) handleValidate(cell);
      return;
    }

    var resetBtn = e.target.closest('.reset-btn');
    if (resetBtn) {
      e.preventDefault();
      e.stopPropagation();
      var cell = resetBtn.closest('td[data-var]');
      if (cell) handleReset(cell);
      return;
    }
  });

  // ---- Clic sur le bouton global "Enregistrer les modifications" ----
  // Ce bouton doit avoir la classe .save-table-btn et un attribut data-input-id
  // contenant le nom d'input namespaced (ex: module-1-table_modifications).
  document.addEventListener('click', function(e) {
    var saveBtn = e.target.closest('.save-table-btn');
    if (!saveBtn) return;

    e.preventDefault();
    e.stopPropagation();

    var inputId = saveBtn.getAttribute('data-input-id') || 'table_modifications';
    var modifications = {};

    // Parcourir toutes les cellules et collecter celles modifiées (data-saved != data-original)
    document.querySelectorAll('td[data-var][data-row][data-col]').forEach(function(cell) {
      var saved = cell.getAttribute('data-saved') || '';
      var original = cell.getAttribute('data-original') || '';
      // n'envoyer que si modifié
      if (saved !== null && saved !== original) {
        var qname = cell.getAttribute('data-qname') || cell.getAttribute('data-var');
        var row = cell.getAttribute('data-row');
        var col = cell.getAttribute('data-col');
        var key = 'tab_' + qname + '_' + row + '_' + col;
        modifications[key] = saved;
      }
    });

    if (Object.keys(modifications).length === 0) {
      console.log('[editable-table] aucune modification à envoyer');
      // On peut déclencher une notification côté Shiny si souhaité, sinon rien
      return;
    }

    if (typeof Shiny === 'undefined') {
      console.warn('[editable-table] Shiny indisponible');
      return;
    }

    Shiny.setInputValue(inputId, modifications, { priority: 'event' });
    console.log('[editable-table] envoi sauvegarde tableau ->', inputId, modifications);
  });

  // Saisie dans les inputs -> met a jour l'etat visuel en temps reel
  document.addEventListener('input', function(e) {
    var cell = e.target.closest('td[data-var]');
    if (cell) updateCellState(cell);
  });

  // Perte de focus -> nettoyage numerique + mise a jour visuelle
  document.addEventListener('focusout', function(e) {
    var cell = e.target.closest('td[data-var]');
    if (!cell) return;

    if (cell.classList.contains('numeric-cell')) {
      cleanNumericInput(e.target);
    }
    updateCellState(cell);
  });

  // Touche Entree = Valider (si un bouton Valider est visible)
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Enter') {
      var cell = e.target.closest('td[data-var]');
      if (!cell) return;
      var validateBtn = cell.querySelector('.validate-btn');
      if (validateBtn && validateBtn.style.display !== 'none') {
        e.preventDefault();
        handleValidate(cell);
      }
    }
  });

  // ===== Initialisation =====

  function start() {
    initAllCells();
    console.log('[editable-table] initialisation terminee');
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }

  // Observer les changements DOM (Shiny re-rend les tableaux quand on
  // change de module ou quand les donnees changent)
  var observer = new MutationObserver(function(mutations) {
    var needsInit = false;
    mutations.forEach(function(m) {
      if (m.type !== 'childList') return;
      for (var i = 0; i < m.addedNodes.length; i++) {
        var node = m.addedNodes[i];
        if (node.nodeType !== 1) continue;
        if (node.matches && node.matches('td[data-var]')) {
          needsInit = true; break;
        }
        if (node.querySelectorAll && node.querySelectorAll('td[data-var]').length > 0) {
          needsInit = true; break;
        }
      }
    });
    if (needsInit) initAllCells();
  });

  observer.observe(document.body, { childList: true, subtree: true });

  // ===== Fonction appelée par le serveur (shinyjs::runjs) pour appliquer le résultat =====
  // Le serveur exécute : shinyjs::runjs(sprintf("window.applySaveResults(%s);", jsonlite::toJSON(results)))
  // results attendu : { success: { "tab_Q_1_1": true, ... }, error: { "tab_Q_2_3": "message", ... } }
  window.applySaveResults = function(results) {
    if (!results) return;
    function markSaved(key) {
      var m = key.match(/^tab_(.+)_(\d+)_(\d+)$/);
      if (!m) return;
      var qname = m[1], row = m[2], col = m[3];
      // Prefer data-var selector, fallback to data-qname
      var sel = 'td[data-var="'+qname+'"][data-row="'+row+'"][data-col="'+col+'"]';
      var cell = document.querySelector(sel) || document.querySelector('td[data-qname="'+qname+'"][data-row="'+row+'"][data-col="'+col+'"]');
      if (!cell) return;
      var saved = cell.getAttribute('data-saved') || '';
      cell.setAttribute('data-original', saved);
      cell.classList.remove('pending-edit');
      cell.classList.remove('error');
      cell.classList.add('modified');
      // update visual state of child input
      updateCellState(cell);
    }
    function markError(key, msg) {
      var m = key.match(/^tab_(.+)_(\d+)_(\d+)$/);
      if (!m) return;
      var qname = m[1], row = m[2], col = m[3];
      var cell = document.querySelector('td[data-var="'+qname+'"][data-row="'+row+'"][data-col="'+col+'"]') ||
                 document.querySelector('td[data-qname="'+qname+'"][data-row="'+row+'"][data-col="'+col+'"]');
      if (!cell) return;
      cell.classList.add('error');
      if (msg) cell.setAttribute('title', msg);
      updateCellState(cell);
    }

    if (results.success) Object.keys(results.success).forEach(function(k){ markSaved(k); });
    if (results.error) Object.keys(results.error).forEach(function(k){ markError(k, results.error[k]); });

    console.log('[editable-table] applySaveResults', results);
  };

})();