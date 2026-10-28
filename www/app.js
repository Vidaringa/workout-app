// Client-side behaviour for the logging cards, rest timer and login.
// Steppers and RIR chips work without a server round trip; only a tap on a
// check (or "log all") sends values to Shiny.
(function () {
  "use strict";

  function parseNum(value) {
    if (value === null || value === undefined) return null;
    var text = String(value).replace(",", ".").trim();
    if (text === "") return null;
    var n = Number(text);
    return isNaN(n) ? null : n;
  }

  function round3(x) {
    return Math.round(x * 1000) / 1000;
  }

  function cardValues(card) {
    var active = card.querySelector(".rir-btn.active");
    return {
      id: card.dataset.setId,
      kg: parseNum(card.querySelector('[data-field="kg"] input').value),
      reps: parseNum(card.querySelector('[data-field="reps"] input').value),
      rir: active ? Number(active.dataset.rir) : null
    };
  }

  function sendSets(cards) {
    if (!cards.length) return;
    var container = cards[0].closest("[data-log-input]");
    if (!container || !window.Shiny) return;
    cards.forEach(function (card) {
      card.classList.add("is-saving");
      card.classList.remove("is-failed");
      card.querySelector(".set-status").textContent = "Saving…";
    });
    Shiny.setInputValue(container.dataset.logInput,
      { sets: cards.map(cardValues), nonce: Date.now() },
      { priority: "event" });
  }

  function findCard(id) {
    return document.querySelector('.set-card[data-set-id="' + id + '"]');
  }

  // Rest timer -------------------------------------------------------------
  var timerEnd = null;
  var timerInterval = null;

  function formatSeconds(s) {
    var m = Math.floor(s / 60);
    var r = s % 60;
    return m + ":" + (r < 10 ? "0" : "") + r;
  }

  function stopTimer() {
    var el = document.getElementById("rest-timer");
    clearInterval(timerInterval);
    timerInterval = null;
    if (el) {
      el.hidden = true;
      el.classList.remove("is-done");
    }
  }

  function tick() {
    var el = document.getElementById("rest-timer");
    var left = Math.max(0, Math.round((timerEnd - Date.now()) / 1000));
    el.querySelector(".rest-time").textContent = formatSeconds(left);
    if (left <= 0) {
      clearInterval(timerInterval);
      el.classList.add("is-done");
      el.querySelector(".rest-label").textContent = "Go";
      if (navigator.vibrate) navigator.vibrate([200, 100, 200]);
      setTimeout(stopTimer, 5000);
    }
  }

  function startTimer(seconds) {
    var el = document.getElementById("rest-timer");
    if (!el) return;
    timerEnd = Date.now() + seconds * 1000;
    el.hidden = false;
    el.classList.remove("is-done");
    el.querySelector(".rest-label").textContent = "Rest";
    clearInterval(timerInterval);
    tick();
    timerInterval = setInterval(tick, 250);
  }

  // Clicks -----------------------------------------------------------------
  document.addEventListener("click", function (e) {
    var step = e.target.closest(".step-btn");
    if (step) {
      var stepper = step.closest(".stepper");
      var input = stepper.querySelector("input");
      var size = parseFloat(stepper.dataset.step) || 1;
      var value = parseNum(input.value) || 0;
      input.value = Math.max(0, round3(value + size * Number(step.dataset.dir)));
      step.closest(".set-card").classList.add("is-dirty");
      return;
    }

    var rir = e.target.closest(".rir-btn");
    if (rir) {
      var wasActive = rir.classList.contains("active");
      rir.closest(".rir-row").querySelectorAll(".rir-btn").forEach(function (b) {
        b.classList.remove("active");
      });
      if (!wasActive) rir.classList.add("active");
      rir.closest(".set-card").classList.add("is-dirty");
      return;
    }

    var done = e.target.closest(".done-btn");
    if (done) {
      sendSets([done.closest(".set-card")]);
      return;
    }

    var all = e.target.closest(".log-all-btn");
    if (all) {
      var fold = all.closest(".set-fold");
      sendSets(Array.prototype.slice.call(fold.querySelectorAll(".set-card:not(.is-logged)")));
      return;
    }

    var hint = e.target.closest(".hint-btn");
    if (hint) {
      hint.closest(".set-group").querySelectorAll(".set-card:not(.is-logged)").forEach(function (card) {
        card.querySelector('[data-field="kg"] input').value = hint.dataset.kg;
        card.querySelector('[data-field="reps"] input').value = hint.dataset.reps;
        card.classList.add("is-dirty");
      });
      hint.classList.add("is-applied");
      return;
    }

    if (e.target.closest("#rest-timer")) stopTimer();
  });

  document.addEventListener("input", function (e) {
    var card = e.target.closest && e.target.closest(".set-card");
    if (card) card.classList.add("is-dirty");
  });

  document.addEventListener("keydown", function (e) {
    if (e.key === "Enter" && e.target.id === "login_password") {
      document.getElementById("login_submit").click();
    }
  });

  // Server messages ----------------------------------------------------------
  function register() {
    Shiny.addCustomMessageHandler("set-saved", function (msg) {
      var card = findCard(msg.id);
      if (!card) return;
      card.classList.remove("is-saving", "is-failed", "is-dirty");
      card.classList.add("is-logged");
      card.querySelector(".set-status").textContent = "";
      if (msg.rest) startTimer(msg.rest);
    });

    Shiny.addCustomMessageHandler("set-failed", function (msg) {
      var card = findCard(msg.id);
      if (!card) return;
      card.classList.remove("is-saving");
      card.classList.add("is-failed");
      card.querySelector(".set-status").textContent =
        "Not saved (" + msg.message + "). Your numbers are kept: tap ✓ to retry.";
    });

    Shiny.addCustomMessageHandler("auth-remember", function (msg) {
      try { localStorage.setItem("wl-auth", msg.token); } catch (err) { /* private mode */ }
    });
  }

  // shiny:connected is a jQuery event; both Shiny and jQuery are loaded by
  // the time the DOM is ready.
  document.addEventListener("DOMContentLoaded", function () {
    register();
    window.jQuery(document).on("shiny:connected", function () {
      var token = null;
      try { token = localStorage.getItem("wl-auth"); } catch (err) { token = null; }
      if (token) Shiny.setInputValue("auth_token", token);
    });
  });
})();
