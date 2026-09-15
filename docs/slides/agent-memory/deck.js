(function () {
  "use strict";

  function initDeck() {
    var slides = Array.prototype.slice.call(document.querySelectorAll(".slide"));
    if (!slides.length) return;

    var current = 0;
    var total = slides.length;

    var progress = document.createElement("div");
    progress.className = "progress";
    document.body.appendChild(progress);

    var footer = document.createElement("div");
    footer.className = "footer-bar";
    var deckTitle = document.title || "";
    footer.innerHTML =
      '<span>' + deckTitle + '</span><span class="counter"></span>';
    document.body.appendChild(footer);

    var hint = document.createElement("div");
    hint.className = "nav-hint";
    hint.textContent = "← → / space · f plein écran";
    document.body.appendChild(hint);

    var zoneLeft = document.createElement("div");
    zoneLeft.className = "click-zone left";
    var zoneRight = document.createElement("div");
    zoneRight.className = "click-zone right";
    document.body.appendChild(zoneLeft);
    document.body.appendChild(zoneRight);

    function render() {
      slides.forEach(function (s, i) {
        s.classList.toggle("active", i === current);
      });
      var counter = footer.querySelector(".counter");
      counter.textContent = (current + 1) + " / " + total;
      progress.style.width = ((current + 1) / total * 100) + "%";
      var hash = "#" + (current + 1);
      if (history.replaceState) history.replaceState(null, "", hash);
    }

    function go(delta) {
      current = Math.min(total - 1, Math.max(0, current + delta));
      render();
    }

    function goTo(n) {
      current = Math.min(total - 1, Math.max(0, n));
      render();
    }

    document.addEventListener("keydown", function (e) {
      if (e.key === "ArrowRight" || e.key === " " || e.key === "PageDown") { go(1); e.preventDefault(); }
      else if (e.key === "ArrowLeft" || e.key === "PageUp") { go(-1); e.preventDefault(); }
      else if (e.key === "Home") { goTo(0); }
      else if (e.key === "End") { goTo(total - 1); }
      else if (e.key === "f" || e.key === "F") {
        if (!document.fullscreenElement) document.documentElement.requestFullscreen();
        else document.exitFullscreen();
      }
    });

    zoneLeft.addEventListener("click", function () { go(-1); });
    zoneRight.addEventListener("click", function () { go(1); });

    var startX = null;
    document.addEventListener("touchstart", function (e) { startX = e.touches[0].clientX; });
    document.addEventListener("touchend", function (e) {
      if (startX === null) return;
      var dx = e.changedTouches[0].clientX - startX;
      if (Math.abs(dx) > 50) go(dx < 0 ? 1 : -1);
      startX = null;
    });

    var initial = parseInt((location.hash || "").replace("#", ""), 10);
    if (!isNaN(initial) && initial >= 1 && initial <= total) current = initial - 1;

    render();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initDeck);
  } else {
    initDeck();
  }
})();
