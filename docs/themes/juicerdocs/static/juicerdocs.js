/*
 * juicerdocs — small client-side helpers.
 *
 *   - Theme toggle (data-theme attribute on <html>; persisted in
 *     localStorage; the <head> snippet applies it before paint so there's
 *     no white flash on dark-mode reload).
 *   - Mobile sidebar toggle (body[data-sidebar-open="true"]).
 *   - "Copy" buttons + language badges on every <pre> code block.
 *   - Mark active sidebar link based on current URL.
 *   - Tabs widget (synthesizes button bar from panels).
 *   - Client-side search via /search.json.
 *   - "On this page" right-rail active-heading highlight via
 *     IntersectionObserver.
 *
 * State is conveyed exclusively via the `is-*` modifier classes
 * (.is-active, .is-copied) and named component classes — no Tailwind
 * utility strings live in this file.
 *
 * Self-contained — no external dependencies.
 */

(function () {
  "use strict";

  // ===== Theme toggle (data-theme attribute) =====
  const themeBtn = document.getElementById("juicerdocs-theme-toggle");
  if (themeBtn) {
    themeBtn.addEventListener("click", () => {
      const cur  = document.documentElement.getAttribute("data-theme");
      const next = cur === "dark" ? "light" : "dark";
      document.documentElement.setAttribute("data-theme", next);
      try { localStorage.setItem("juicerdocs-theme", next); } catch (e) { /* private mode */ }
    });
  }

  // ===== Mobile sidebar overlay =====
  // Toggles `body[data-sidebar-open="true"]`, which the CSS uses to slide
  // the sidebar in from the left as a fixed overlay. Uses event delegation
  // so a missing element on this page doesn't matter; clicks elsewhere
  // (backdrop, nav links, Esc key) all close the sidebar.
  function setSidebar(open) {
    if (open) document.body.setAttribute("data-sidebar-open", "true");
    else      document.body.removeAttribute("data-sidebar-open");
  }
  document.addEventListener("click", (e) => {
    if (e.target.closest("#juicerdocs-sidebar-toggle")) {
      e.preventDefault();
      e.stopPropagation();
      setSidebar(document.body.getAttribute("data-sidebar-open") !== "true");
      return;
    }
    if (e.target.closest(".juicerdocs-sidebar-backdrop")) {
      setSidebar(false);
      return;
    }
    // Close when a sidebar nav link is tapped (mobile UX).
    const link = e.target.closest(".juicerdocs-sidebar-aside a");
    if (link) setSidebar(false);
  });
  document.addEventListener("keydown", (e) => { if (e.key === "Escape") setSidebar(false); });

  // ===== Code block copy buttons + language badge =====
  document.querySelectorAll("pre > code").forEach((code) => {
    const pre = code.parentElement;
    if (!pre || pre.dataset.juicerdocsCopyDone) return;
    pre.dataset.juicerdocsCopyDone = "1";

    // Tag <pre> with data-language so the CSS ::before can show it.
    const cls   = code.className || "";
    const match = cls.match(/language-(\S+)/);
    if (match) pre.dataset.language = match[1];

    const btn = document.createElement("button");
    btn.className = "juicerdocs-copy";
    btn.type = "button";
    btn.setAttribute("aria-label", "Copy code");
    btn.textContent = "Copy";
    btn.addEventListener("click", async () => {
      try {
        await navigator.clipboard.writeText(code.innerText);
        btn.textContent = "Copied!";
        btn.classList.add("is-copied");
        setTimeout(() => {
          btn.textContent = "Copy";
          btn.classList.remove("is-copied");
        }, 1500);
      } catch (e) {
        btn.textContent = "Error";
      }
    });
    pre.appendChild(btn);
  });

  // ===== Tabs widget — see {= tabs / tab =} shortcodes =====
  // Builds a button bar from the .juicerdocs-tab-panel children of a
  // .juicerdocs-tabs container; uses .is-active on both the button and
  // the panel to express which one is current.
  document.querySelectorAll(".juicerdocs-tabs[data-juicerdocs-tabs]").forEach((root) => {
    const panels = Array.from(root.querySelectorAll(":scope > .juicerdocs-tab-panel"));
    if (panels.length === 0) return;
    const bar = document.createElement("div");
    bar.className = "juicerdocs-tabs-bar";
    panels.forEach((panel, i) => {
      const label = panel.dataset.tabLabel || `Tab ${i + 1}`;
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "juicerdocs-tabs-button";
      if (i === 0) {
        btn.classList.add("is-active");
        panel.classList.add("is-active");
      }
      btn.textContent = label;
      btn.addEventListener("click", () => {
        bar.querySelectorAll(".juicerdocs-tabs-button").forEach((b) => b.classList.remove("is-active"));
        panels.forEach((p) => p.classList.remove("is-active"));
        btn.classList.add("is-active");
        panel.classList.add("is-active");
      });
      bar.appendChild(btn);
    });
    root.insertBefore(bar, root.firstChild);
    root.removeAttribute("data-juicerdocs-tabs");
  });

  // ===== Sidebar active-link highlight =====
  const here = location.pathname.replace(/\/+$/, "/") || "/";
  document.querySelectorAll("[data-juicerdocs-nav-link]").forEach((a) => {
    const href = a.getAttribute("href");
    if (!href) return;
    const norm = href.replace(/\/+$/, "/") || "/";
    if (norm === here) a.classList.add("is-active");
  });

  // ===== "On this page" — active-heading highlight =====
  //
  // We use IntersectionObserver to track which heading is currently
  // closest to the top of the viewport. Whichever heading just crossed
  // a 0–25% horizontal band gets the "active" style on its TOC link.
  // Falls back to highlighting nothing when no headings exist or
  // IntersectionObserver isn't available.
  (function highlightActiveHeading() {
    const tocLinks = document.querySelectorAll("[data-juicerdocs-toc-link]");
    if (tocLinks.length === 0 || !("IntersectionObserver" in window)) return;

    const linkById = new Map();
    tocLinks.forEach((a) => linkById.set(a.dataset.jdTocId, a));

    const headings = Array.from(document.querySelectorAll("article h2[id], article h3[id], article h4[id]"))
      .filter((h) => linkById.has(h.id));

    let active = null;
    function setActive(id) {
      if (active === id) return;
      if (active) {
        const prev = linkById.get(active);
        if (prev) prev.classList.remove("is-active");
      }
      active = id;
      if (id) {
        const next = linkById.get(id);
        if (next) next.classList.add("is-active");
      }
    }

    // Observe each heading. The `rootMargin` shrinks the viewport from
    // the top and bottom so we get a stable "above the fold" band that
    // tracks reading position rather than the whole page.
    const visible = new Set();
    const observer = new IntersectionObserver(
      (entries) => {
        entries.forEach((e) => {
          if (e.isIntersecting) visible.add(e.target.id);
          else visible.delete(e.target.id);
        });
        // Pick whichever visible heading appears earliest in document order.
        if (visible.size > 0) {
          const earliest = headings.find((h) => visible.has(h.id));
          if (earliest) setActive(earliest.id);
        }
      },
      { rootMargin: "-80px 0px -75% 0px", threshold: 0 },
    );
    headings.forEach((h) => observer.observe(h));

    // Initial state — whatever heading is highest above the fold.
    const initial = headings.find((h) => h.getBoundingClientRect().top >= 80) || headings[0];
    if (initial) setActive(initial.id);
  })();

  // ===== Search =====
  const searchInput = document.getElementById("juicerdocs-search");
  const searchResults = document.getElementById("juicerdocs-search-results");
  if (searchInput && searchResults) {
    let index = null;
    let activeIndex = -1;

    async function ensureIndex() {
      if (index) return index;
      try {
        const res = await fetch("/search.json");
        index = await res.json();
      } catch (e) {
        index = [];
      }
      return index;
    }

    function snippet(content, q) {
      const lc = content.toLowerCase();
      const idx = lc.indexOf(q.toLowerCase());
      if (idx < 0) return content.slice(0, 80) + (content.length > 80 ? "…" : "");
      const start = Math.max(0, idx - 30);
      const end = Math.min(content.length, idx + q.length + 50);
      return (start > 0 ? "…" : "") + content.slice(start, end) + (end < content.length ? "…" : "");
    }

    function setHidden(hidden) {
      if (hidden) searchResults.setAttribute("hidden", "");
      else        searchResults.removeAttribute("hidden");
    }

    function renderResults(matches, q) {
      searchResults.innerHTML = "";
      if (matches.length === 0) {
        const empty = document.createElement("div");
        empty.className = "juicerdocs-result-empty";
        empty.textContent = "No matches.";
        searchResults.appendChild(empty);
      } else {
        for (let i = 0; i < matches.length; i++) {
          const r = matches[i];
          const a = document.createElement("a");
          a.className = "juicerdocs-result";
          a.href = r.url;
          a.dataset.idx = i;
          const title = document.createElement("span");
          title.className = "juicerdocs-result-title";
          title.textContent = r.title || r.url;
          const snip = document.createElement("span");
          snip.className = "juicerdocs-result-snippet";
          snip.textContent = snippet(r.content || r.summary || "", q);
          a.appendChild(title);
          a.appendChild(snip);
          searchResults.appendChild(a);
        }
      }
      setHidden(false);
      activeIndex = -1;
    }

    async function doSearch(q) {
      if (!q) {
        setHidden(true);
        return;
      }
      const data = await ensureIndex();
      const lc = q.toLowerCase();
      const matches = data
        .filter((r) =>
          (r.title && r.title.toLowerCase().includes(lc)) ||
          (r.summary && r.summary.toLowerCase().includes(lc)) ||
          (r.content && r.content.toLowerCase().includes(lc))
        )
        .slice(0, 10);
      renderResults(matches, q);
    }

    searchInput.addEventListener("input", (e) => doSearch(e.target.value.trim()));
    searchInput.addEventListener("focus", (e) => {
      if (e.target.value.trim()) doSearch(e.target.value.trim());
    });
    searchInput.addEventListener("keydown", (e) => {
      const items = searchResults.querySelectorAll(".juicerdocs-result");
      if (e.key === "ArrowDown") {
        e.preventDefault();
        activeIndex = Math.min(items.length - 1, activeIndex + 1);
      } else if (e.key === "ArrowUp") {
        e.preventDefault();
        activeIndex = Math.max(0, activeIndex - 1);
      } else if (e.key === "Enter" && activeIndex >= 0) {
        e.preventDefault();
        items[activeIndex].click();
      } else if (e.key === "Escape") {
        setHidden(true);
        searchInput.blur();
      }
      items.forEach((it, i) => {
        if (i === activeIndex) it.classList.add("is-active");
        else                   it.classList.remove("is-active");
      });
    });
    document.addEventListener("click", (e) => {
      if (!searchInput.contains(e.target) && !searchResults.contains(e.target)) {
        setHidden(true);
      }
    });
  }
})();
