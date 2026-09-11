// Page list: one entry per wiki page. `id` is also the URL hash and the
// markdown filename (pages/<id>.md). Grouped into sections purely for
// the sidebar's visual grouping -- order here is the order shown.
const GUIDE_SECTIONS = [
  { label: 'Getting Started', pages: [
    { id: 'getting-started', title: 'Getting Started' },
  ]},
  { label: 'Workflow', pages: [
    { id: 'dashboard',       title: 'Campaign Dashboard' },
    { id: 'orientation',     title: 'Landmarks' },
    { id: 'campaign-scope',  title: 'Campaign Scope' },
    { id: 'facilities',      title: 'Facilities' },
    { id: 'health-areas',    title: 'Health Area Mapping' },
    { id: 'team-areas',      title: 'Team Area Mapping' },
    { id: 'export',          title: 'Export' },
  ]},
  { label: 'Reference', pages: [
    { id: 'mapping-mechanics', title: 'Mapping Mechanics' },
    { id: 'admin',      title: 'Admin' },
    { id: 'reference',  title: 'Glossary & Troubleshooting' },
  ]},
];

// Flat lookup: id -> title, built once from GUIDE_SECTIONS above.
const PAGE_TITLES = {};
GUIDE_SECTIONS.forEach(function(section) {
  section.pages.forEach(function(p) { PAGE_TITLES[p.id] = p.title; });
});

const pageCache = {}; // id -> raw markdown text, filled in as pages are opened/searched

function buildSidebarNav() {
  const nav = document.getElementById('guide-nav');
  GUIDE_SECTIONS.forEach(function(section) {
    const label = document.createElement('div');
    label.className = 'guide-section-label';
    label.textContent = section.label;
    nav.appendChild(label);
    section.pages.forEach(function(p) {
      const a = document.createElement('a');
      a.href = '#' + p.id;
      a.textContent = p.title;
      a.dataset.pageId = p.id;
      nav.appendChild(a);
    });
  });
}

function setActiveNavLink(pageId) {
  document.querySelectorAll('#guide-nav a').forEach(function(a) {
    a.classList.toggle('active', a.dataset.pageId === pageId);
  });
}

function fetchPage(pageId) {
  if (pageCache[pageId]) return Promise.resolve(pageCache[pageId]);
  return fetch('pages/' + pageId + '.md')
    .then(function(res) {
      if (!res.ok) throw new Error('Page not found: ' + pageId);
      return res.text();
    })
    .then(function(text) { pageCache[pageId] = text; return text; });
}

function renderPage(pageId) {
  const content = document.getElementById('guide-content-inner');
  if (!PAGE_TITLES[pageId]) pageId = 'getting-started';
  fetchPage(pageId).then(function(md) {
    content.innerHTML = marked.parse(md);
    setActiveNavLink(pageId);
    document.getElementById('guide-content').scrollTop = 0;
    // Cross-links between pages are written in markdown as plain
    // relative links (e.g. [Health Areas](health-areas)) -- intercept
    // clicks on those so they navigate within the guide via the hash
    // router below, instead of the browser trying to load a file that
    // doesn't exist at that path.
    content.querySelectorAll('a').forEach(function(a) {
      const href = a.getAttribute('href') || '';
      if (PAGE_TITLES[href]) {
        a.addEventListener('click', function(e) {
          e.preventDefault();
          window.location.hash = href;
        });
      }
    });
  }).catch(function(err) {
    content.innerHTML = '<p>Could not load this page.</p>';
    console.error(err);
  });
}

function goToHash() {
  const pageId = window.location.hash.replace('#', '') || 'getting-started';
  renderPage(pageId);
}

// Search: fetches every page's raw text once (lazily, on first
// keystroke), then does a plain substring match against the query --
// no indexing, no fuzzy matching, per "keep this simple". Shows up to
// 8 matching pages with a short snippet of surrounding text.
let allPagesLoaded = false;
function ensureAllPagesLoaded() {
  if (allPagesLoaded) return Promise.resolve();
  const ids = Object.keys(PAGE_TITLES);
  return Promise.all(ids.map(fetchPage)).then(function() { allPagesLoaded = true; });
}

function runSearch(query) {
  const resultsBox = document.getElementById('guide-search-results');
  if (!query || query.trim().length < 2) { resultsBox.innerHTML = ''; return; }
  ensureAllPagesLoaded().then(function() {
    const q = query.trim().toLowerCase();
    const matches = [];
    Object.keys(pageCache).forEach(function(id) {
      const text = pageCache[id];
      const idx = text.toLowerCase().indexOf(q);
      if (idx > -1) {
        const start = Math.max(0, idx - 30);
        const snippet = text.substring(start, idx + q.length + 30).replace(/\n/g, ' ');
        matches.push({ id: id, title: PAGE_TITLES[id], snippet: snippet });
      }
    });
    resultsBox.innerHTML = matches.slice(0, 8).map(function(m) {
      return '<a href="#' + m.id + '"><div>' + m.title + '</div>' +
             '<div class="snippet">…' + m.snippet + '…</div></a>';
    }).join('');
  });
}

document.addEventListener('DOMContentLoaded', function() {
  buildSidebarNav();
  window.addEventListener('hashchange', goToHash);
  goToHash();

  const searchInput = document.getElementById('guide-search');
  let searchDebounce = null;
  searchInput.addEventListener('input', function() {
    clearTimeout(searchDebounce);
    const value = searchInput.value;
    searchDebounce = setTimeout(function() { runSearch(value); }, 200);
  });
});
