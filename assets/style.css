// Simple renderer for paper.html: fetch paper.md and render with marked
(async function(){
  const paperEl = document.getElementById('paper');
  const loading = document.getElementById('paper-loading');

  async function loadMarkdown(file='paper.md'){
    try{
      loading && (loading.textContent = `Loading ${file}...`);
      const res = await fetch(file);
      if(!res.ok) throw new Error(`Failed to fetch ${file}: ${res.status}`);
      const md = await res.text();
      renderMarkdown(md);
      loading && loading.remove();
    }catch(err){
      console.error('Error loading markdown:', err);
      paperEl.innerHTML = `<p class="muted">Could not load paper content. Ensure <code>${file}</code> exists and the site is served over HTTP(S). Error: ${err.message}</p>`;
    }
  }

  function renderMarkdown(md){
    if(typeof marked === 'undefined'){
      paperEl.innerHTML = '<p class="muted">Markdown renderer not available. Ensure marked.js is accessible (CDN or assets/marked.min.js).</p>';
      console.error('marked is undefined; cannot render markdown.');
      return;
    }
    try{
      const html = marked.parse(md);
      paperEl.innerHTML = html;
    }catch(e){
      console.error('marked parse error:', e);
      paperEl.innerHTML = '<p class="muted">An error occurred while rendering the manuscript. See console for details.</p>';
    }
  }

  await loadMarkdown('paper.md');
})();
