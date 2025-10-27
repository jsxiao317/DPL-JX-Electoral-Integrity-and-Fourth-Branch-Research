// Minimal client-side logic to fetch and render markdown, build TOC, and enable citation modal + copy.
(async function(){
  const paperEl = document.getElementById('paper');
  const loading = document.getElementById('paper-loading');
  const tocList = document.getElementById('toc-list');
  const versionsContainer = document.getElementById('versions');
  const citeBtn = document.getElementById('cite-btn');
  const modal = document.getElementById('modal');
  const modalClose = document.getElementById('modal-close');
  const copyBib = document.getElementById('copy-bibtex');
  const downloadBib = document.getElementById('download-bib');

  async function loadMarkdown(file='paper.md'){
    try{
      loading && (loading.textContent = `Loading ${file}...`);
      const res = await fetch(file);
      if(!res.ok) throw new Error(`Failed to fetch ${file}: ${res.status}`);
      const md = await res.text();
      renderMarkdown(md);
      buildTOC(md);
      loading && loading.remove();
    }catch(err){
      paperEl.innerHTML = `<p class="muted">Could not load paper content. Check that <code>${file}</code> exists in the repository root. Error: ${err.message}</p>`;
    }
  }

  function renderMarkdown(md){
    // Use marked (loaded via CDN)
    if(typeof marked === 'undefined'){
      paperEl.innerHTML = '<p class="muted">Markdown renderer not available. Please include marked.js</p>';
      return;
    }
    const html = marked.parse(md);
    paperEl.innerHTML = html;
    // Add anchors to headings for TOC linking
    document.querySelectorAll('#paper h1, #paper h2, #paper h3').forEach(h=>{
      if(!h.id) h.id = h.textContent.trim().toLowerCase().replace(/\s+/g,'-').replace(/[^\w\-]/g,'');
    });
  }

  function buildTOC(md){
    tocList.innerHTML = '';
    // Simple approach: extract lines starting with ## or ### for a basic TOC
    const lines = md.split('\n');
    const items = [];
    for(const line of lines){
      let m = line.match(/^##\s+(.*)/);
      if(m) items.push({level:2,text:m[1]});
      m = line.match(/^###\s+(.*)/);
      if(m) items.push({level:3,text:m[1]});
    }
    if(items.length===0){
      tocList.innerHTML = '<p class="muted">No headings to build a table of contents.</p>';
      return;
    }
    const ul = document.createElement('ul');
    for(const it of items){
      const li = document.createElement('li');
      const a = document.createElement('a');
      a.href = '#'+it.text.trim().toLowerCase().replace(/\s+/g,'-').replace(/[^\w\-]/g,'');
      a.textContent = it.text;
      li.appendChild(a);
      if(it.level===3) li.style.marginLeft = '0.6rem';
      ul.appendChild(li);
    }
    tocList.appendChild(ul);
  }

  // Version switching
  document.querySelectorAll('.link-version').forEach(btn=>{
    btn.addEventListener('click', e=>{
      const file = e.currentTarget.getAttribute('data-file');
      loadMarkdown(file);
    });
  });

  // Citation modal handlers
  citeBtn.addEventListener('click', ()=>{
    modal.setAttribute('aria-hidden','false');
  });
  modalClose.addEventListener('click', ()=> modal.setAttribute('aria-hidden','true'));
  modal.addEventListener('click', (e)=> {
    if(e.target === modal) modal.setAttribute('aria-hidden','true');
  });

  copyBib.addEventListener('click', async()=>{
    const bib = document.getElementById('citation-bibtex').textContent;
    try{
      await navigator.clipboard.writeText(bib);
      copyBib.textContent = 'Copied!';
      setTimeout(()=> copyBib.textContent = 'Copy BibTeX', 1500);
    }catch(e){
      alert('Could not copy to clipboard. Please copy manually.');
    }
  });

  downloadBib.addEventListener('click', ()=>{
    const bib = document.getElementById('citation-bibtex').textContent;
    const blob = new Blob([bib], {type:'text/plain'});
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'electoral_integrity.bib';
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  });

  // Initial load
  await loadMarkdown('paper.md');

})();
