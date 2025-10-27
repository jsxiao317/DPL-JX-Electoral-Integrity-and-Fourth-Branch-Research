````markdown
# Electoral Integrity — Minimal site

This branch provides a minimal site with an About page (index.html) and a single working-paper view (paper.html) that renders `paper.md`.

How to preview locally:
- From repository root run:
  - Python 3: `python -m http.server 8000`
  - or: `npx http-server -p 8000`
- Open: http://localhost:8000

How to publish on GitHub Pages:
- Merge the branch into main, or set Pages to serve from this branch.
- Ensure `paper.md` is present at the repository root (next to index.html) so paper.html can fetch it.

Notes:
- The renderer uses marked.js via CDN; if your environment blocks the CDN, add a local `assets/marked.min.js` file.
- If you want PDF generation from `paper.md` automatically, I can help add a GitHub Actions workflow to produce `paper.pdf`.
