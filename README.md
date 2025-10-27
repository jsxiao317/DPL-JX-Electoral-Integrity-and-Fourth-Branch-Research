# Research Paper Page — Electoral Integrity and the Fourth Branch

This repository contains a small static site to present a political science working paper. The site is designed to be hosted on GitHub Pages and includes:

- index.html — landing page that renders Markdown, shows metadata, provides PDF download and citation options.
- paper.md — the working manuscript (editable).
- paper.pdf — compiled PDF (add this file to the repo root for downloads).
- assets/ — CSS and JavaScript for rendering and small interactions.

This README contains a short plan and deployment instructions.

Plan / Goals
- Make the working draft easily viewable online.
- Offer downloads (PDF, BibTeX).
- Provide an easy workflow for updating drafts and preserving versions.
- Link back to the GitHub repo for transparency and issue reporting.

Quick usage
1. Edit paper.md to update the manuscript.
2. Optionally add earlier versions like paper_v1.md, paper_v2.md for the versions menu.
3. Add a compiled PDF at `paper.pdf` (same repo root) so the "Download PDF" button works.

Local preview
- Open `index.html` in a browser (modern browsers will allow fetch of local files only if served through a local server).
- For correct behavior (fetching markdown), serve files via a simple local HTTP server. For example:

  Python 3:
  - In repo folder: python -m http.server 8000
  - Open http://localhost:8000

  Node (http-server):
  - npm install -g http-server
  - http-server -p 8000

Deploy to GitHub Pages (two simple options)

Option A: Use the repository root / main branch
1. Commit files to the repository root (index.html, paper.md, assets/, paper.pdf).
2. On GitHub, go to Settings → Pages → Source → choose "main" branch and "/" (root) folder. Save.
3. GitHub will publish the site to: https://<your-username>.github.io/<repo-name>/ (may take a few minutes).

Option B: Use gh-pages branch (recommended for automatic workflows)
1. Install the gh-pages npm package or use GitHub Actions to deploy.
2. Create a branch `gh-pages` and push the built site there (for a static site the files themselves).
   Example:
   - git checkout -b gh-pages
   - (Place the site files at the root)
   - git add . && git commit -m "Publish site"
   - git push origin gh-pages
3. On GitHub: Settings → Pages → Source → gh-pages branch.

Automated deploy with GitHub Actions (sketch)
- Add a workflow that runs on pushes to main and copies files to gh-pages, or uses actions-gh-pages to publish.

Citation
- Use the provided BibTeX snippet in the site (open Cite → Copy BibTeX).
- Customize authors, year, title, and URL before final publication.

Good practices
- Keep `paper.md` as the canonical source for easy editing.
- Tag release versions (e.g., v0.1-draft, v1.0-preprint) to mark important milestones.
- Add `paper_vN.md` files for earlier versions and the site will allow switching between them (update the versions list in index.html if you change names).
- For replication, add a `replication/` folder with code, data (or data access instructions), and README for reproducibility.

Licensing and Ethics
- Add an explicit license (e.g., CC-BY-NC) if you want reuse permissions.
- If your paper uses human subjects or restricted data, include a note about data sharing constraints.

Contact / Issues
- Use GitHub Issues: https://github.com/jsxiao317/DPL-JX-Electoral-Integrity-and-Fourth-Branch-Research/issues

Support
- If you want, I can:
  - Create the GitHub Action workflow to auto-deploy to gh-pages.
  - Convert the repository to include a /docs folder for GitHub Pages.
  - Help compile paper.md to PDF with Pandoc and add a basic Makefile.
---
