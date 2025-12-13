# Collaboration & Synchronisation Protocol

**Project:** DPL-JX Electoral Integrity and Fourth Branch Research
**Purpose:** Ensure clean, reproducible, conflict-free collaboration using GitHub + RStudio

---

## 1. Core Principle (Non‑Negotiable)

> **Always pull before you work. Always push after you commit.**

If this rule is followed, the repository will never desynchronise.

---

## 2. Required Setup (One‑Time)

Each collaborator must:

1. Clone the repository locally:

   ```bash
   git clone https://github.com/jsxiao317/DPL-JX-Electoral-Integrity-and-Fourth-Branch-Research.git
   ```

2. Open the project **only via the `.Rproj` file** in RStudio:

   * File → Open Project → select `DPL-JX-Electoral-Integrity-and-Fourth-Branch-Research.Rproj`

3. Confirm:

   * Project name appears top‑right in RStudio
   * **Git tab** is visible

4. Authenticate GitHub using a **Personal Access Token** (PAT), not a password.

---

## 3. Daily Workflow (Every Session)

### Step 1 — Pull (before doing anything)

In RStudio:

* Git tab → **Pull**

Or Terminal:

```bash
git pull
```

This ensures your local copy includes all collaborator changes.

---

### Step 2 — Work (File‑level division)

* Never edit the same script simultaneously.
* Each script has a clear owner.

**Example:**

* `02_cleaning.R` → Collaborator A
* `03_indexing.R` → Collaborator A
* `04_analysis_H1.R` → Collaborator B
* `05_analysis_H2.R` → Collaborator B

---

### Step 3 — Commit (small, logical units)

```bash
git add scripts/04_analysis_H2.R
git commit -m "Add multilevel model specification for H2"
```

Rules:

* Commit messages must describe *what changed* and *why*.
* Do not commit half‑finished work.

---

### Step 4 — Push (immediately)

```bash
git push
```

Your collaborator will receive your changes on their next pull.

---

## 4. Data Handling Rules (Critical)

* **Raw data is never pushed to GitHub.**
* Each collaborator stores data locally in:

  ```
  data/raw/
  ```
* All raw data formats (e.g. `.csv`) are ignored by `.gitignore`.
* Only **code, documentation, and small derived outputs** are tracked.

This ensures licensing compliance and reproducibility.

---

## 5. Synchronisation Checks (Any Time)

Run:

```bash
git status
```

Correct state:

```text
On branch main
Your branch is up to date with 'origin/main'.
```

If you see:

* **“behind”** → Pull
* **“ahead”** → Push
* **both** → Pull first, then Push

---

## 6. Conflict Avoidance Rules

* Always pull before working
* Never use `setwd()`
* Never edit the same file concurrently
* Never commit raw data
* Never push without understanding `git status`

If a conflict appears:

* Stop
* Communicate
* Resolve once, calmly

---

## 7. Emergency Commands (Rare)

If Git refuses to push:

```bash
git pull --rebase
git push
```

If unsure about repo state:

```bash
git log --oneline --decorate --graph --all
```

---

## 8. Final Rule to Remember

> **Git should feel boring.**
> If it feels dramatic, stop and check `git status`.

---

*This protocol is binding for all collaborators on this project.*
