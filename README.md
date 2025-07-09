# My Emacs Config

An advanced and evolving Emacs configuration that stream‑lines everyday development across Python, JS/TS, Go and more.  It mixes modern Tree‑sitter modes, LSP back‑ends, and UI niceties while staying **vanilla‑Emacs‑first** so you can cherry‑pick what you like.

> **Obs.** This setup assumes **`zsh`** is your login shell.  Your NVM initialisation must live in `~/.zprofile` (not just `~/.zshrc`) so GUI‑launched Emacs can inherit `PATH`, `NVM_DIR`, etc.

![image](https://github.com/user-attachments/assets/78528fce-d285-4353-b3a6-9980729c7761)

## ✨ Highlights

### Modern interface & navigation

* **`ivy`**\*\* / ****`counsel`**** / \*\***`flx`** – fuzzy search for commands, files and buffers.
* **`avy`** – instant navigation to visible text.
* **`doom-modeline`** – clean, informative modeline with LSP and Git status.
* **`which-key`** – discover keybindings on the fly.
* **`zoom`** – Goldilocks window resizing.

### Coding experience

* **`lsp-mode`** with language‑specific servers (Pyright, typescript‑language‑server, gopls).
* **Tree‑sitter major modes** (`typescript‑ts‑mode`, `tsx‑ts‑mode`) for blazing‑fast JS/TS highlighting.
* **`company`**\*\* + \*\***`company‑box`** – zero‑lag completions.
* **`yasnippet`** – snippet expansion.
* **`flycheck`** – on‑the‑fly diagnostics, including ESLint via local `node_modules/`.
* **`git-gutter`** – live diff in the fringe.
* **`multiple-cursors`**, **`drag-stuff`**, **`expand-region`** – edit at warp speed.
* **GitHub Copilot** via `copilot.el` (device‑code auth, uses NVM’s Node).

### Language support

* **Python** – Pyright LSP, Poetry env detection.
* **JavaScript / TypeScript** – TS/TSX Tree‑sitter modes, ESLint, Prettier via Apheleia.
* **Go** – `go-mode` + `gopls`.
* **Markdown / YAML / Dockerfile / Terraform** – dedicated modes.
* **Org‑mode AI** – `org-ai` brings GPT inside Org buffers.

### Visual polish & ergonomics

* **`material-theme`** plus highlight‑indent‑guides.
* Smartparens, undo‑fu + vundo visual tree.
* Header‑line breadcrumb showing truncated path.

## 🛠 Installation

### 1 · Clone the repo

```bash
git clone git@github.com:alissonperez/emacs-conf.git ~/.emacs.d
```

### 2 · Install Emacs 29+

```bash
brew install --cask emacs  # macOS example
```

### 3 · Zsh & NVM setup

```zsh
# ~/.zprofile – ensure GUI Emacs inherits Node
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
```

Then inside Emacs run:

```text
M-x copilot-install-server RET
M-x copilot-login RET
```

## 📦 Package roster (core)

| Category        | Packages                                                                                                                                                                   |
| --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| UI / Navigation | ivy, counsel, swiper, flx, avy, doom-modeline, which-key, zoom                                                                                                             |
| Editing         | company, company-box, yasnippet, flycheck, git-gutter, multiple-cursors, smartparens, drag-stuff, expand-region, highlight-indent-guides, undo-fu, vundo                   |
| Languages       | lsp-mode, lsp-pyright, go-mode, typescript-ts-mode, tsx-ts-mode, web-mode (for JSX), nodejs-repl, markdown-mode, yaml-mode, dockerfile-mode, terraform-mode, protobuf-mode |
| Tooling         | exec-path-from-shell, editorconfig, magit, projectile, counsel-projectile, treemacs, git-gutter                                                                            |
| AI              | copilot.el, org-ai                                                                                                                                                         |
| Appearance      | material-theme, nerd-icons                                                                                                                                                 |

## 🚀 Usage tips

* `M-x lsp-describe-session` – inspect active LSP workspaces.
* `C-S-d` duplicates the current line; `M-p / M-n` drags it.
* `C-x u` launches **vundo** visual undo tree.

Fork away and tailor to your workflow!  PRs and suggestions welcome.
