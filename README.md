# My Emacs Config

An advanced and evolving Emacs configuration that stream‑lines everyday development across Python, JS/TS, Go and more.  It mixes modern Tree‑sitter modes, LSP back‑ends, and UI niceties while staying **vanilla‑Emacs‑first** so you can cherry‑pick what you like.

> **Obs.** This setup assumes **`zsh`**.  GUI‑launched Emacs spawns a *non‑login* shell, so `PATH`/`NVM_DIR` must be exported from `~/.zshenv` (sourced by every zsh, login or not) — see the Zsh & NVM section below.

![image](https://github.com/user-attachments/assets/78528fce-d285-4353-b3a6-9980729c7761)

## ✨ Highlights

### Modern interface & navigation

* **`ivy`** / **`counsel`** / **`flx`** – fuzzy search for commands, files and buffers.
* **`avy`** – instant navigation to visible text.
* **`doom-modeline`** – clean, informative modeline with LSP and Git status.
* **`which-key`** – discover keybindings on the fly.

### Coding experience

* **`lsp-mode`** with language‑specific servers (Pyright, typescript‑language‑server, gopls).
* **Tree‑sitter major modes** (`typescript‑ts‑mode`, `tsx‑ts‑mode`) for blazing‑fast JS/TS highlighting.
* **`company`**\*\* + \*\***`company‑box`** – zero‑lag completions.
* **`yasnippet`** – snippet expansion.
* **`flycheck`** – on‑the‑fly diagnostics, including ESLint via local `node_modules/`.
* **`git-gutter`** – live diff in the fringe.
* **`multiple-cursors`**, **`drag-stuff`**, **`expand-region`** – edit at warp speed.
* **GitHub Copilot** via `copilot.el` (device‑code auth, uses NVM’s Node).

### Language support

* **Python** – Pyright LSP, Poetry env detection.
* **JavaScript / TypeScript** – TS/TSX Tree‑sitter modes (via `treesit-auto`), ESLint, Prettier via Apheleia.
* **Go** – `go-mode` + `gopls`.
* **Markdown / YAML / Dockerfile / Terraform** – dedicated modes.
* **Org‑mode AI** – `org-ai` brings GPT inside Org buffers.

### Visual polish & ergonomics

* **`material-theme`** plus highlight‑indent‑guides.
* Built‑in `electric-pair-mode` for paren pairing, `undo‑fu` + `vundo` visual tree.
* Header‑line breadcrumb showing truncated path.

## 🛠 Installation

### Clone the repo

```bash
git clone git@github.com:alissonperez/emacs-conf.git ~/.emacs.d
```

### Install Emacs 30+

```bash
brew install --cask emacs  # macOS example
```

### Install ripgrep

```bash
brew install ripgrep
```

### Language servers & formatters

```bash
npm i -g typescript-language-server typescript prettier pyright
brew install gopls
```

### Zsh & NVM setup

GUI‑launched Emacs spawns a **non‑login** zsh, which only sources `~/.zshenv` — not `~/.zshrc` or `~/.zprofile`. Keep `~/.zshenv` fast (static `PATH` only, no `nvm.sh`/`pyenv init` evals):

```zsh
# ~/.zshenv – read by every zsh, login or not
export NVM_DIR="$HOME/.nvm"
export PYENV_ROOT="$HOME/.pyenv"

_path_prepend() { [[ -d "$1" ]] && export PATH="$1:$PATH"; }
_path_prepend /opt/homebrew/bin
_path_prepend "$PYENV_ROOT/shims"

# Resolve nvm's default alias without sourcing nvm.sh.
if [[ -d "$NVM_DIR/versions/node" ]]; then
  _nvm_default=$(cat "$NVM_DIR/alias/default" 2>/dev/null)
  _nvm_ver=$(command ls "$NVM_DIR/versions/node" | grep "^v${_nvm_default#v}" | sort -V | tail -1)
  [[ -z "$_nvm_ver" ]] && _nvm_ver=$(command ls "$NVM_DIR/versions/node" | sort -V | tail -1)
  [[ -n "$_nvm_ver" ]] && _path_prepend "$NVM_DIR/versions/node/$_nvm_ver/bin"
fi

unset -f _path_prepend
```

Full `nvm.sh`/`pyenv init` sourcing for interactive terminals stays in `~/.zprofile` as usual.

Then inside Emacs run:

```text
M-x copilot-install-server RET
M-x copilot-login RET
```

## 📦 Package roster (core)

| Category        | Packages                                                                                                                                                                   |
| --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| UI / Navigation | ivy, counsel, swiper, flx, avy, doom-modeline, which-key                                                                                                                   |
| Editing         | company, company-box, yasnippet, flycheck, git-gutter, multiple-cursors, drag-stuff, expand-region, highlight-indent-guides, undo-fu, vundo                              |
| Languages       | lsp-mode, lsp-pyright, go-mode, treesit-auto, apheleia, poetry, add-node-modules-path, web-mode (for JSX), nodejs-repl, markdown-mode, yaml-mode, dockerfile-mode, terraform-mode, protobuf-mode |
| Tooling         | exec-path-from-shell, editorconfig, magit, projectile, counsel-projectile, treemacs, treemacs-projectile                                                                  |
| AI              | copilot.el, org-ai                                                                                                                                                         |
| Appearance      | material-theme, nerd-icons, org-bullets                                                                                                                                   |

## 🚀 Usage tips

* `M-x lsp-describe-session` – inspect active LSP workspaces.
* `C-S-d` duplicates the current line; `M-p / M-n` drags it.
* `C-x u` launches **vundo** visual undo tree.
* `C-c p s g` – ripgrep from the project root (`my/counsel-rg-at-project-root`).
* `M-0` – toggle the treemacs sidebar for the current project.
* `C-x C-a` – rename the current file and its buffer in one go.
* `C-M-j` (`ivy-immediate-done`) in any Ivy minibuffer — use the literal text you typed instead of the highlighted candidate. Handy when creating a file whose name is a prefix of an existing one (e.g. typing `.env` when `.env.example` already exists).

Fork away and tailor to your workflow!  PRs and suggestions welcome.
