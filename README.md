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
* **`company`** – zero‑lag completions.
* **`yasnippet`** – snippet expansion.
* **`flycheck`** – on‑the‑fly diagnostics, including ESLint via local `node_modules/`.
* **`git-gutter`** – live diff in the fringe.
* **`multiple-cursors`**, **`drag-stuff`**, **`expand-region`** – edit at warp speed.

### Language support

* **Python** – Pyright LSP, Poetry env detection.
* **JavaScript / TypeScript** – TS/TSX Tree‑sitter modes (via `treesit-auto`), ESLint, Prettier via Apheleia.
* **Go** – `go-mode` + `gopls`.
* **Markdown / YAML / Dockerfile** – dedicated modes.

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

### Terminal setup (`emacs -nw`)

Two pieces of terminal configuration live outside this repo but are required for terminal Emacs to receive every key.

**Meta key (Ghostty).** macOS reserves Option for accented characters, so Ghostty does not send it as Alt/Meta. In `~/.config/ghostty/config`:

```
macos-option-as-alt = true
```

Use `left` or `right` instead of `true` to keep one Option key free for accents.

**`C-S-<letter>` inside tmux.** Legacy terminal encoding has no Shift bit for Control chords, so `C-S-d` and `C-d` arrive as the same byte — which breaks `duplicate-current-line` and `C-S-f`/`C-S-b`/`C-S-p`/`C-S-n` shift‑selection. Outside tmux the `kkp` package handles this through Ghostty's Kitty keyboard protocol; inside tmux it cannot, because tmux never answers kkp's handshake. Copy the settings from [`examples/tmux.conf`](examples/tmux.conf) into your `~/.tmux.conf`.

That file is only tmux's half — the Emacs half is the "Kitty Keyboard Protocol" section of `init.el`, and both are required.

## 📦 Package roster (core)

| Category        | Packages                                                                                                                                                                   |
| --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| UI / Navigation | ivy, counsel, swiper, flx, avy, doom-modeline, which-key                                                                                                                   |
| Editing         | company, yasnippet, flycheck, git-gutter, multiple-cursors, drag-stuff, expand-region, highlight-indent-guides, undo-fu, vundo                              |
| Languages       | lsp-mode, lsp-pyright, go-mode, treesit-auto, apheleia, poetry, add-node-modules-path, web-mode (for JSX), markdown-mode, yaml-mode, dockerfile-mode |
| Tooling         | exec-path-from-shell, editorconfig, magit, projectile, counsel-projectile, treemacs, treemacs-projectile                                                                  |
| Appearance      | material-theme, nerd-icons, org-modern                                                                                                                                   |

## ⌨️ Key bindings (custom)

| Key | Command |
|---|---|
| `C-x C-f` | `counsel-find-file` |
| `C-x b` | `ivy-switch-buffer` |
| `C-s` | `swiper` |
| `C-x g` | `magit-status` |
| `C-c p` | projectile prefix |
| `C-c p s g` | ripgrep from project root (`my/counsel-rg-at-project-root`) |
| `M-0` | toggle treemacs sidebar for the current project |
| `C-S-d` | duplicate current line |
| `C-x C-a` | rename current file and buffer in one go |
| `C-x u` | `vundo` (visual undo tree) |
| `C-/` | undo (`undo-fu-only-undo`) |
| `C-?` / `C-M-_` | redo (`undo-fu-only-redo`) |
| `C->` / `C-<` | multiple-cursors next/previous |
| `C-c C-w` | multiple-cursors mark all |
| `C-:` | `avy-goto-char` |
| `M-p` / `M-n` | drag line up/down |
| `C-M-SPC` / `C-+` | expand / contract region |
| `M-RET` | open a new indented line below |

## 🚀 Usage tips

* `M-x lsp-describe-session` – inspect active LSP workspaces.
* `C-M-j` (`ivy-immediate-done`) in any Ivy minibuffer — use the literal text you typed instead of the highlighted candidate. Handy when creating a file whose name is a prefix of an existing one (e.g. typing `.env` when `.env.example` already exists).

Fork away and tailor to your workflow!  PRs and suggestions welcome.
