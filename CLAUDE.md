# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration targeting Emacs 30+ on macOS with `zsh` as the login shell. All packages are managed by **straight.el** (not `package.el`, which is disabled in `early-init.el`).

## Architecture

Everything lives in `init.el` — there is no modular split into separate files. The config:

1. Bootstraps `straight.el` on first load (auto-downloads from GitHub if missing).
2. Loads all packages via `use-package` with `straight-use-package-by-default t`.
3. Defines a small number of custom interactive functions (`rename-file-and-buffer`, `duplicate-current-line`, `camel-to-snake`, `treemacs-toggle`, `my/counsel-rg-at-project-root`).

Key files:
- `early-init.el` — disables `package.el` before init
- `init.el` — entire configuration
- `snippets/` — yasnippet snippets (c++, python, prog-mode)
- `straight/` — straight.el repos and build artifacts (do not edit manually)
- `tree-sitter/` — compiled tree-sitter grammar `.dylib` files

## Package management

To add a package: add a `(use-package foo ...)` block to `init.el`. straight.el will fetch it on next Emacs startup.

To install tree-sitter grammars: run `M-x treesit-install-language-grammar` inside Emacs and select the language. Grammars live in `~/.emacs.d/tree-sitter/`.

## Key design decisions

- **Ivy/counsel/flx** stack for completion (not Helm, not Vertico). Fuzzy matching via `ivy--regex-fuzzy` everywhere except swiper.
- **LSP via lsp-mode** (not Eglot). Servers: `pyright` (Python), `typescript-language-server` (TS/TSX), `gopls` (Go).
- **Tree-sitter modes** replace legacy JS/TS modes: `.ts` → `typescript-ts-mode`, `.tsx` → `tsx-ts-mode`, `.js` → `js-ts-mode`.
- **Apheleia** handles on-save formatting (Prettier for TS/TSX). Go uses `gofmt-before-save`.
- **Copilot** is loaded from GitHub (`copilot-emacs/copilot.el`) via straight, not MELPA. Accepts completion with `C-<return>`.
- **`package.el` is intentionally disabled** — never add `(package-initialize)` or `:ensure t` outside of the copilot block (which uses `:ensure t` for historical reasons but straight handles it).

## Important keybindings (custom)

| Key | Command |
|---|---|
| `C-x C-f` | `counsel-find-file` |
| `C-x b` | `ivy-switch-buffer` |
| `C-s` | `swiper` |
| `C-x g` | `magit-status` |
| `C-c p` | projectile prefix |
| `C-c p s g` | `my/counsel-rg-at-project-root` (ripgrep from project root) |
| `M-0` | `treemacs-toggle` |
| `C-S-d` | `duplicate-current-line` |
| `C-x C-a` | `rename-file-and-buffer` |
| `C-x u` | `vundo` (visual undo tree) |
| `C->` / `C-<` | multiple-cursors next/previous |
| `C-:` | `avy-goto-char` |
| `C-<return>` | `copilot-accept-completion` |
| `M-p` / `M-n` | drag line up/down |
| `C-M-SPC` | `er/expand-region` |

## External dependencies

These must be installed on the system before the config is fully functional:

```bash
brew install ripgrep          # for counsel-rg / C-c p s g
brew install --cask emacs     # Emacs 30+
npm i -g typescript-language-server typescript  # TS LSP server
```

NVM init must be in `~/.zprofile` (not `~/.zshrc`) so GUI Emacs inherits `PATH` and `NVM_DIR`.

Python virtualenvs are expected at `~/.cache/pypoetry/virtualenvs` (Poetry default).
