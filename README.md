# My Emacs Config

An advanced and feature-rich Emacs configuration designed to enhance your development workflow that I've been building over the years. This setup leverages modern Emacs packages for better performance, improved navigation, enhanced coding experience, and seamless integrations across various programming languages.

![image](https://github.com/user-attachments/assets/78528fce-d285-4353-b3a6-9980729c7761)

## Features

- **Modern Interface and Navigation**:
  - **`ivy` and `counsel`**: Provides a powerful interface for file and buffer switching, command execution, and more, with fuzzy searching enabled.
  - **`avy`**: Quickly jump to any visible text in Emacs, replacing `ace-jump-mode`.
  - **`doom-modeline`**: A sleek and modern modeline that provides rich contextual information such as buffer status, Git branch, LSP status, and more.
  - **`which-key`**: Discover available keybindings on-the-fly.

- **Enhanced Coding Experience**:
  - **`company-mode`**: A robust, context-aware auto-completion framework, with `company-box` for an enhanced UI.
  - **`yasnippet`**: Snippet support for faster coding.
  - **`flycheck`**: On-the-fly syntax checking for various languages.
  - **`git-gutter`**: Visualize git changes directly in the editor.
  - **`multiple-cursors`**: Edit text in multiple places simultaneously.

- **Language Support**:
  - **Python**:
    - `lsp-mode` and `lsp-pyright` for advanced code intelligence (commented out for future activation).
    - Virtual environment management with `pipenv`.
  - **JavaScript and TypeScript**:
    - `web-mode` and `tide` for comprehensive support, including JSX, TSX, and more.
  - **Go**:
    - Integrated Go tools with `go-mode`.
  - **Markdown, YAML, Dockerfile, Terraform**:
    - Syntax highlighting and editing enhancements for common file formats.
  - **Org Mode**:
    - AI-enhanced editing with `org-ai`.

- **Productivity and Visual Enhancements**:
  - **`material-theme`**: A modern and visually appealing theme.
  - **`highlight-indent-guides`**: Visualize code indentation levels.
  - **`zoom`**: Intelligent window resizing for an optimal editing experience.
  - **`swiper`**: Enhanced in-buffer searching with regex matching.
  - **`drag-stuff`**: Easily move lines or regions up and down.

- **Other Utilities**:
  - **`editorconfig`**: Consistent coding styles across various editors.
  - **`smartparens`**: Smart management of parentheses and other paired characters.
  - **`exec-path-from-shell`**: Ensures Emacs inherits the correct environment variables.

## Installation on macOS

Install Emacs using Homebrew with the latest options for a native macOS experience:

```bash
brew install --cask emacs
```

## How to Install This Config

Clone this repository into your `~/.emacs.d` directory:

```bash
git clone git@github.com:alissonperez/emacs-conf.git ~/.emacs.d
```

## List of Libraries

Here is a complete list of the libraries included in this configuration:

- **Core Enhancements**:
  - `ivy`
  - `counsel`
  - `swiper`
  - `avy`
  - `doom-modeline`
  - `which-key`
  - `flx`

- **Coding and Editing**:
  - `company`
  - `company-box`
  - `yasnippet`
  - `flycheck`
  - `git-gutter`
  - `multiple-cursors`
  - `smartparens`
  - `drag-stuff`
  - `highlight-indent-guides`
  - `zoom`

- **Language Support**:
  - **Python**:
    - `lsp-mode` (commented out for future activation)
    - `lsp-pyright` (commented out for future activation)
    - `pipenv`
  - **JavaScript/TypeScript**:
    - `web-mode`
    - `tide`
    - `js2-mode`
    - `nodejs-repl`
  - **Go**:
    - `go-mode`
  - **Markdown/YAML/Dockerfile/Terraform**:
    - `markdown-mode`
    - `yaml-mode`
    - `dockerfile-mode`
    - `terraform-mode`
  - **Org Mode**:
    - `org-ai`

- **Version Control**:
  - `magit`

- **Other Utilities**:
  - `exec-path-from-shell`
  - `editorconfig`
  - `textmate`
  - `protobuf-mode`

- **Appearance**:
  - `material-theme`
  - `doom-modeline`

## Usage

This configuration is designed to be used as-is, but it's also highly customizable. Feel free to fork and adapt it to suit your personal workflow. For detailed usage instructions and keybindings, refer to the individual package documentation.
