# Emacs Configuration

A modern, minimalist Emacs configuration focused on Python development with LSP support, Git integration, and a clean UI.

## Features

- **Minimal UI**: No toolbar, menu bar, or scroll bar for distraction-free editing
- **Modern Completion**: Vertico + Orderless + Marginalia for fast and fuzzy completion
- **LSP Support**: Eglot with basedpyright/ruff for Python development
- **Git Integration**: Magit and git-gutter for version control
- **Python Environment Management**: Conda integration with automatic environment activation
- **Custom Packages**: Jump navigation, VSCode-like features, and custom dashboard
- **Performance**: Optimized with eglot-booster for faster LSP responses

## Prerequisites

- Emacs 29+ (for tree-sitter support)
- Python development tools:
  - `basedpyright-langserver` or `pyright-langserver`
  - `ruff`
  - Conda/Miniconda (installed at `~/miniconda3/`)
- Git

## Installation

1. Clone this repository to `~/.emacs.d`:
   ```bash
   git clone <repository-url> ~/.emacs.d
   ```

2. Install required Python tools:
   ```bash
   pip install basedpyright ruff
   ```

3. Start Emacs - packages will be automatically installed on first launch

## Structure

```
.emacs.d/
├── init.el              # Main configuration file
├── lisp/                # Custom elisp packages
│   ├── dashboard.el     # Custom dashboard
│   ├── eglot-booster.el # LSP performance enhancement
│   ├── jumpy.el         # Jump navigation system
│   └── vscode.el        # VSCode-like features
├── themes/              # Custom themes
│   └── pylight-theme.el # Light theme
└── var/                 # Package installations and data
```

## Key Packages

### Completion & Navigation
- **vertico**: Vertical completion interface
- **orderless**: Flexible matching for completion
- **marginalia**: Rich annotations in minibuffer
- **breadcrumb**: Show location context in mode line

### Development
- **eglot**: Built-in LSP client
- **company**: Auto-completion framework
- **reformatter**: Code formatting with ruff
- **git-gutter**: Show git diff in gutter

### Python
- **python-ts-mode**: Python support with tree-sitter
- **conda**: Conda environment management
- **ruff**: Fast Python linter and formatter

### Utilities
- **magit**: Git porcelain for Emacs
- **multiple-cursors**: Edit with multiple cursors
- **eat**: Terminal emulator with better eshell integration

## Key Bindings

### General
- `<escape>` - Quit current operation
- `C->` - Mark next like this (multiple cursors)
- `C-<` - Mark previous like this
- `C-c C-<` - Mark all like this

### Navigation
- `C--` - Jump back
- `C-_` - Jump forward

### LSP (in eglot-mode)
- `<f2>` - Rename symbol

### Git
- Standard magit bindings available via `M-x magit-status`

## Python Development

This configuration is optimized for Python development with:

1. **Automatic environment activation**: Conda environments are detected and activated automatically
2. **On-save formatting**: Ruff automatically formats code on save
3. **Import organization**: Imports are sorted automatically
4. **Type checking**: Basedpyright provides type checking and LSP features
5. **Inlay hints**: Function signatures and type hints displayed inline

## Customization

User-specific customizations are stored in `.emacs.custom.el` (not tracked in git).

To customize:
- Use `M-x customize-group` to configure packages
- Edit `init.el` directly for configuration changes
- Add custom packages to `lisp/` directory

## Notes

- First startup may take a few minutes while packages are downloaded
- Tree-sitter grammars are automatically managed
- The configuration uses deferred loading (`use-package-always-defer t`) for faster startup
- Backup and auto-save files are stored in `tmp/` directory

## License

Personal configuration - feel free to use and modify as needed.
