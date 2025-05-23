# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository for configuring development environments across macOS and Linux systems. The core purpose is to maintain consistent shell, editor, and development tool configurations.

## Key Commands

### Setup and Installation
- `./link-dotfiles.sh` - Creates symlinks from dotfiles to home directory for: .vimrc, .zshrc, .gitconfig, .emacs.d
- `./razer_as_gpu_workhorse/setup_comp.sh` - Linux setup script for GPU workstation with development packages

### Git Configuration  
The `.gitconfig` includes common aliases:
- `git a` (add), `git c` (commit), `git s` (status), `git d` (diff)
- `git ps` (push), `git pl` (pull), `git co` (checkout), `git b` (branch)

## Architecture

### Configuration Structure
- **Shell**: `.zshrc` with custom prompt showing current directory and git branch, extensive aliases
- **Editor**: Dual configuration for Vim (`.vimrc`) and Emacs (`.emacs.d/`)
- **Git**: Standardized git configuration with helpful aliases

### Emacs Configuration
The Emacs setup is modularized across multiple files:
- `init.el` - Main entry point loading other modules
- `general.el` - Core Evil mode configuration, window management, project navigation
- Language-specific files: `ocaml-stuff.el`, `python-stuff.el`, `swift-stuff.el`, etc.

Key features:
- Evil mode (Vim keybindings) with custom leader key mappings
- Helm for fuzzy finding with Projectile integration  
- LSP support for multiple languages via various language servers
- Terminal integration with custom term mode handling

### Development Tools
- **Languages**: Configured for Python, OCaml, Haskell, JavaScript/TypeScript, Swift
- **Package Managers**: Uses OPAM for OCaml, Conda for Python, NVM for Node.js
- **Search Tools**: Fasd for directory jumping, FZF for fuzzy finding, ripgrep for text search

## Environment-Specific Notes

### macOS Setup
- Uses Homebrew packages (PostgreSQL, etc.)
- Includes Conda and OPAM configurations

### Linux Setup  
- Automated Ubuntu/Debian setup with essential development packages
- GPU computing support with CUDA paths
- Terminator terminal configuration