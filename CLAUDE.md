# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is "Cunene" - a personal Emacs configuration using the literate configuration approach with org-mode. The configuration is built from org files that are tangled into Emacs Lisp files at startup.

## Architecture

### Configuration Structure
- `init.el`: Main initialization file that orchestrates the configuration loading
- `config/`: Directory containing org-mode configuration files that are tangled to `.el` files:
  - `core.org`: Basic Emacs configuration
  - `quality_of_life.org`: User experience improvements  
  - `features.org`: Feature configuration
  - `development.org`: Development tools setup
  - `music.org`: Music-related configuration
  - `external.org`: External tool integrations
- `vendor/`: Third-party packages not available in repositories
- `site-lisp/`: Site-specific code (not pushed to public repo)
- `custom.el`: Emacs custom settings

### Loading Process
1. `init.el` sets up basic environment and paths
2. Org files in `config/` are tangled to `.el` files if needed
3. Generated `.el` files are compiled to `.elc` and loaded
4. Vendor packages are compiled but not loaded (loaded by org config)
5. Site-specific code is compiled and loaded

## Common Development Tasks

### Testing Configuration Changes
```bash
# Test configuration without affecting current setup
emacs --no-init-file -l ./init.el
```

### Recompiling Configuration
The configuration automatically handles tangling and compilation, but if you need to force recompilation:
- Delete `.elc` files to force recompilation
- Modify org files to trigger re-tangling

### Package Management
Packages are managed through the standard Emacs package system. The configuration includes a comprehensive list of packages in `custom.el`.

## File Editing Guidelines

- Edit `.org` files in `config/` directory, not the generated `.el` files
- The org files use literate programming - code is in `#+begin_src emacs-lisp` blocks
- Generated `.el` files are automatically created and should not be edited directly
- Custom settings go in `custom.el` (managed by Emacs customize system)