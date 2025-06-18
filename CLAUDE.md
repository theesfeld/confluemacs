# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Confluemacs is an Emacs Lisp package that provides a Dired-like interface for browsing and editing Confluence Cloud content. It integrates with Org-mode for content editing and uses the Confluence REST API v5.7.1.

## Development Commands

### Installation and Setup

1. **Install Package Dependencies:**
   ```elisp
   ;; Ensure these packages are installed:
   ;; - request (0.3.3+)
   ;; - org (9.4+)
   ;; - transient (0.7.8+)
   ```

2. **Load the Package:**
   ```elisp
   (require 'confluemacs)
   ```

3. **Configure Authentication:**
   - Set up `.authinfo.gpg` with Confluence credentials:
     ```
     machine your-site.atlassian.net login your-email@example.com password your-api-token
     ```
   - Configure in Emacs:
     ```elisp
     (setq confluemacs-base-url "https://your-site.atlassian.net/wiki")
     (setq confluemacs-auth-source-host "your-site.atlassian.net")
     ```

### Testing and Validation

Since this is an Emacs package, testing is done within Emacs:

1. **Byte Compilation Check:**
   ```bash
   emacs -batch -f batch-byte-compile confluemacs.el
   ```

2. **Load and Test Interactively:**
   ```elisp
   M-x confluemacs  ; Opens the main interface
   ```

3. **Check for Warnings:**
   ```elisp
   M-x byte-compile-file RET confluemacs.el RET
   ```

## Architecture

### Core Components

1. **API Integration (`confluemacs--make-request`):**
   - Handles all HTTP requests to Confluence REST API
   - Manages authentication via auth-source
   - Synchronous requests using the `request` package

2. **Dired-like Interface:**
   - `confluemacs-mode`: Special mode for browsing spaces/content
   - Navigation functions: `confluemacs-open`, `confluemacs-up`, `confluemacs-refresh`
   - Content display: `confluemacs--display-spaces`, `confluemacs--display-content`

3. **Content Conversion:**
   - `confluemacs--org-to-confluence`: Converts Org-mode to Confluence HTML storage format
   - `confluemacs--confluence-to-org`: Converts Confluence HTML to Org-mode
   - Supports both Pandoc (preferred) and built-in Org export

4. **Permission System:**
   - `confluemacs--check-edit-permission`: Verifies edit access
   - Buffers become read-only when user lacks permissions

### Key Design Patterns

- **Buffer Management:** Each content page opens in its own buffer named `*Confluemacs: [title]*`
- **State Tracking:** Buffer-local variables store content metadata (ID, version, space key)
- **Transient Menu:** Uses transient.el for command palette (`confluemacs-menu`)

## API Reference

The package interacts with Confluence Cloud REST API v5.7.1:
- Documentation: https://docs.atlassian.com/atlassian-confluence/REST/5.7.1/

Key endpoints used:
- `/rest/api/space` - List spaces
- `/rest/api/content` - List/create/update content
- `/rest/api/content/{id}` - Get/update/delete specific content
- `/rest/api/content/{id}/restriction/byOperation/update` - Check edit permissions