# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Confluemacs is an Emacs Lisp package that provides a Dired-like interface for browsing and editing Confluence Cloud content. It integrates with Org-mode for content editing and uses the Confluence Cloud REST API with support for both v1 and v2 endpoints.

**Version:** 0.6.0 (Enhanced UX and package structure)
**Status:** Production-ready with comprehensive feature set

## Development Commands

### Installation and Setup

1. **Install Package Dependencies:**
   ```elisp
   ;; Ensure these packages are installed:
   ;; - plz (0.9+) - Modern HTTP client (replaces deprecated request.el)
   ;; - org (9.5+)
   ;; - transient (0.7.8+)
   ;; - Emacs 29.1+ for best compatibility
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
     (setq confluemacs-api-version "v1")  ; or "v2" for new API
     ```

### Critical Security & API Updates (v0.5.0)

⚠️ **IMPORTANT:** Version 0.5.0 includes critical security fixes and API deprecation preparation:

1. **Security Fixes Applied:**
   - Fixed shell command injection vulnerability in pandoc integration
   - Added HTML sanitization for Confluence content
   - Replaced unsafe `shell-command-on-region` with secure `call-process`

2. **API Deprecation Preparation:**
   - Confluence REST API v1 will be deprecated **April 30, 2025**
   - Added v2 API support with automatic endpoint mapping
   - Use `M-x confluemacs-check-api-version` to test both versions
   - Use `M-x confluemacs-migrate-to-v2` to switch to v2

3. **Compatibility Updates:**
   - Replaced deprecated `request.el` with modern `plz.el`
   - Added asynchronous operations with progress indicators
   - Enhanced error handling for different HTTP status codes

### Testing and Validation

1. **Byte Compilation Check:**
   ```bash
   emacs -batch -f batch-byte-compile confluemacs.el
   ```

2. **API Connectivity Test:**
   ```elisp
   M-x confluemacs-check-api-version  ; Tests both v1 and v2 APIs
   ```

3. **Load and Test Interactively:**
   ```elisp
   M-x confluemacs  ; Opens the main interface (now async!)
   ```

4. **Check Security Updates:**
   - Content conversion now uses secure pandoc integration
   - HTML content is automatically sanitized
   - Progress indicators show during API operations

### Phase 2 Updates (v0.6.0)

5. **Buffer Management:**
   ```elisp
   M-x confluemacs-list-content-buffers  ; View all active content buffers
   M-x confluemacs-save-all-modified     ; Save all modified buffers
   ```

6. **Configuration Management:**
   ```elisp
   M-x confluemacs-validate-configuration  ; Check configuration
   M-x confluemacs--setup-configuration    ; Interactive setup wizard
   ```

7. **Error Recovery:**
   ```elisp
   M-x confluemacs-recover-from-crash     ; Restore drafts after crash
   M-x confluemacs-clean-old-drafts       ; Clean up old auto-saves
   ```

8. **Auto-Save Features:**
   - Drafts automatically saved every 5 minutes
   - Draft files stored in `~/.emacs.d/confluemacs-drafts/`
   - Automatic draft loading when reopening content
   - Smart buffer naming prevents conflicts

## Architecture

### Core Components

1. **API Integration (`confluemacs--make-request`):**
   - Handles all HTTP requests to Confluence REST API with both v1 and v2 support
   - Manages authentication via auth-source with API tokens
   - Asynchronous requests using modern `plz.el` package
   - Enhanced error handling with smart recovery options

2. **Dired-like Interface:**
   - `confluemacs-mode`: Special mode for browsing spaces/content
   - Navigation functions: `confluemacs-open`, `confluemacs-up`, `confluemacs-refresh`
   - Async content display: `confluemacs--display-spaces-async`, `confluemacs--display-content-async`
   - Progress indicators for all operations

3. **Buffer Management System:**
   - Smart buffer naming with space keys and conflict prevention
   - Buffer tracking and automatic cleanup (`confluemacs--content-buffers`)
   - Content state management with modification detection
   - Buffer lifecycle management with proper registration/unregistration

4. **Auto-Save and Draft System:**
   - Configurable auto-save intervals with timer management
   - Draft storage in dedicated directory with safe filenames
   - Automatic draft loading and cleanup after saves
   - Crash recovery functionality

5. **Content Conversion (Secure):**
   - `confluemacs--convert-with-pandoc`: Secure pandoc integration via `call-process`
   - `confluemacs--sanitize-html`: HTML sanitization for security
   - Enhanced fallback conversion with proper HTML entity handling
   - Safe temporary file handling

6. **Permission and Error Handling:**
   - `confluemacs--check-edit-permission`: Verifies edit access with error recovery
   - Comprehensive error recovery with user-friendly prompts
   - Smart retry mechanisms for network and API issues
   - Configuration validation and guided setup

### Key Design Patterns

- **Smart Buffer Naming:** Format `*Confluemacs: [SpaceKey] Title*` or `*Confluemacs: [SpaceKey] Title<N>*` for conflicts
- **State Management:** Buffer-local variables track content metadata (ID, version, space key, modification state)
- **Timer Management:** Periodic cleanup and auto-save timers with proper lifecycle management
- **Hash-based Tracking:** Content buffers tracked by ID with automatic cleanup of dead buffers
- **Error Recovery:** Layered error handling with user choice prompts and smart retry logic
- **Async Operations:** All API calls non-blocking with progress indicators
- **Configuration Groups:** Organized customization with validation and guided setup
- **Draft System:** Automatic persistence with crash recovery and cleanup mechanisms

## API Reference

The package interacts with Confluence Cloud REST API v5.7.1:
- Documentation: https://docs.atlassian.com/atlassian-confluence/REST/5.7.1/

Key endpoints used:
- `/rest/api/space` - List spaces
- `/rest/api/content` - List/create/update content
- `/rest/api/content/{id}` - Get/update/delete specific content
- `/rest/api/content/{id}/restriction/byOperation/update` - Check edit permissions