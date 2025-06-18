# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.0] - 2025-01-XX - Phase 2: Package Structure & UX Improvements

### Added
- **Smart Buffer Management**
  - Unique buffer naming scheme preventing conflicts (includes space key and counter)
  - Buffer tracking system with automatic cleanup
  - `confluemacs-list-content-buffers` to view all active content buffers
  - Automatic buffer registration and cleanup on kill

- **Content State Management**
  - Real-time detection of local vs remote changes
  - Buffer modification tracking with MD5 hashing
  - Visual indicators for modified content
  - `confluemacs-save-all-modified` to save all changed buffers
  - `confluemacs-check-unsaved-changes` to warn about unsaved work

- **Auto-Save Functionality**
  - Configurable auto-save interval (default: 5 minutes)
  - Draft files stored in dedicated directory
  - Automatic draft loading when reopening content
  - `confluemacs-clean-old-drafts` to remove files older than 30 days
  - Draft deletion after successful saves

- **Enhanced Configuration System**
  - Comprehensive customization groups:
    - `confluemacs-connection` - API and authentication settings
    - `confluemacs-interface` - UI preferences
    - `confluemacs-editing` - Content editing and auto-save
    - `confluemacs-advanced` - Debug and advanced options
  - `confluemacs-validate-configuration` for setup validation
  - `confluemacs--setup-configuration` for guided setup
  - Improved configuration validation with helpful error messages

- **Error Recovery Mechanisms**
  - Smart error recovery with user-friendly options
  - Network error retry functionality
  - Authentication error diagnosis
  - API deprecation warnings with migration prompts
  - `confluemacs-recover-from-crash` to restore drafts after crashes
  - Interactive error handling with retry/ignore/debug options

- **Enhanced Transient Menu**
  - New "Content Management" section with buffer operations
  - "Maintenance" section for configuration and recovery tools
  - Better organization of existing commands
  - Added shortcuts for common operations

### Changed
- **Buffer Creation Process**
  - Content buffers now include space key in names for clarity
  - Automatic registration with tracking system
  - Draft loading integrated into content retrieval
  - Improved buffer lifecycle management

- **Save Process**
  - Content state updates after successful saves
  - Draft cleanup after publishing
  - Better feedback for save operations
  - Version tracking improvements

### Technical Improvements
- Consistent function namespacing with proper private/public distinction
- Added comprehensive autoload cookies for main commands
- Improved timer management for cleanup and auto-save
- Better memory management for buffer tracking
- Enhanced error handling throughout the codebase

---

## [0.5.0] - 2025-01-XX - Phase 1: Critical Security & Compatibility Fixes

### Security Fixes ⚠️
- **CRITICAL**: Fixed shell command injection vulnerability in pandoc integration
- Replaced unsafe `shell-command-on-region` with secure `call-process`
- Added comprehensive HTML sanitization for Confluence content
- Implemented input validation for external tool integration

### API Compatibility
- **IMPORTANT**: Added Confluence REST API v2 support (v1 deprecated April 30, 2025)
- `confluemacs-check-api-version` to test both v1 and v2 connectivity
- `confluemacs-migrate-to-v2` for easy API version migration
- Automatic deprecation warnings and migration guidance
- Enhanced error handling for API status codes (401, 403, 429, 410)

### HTTP Client Modernization
- Replaced deprecated `request.el` with modern `plz.el`
- Asynchronous operations with progress indicators
- Non-blocking UI during API operations
- Better error recovery and timeout handling
- Enhanced connection reliability

### Package Updates
- Updated minimum Emacs version to 29.1
- Updated dependencies: plz 0.9+, org 9.5+
- Added proper autoload cookies for main commands
- Enhanced package commentary with current API information

### Content Processing
- Secure pandoc integration with temporary files
- Enhanced HTML sanitization removing dangerous elements
- Better fallback content conversion
- Improved Org-mode integration

---

## [Earlier Versions]

Previous versions (0.1-0.4) were development releases with basic functionality.
This changelog begins with version 0.5.0, which represents the first production-ready release.