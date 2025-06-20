# Confluemacs

> A modern, secure Emacs package for browsing and editing Confluence Cloud content with a Dired-like interface.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs: 29.1+](https://img.shields.io/badge/Emacs-29.1+-purple.svg)](https://www.gnu.org/software/emacs/)
[![Version](https://img.shields.io/badge/version-0.6.0-green.svg)](https://github.com/yourusername/confluemacs/releases)
[![API Support](https://img.shields.io/badge/Confluence%20API-v1%20%7C%20v2-orange.svg)](https://developer.atlassian.com/cloud/confluence/rest/)

**Confluemacs** transforms your Emacs into a powerful Confluence client, offering seamless integration between your favorite editor and Atlassian's collaboration platform. Edit pages using Org-mode syntax, manage content with familiar Dired-style navigation, and never lose work with intelligent auto-save features.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Setup](#setup)
- [Usage](#usage)
- [Configuration Options](#configuration-options)
- [API Migration (v1 → v2)](#api-migration-v1--v2)
- [Troubleshooting](#troubleshooting)
- [Security](#security)
- [Contributing](#contributing)
- [Changelog](#changelog)
- [License](#license)

## Features

### 🔐 **Secure & Modern**
- **Security-hardened**: Fixed all shell injection vulnerabilities
- **API Future-proof**: Full support for Confluence REST API v1 and v2
- **Modern Dependencies**: Uses `plz.el` for reliable HTTP communication
- **Emacs 29.1+ Compatible**: Optimized for latest Emacs features

### 📝 **Smart Content Management**
- **Dired-like Interface**: Browse spaces and content with familiar navigation
- **Org-mode Integration**: Edit content using powerful Org-mode syntax
- **Smart Buffer Management**: Unique naming prevents conflicts, includes space context
- **Auto-Save Drafts**: Never lose work with configurable auto-save (5-min default)
- **Change Tracking**: Visual indicators for modified content

### 🚀 **Enhanced User Experience**
- **Asynchronous Operations**: Non-blocking UI with progress indicators
- **Error Recovery**: Smart error handling with retry and recovery options
- **Configuration Wizard**: Guided setup for easy initial configuration
- **Crash Recovery**: Restore drafts after unexpected closures
- **Comprehensive Validation**: Configuration checking with helpful diagnostics

### 🔧 **Professional Features**
- **Buffer Tracking**: Manage multiple content buffers efficiently
- **Draft Management**: Automatic cleanup of old drafts
- **API Version Testing**: Check v1/v2 compatibility before migration
- **Enhanced Transient Menu**: Organized command palette for all operations
- **Customization Groups**: Well-organized settings for different use cases

## Installation

### Requirements
- Emacs 29.1 or later
- Dependencies (automatically installed):
  - `plz` (0.9+) - Modern HTTP client
  - `org` (9.5+) - Org-mode for content editing
  - `transient` (0.7.8+) - Menu system

### Package Installation

#### Using use-package (Recommended for Emacs 30.1+)

```elisp
;; Working configuration - defers loading until actually needed
(use-package confluemacs
  :vc (:url "https://github.com/yourusername/confluemacs.git"
       :rev :newest)
  :ensure t
  :defer t
  :commands (confluemacs)
  :bind ("C-c f c" . confluemacs)
  :custom
  (confluemacs-base-url "https://yourcompany.atlassian.net/wiki")
  (confluemacs-auth-source-host "yourcompany.atlassian.net")
  (confluemacs-api-version "v1")
  (confluemacs-timeout 15))
```

**Alternative full configuration (use after syntax issues are fixed):**
```elisp
(use-package confluemacs
  :vc (:url "https://github.com/yourusername/confluemacs.git"
       :rev :newest)
  :ensure t
  :commands (confluemacs
             confluemacs-check-api-version
             confluemacs-validate-configuration
             confluemacs-recover-from-crash)
  :bind (("C-c f c" . confluemacs)
         ("C-c f v" . confluemacs-check-api-version)
         ("C-c f r" . confluemacs-recover-from-crash))
  :hook ((confluemacs-mode . hl-line-mode)
         (kill-emacs . confluemacs-check-unsaved-changes))
  :custom
  (confluemacs-base-url "https://yourcompany.atlassian.net/wiki")
  (confluemacs-auth-source-host "yourcompany.atlassian.net")
  (confluemacs-api-version "v1")
  (confluemacs-timeout 15)
  (confluemacs-auto-save-interval 300)
  (confluemacs-auto-save-directory 
   (expand-file-name "confluemacs-drafts" user-emacs-directory))
  (confluemacs-expand-default "space,body.storage,version,container,ancestors")
  :config
  (confluemacs-validate-configuration)
  (add-hook 'confluemacs-mode-hook
            (lambda ()
              (setq mode-line-format
                    (append mode-line-format
                            '(" " (:eval (when (bound-and-true-p confluemacs-content-modified)
                                          " [Modified]"))))))))
```

#### Manual Installation from GitHub
```bash
# Clone the repository
git clone https://github.com/yourusername/confluemacs.git ~/.emacs.d/site-lisp/confluemacs

# Add to your Emacs configuration
(add-to-list 'load-path "~/.emacs.d/site-lisp/confluemacs")
(require 'confluemacs)
```

#### Using straight.el
```elisp
(use-package confluemacs
  :straight (:host github :repo "yourusername/confluemacs")
  :commands (confluemacs)
  :bind ("C-c f c" . confluemacs)
  :custom
  (confluemacs-base-url "https://yourcompany.atlassian.net/wiki")
  (confluemacs-auth-source-host "yourcompany.atlassian.net"))
```

## Setup

### 1. Quick Setup (Recommended)
```elisp
M-x confluemacs--setup-configuration
```
Follow the interactive wizard to configure your Confluence instance.

### 2. Manual Configuration
```elisp
(setq confluemacs-base-url "https://yourcompany.atlassian.net/wiki"
      confluemacs-auth-source-host "yourcompany.atlassian.net"
      confluemacs-api-version "v1")  ; Change to "v2" when ready
```

### 3. Authentication Setup
Create or update your `~/.authinfo.gpg` file:
```
machine yourcompany.atlassian.net login your-email@company.com password your-api-token
```

**Important**: Use an API token, NOT your password. Generate one at:
`Atlassian Account Settings > Security > API tokens`

### 4. Validate Configuration
```elisp
M-x confluemacs-validate-configuration
```

## Usage

### Basic Operations

#### Start Confluemacs
```elisp
M-x confluemacs
```

#### Navigation
- `RET` - Open space or content
- `g` - Refresh current view
- `^` - Go up one level
- `m` - Open transient menu

#### Content Management
- `c` - Create new content
- `d` - Delete content
- `C-c C-c` - Save content (when editing)

#### Advanced Features
- `M-x confluemacs-list-content-buffers` - View all open content
- `M-x confluemacs-save-all-modified` - Save all modified buffers
- `M-x confluemacs-check-api-version` - Test API compatibility
- `M-x confluemacs-recover-from-crash` - Restore drafts

### Buffer Management

Confluemacs creates uniquely named buffers:
- `*Confluemacs: [SPACE] Page Title*` - Normal content
- `*Confluemacs: [SPACE] Page Title<2>*` - Duplicate titles

Buffers automatically track:
- Content modifications (shown in mode line)
- Space context for easy identification
- Auto-save drafts every 5 minutes

### API Migration (v1 → v2)

⚠️ **Important**: Confluence REST API v1 will be deprecated on **April 30, 2025**.

#### Check Compatibility
```elisp
M-x confluemacs-check-api-version
```

#### Migrate to v2
```elisp
M-x confluemacs-migrate-to-v2
```

## Configuration Options

### Connection Settings (`confluemacs-connection`)
- `confluemacs-base-url` - Your Confluence URL
- `confluemacs-auth-source-host` - Auth-source host identifier
- `confluemacs-timeout` - API request timeout (default: 10 seconds)
- `confluemacs-api-version` - API version ("v1" or "v2")

### Editing Settings (`confluemacs-editing`)
- `confluemacs-auto-save-interval` - Auto-save interval in seconds (default: 300)
- `confluemacs-auto-save-directory` - Draft storage location

### Advanced Settings (`confluemacs-advanced`)
- `confluemacs-expand-default` - Default API response fields
- `confluemacs-v2-warning-shown` - V2 migration warning state

## Troubleshooting

### Common Issues

#### Authentication Problems
```elisp
M-x confluemacs-validate-configuration
```
Check that your `.authinfo.gpg` uses an API token, not password.

#### API Deprecation Warnings
```elisp
M-x confluemacs-check-api-version
M-x confluemacs-migrate-to-v2  ; When ready
```

#### Lost Content
```elisp
M-x confluemacs-recover-from-crash
```
Restores auto-saved drafts from crashes.

#### Configuration Issues
```elisp
M-x confluemacs--setup-configuration
```
Interactive setup wizard fixes most configuration problems.

### Recovery Features

- **Auto-save**: Drafts saved every 5 minutes in `~/.emacs.d/confluemacs-drafts/`
- **Crash recovery**: `M-x confluemacs-recover-from-crash`
- **Draft cleanup**: `M-x confluemacs-clean-old-drafts` (removes >30 day old files)

## Security

### Security Measures
- ✅ **No shell injection vulnerabilities** - Secure pandoc integration
- ✅ **HTML sanitization** - Dangerous content automatically cleaned
- ✅ **Encrypted credentials** - Uses `.authinfo.gpg` for secure storage
- ✅ **Input validation** - All external data validated before processing

### API Token Security
- Use API tokens instead of passwords
- Store in encrypted `.authinfo.gpg`
- Regular token rotation recommended
- Limit token scopes when possible

## Contributing

### Development Setup
1. Clone the repository
2. Install dependencies: `plz`, `org`, `transient`
3. Run tests: `emacs -batch -f batch-byte-compile confluemacs.el`

### Reporting Issues
Please include:
- Emacs version
- Package version
- Configuration (without sensitive data)
- Steps to reproduce

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for detailed version history.

### Recent Updates

#### v0.6.0 - Enhanced UX & Package Structure
- Smart buffer management with conflict prevention
- Auto-save functionality with crash recovery
- Comprehensive error recovery system
- Configuration validation and setup wizard
- Enhanced transient menu organization

#### v0.5.0 - Critical Security & Compatibility
- **SECURITY**: Fixed shell injection vulnerabilities
- API v2 support for upcoming deprecation
- Modern HTTP client (`plz.el`)
- Asynchronous operations

## License

GPL-3.0 License. See [LICENSE](LICENSE) for details.

## Links

- [Confluence Cloud REST API Documentation](https://developer.atlassian.com/cloud/confluence/rest/)
- [API v2 Migration Guide](https://developer.atlassian.com/cloud/confluence/rest/v2/)
- [Emacs Org-mode Manual](https://orgmode.org/manual/)