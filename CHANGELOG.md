# Changelog

All notable changes to Confluemacs will be documented in this file.

## [0.5] - 2025-01-18

### Added
- Preparation for Confluence REST API v2 migration (v1 deprecation on April 30, 2025)
- New customization variable `confluemacs-api-version` to switch between v1 and v2
- User-Agent header for better API compliance
- Enhanced error handling with specific messages for different HTTP status codes
- Rate limiting awareness (429 status)
- Improved authentication documentation emphasizing API tokens

### Changed
- Updated package documentation to reflect current Confluence Cloud API
- Enhanced error messages for authentication failures
- Improved permission checking with better error handling
- Fixed typo in X-Atlassian-Token header ("no-check" instead of "nocheck")

### Security
- Added explicit warnings about using API tokens instead of passwords
- Better error messages guide users to proper authentication setup

## [0.4] - Previous Version

- Initial release with basic Confluence browsing and editing capabilities
- Dired-like interface for navigating spaces and content
- Org-mode integration for content editing
- Permission-based read-only buffers