# Confluemacs

A Dired-like interface for browsing and editing Confluence Cloud content directly from Emacs using Org-mode.

## Features

- Browse Confluence spaces and content in a Dired-like interface
- View and edit pages in Org-mode format
- Automatic conversion between Org-mode and Confluence storage format (HTML)
- Read-only buffers when lacking edit permissions
- Authentication via `.authinfo.gpg` with API tokens

## Installation

### Prerequisites

- Emacs 27.1 or later
- `request` package for HTTP requests
- `org-mode` 9.4 or later
- `transient` 0.7.8 or later
- Optional: `pandoc` for better HTML/Org conversion

### Setup

1. Install the package and its dependencies
2. Configure your Confluence instance:

```elisp
(setq confluemacs-base-url "https://your-site.atlassian.net/wiki")
(setq confluemacs-auth-source-host "your-site.atlassian.net")
```

3. Add your credentials to `~/.authinfo.gpg`:

```
machine your-site.atlassian.net login your-email@example.com password your-api-token
```

**Important**: Use an API token, not your password. Create one at https://id.atlassian.com/manage/api-tokens

## Usage

Start Confluemacs with `M-x confluemacs`.

### Key Bindings

In the Confluemacs buffer:
- `RET` - Open space or content
- `g` - Refresh current view
- `c` - Create new content
- `d` - Delete item at point
- `^` - Go up one level
- `m` - Open transient menu

In Org-mode content buffers:
- `C-c C-c` - Save content to Confluence (when you have edit permission)

## API Version Support

This package currently uses Confluence REST API v1, which will be deprecated on April 30, 2025. The package includes preparations for migration to v2:

- Set `confluemacs-api-version` to "v2" to test v2 endpoints (experimental)
- The package will display warnings when deprecated endpoints are detected

## Troubleshooting

### Authentication Issues

- Ensure you're using an API token, not a password
- Check that your `.authinfo.gpg` entry matches the `confluemacs-auth-source-host` setting
- API tokens should be created at https://id.atlassian.com/manage/api-tokens

### Permission Issues

- The package checks edit permissions before allowing modifications
- If permission checking fails, buffers will be read-only by default
- Space permission endpoints have limitations for apps

### Rate Limiting

- The API has rate limits; you'll see a message if exceeded
- Wait a few minutes before retrying

## Contributing

Contributions are welcome! Please ensure your changes maintain compatibility with both v1 and v2 APIs where possible.

## License

[Your license here]