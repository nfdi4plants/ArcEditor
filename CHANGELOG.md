# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

**Types of changes**

- `Added` for new features.
- `Changed` for changes in existing functionality.
- `Deprecated` for soon-to-be removed features.
- `Removed` for now removed features.
- `Fixed` for any bug fixes.
- `Security` in case of vulnerabilities.

## [Unreleased]

### Changed
- Rewrote the provenance grouping annotation model on canonical node/process identity: equal kind/name endpoints share one identity across sides, layers, and sources; node and process assignments are the only annotation ownership mechanism; and availability, grouping, and color are all derived rather than stored.

### Added
- A global sidebar for viewing, editing, and deleting annotation values and properties across the whole session, with destructive-action confirmation.
- Downstream editing of a propagated annotation at its unambiguous origin, with automatic refusal when several distinct origins are pooled.
- Right-click removal of node and process annotations from group cards and connectors, including bulk removal across pooled links.
- Assignment, replacement, and detachment of existing stored Recipes and their read-only Components from the process rail and shelf, with same-label Recipes disambiguated by their stored resource identity.

## [0.0.1] - 2024-06-12
- Initial release
