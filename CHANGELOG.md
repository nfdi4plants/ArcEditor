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
- Added separate sort choices for input/output cards and annotation rows. Sorting now follows displayed names, handles numbers naturally, and breaks ties consistently.
- Replaced provenance origin icons with plain, hatched, and half-hatched chip backgrounds, with matching legend text for current and upstream values.
- Reserved space for provenance rail controls so hover and keyboard focus do not move labels or connectors.
- Process-value drops now require at least one existing target link, so disconnected groups are not offered as valid targets.

### Added
- A global sidebar for viewing, editing, and deleting annotation values and properties across the whole session, with destructive-action confirmation.
- Downstream editing of a propagated annotation at its unambiguous origin, with automatic refusal when several distinct origins are pooled.
- Right-click removal of node and process annotations from group cards and connectors, including bulk removal across pooled links.
- Assignment, replacement, and detachment of existing stored Recipes and their read-only Components from the process rail and shelf, with same-label Recipes disambiguated by their stored resource identity.
- A Select all button for each input and output group column.
- Command previews for provenance assignment, editing, and removal actions. Invalid controls now show the reason an action is unavailable, including when a value or property has a read-only backing.

### Fixed
- Fixed value copy round trips through ProcessCore writeback, including edits to shared annotation references without duplicating their stored backing data.
- Fixed value and connector overlap in the provenance surface by isolating its layout layers.
- Limited group-card expansion and context menus to the card's upper surface, so lower member details do not trigger those actions.

## [0.0.1] - 2024-06-12
- Initial release
