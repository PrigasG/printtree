# printtree 0.2.2

- `max_depth` is now validated: it must be `NULL` or a single non-negative whole number (previously invalid values behaved cryptically or silently).
- `snapshot_width` must now be a single whole number between 1 and 15000 (previously any positive value passed, including fractions that later failed inside the graphics device).
- `snapshot_path` is now checked before the tree is built, and `~` in `snapshot_file` is expanded; Windows UNC paths are recognized as absolute.
- Snapshot PNG height is now capped so very large trees cannot request an enormous raster.
- Git directory labels now reflect the nested status (`?` untracked, `+` staged, `M` modified, with `M` taking precedence) instead of always showing `M`.
- Git status is now parsed from `git status --porcelain -z` (NUL-separated, never quoted), so file names with spaces, Unicode characters, or renames are labeled correctly.
- Git integration tests now skip on CRAN with exit-status-checked repository setup, and the vignette Git example is no longer evaluated, so no Git repository is initialized during CRAN checks.
- Clarified that `project = "auto"` is an alias of `"none"`, and that `write_tree()`'s `...` goes to the underlying tree builder.
- Added a multi-platform R CMD check workflow.


# printtree 0.2.1

- Fixed hidden file handling on Windows so directories with the hidden file-system attribute are omitted when `show_hidden = FALSE`, even when their names do not start with ".".
- Added count summaries for displayed directories and files.
- Added pattern-based ignores with fixed, glob, regex, and automatic matching modes.
- Added `git = TRUE` to annotate displayed files and directories with simple Git status markers.
- Added `quiet = TRUE` for programmatic use and a Git status legend when `git = TRUE`.
- Added `write_tree()` for text and Markdown tree exports, with automatic parent directory creation by default.
- Added `prune = TRUE` to hide directories with no displayable children.


# printtree 0.2.0

- Added optional PNG snapshot export via `snapshot = TRUE`, with light/dark backgrounds.
- Improved project root detection using `root_markers` (e.g., `.Rproj` and `DESCRIPTION`).
- Improved project lookup to accept `.Rproj` filenames and project names in `search_paths`.


# printtree 0.1.0

-   Initial CRAN release.
-   `print_rtree()` prints directory trees and optionally detects `.Rproj` roots.
