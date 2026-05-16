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
