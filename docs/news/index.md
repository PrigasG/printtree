# Changelog

## printtree 0.3.0

- New
  [`view_mermaid()`](https://prigasg.github.io/printtree/reference/view_mermaid.md)
  writes a browser-ready HTML preview of the Mermaid tree and opens it
  only in interactive R sessions, keeping automated and CRAN checks
  browser-free.
- [`view_mermaid()`](https://prigasg.github.io/printtree/reference/view_mermaid.md)
  gains `pan_zoom` controls for navigating large diagrams, one-click
  SVG/PNG/JPEG downloads in the HTML viewer, and a `save` argument
  (`"png"`, `"jpeg"`, or `"pdf"`) for clean diagram export through the
  suggested `webshot2` package and headless Chrome. Explicit output
  extensions are validated against `save`.
- New
  [`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md)
  converts a directory tree to a Mermaid flowchart, which renders
  natively in Quarto `{mermaid}` chunks, GitHub Markdown, and anywhere
  else Mermaid is supported. Directories use the stadium shape, files
  use rectangles.
- New
  [`tree_to_dot()`](https://prigasg.github.io/printtree/reference/tree_to_dot.md)
  converts a directory tree to a Graphviz DOT graph (`folder`/`note`
  shapes), renderable in Quarto `{dot}` chunks and any DOT-compatible
  tool.
- [`write_tree()`](https://prigasg.github.io/printtree/reference/write_tree.md)
  gains `"mermaid"`, `"dot"`, and `"qmd"` formats: the first two write
  the raw diagram text, and `"qmd"` writes a minimal Quarto document
  embedding the Mermaid flowchart.
- The internal tree builder now also returns the displayed nodes as a
  data frame (path, name, depth, directory flag, parent), so diagram
  export sees exactly the same nodes as the printed tree, including
  `prune` and ignore filtering.
- Diagram extras:
  [`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md)
  and
  [`tree_to_dot()`](https://prigasg.github.io/printtree/reference/tree_to_dot.md)
  gain `git_colors` (nodes tinted by Git status: amber modified, green
  untracked, blue staged), `repo_url`/`repo_branch` (every node links to
  the file or directory in the repository), and
  [`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md)
  gains `subgraph` (each directory wrapped in a Mermaid subgraph
  container).
- New
  [`tree_to_mindmap()`](https://prigasg.github.io/printtree/reference/tree_to_mindmap.md)
  renders the tree as a radial Mermaid mindmap, and
  [`write_tree()`](https://prigasg.github.io/printtree/reference/write_tree.md)
  gains a `"mindmap"` format.
- New
  [`tree_to_html()`](https://prigasg.github.io/printtree/reference/tree_to_html.md)
  (and `write_tree(format = "html")`) writes a self-contained
  collapsible HTML tree with click-to-expand directories and optional
  Git status badges; no external dependencies.
- New
  [`tree_diff_mermaid()`](https://prigasg.github.io/printtree/reference/tree_diff_mermaid.md)
  and
  [`tree_diff_dot()`](https://prigasg.github.io/printtree/reference/tree_diff_dot.md)
  render the difference between two directory trees: nodes only in the
  second tree are green, nodes only in the first are red and dashed.

## printtree 0.2.2

CRAN release: 2026-09-28

- `max_depth` is now validated: it must be `NULL` or a single
  non-negative whole number.
- `snapshot_width` must now be a single whole number between 1 and 15000
  (previously any positive value passed, including fractions that later
  failed inside the graphics device).
- `snapshot_path` is now checked before the tree is built, and `~` in
  `snapshot_file` is expanded; Windows UNC paths are recognized as
  absolute.
- Snapshot PNG height is now capped so very large trees cannot request
  an enormous raster.
- Git directory labels now reflect the nested status (`?` untracked, `+`
  staged, `M` modified, with `M` taking precedence) instead of always
  showing `M`.
- Git status is now parsed from `git status --porcelain -z`
  (NUL-separated, never quoted), so file names with spaces, Unicode
  characters, or renames are labeled correctly.
- Git repository paths are now quoted when invoking Git, so status
  annotations work when the repository directory contains spaces (common
  on Windows and OneDrive-synced folders).
- Git path bytes are now decoded with explicit UTF-8 marking, so Unicode
  file names keep their status labels in non-UTF-8 R sessions.
- Git integration tests now skip on CRAN with exit-status-checked
  repository setup, and the vignette Git example is no longer evaluated,
  so no Git repository is initialized during CRAN checks.
- Clarified that `project = "auto"` is an alias of `"none"`, and that
  [`write_tree()`](https://prigasg.github.io/printtree/reference/write_tree.md)’s
  `...` goes to the underlying tree builder.
- Added a multi-platform R CMD check workflow.

## printtree 0.2.1

CRAN release: 2026-05-16

- Fixed hidden file handling on Windows so directories with the hidden
  file-system attribute are omitted when `show_hidden = FALSE`, even
  when their names do not start with “.”.
- Added count summaries for displayed directories and files.
- Added pattern-based ignores with fixed, glob, regex, and automatic
  matching modes.
- Added `git = TRUE` to annotate displayed files and directories with
  simple Git status markers.
- Added `quiet = TRUE` for programmatic use and a Git status legend when
  `git = TRUE`.
- Added
  [`write_tree()`](https://prigasg.github.io/printtree/reference/write_tree.md)
  for text and Markdown tree exports, with automatic parent directory
  creation by default.
- Added `prune = TRUE` to hide directories with no displayable children.

## printtree 0.2.0

CRAN release: 2026-01-30

- Added optional PNG snapshot export via `snapshot = TRUE`, with
  light/dark backgrounds.
- Improved project root detection using `root_markers` (e.g., `.Rproj`
  and `DESCRIPTION`).
- Improved project lookup to accept `.Rproj` filenames and project names
  in `search_paths`.

## printtree 0.1.0

- Initial CRAN release.
- [`print_rtree()`](https://prigasg.github.io/printtree/reference/print_rtree.md)
  prints directory trees and optionally detects `.Rproj` roots.
