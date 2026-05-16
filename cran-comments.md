## Release summary

This is an update to `printtree` 0.2.1.

In this release I have:

- Fixed Windows hidden file handling so directories with the hidden file-system attribute are omitted when `show_hidden = FALSE`.
- Added displayed directory/file count summaries.
- Added pattern-based ignores using fixed, glob, regex, or automatic matching.
- Added optional Git status annotations with `git = TRUE`, including a short legend.
- Added `quiet = TRUE` for programmatic use.
- Added `write_tree()` for text and Markdown tree exports, with automatic parent directory creation.
- Added `prune = TRUE` to hide directories with no displayable children.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no known reverse dependencies.
