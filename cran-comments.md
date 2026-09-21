## Release summary

This is an update bringing `printtree` to 0.2.2.

In this release I have:

- Validated `max_depth`: it must be `NULL` or a single non-negative whole number (previously invalid values behaved cryptically or silently).
- Validated `snapshot_width`: it must now be a single whole number between 1 and 15000 (previously any positive value passed, including fractions that later failed inside the graphics device).
- Checked `snapshot_path` before the tree is built; expanded `~` in `snapshot_file`; recognized Windows UNC paths as absolute.
- Capped snapshot PNG height so very large trees cannot request an enormous raster.
- Made Git directory labels reflect nested status (`?` untracked, `+` staged, `M` modified, with `M` taking precedence) instead of always showing `M`.
- Parsed `git status --porcelain` with `-z` (NUL-separated, never quoted) so file names with spaces, Unicode characters, or renames are labeled correctly.
- Hardened the Git integration tests and vignette so no Git repository is initialized during CRAN checks (`skip_on_cran()`, exit-status-checked setup, vignette Git example not evaluated).
- Clarified that `project = "auto"` is an alias of `"none"`, and that `write_tree()`'s `...` goes to the underlying tree builder.
- Added a multi-platform R CMD check workflow.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no known reverse dependencies.
