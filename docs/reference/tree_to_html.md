# Convert a Directory Tree to a Collapsible HTML Page

Builds the directory tree with the same options as `build_tree()` and
returns it as a self-contained HTML document: directories expand and
collapse on click, with no external dependencies. When the tree is built
with `git = TRUE`, Git status badges are shown next to changed entries.

## Usage

``` r
tree_to_html(path = NULL, title = NULL, file = NULL, git_colors = FALSE, ...)
```

## Arguments

- path:

  Character. Directory path, project name, or `.Rproj` file. If NULL,
  uses the current directory.

- title:

  Character or NULL. Page title and heading. Defaults to
  `"Directory tree"`.

- file:

  Character or NULL. If given, the HTML is written to this file with
  [`writeLines()`](https://rdrr.io/r/base/writeLines.html).

- git_colors:

  Logical. If TRUE, nodes are colored by their Git status (badges for
  modified, untracked, and staged files). Implies `git = TRUE`.

- ...:

  Additional arguments passed to `build_tree()` (e.g. `ignore`,
  `max_depth`, `show_hidden`, `prune`, `git`).

## Value

Invisibly, the HTML document as a single string (or `file` when `file`
is given).

## Examples

``` r
demo <- file.path(tempdir(), "printtree-html-demo")
if (dir.exists(demo)) unlink(demo, recursive = TRUE)
dir.create(file.path(demo, "R"), recursive = TRUE)
file.create(file.path(demo, "R", "hello.R"))
#> [1] TRUE

html <- tree_to_html(demo, title = "Demo project")
out <- tempfile(fileext = ".html")
tree_to_html(demo, file = out)

# Inside a Git checkout, git_colors = TRUE adds status badges
# next to modified, untracked, and staged files
```
