# Convert a Directory Tree to a Graphviz DOT Graph

Builds the directory tree with the same options as `build_tree()` and
returns it as [Graphviz](https://graphviz.org/) DOT text. The output
renders in Quarto documents (in a `{dot}` chunk when Graphviz is
installed) and with any DOT-compatible tool. Directories use the
`folder` shape and files use the `note` shape.

## Usage

``` r
tree_to_dot(
  path = NULL,
  rankdir = c("TB", "LR", "RL", "BT"),
  file = NULL,
  git_colors = FALSE,
  repo_url = NULL,
  repo_branch = "main",
  ...
)
```

## Arguments

- path:

  Character. Directory path, project name, or `.Rproj` file. If NULL,
  uses the current directory.

- rankdir:

  Character. Graph direction: `"TB"` (top-bottom), `"LR"` (left-right),
  `"RL"`, or `"BT"`.

- file:

  Character or NULL. If given, the diagram text is written to this file
  with [`writeLines()`](https://rdrr.io/r/base/writeLines.html).

- git_colors:

  Logical. If TRUE, nodes are filled by their Git status (amber for
  modified, green for untracked, blue for staged). Implies `git = TRUE`
  unless `git` is set explicitly.

- repo_url:

  Character or NULL. Base URL of the Git repository, e.g.
  `"https://github.com/user/repo"`. If given, every node gets a `URL`
  attribute linking to the file (`blob`) or directory (`tree`) on
  `repo_branch`, so SVG output is clickable.

- repo_branch:

  Character. Branch used to build the `repo_url` links.

- ...:

  Additional arguments passed to `build_tree()` (e.g. `ignore`,
  `max_depth`, `show_hidden`, `prune`).

## Value

Invisibly, the diagram text as a single string (or `file` when `file` is
given). Use [`cat()`](https://rdrr.io/r/base/cat.html) to print it.

## Examples

``` r
demo <- file.path(tempdir(), "printtree-dot-demo")
if (dir.exists(demo)) unlink(demo, recursive = TRUE)
dir.create(file.path(demo, "R"), recursive = TRUE)
file.create(file.path(demo, "R", "hello.R"))
#> [1] TRUE
file.create(file.path(demo, "README.md"))
#> [1] TRUE

cat(tree_to_dot(demo))
#> digraph printtree {
#>   rankdir=TB;
#>   node [fontname="Helvetica"];
#>   "n1" [label="printtree-dot-demo/", shape=folder];
#>   "n2" [label="hello.R", shape=note];
#>   "n3" [label="R/", shape=folder];
#>   "n4" [label="README.md", shape=note];
#>   "n3" -> "n2";
#>   "n1" -> "n3";
#>   "n1" -> "n4";
#> }

# Left-to-right layout with clickable nodes
cat(tree_to_dot(demo, rankdir = "LR",
                repo_url = "https://github.com/PrigasG/printtree"))
#> digraph printtree {
#>   rankdir=LR;
#>   node [fontname="Helvetica"];
#>   "n1" [label="printtree-dot-demo/", shape=folder, URL="https://github.com/PrigasG/printtree/tree/main"];
#>   "n2" [label="hello.R", shape=note, URL="https://github.com/PrigasG/printtree/blob/main/R/hello.R"];
#>   "n3" [label="R/", shape=folder, URL="https://github.com/PrigasG/printtree/tree/main/R"];
#>   "n4" [label="README.md", shape=note, URL="https://github.com/PrigasG/printtree/blob/main/README.md"];
#>   "n3" -> "n2";
#>   "n1" -> "n3";
#>   "n1" -> "n4";
#> }
```
