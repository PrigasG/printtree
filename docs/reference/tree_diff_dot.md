# Diff Two Directory Trees as a Graphviz DOT Graph

Builds both trees with the same options and renders the difference as a
Graphviz DOT graph: nodes only in `after` are filled green, nodes only
in `before` are filled red and dashed, and unchanged nodes are neutral.
Nodes are matched by their path relative to each tree root.

## Usage

``` r
tree_diff_dot(
  before,
  after,
  rankdir = c("TB", "LR", "RL", "BT"),
  file = NULL,
  ...
)
```

## Arguments

- before:

  Character. Directory path for the "before" tree.

- after:

  Character. Directory path for the "after" tree.

- rankdir:

  Character. Graph direction: `"TB"` (top-bottom), `"LR"` (left-right),
  `"RL"`, or `"BT"`.

- file:

  Character or NULL. If given, the diagram text is written to this file
  with [`writeLines()`](https://rdrr.io/r/base/writeLines.html).

- ...:

  Additional arguments passed to `build_tree()` for both trees (e.g.
  `ignore`, `max_depth`, `show_hidden`, `prune`).

## Value

Invisibly, the diagram text as a single string (or `file` when `file` is
given).

## Examples

``` r
old <- file.path(tempdir(), "printtree-diff-old")
new <- file.path(tempdir(), "printtree-diff-new")
for (d in c(old, new)) {
  if (dir.exists(d)) unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
}
file.create(file.path(old, "gone.txt"))
#> [1] TRUE
file.create(file.path(new, "added.txt"))
#> [1] TRUE

cat(tree_diff_dot(old, new))
#> digraph printtree {
#>   rankdir=TB;
#>   node [fontname="Helvetica"];
#>   "n1" [label="printtree-diff-new/", shape=folder];
#>   "n2" [label="added.txt", shape=note, style="filled", fillcolor="#dcfce7"];
#>   "n3" [label="gone.txt", shape=note, style="filled,dashed", fillcolor="#fee2e2"];
#>   "n1" -> "n2";
#>   "n1" -> "n3";
#> }

# Left-to-right layout
cat(tree_diff_dot(old, new, rankdir = "LR"))
#> digraph printtree {
#>   rankdir=LR;
#>   node [fontname="Helvetica"];
#>   "n1" [label="printtree-diff-new/", shape=folder];
#>   "n2" [label="added.txt", shape=note, style="filled", fillcolor="#dcfce7"];
#>   "n3" [label="gone.txt", shape=note, style="filled,dashed", fillcolor="#fee2e2"];
#>   "n1" -> "n2";
#>   "n1" -> "n3";
#> }
```
