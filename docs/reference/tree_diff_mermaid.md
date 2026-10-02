# Diff Two Directory Trees as a Mermaid Flowchart

Builds both trees with the same options and renders the difference as a
Mermaid flowchart: nodes only in `after` are green, nodes only in
`before` are red and dashed, and unchanged nodes are neutral. Nodes are
matched by their path relative to each tree root.

## Usage

``` r
tree_diff_mermaid(
  before,
  after,
  direction = c("TD", "LR", "RL", "BT"),
  file = NULL,
  ...
)
```

## Arguments

- before:

  Character. Directory path for the "before" tree.

- after:

  Character. Directory path for the "after" tree.

- direction:

  Character. Flowchart direction: `"TD"` (top-down), `"LR"`
  (left-right), `"RL"`, or `"BT"`.

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
file.create(file.path(old, "kept.txt"))
#> [1] TRUE
file.create(file.path(new, "kept.txt"))
#> [1] TRUE

cat(tree_diff_mermaid(old, new))
#> flowchart TD
#>     n1(["printtree-diff-new/"])
#>     n2["added.txt"]
#>     n3["kept.txt"]
#>     n4["gone.txt"]
#>     classDef ptAdd fill:#dcfce7,stroke:#15803d;
#>     classDef ptDel fill:#fee2e2,stroke:#b91c1c,stroke-dasharray:5 5;
#>     class n2 ptAdd;
#>     class n4 ptDel;
#>     n1 --> n2
#>     n1 --> n3
#>     n1 --> n4

# Left-to-right layout
cat(tree_diff_mermaid(old, new, direction = "LR"))
#> flowchart LR
#>     n1(["printtree-diff-new/"])
#>     n2["added.txt"]
#>     n3["kept.txt"]
#>     n4["gone.txt"]
#>     classDef ptAdd fill:#dcfce7,stroke:#15803d;
#>     classDef ptDel fill:#fee2e2,stroke:#b91c1c,stroke-dasharray:5 5;
#>     class n2 ptAdd;
#>     class n4 ptDel;
#>     n1 --> n2
#>     n1 --> n3
#>     n1 --> n4
```
