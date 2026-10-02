# Convert a Directory Tree to a Mermaid Mindmap

Builds the directory tree with the same options as `build_tree()` and
returns it as [Mermaid](https://mermaid.js.org/) mindmap text: a radial
alternative to the flowchart produced by
[`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md).
The root uses the cloud shape and every level is indented beneath its
parent.

## Usage

``` r
tree_to_mindmap(path = NULL, file = NULL, ...)
```

## Arguments

- path:

  Character. Directory path, project name, or `.Rproj` file. If NULL,
  uses the current directory.

- file:

  Character or NULL. If given, the diagram text is written to this file
  with [`writeLines()`](https://rdrr.io/r/base/writeLines.html).

- ...:

  Additional arguments passed to `build_tree()` (e.g. `ignore`,
  `max_depth`, `show_hidden`, `prune`).

## Value

Invisibly, the diagram text as a single string (or `file` when `file` is
given). Use [`cat()`](https://rdrr.io/r/base/cat.html) to print it, e.g.
into a Quarto `{mermaid}` chunk.

## Examples

``` r
demo <- file.path(tempdir(), "printtree-mindmap-demo")
if (dir.exists(demo)) unlink(demo, recursive = TRUE)
dir.create(file.path(demo, "R"), recursive = TRUE)
file.create(file.path(demo, "R", "hello.R"))
#> [1] TRUE

cat(tree_to_mindmap(demo))
#> mindmap
#>   root((printtree-mindmap-demo/))
#>     R/
#>       hello.R

# Save a mindmap file, ready for a Quarto {mermaid} chunk
tree_to_mindmap(demo, file = tempfile(fileext = ".mmd"))
```
