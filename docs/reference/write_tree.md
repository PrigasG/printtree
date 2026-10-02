# Write a Directory Tree to a Text, Markdown, Diagram, or Quarto File

Builds a directory tree with the same options as
[`print_rtree()`](https://prigasg.github.io/printtree/reference/print_rtree.md)
and writes it to a plain text, Markdown, diagram, or Quarto file.

## Usage

``` r
write_tree(
  path = NULL,
  file,
  format = c("txt", "md", "mermaid", "dot", "qmd", "mindmap", "html"),
  title = NULL,
  create_dirs = TRUE,
  ...
)
```

## Arguments

- path:

  Character. Directory path, project name, or `.Rproj` file. If NULL,
  uses current directory.

- file:

  Character. Output file path.

- format:

  One of `"txt"`, `"md"`, `"mermaid"`, `"dot"`, `"qmd"`, `"mindmap"`, or
  `"html"`. `"mermaid"` writes a Mermaid flowchart (renders in Quarto
  `{mermaid}` chunks and GitHub Markdown), `"dot"` writes a Graphviz DOT
  graph, `"qmd"` writes a minimal Quarto document embedding the Mermaid
  flowchart, `"mindmap"` writes a Mermaid mindmap, and `"html"` writes a
  self-contained collapsible HTML tree.

- title:

  Optional heading: used as the Markdown heading when `format = "md"`,
  as the Quarto document title when `format = "qmd"`, and as the page
  title and heading when `format = "html"`.

- create_dirs:

  Logical. If TRUE, create the output file's parent directory when it
  does not exist.

- ...:

  Additional arguments passed to the underlying tree builder (the same
  tree options as
  [`print_rtree()`](https://prigasg.github.io/printtree/reference/print_rtree.md),
  such as `ignore`, `max_depth`, `git`, or `prune`). Diagram engines
  also accept `direction` (Mermaid), `rankdir` (DOT), `git_colors`,
  `subgraph`, and `repo_url`/`repo_branch`; see
  [`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md)
  and
  [`tree_to_dot()`](https://prigasg.github.io/printtree/reference/tree_to_dot.md).

## Value

Invisibly returns the output file path.

## Examples

``` r
demo <- file.path(tempdir(), "printtree-write-demo")
if (dir.exists(demo)) unlink(demo, recursive = TRUE)
dir.create(demo, recursive = TRUE)
file.create(file.path(demo, "README.md"))
#> [1] TRUE

out <- tempfile(fileext = ".md")
write_tree(demo, out, format = "md")

# Mermaid flowchart and a Quarto document embedding it
write_tree(demo, tempfile(fileext = ".mmd"), format = "mermaid")
write_tree(demo, tempfile(fileext = ".qmd"), format = "qmd",
           title = "Demo project tree")

# Mermaid mindmap and a collapsible HTML tree
write_tree(demo, tempfile(fileext = ".mmd"), format = "mindmap")
write_tree(demo, tempfile(fileext = ".html"), format = "html",
           title = "Demo project tree")
```
