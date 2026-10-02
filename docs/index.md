# printtree ![printtree logo](reference/figures/new_tree.png)

`printtree` prints a compact directory tree for R projects or any
folder.\
It can optionally detect project roots associated with common R
workflows (e.g., RStudio projects via `.Rproj` files) and print the tree
from the appropriate root directory. Trees can include count summaries,
Git status annotations, pattern-based ignores, pruning of empty
directories, PNG snapshots, and text or Markdown exports.

The package is IDE-agnostic: if no project metadata is detected, it
simply prints the directory tree for the specified folder.

## Installation

``` r

# install.packages("printtree")  
```

## Usage

``` r

library(printtree)

# Current working directory
print_rtree()

# Explicit path
print_rtree("~/Projects/myproj")

# Project name (searched in search_paths)
print_rtree("myproj", search_paths = c("~/Projects", "~/Documents"))

# Limit depth
print_rtree(max_depth = 2)

# Ignore by glob pattern
print_rtree(ignore = c("*.log", "test_*"))

# Annotate files with Git status
print_rtree(git = TRUE)

# Return lines without printing
lines <- print_rtree(return_lines = TRUE, quiet = TRUE)

# Use the old compact output without a count footer
print_rtree(count_footer = FALSE)

# Hide directories with no displayable children
print_rtree(ignore = "*.log", prune = TRUE)

# Unicode tree (if your terminal supports it)
print_rtree(format = "unicode")

# Save a PNG snapshot
print_rtree(snapshot = TRUE)

# Dark background snapshot
print_rtree(snapshot = TRUE, snapshot_bg = "black", snapshot_file = "tree-dark.png")

#or White background with dark text
print_rtree(snapshot = TRUE, snapshot_bg = "white", snapshot_file = "tree-white.png")

# Save to a specific directory
print_rtree(snapshot = TRUE, snapshot_path = "~/Pictures")

# Save a Markdown tree
write_tree(".", "tree.md", format = "md", title = "Project Tree")

# Mermaid flowchart (renders in Quarto {mermaid} chunks and GitHub Markdown)
cat(tree_to_mermaid("."))

# Graphviz DOT graph
cat(tree_to_dot("."))

# A ready-to-render Quarto document with the tree as a Mermaid diagram
write_tree(".", "tree.qmd", format = "qmd", title = "Project Tree")

# Flowchart with Git-status colors, directory subgraphs, and clickable nodes
cat(tree_to_mermaid(".", git_colors = TRUE, subgraph = TRUE,
                    repo_url = "https://github.com/PrigasG/printtree"))

# Mermaid mindmap and a self-contained collapsible HTML tree
cat(tree_to_mindmap(".", max_depth = 2))
write_tree(".", "tree.html", format = "html", title = "Project Tree")

# Diff two trees as a Mermaid flowchart (green = added, red dashed = removed)
cat(tree_diff_mermaid("old-project", "new-project"))
```

## Project root detection

When project = “root”, printtree can walk upward from the given path to
detect a project root using simple markers:

- .Rproj files (RStudio / Posit projects)

- DESCRIPTION files (R package roots)

This behavior can be customized using the root_markers argument.

``` r

# Detect R package root (DESCRIPTION)
print_rtree(project = "root")

# Include Quarto projects
print_rtree(project = "root",
            root_markers = c(".Rproj", "DESCRIPTION", "_quarto.yml"))
```

If no project root is detected, the tree is printed from the provided
path.

## Notes

- Default output uses ASCII for portability.

- Hidden files are excluded unless show_hidden = TRUE.

- On Windows, hidden file-system attributes are also respected when
  hidden files are excluded.
