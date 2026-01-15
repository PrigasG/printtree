# printtree <img src="man/figures/favicon.png" alt="printtree logo" align="right" width="120" />


`printtree` prints a compact directory tree for R projects or any folder.\
It can optionally detect project roots associated with common R workflows (e.g., RStudio projects via `.Rproj` files) and print the tree from the appropriate root directory. A snapshot of the tree directory can be generated using the ***snapshot*** feature.

The package is IDE-agnostic: if no project metadata is detected, it simply prints the directory tree for the specified folder.

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

```

## Project root detection

When project = "root", printtree can walk upward from the given path to detect a project root using simple markers:

-   .Rproj files (RStudio / Posit projects)

-   DESCRIPTION files (R package roots)

This behavior can be customized using the root_markers argument.

``` r
# Detect R package root (DESCRIPTION)
print_rtree(project = "root")

# Include Quarto projects
print_rtree(project = "root",
            root_markers = c(".Rproj", "DESCRIPTION", "_quarto.yml"))
```

If no project root is detected, the tree is printed from the provided path.

## Notes

-   Default output uses ASCII for portability.

-   Hidden files are excluded unless show_hidden = TRUE.
