# printtree

`printtree` prints a compact directory tree for R projects or any folder. It can optionally detect an Integrated Development Environment (IDE) (e.g.,RStudio project (`.Rproj`)) and print from the project root.

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
```

## Notes

-   Default output uses ASCII for portability.

-   Hidden files are excluded unless show_hidden = TRUE.
