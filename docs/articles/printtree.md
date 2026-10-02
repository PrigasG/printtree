# Printing directory trees with printtree

``` r

library(printtree)
```

## Basic Usage

Print the tree for current working directory

``` r

tmp <- tempdir()
demo <- file.path(tmp, "printtree-demo")

# Start fresh

if (dir.exists(demo)) unlink(demo, recursive = TRUE, force = TRUE)

dir.create(demo, recursive = TRUE)
dir.create(file.path(demo, "R"))
dir.create(file.path(demo, "data", "raw"), recursive = TRUE)

file.create(file.path(demo, "R", "hello.R"))
#> [1] TRUE
file.create(file.path(demo, "README.md"))
#> [1] TRUE
file.create(file.path(demo, ".Rhistory"))
#> [1] TRUE
```

Print the tree

``` r

print_rtree()
#> vignettes/
#> |-- feature-tour.Rmd
#> |-- printtree.Rmd
#> |-- tree-dark.png
#> |-- tree-white.png
#> `-- tree.png
#> 
#> 0 directories, 5 files
```

## Project root detection

When project = “root”, printtree can walk upward from the provided path
to detect a project root using simple markers. By default this includes:

- .Rproj files (RStudio / Posit projects)

- DESCRIPTION files (R package roots)

In this example, we mark demo as a package-like root by creating a
DESCRIPTION file, then print from a subdirectory:

``` r

file.create(file.path(demo, "DESCRIPTION"))
#> [1] TRUE
subdir <- file.path(demo, "data", "raw")
print_rtree(subdir, project = "root")
#> printtree-demo/
#> |-- data/
#> |   `-- raw/
#> |-- R/
#> |   `-- hello.R
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 3 directories, 3 files
```

You can customize detection using rootmarkers:

``` r

print_rtree(subdir,
project = "root",
root_markers = c(".Rproj", "DESCRIPTION", "_quarto.yml"))
#> printtree-demo/
#> |-- data/
#> |   `-- raw/
#> |-- R/
#> |   `-- hello.R
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 3 directories, 3 files
```

## Common Options

Limit depth:

``` r

print_rtree(max_depth = 2)
#> vignettes/
#> |-- feature-tour.Rmd
#> |-- printtree.Rmd
#> |-- tree-dark.png
#> |-- tree-white.png
#> `-- tree.png
#> 
#> 0 directories, 5 files
```

By default, output ends with a count summary for the displayed tree:

``` r

print_rtree(demo, max_depth = 1)
#> printtree-demo/
#> |-- data/
#> |-- R/
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 2 directories, 2 files
```

Suppress the footer when you want the older compact output:

``` r

print_rtree(demo, max_depth = 1, count_footer = FALSE)
#> printtree-demo/
#> |-- data/
#> |-- R/
#> |-- DESCRIPTION
#> `-- README.md
```

Ignore exact names or patterns. With the default `ignore_type = "auto"`,
wildcard values are treated as glob patterns:

``` r

file.create(file.path(demo, "debug.log"))
#> [1] TRUE
print_rtree(demo, ignore = c("*.log", ".Rhistory"), max_depth = 2)
#> printtree-demo/
#> |-- data/
#> |   `-- raw/
#> |-- R/
#> |   `-- hello.R
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 3 directories, 3 files
```

Use regular expressions when you need them:

``` r

print_rtree(demo, ignore = "^README", ignore_type = "regex", max_depth = 1)
#> printtree-demo/
#> |-- data/
#> |-- R/
#> |-- debug.log
#> `-- DESCRIPTION
#> 
#> 2 directories, 2 files
```

Prune directories that have no displayable children:

``` r

dir.create(file.path(demo, "logs"))
file.create(file.path(demo, "logs", "debug.log"))
#> [1] TRUE
print_rtree(demo, ignore = "*.log", prune = TRUE)
#> printtree-demo/
#> |-- R/
#> |   `-- hello.R
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 1 directory, 3 files
```

Show hidden files:

``` r

print_rtree(demo, show_hidden = TRUE, max_depth = 2)
#> printtree-demo/
#> |-- data/
#> |   `-- raw/
#> |-- logs/
#> |   `-- debug.log
#> |-- R/
#> |   `-- hello.R
#> |-- debug.log
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 4 directories, 5 files
```

Use unicode tree glyphs (if your terminal supports them):

``` r

print_rtree(demo, format = "unicode", max_depth = 2)
#> printtree-demo/
#> ├── data/
#> │   └── raw/
#> ├── logs/
#> │   └── debug.log
#> ├── R/
#> │   └── hello.R
#> ├── debug.log
#> ├── DESCRIPTION
#> └── README.md
#> 
#> 4 directories, 5 files
```

Annotate files with Git status when the tree is inside a Git work tree:

``` r

print_rtree(git = TRUE)
```

Use `quiet = TRUE` when you want to capture lines without printing:

``` r

lines <- print_rtree(demo, return_lines = TRUE, quiet = TRUE)
head(lines)
#> [1] "printtree-demo/"   "|-- data/"         "|   `-- raw/"     
#> [4] "|-- logs/"         "|   `-- debug.log" "|-- R/"
```

Write a tree to a text or Markdown file:

``` r

tree_md <- file.path(tempdir(), "printtree-output", "tree.md")
write_tree(demo, tree_md, format = "md", title = "Demo Tree")
```

Preview the same directory as a rendered Mermaid diagram. The browser
opens only in an interactive R session:

``` r

view_mermaid(demo)
```

Generate a PNG snapshot:

``` r

# Save PNG snapshots 
png_light <- tempfile("tree-light-", fileext = ".png")
png_dark  <- tempfile("tree-dark-",  fileext = ".png")

# Light (white bg, black text)
print_rtree(demo, snapshot = TRUE, snapshot_bg = "white", snapshot_file = png_light)
#> printtree-demo/
#> |-- data/
#> |   `-- raw/
#> |-- logs/
#> |   `-- debug.log
#> |-- R/
#> |   `-- hello.R
#> |-- debug.log
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 4 directories, 5 files

# Dark (black bg, white text)
print_rtree(demo, snapshot = TRUE, snapshot_bg = "black", snapshot_file = png_dark)
#> printtree-demo/
#> |-- data/
#> |   `-- raw/
#> |-- logs/
#> |   `-- debug.log
#> |-- R/
#> |   `-- hello.R
#> |-- debug.log
#> |-- DESCRIPTION
#> `-- README.md
#> 
#> 4 directories, 5 files
```
