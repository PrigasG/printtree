# Feature tour

``` r

library(printtree)
```

This article walks through the output features added in `printtree`
0.2.1.

``` r

demo <- file.path(tempdir(), "printtree-feature-tour")
if (dir.exists(demo)) unlink(demo, recursive = TRUE, force = TRUE)

dir.create(file.path(demo, "R"), recursive = TRUE)
dir.create(file.path(demo, "data", "raw"), recursive = TRUE)
dir.create(file.path(demo, "logs"), recursive = TRUE)
dir.create(file.path(demo, "empty"), recursive = TRUE)

file.create(file.path(demo, "R", "helpers.R"))
#> [1] TRUE
file.create(file.path(demo, "README.md"))
#> [1] TRUE
file.create(file.path(demo, "data", "raw", "sales.csv"))
#> [1] TRUE
file.create(file.path(demo, "logs", "debug.log"))
#> [1] TRUE
file.create(file.path(demo, "test_cache"))
#> [1] TRUE
```

## Count summaries

By default,
[`print_rtree()`](https://prigasg.github.io/printtree/reference/print_rtree.md)
ends with a displayed directory/file count.

``` r

print_rtree(demo, max_depth = 2)
#> printtree-feature-tour/
#> |-- data/
#> |   `-- raw/
#> |-- empty/
#> |-- logs/
#> |   `-- debug.log
#> |-- R/
#> |   `-- helpers.R
#> |-- README.md
#> `-- test_cache
#> 
#> 5 directories, 4 files
```

Set `count_footer = FALSE` for compact output.

``` r

print_rtree(demo, max_depth = 1, count_footer = FALSE)
#> printtree-feature-tour/
#> |-- data/
#> |-- empty/
#> |-- logs/
#> |-- R/
#> |-- README.md
#> `-- test_cache
```

## Pattern ignores

The `ignore` argument still supports exact basenames, but with
`ignore_type = "auto"` it also treats wildcard entries as glob patterns.

``` r

print_rtree(demo, ignore = c("*.log", "test_*"), max_depth = 2)
#> printtree-feature-tour/
#> |-- data/
#> |   `-- raw/
#> |-- empty/
#> |-- logs/
#> |-- R/
#> |   `-- helpers.R
#> `-- README.md
#> 
#> 5 directories, 2 files
```

You can opt into regular expression matching for more control.

``` r

print_rtree(demo, ignore = "^(README|test_)", ignore_type = "regex", max_depth = 1)
#> printtree-feature-tour/
#> |-- data/
#> |-- empty/
#> |-- logs/
#> `-- R/
#> 
#> 4 directories, 0 files
```

## Prune empty directories

Use `prune = TRUE` to hide directories with no displayable children
after ignores and depth limits are applied.

``` r

print_rtree(demo, ignore = "*.log", prune = TRUE)
#> printtree-feature-tour/
#> |-- data/
#> |   `-- raw/
#> |       `-- sales.csv
#> |-- R/
#> |   `-- helpers.R
#> |-- README.md
#> `-- test_cache
#> 
#> 3 directories, 4 files
```

## Quiet capture

Use `quiet = TRUE` with `return_lines = TRUE` when you want to work with
the tree programmatically.

``` r

lines <- print_rtree(demo, return_lines = TRUE, quiet = TRUE)
head(lines, 4)
#> [1] "printtree-feature-tour/" "|-- data/"              
#> [3] "|   `-- raw/"            "|       `-- sales.csv"
```

## Text and Markdown export

[`write_tree()`](https://prigasg.github.io/printtree/reference/write_tree.md)
writes the same tree output to a text or Markdown file. Parent
directories are created automatically by default.

``` r

tree_md <- file.path(tempdir(), "printtree-feature-tour-output", "tree.md")
write_tree(demo, tree_md, format = "md", title = "Feature Tour Tree")
readLines(tree_md, n = 8)
```

    #> [1] "# Feature Tour Tree"     ""                       
    #> [3] "```"                     "printtree-feature-tour/"
    #> [5] "|-- data/"               "|   `-- raw/"           
    #> [7] "|       `-- sales.csv"   "|-- empty/"

## Git status annotations

When the target folder is inside a Git work tree, `git = TRUE` annotates
changed paths with simple status markers and includes a legend.

``` r

repo <- file.path(tempdir(), "printtree-feature-tour-git")
if (dir.exists(repo)) unlink(repo, recursive = TRUE, force = TRUE)
dir.create(repo, recursive = TRUE)

system2("git", c("-C", repo, "init"), stdout = FALSE, stderr = FALSE)
system2("git", c("-C", repo, "config", "user.email", "test@example.com"))
system2("git", c("-C", repo, "config", "user.name", "Test User"))

file.create(file.path(repo, "tracked.txt"))
#> [1] TRUE
system2("git", c("-C", repo, "add", "tracked.txt"), stdout = FALSE, stderr = FALSE)
system2("git", c("-C", repo, "commit", "-m", "initial"), stdout = FALSE, stderr = FALSE)

writeLines("changed", file.path(repo, "tracked.txt"))
file.create(file.path(repo, "new.txt"))
#> [1] TRUE

print_rtree(repo, git = TRUE)
#> printtree-feature-tour-git/ M
#> |-- new.txt ?
#> `-- tracked.txt M
#> 
#> 0 directories, 2 files
#> Git status: ? untracked, M modified, + staged
```

``` r

print_rtree("path/to/repo", git = TRUE)
```
