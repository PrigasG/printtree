# Feature tour

``` r

library(printtree)
```

This article walks through the output features of `printtree`.

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

## Mermaid and Graphviz diagrams

[`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md)
and
[`tree_to_dot()`](https://prigasg.github.io/printtree/reference/tree_to_dot.md)
convert the tree to diagram text that renders in Quarto documents:
Mermaid flowcharts render natively in `{mermaid}` chunks (and on
GitHub), while DOT graphs render in `{dot}` chunks when Graphviz is
installed. Both accept the same filtering options as the printed tree
(`ignore`, `max_depth`, `show_hidden`, `prune`).

``` mermaid

flowchart TD
    n1(["printtree-feature-tour/"])
    n2(["raw/"])
    n3(["data/"])
    n4(["empty/"])
    n5["debug.log"]
    n6(["logs/"])
    n7["helpers.R"]
    n8(["R/"])
    n9["README.md"]
    n10["test_cache"]
    n3 --> n2
    n1 --> n3
    n1 --> n4
    n6 --> n5
    n1 --> n6
    n8 --> n7
    n1 --> n8
    n1 --> n9
    n1 --> n10
```

Use
[`view_mermaid()`](https://prigasg.github.io/printtree/reference/view_mermaid.md)
for the same rendered preview from an interactive R session. The raw
Mermaid source remains available from
[`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md).

``` r

view_mermaid(demo)
cat(tree_to_mermaid(demo, max_depth = 2))
```

``` r

cat(tree_to_dot(demo, max_depth = 2))
#> digraph printtree {
#>   rankdir=TB;
#>   node [fontname="Helvetica"];
#>   "n1" [label="printtree-feature-tour/", shape=folder];
#>   "n2" [label="raw/", shape=folder];
#>   "n3" [label="data/", shape=folder];
#>   "n4" [label="empty/", shape=folder];
#>   "n5" [label="debug.log", shape=note];
#>   "n6" [label="logs/", shape=folder];
#>   "n7" [label="helpers.R", shape=note];
#>   "n8" [label="R/", shape=folder];
#>   "n9" [label="README.md", shape=note];
#>   "n10" [label="test_cache", shape=note];
#>   "n3" -> "n2";
#>   "n1" -> "n3";
#>   "n1" -> "n4";
#>   "n6" -> "n5";
#>   "n1" -> "n6";
#>   "n8" -> "n7";
#>   "n1" -> "n8";
#>   "n1" -> "n9";
#>   "n1" -> "n10";
#> }
```

[`write_tree()`](https://prigasg.github.io/printtree/reference/write_tree.md)
can write either diagram format directly, or produce a minimal Quarto
document with the tree embedded as a Mermaid diagram:

``` r

tree_qmd <- file.path(tempdir(), "printtree-feature-tour-output", "tree.qmd")
write_tree(demo, tree_qmd, format = "qmd", title = "Feature Tour Tree",
           max_depth = 2)
readLines(tree_qmd, n = 12)
```

    #>  [1] "---"                                  
    #>  [2] "title: 'Feature Tour Tree'"
    #>  [3] "---"                                  
    #>  [4] ""                                     
    #>  [5] "```{mermaid}"                         
    #>  [6] "flowchart TD"                         
    #>  [7] "    n1([\"printtree-feature-tour/\"])"
    #>  [8] "    n2([\"raw/\"])"                   
    #>  [9] "    n3([\"data/\"])"                  
    #> [10] "    n4([\"empty/\"])"                 
    #> [11] "    n5[\"debug.log\"]"                
    #> [12] "    n6([\"logs/\"])"

### Diagram extras

The diagram functions have a few more tricks. `git_colors = TRUE` tints
nodes by Git status (amber = modified, green = untracked, blue =
staged), `subgraph = TRUE` wraps each directory in a Mermaid subgraph
container, and `repo_url` makes every node clickable, linking to the
file or directory on GitHub:

``` r

cat(tree_to_mermaid(demo, max_depth = 2, git_colors = TRUE, subgraph = TRUE,
                    repo_url = "https://github.com/PrigasG/printtree"))
#> flowchart TD
#>     subgraph n1["printtree-feature-tour/"]
#>         subgraph n3["data/"]
#>             subgraph n2["raw/"]
#>             end
#>         end
#>         subgraph n4["empty/"]
#>         end
#>         subgraph n6["logs/"]
#>             n5["debug.log"]
#>         end
#>         subgraph n8["R/"]
#>             n7["helpers.R"]
#>         end
#>         n9["README.md"]
#>         n10["test_cache"]
#>     end
#>     click n1 "https://github.com/PrigasG/printtree/tree/main"
#>     click n2 "https://github.com/PrigasG/printtree/tree/main/data/raw"
#>     click n3 "https://github.com/PrigasG/printtree/tree/main/data"
#>     click n4 "https://github.com/PrigasG/printtree/tree/main/empty"
#>     click n5 "https://github.com/PrigasG/printtree/blob/main/logs/debug.log"
#>     click n6 "https://github.com/PrigasG/printtree/tree/main/logs"
#>     click n7 "https://github.com/PrigasG/printtree/blob/main/R/helpers.R"
#>     click n8 "https://github.com/PrigasG/printtree/tree/main/R"
#>     click n9 "https://github.com/PrigasG/printtree/blob/main/README.md"
#>     click n10 "https://github.com/PrigasG/printtree/blob/main/test_cache"
#>     n3 --> n2
#>     n1 --> n3
#>     n1 --> n4
#>     n6 --> n5
#>     n1 --> n6
#>     n8 --> n7
#>     n1 --> n8
#>     n1 --> n9
#>     n1 --> n10
```

[`tree_to_mindmap()`](https://prigasg.github.io/printtree/reference/tree_to_mindmap.md)
renders the same tree as a radial Mermaid mindmap, and
[`tree_to_html()`](https://prigasg.github.io/printtree/reference/tree_to_html.md)
produces a self-contained HTML page with click-to-expand directories (no
external dependencies):

``` r

cat(tree_to_mindmap(demo, max_depth = 2))
#> mindmap
#>   root((printtree-feature-tour/))
#>     data/
#>       raw/
#>     empty/
#>     logs/
#>       debug.log
#>     R/
#>       helpers.R
#>     README.md
#>     test_cache
```

``` r

tree_html <- file.path(tempdir(), "printtree-feature-tour-output", "tree.html")
write_tree(demo, tree_html, format = "html", title = "Feature Tour Tree",
           max_depth = 2)
```

Finally,
[`tree_diff_mermaid()`](https://prigasg.github.io/printtree/reference/tree_diff_mermaid.md)
and
[`tree_diff_dot()`](https://prigasg.github.io/printtree/reference/tree_diff_dot.md)
compare two directory trees: nodes only in the second tree are green,
nodes only in the first are red and dashed.

``` r

cat(tree_diff_mermaid("project-v1", "project-v2"))
```

To use the output in your own Quarto document, generate the diagram once
and paste it into a `{mermaid}` chunk — or let
[`write_tree()`](https://prigasg.github.io/printtree/reference/write_tree.md)
produce the whole Quarto document for you:

``` r

# Generate once, then paste the output into a {mermaid} chunk
# in your Quarto document:
cat(tree_to_mermaid("myproject", max_depth = 2))

# Or write a ready-to-render Quarto document directly:
write_tree("myproject", "tree.qmd", format = "qmd", title = "Project tree")
```

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
system2("git", c("-C", repo, "add", "tracked.txt"), stdout = FALSE, stderr = FALSE)
system2("git", c("-C", repo, "commit", "-m", "initial"), stdout = FALSE, stderr = FALSE)

writeLines("changed", file.path(repo, "tracked.txt"))
file.create(file.path(repo, "new.txt"))

print_rtree(repo, git = TRUE)
```

``` r

print_rtree("path/to/repo", git = TRUE)
```
