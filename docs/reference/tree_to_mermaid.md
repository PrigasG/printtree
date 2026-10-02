# Convert a Directory Tree to a Mermaid Flowchart

Builds the directory tree with the same options as `build_tree()` and
returns it as [Mermaid](https://mermaid.js.org/) flowchart text. The
output renders natively in Quarto documents (in a `{mermaid}` chunk),
GitHub Markdown, and anywhere else Mermaid is supported. Directories use
the stadium shape and files use rectangles.

## Usage

``` r
tree_to_mermaid(
  path = NULL,
  direction = c("TD", "LR", "RL", "BT"),
  file = NULL,
  git_colors = FALSE,
  subgraph = FALSE,
  repo_url = NULL,
  repo_branch = "main",
  ...
)
```

## Arguments

- path:

  Character. Directory path, project name, or `.Rproj` file. If NULL,
  uses the current directory.

- direction:

  Character. Flowchart direction: `"TD"` (top-down), `"LR"`
  (left-right), `"RL"`, or `"BT"`.

- file:

  Character or NULL. If given, the diagram text is written to this file
  with [`writeLines()`](https://rdrr.io/r/base/writeLines.html).

- git_colors:

  Logical. If TRUE, nodes are colored by their Git status (amber for
  modified, green for untracked, blue for staged). Implies `git = TRUE`
  unless `git` is set explicitly.

- subgraph:

  Logical. If TRUE, each directory is wrapped in a Mermaid `subgraph`
  block so directories render as visual containers.

- repo_url:

  Character or NULL. Base URL of the Git repository, e.g.
  `"https://github.com/user/repo"`. If given, every node becomes
  clickable and links to the file (`blob`) or directory (`tree`) on
  `repo_branch`.

- repo_branch:

  Character. Branch used to build the `repo_url` links.

- ...:

  Additional arguments passed to `build_tree()` (e.g. `ignore`,
  `max_depth`, `show_hidden`, `prune`).

## Value

Invisibly, the diagram text as a single string (or `file` when `file` is
given). Use [`cat()`](https://rdrr.io/r/base/cat.html) to print it, e.g.
into a Quarto `{mermaid}` chunk.

## Examples

``` r
demo <- file.path(tempdir(), "printtree-mermaid-demo")
if (dir.exists(demo)) unlink(demo, recursive = TRUE)
dir.create(file.path(demo, "R"), recursive = TRUE)
file.create(file.path(demo, "R", "hello.R"))
#> [1] TRUE
file.create(file.path(demo, "README.md"))
#> [1] TRUE

cat(tree_to_mermaid(demo))
#> flowchart TD
#>     n1(["printtree-mermaid-demo/"])
#>     n2["hello.R"]
#>     n3(["R/"])
#>     n4["README.md"]
#>     n3 --> n2
#>     n1 --> n3
#>     n1 --> n4

# Directories as visual containers, laid out left to right
cat(tree_to_mermaid(demo, subgraph = TRUE, direction = "LR"))
#> flowchart LR
#>     subgraph n1["printtree-mermaid-demo/"]
#>         subgraph n3["R/"]
#>             n2["hello.R"]
#>         end
#>         n4["README.md"]
#>     end
#>     n3 --> n2
#>     n1 --> n3
#>     n1 --> n4

# Nodes that link into the repository on GitHub
cat(tree_to_mermaid(demo,
                    repo_url = "https://github.com/PrigasG/printtree"))
#> flowchart TD
#>     n1(["printtree-mermaid-demo/"])
#>     n2["hello.R"]
#>     n3(["R/"])
#>     n4["README.md"]
#>     click n1 "https://github.com/PrigasG/printtree/tree/main"
#>     click n2 "https://github.com/PrigasG/printtree/blob/main/R/hello.R"
#>     click n3 "https://github.com/PrigasG/printtree/tree/main/R"
#>     click n4 "https://github.com/PrigasG/printtree/blob/main/README.md"
#>     n3 --> n2
#>     n1 --> n3
#>     n1 --> n4
```
