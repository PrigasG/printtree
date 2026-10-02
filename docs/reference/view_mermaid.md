# Preview a Directory Tree as a Mermaid Diagram

Writes an HTML page that renders
[`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md)
output as an SVG diagram in a web browser. The page loads Mermaid from
its public CDN, so rendering requires an internet connection. Browser
opening is always suppressed in non-interactive sessions, including
package checks and automated jobs.

## Usage

``` r
view_mermaid(
  path = NULL,
  file = tempfile("printtree-mermaid-", fileext = ".html"),
  title = "Directory tree",
  theme = c("default", "neutral", "dark", "forest", "base"),
  open = interactive(),
  pan_zoom = TRUE,
  save = NULL,
  ...
)
```

## Arguments

- path:

  Character. Directory path, project name, or `.Rproj` file. If NULL,
  uses the current directory.

- file:

  Character. HTML output path. By default, a temporary file is used.

- title:

  Character. Page title and heading.

- theme:

  Mermaid theme: `"default"`, `"neutral"`, `"dark"`, `"forest"`, or
  `"base"`.

- open:

  Logical. Open the generated page when running interactively. Browser
  opening is skipped when
  [`interactive()`](https://rdrr.io/r/base/interactive.html) is false,
  even if this is explicitly set to TRUE.

- pan_zoom:

  Logical. Enable pan/zoom controls for navigating large diagrams (via
  the svg-pan-zoom library).

- save:

  Character or NULL. If given, render the diagram to an image file
  instead of the HTML page: one of `"png"`, `"jpeg"` (or `"jpg"`), or
  `"pdf"`. Requires the webshot2 package and a Chrome/Chromium browser.
  When `save` is given, `file` is treated as the image output path; if
  `file` was not explicitly supplied, the path is derived from `title`
  and the `save` extension.

- ...:

  Additional arguments passed to
  [`tree_to_mermaid()`](https://prigasg.github.io/printtree/reference/tree_to_mermaid.md),
  such as `direction`, `ignore`, `max_depth`, `git_colors`, or
  `subgraph`.

## Value

Invisibly, the generated HTML file path (or the image path when `save`
is given).

## Examples

``` r
demo <- file.path(tempdir(), "printtree-view-demo")
if (dir.exists(demo)) unlink(demo, recursive = TRUE)
dir.create(file.path(demo, "R"), recursive = TRUE)
file.create(file.path(demo, "R", "hello.R"))
#> [1] TRUE
file.create(file.path(demo, "README.md"))
#> [1] TRUE

preview <- view_mermaid(demo, open = FALSE)
file.exists(preview)
#> [1] TRUE
```
