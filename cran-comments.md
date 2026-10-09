## Release summary

This is an update bringing `printtree` to 0.3.0.

In this release I have:

- Added `tree_to_mermaid()` to convert a directory tree to a Mermaid flowchart, which renders natively in Quarto `{mermaid}` chunks, GitHub Markdown, and anywhere else Mermaid is supported.
- Added `view_mermaid()` to generate a browser-ready HTML preview with pan/zoom controls and client-side SVG/PNG/JPEG downloads, while guarding browser opening with `interactive()`.
- Added direct PNG, JPEG, and PDF diagram export through the suggested `webshot2` package and headless Chrome. Output extensions are validated and the saved image omits viewer controls. Browser integration tests are skipped on CRAN.
- Added `tree_to_dot()` to convert a directory tree to a Graphviz DOT graph, renderable in Quarto `{dot}` chunks and any DOT-compatible tool.
- Extended `write_tree()` with `"mermaid"`, `"dot"`, `"qmd"`, `"mindmap"`, and `"html"` formats: the diagram formats write raw diagram text, `"qmd"` writes a minimal Quarto document embedding the Mermaid flowchart, and `"html"` writes a self-contained collapsible HTML tree.
- Added diagram extras: `git_colors` tints nodes by Git status, `subgraph` wraps directories in Mermaid subgraph containers, and `repo_url`/`repo_branch` make diagram nodes link to the repository.
- Added `tree_to_mindmap()` (Mermaid mindmap), `tree_to_html()` (collapsible HTML tree), `tree_diff_mermaid()`, and `tree_diff_dot()` (diff two directory trees as diagrams).
- Made the internal tree builder also return the displayed nodes as a data frame, so diagram export sees exactly the same nodes as the printed tree (including `prune` and ignore filtering).

## R CMD check results

0 errors | 0 warnings | 1 note

- New release only four days after 0.2.2: this is a feature release (diagram
  export) that was already in development when 0.2.2 shipped.

## Reverse dependencies

There are no known reverse dependencies.
