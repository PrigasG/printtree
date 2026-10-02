## Release summary

This is an update bringing `printtree` to 0.3.0.

In this release I have:

- Added `tree_to_mermaid()` to convert a directory tree to a Mermaid flowchart, which renders natively in Quarto `{mermaid}` chunks, GitHub Markdown, and anywhere else Mermaid is supported.
- Added `tree_to_dot()` to convert a directory tree to a Graphviz DOT graph, renderable in Quarto `{dot}` chunks and any DOT-compatible tool.
- Extended `write_tree()` with `"mermaid"`, `"dot"`, and `"qmd"` formats: the first two write the raw diagram text, and `"qmd"` writes a minimal Quarto document embedding the Mermaid flowchart.
- Made the internal tree builder also return the displayed nodes as a data frame, so diagram export sees exactly the same nodes as the printed tree (including `prune` and ignore filtering).

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no known reverse dependencies.
