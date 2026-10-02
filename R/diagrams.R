#' Convert a directory tree to a Mermaid flowchart
#'
#' Builds the directory tree with the same options as `build_tree()` and
#' returns it as [Mermaid](https://mermaid.js.org/) flowchart text. The output
#' renders natively in Quarto documents (in a `{mermaid}` chunk), GitHub
#' Markdown, and anywhere else Mermaid is supported. Directories use the
#' stadium shape and files use rectangles.
#'
#' @param path Character. Directory path, project name, or `.Rproj` file.
#'   If NULL, uses the current directory.
#' @param direction Character. Flowchart direction: `"TD"` (top-down),
#'   `"LR"` (left-right), `"RL"`, or `"BT"`.
#' @param file Character or NULL. If given, the diagram text is written to this
#'   file with [writeLines()].
#' @param ... Additional arguments passed to `build_tree()` (e.g. `ignore`,
#'   `max_depth`, `show_hidden`, `prune`).
#'
#' @return Invisibly, the diagram text as a single string (or `file` when
#'   `file` is given). Use [cat()] to print it, e.g. into a Quarto
#'   `{mermaid}` chunk.
#' @export
#'
#' @examples
#' demo <- file.path(tempdir(), "printtree-mermaid-demo")
#' if (dir.exists(demo)) unlink(demo, recursive = TRUE)
#' dir.create(file.path(demo, "R"), recursive = TRUE)
#' file.create(file.path(demo, "R", "hello.R"))
#' file.create(file.path(demo, "README.md"))
#'
#' cat(tree_to_mermaid(demo))
tree_to_mermaid <- function(path = NULL,
                            direction = c("TD", "LR", "RL", "BT"),
                            file = NULL,
                            ...) {
  direction <- match.arg(direction)
  tree <- build_tree(path = path, ...)
  nodes <- tree$nodes

  ids <- paste0("n", seq_len(nrow(nodes)))
  shapes <- ifelse(
    nodes$is_dir,
    sprintf('(["%s"])', vapply(nodes$name, mermaid_escape, character(1), USE.NAMES = FALSE)),
    sprintf('["%s"]', vapply(nodes$name, mermaid_escape, character(1), USE.NAMES = FALSE))
  )
  node_defs <- sprintf("    %s%s", ids, shapes)

  parent_idx <- match(nodes$parent, nodes$path)
  keep <- !is.na(parent_idx)
  edges <- sprintf("    %s --> %s", ids[parent_idx[keep]], ids[keep])

  text <- paste(
    c(paste0("flowchart ", direction), node_defs, edges),
    collapse = "\n"
  )

  if (!is.null(file)) {
    writeLines(text, file, useBytes = TRUE)
    return(invisible(file))
  }
  invisible(text)
}

#' Convert a directory tree to a Graphviz DOT graph
#'
#' Builds the directory tree with the same options as `build_tree()` and
#' returns it as [Graphviz](https://graphviz.org/) DOT text. The output renders
#' in Quarto documents (in a `{dot}` chunk when Graphviz is installed) and
#' with any DOT-compatible tool. Directories use the `folder` shape and files
#' use the `note` shape.
#'
#' @param path Character. Directory path, project name, or `.Rproj` file.
#'   If NULL, uses the current directory.
#' @param rankdir Character. Graph direction: `"TB"` (top-bottom),
#'   `"LR"` (left-right), `"RL"`, or `"BT"`.
#' @param file Character or NULL. If given, the diagram text is written to this
#'   file with [writeLines()].
#' @param ... Additional arguments passed to `build_tree()` (e.g. `ignore`,
#'   `max_depth`, `show_hidden`, `prune`).
#'
#' @return Invisibly, the diagram text as a single string (or `file` when
#'   `file` is given). Use [cat()] to print it.
#' @export
#'
#' @examples
#' demo <- file.path(tempdir(), "printtree-dot-demo")
#' if (dir.exists(demo)) unlink(demo, recursive = TRUE)
#' dir.create(file.path(demo, "R"), recursive = TRUE)
#' file.create(file.path(demo, "R", "hello.R"))
#' file.create(file.path(demo, "README.md"))
#'
#' cat(tree_to_dot(demo))
tree_to_dot <- function(path = NULL,
                        rankdir = c("TB", "LR", "RL", "BT"),
                        file = NULL,
                        ...) {
  rankdir <- match.arg(rankdir)
  tree <- build_tree(path = path, ...)
  nodes <- tree$nodes

  ids <- paste0("n", seq_len(nrow(nodes)))
  shapes <- ifelse(nodes$is_dir, "folder", "note")
  labels <- vapply(nodes$name, dot_escape, character(1), USE.NAMES = FALSE)
  node_defs <- sprintf('  "%s" [label="%s", shape=%s];', ids, labels, shapes)

  parent_idx <- match(nodes$parent, nodes$path)
  keep <- !is.na(parent_idx)
  edges <- sprintf('  "%s" -> "%s";', ids[parent_idx[keep]], ids[keep])

  text <- paste(
    c(
      "digraph printtree {",
      sprintf("  rankdir=%s;", rankdir),
      '  node [fontname="Helvetica"];',
      node_defs,
      edges,
      "}"
    ),
    collapse = "\n"
  )

  if (!is.null(file)) {
    writeLines(text, file, useBytes = TRUE)
    return(invisible(file))
  }
  invisible(text)
}

#' Escape a node label for Mermaid flowchart text
#'
#' Double quotes become the `#quot;` entity and `#` becomes `#35;` so labels
#' cannot break out of Mermaid's quoted label syntax. Newlines are replaced
#' with spaces.
#'
#' @keywords internal
mermaid_escape <- function(x) {
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("#", "#35;", x, fixed = TRUE)
  x <- gsub('"', "#quot;", x, fixed = TRUE)
  x
}

#' Escape a node label for Graphviz DOT text
#'
#' Backslashes and double quotes are escaped for DOT's quoted strings, and
#' newlines are replaced with spaces.
#'
#' @keywords internal
dot_escape <- function(x) {
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x
}
