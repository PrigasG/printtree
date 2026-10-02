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
#' @param git_colors Logical. If TRUE, nodes are colored by their Git status
#'   (amber for modified, green for untracked, blue for staged). Implies
#'   `git = TRUE` unless `git` is set explicitly.
#' @param subgraph Logical. If TRUE, each directory is wrapped in a Mermaid
#'   `subgraph` block so directories render as visual containers.
#' @param repo_url Character or NULL. Base URL of the Git repository, e.g.
#'   `"https://github.com/user/repo"`. If given, every node becomes clickable
#'   and links to the file (`blob`) or directory (`tree`) on `repo_branch`.
#' @param repo_branch Character. Branch used to build the `repo_url` links.
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
#'
#' # Directories as visual containers, laid out left to right
#' cat(tree_to_mermaid(demo, subgraph = TRUE, direction = "LR"))
#'
#' # Nodes that link into the repository on GitHub
#' cat(tree_to_mermaid(demo,
#'                     repo_url = "https://github.com/PrigasG/printtree"))
tree_to_mermaid <- function(path = NULL,
                            direction = c("TD", "LR", "RL", "BT"),
                            file = NULL,
                            git_colors = FALSE,
                            subgraph = FALSE,
                            repo_url = NULL,
                            repo_branch = "main",
                            ...) {
  direction <- match.arg(direction)
  tree <- diagram_build_tree(path, want_git = git_colors, ...)
  nodes <- tree$nodes

  ids <- paste0("n", seq_len(nrow(nodes)))
  labels <- vapply(nodes$name, mermaid_escape, character(1), USE.NAMES = FALSE)

  node_defs <- if (isTRUE(subgraph)) {
    mermaid_subgraph_block(tree, ids, labels)
  } else {
    shapes <- ifelse(
      nodes$is_dir,
      sprintf('(["%s"])', labels),
      sprintf('["%s"]', labels)
    )
    sprintf("    %s%s", ids, shapes)
  }

  parent_idx <- match(nodes$parent, nodes$path)
  keep <- !is.na(parent_idx)
  edges <- sprintf("    %s --> %s", ids[parent_idx[keep]], ids[keep])

  extras <- mermaid_extras(tree, ids, git_colors, repo_url, repo_branch)

  text <- paste(
    c(paste0("flowchart ", direction), node_defs, extras, edges),
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
#' @param git_colors Logical. If TRUE, nodes are filled by their Git status
#'   (amber for modified, green for untracked, blue for staged). Implies
#'   `git = TRUE` unless `git` is set explicitly.
#' @param repo_url Character or NULL. Base URL of the Git repository, e.g.
#'   `"https://github.com/user/repo"`. If given, every node gets a `URL`
#'   attribute linking to the file (`blob`) or directory (`tree`) on
#'   `repo_branch`, so SVG output is clickable.
#' @param repo_branch Character. Branch used to build the `repo_url` links.
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
#'
#' # Left-to-right layout with clickable nodes
#' cat(tree_to_dot(demo, rankdir = "LR",
#'                 repo_url = "https://github.com/PrigasG/printtree"))
tree_to_dot <- function(path = NULL,
                        rankdir = c("TB", "LR", "RL", "BT"),
                        file = NULL,
                        git_colors = FALSE,
                        repo_url = NULL,
                        repo_branch = "main",
                        ...) {
  rankdir <- match.arg(rankdir)
  tree <- diagram_build_tree(path, want_git = git_colors, ...)
  nodes <- tree$nodes

  ids <- paste0("n", seq_len(nrow(nodes)))
  shapes <- ifelse(nodes$is_dir, "folder", "note")
  labels <- vapply(nodes$name, dot_escape, character(1), USE.NAMES = FALSE)
  attrs <- sprintf('[label="%s", shape=%s]', labels, shapes)

  if (isTRUE(git_colors)) {
    fills <- dot_git_fills(tree)
    hit <- !is.na(fills)
    attrs[hit] <- sprintf(
      '[label="%s", shape=%s, style=filled, fillcolor="%s"]',
      labels[hit], shapes[hit], fills[hit]
    )
  }

  if (!is.null(repo_url)) {
    urls <- diagram_node_urls(tree, repo_url, repo_branch)
    hit <- !is.na(urls)
    attrs[hit] <- sprintf('%s, URL="%s"]',
      substr(attrs[hit], 1L, nchar(attrs[hit]) - 1L),
      vapply(urls[hit], dot_escape, character(1), USE.NAMES = FALSE)
    )
  }

  node_defs <- sprintf('  "%s" %s;', ids, attrs)

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

#' Convert a directory tree to a Mermaid mindmap
#'
#' Builds the directory tree with the same options as `build_tree()` and
#' returns it as [Mermaid](https://mermaid.js.org/) mindmap text: a radial
#' alternative to the flowchart produced by [tree_to_mermaid()]. The root uses
#' the cloud shape and every level is indented beneath its parent.
#'
#' @param path Character. Directory path, project name, or `.Rproj` file.
#'   If NULL, uses the current directory.
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
#' demo <- file.path(tempdir(), "printtree-mindmap-demo")
#' if (dir.exists(demo)) unlink(demo, recursive = TRUE)
#' dir.create(file.path(demo, "R"), recursive = TRUE)
#' file.create(file.path(demo, "R", "hello.R"))
#'
#' cat(tree_to_mindmap(demo))
#'
#' # Save a mindmap file, ready for a Quarto {mermaid} chunk
#' tree_to_mindmap(demo, file = tempfile(fileext = ".mmd"))
tree_to_mindmap <- function(path = NULL, file = NULL, ...) {
  tree <- build_tree(path = path, ...)
  nodes <- tree$nodes

  labels <- vapply(nodes$name, mindmap_escape, character(1), USE.NAMES = FALSE)
  lines <- c("mindmap")
  for (row in mindmap_order(tree)) {
    indent <- strrep("  ", nodes$depth[row] + 1L)
    if (is.na(nodes$parent[row])) {
      lines <- c(lines, sprintf("%sroot((%s))", indent, labels[row]))
    } else {
      lines <- c(lines, paste0(indent, labels[row]))
    }
  }
  text <- paste(lines, collapse = "\n")

  if (!is.null(file)) {
    writeLines(text, file, useBytes = TRUE)
    return(invisible(file))
  }
  invisible(text)
}

#' Convert a directory tree to a collapsible HTML page
#'
#' Builds the directory tree with the same options as `build_tree()` and
#' returns it as a self-contained HTML document: directories expand and
#' collapse on click, with no external dependencies. When the tree is built
#' with `git = TRUE`, Git status badges are shown next to changed entries.
#'
#' @param path Character. Directory path, project name, or `.Rproj` file.
#'   If NULL, uses the current directory.
#' @param title Character or NULL. Page title and heading. Defaults to
#'   `"Directory tree"`.
#' @param file Character or NULL. If given, the HTML is written to this file
#'   with [writeLines()].
#' @param git_colors Logical. If TRUE, nodes are colored by their Git status
#'   (badges for modified, untracked, and staged files). Implies `git = TRUE`.
#' @param ... Additional arguments passed to `build_tree()` (e.g. `ignore`,
#'   `max_depth`, `show_hidden`, `prune`, `git`).
#'
#' @return Invisibly, the HTML document as a single string (or `file` when
#'   `file` is given).
#' @export
#'
#' @examples
#' demo <- file.path(tempdir(), "printtree-html-demo")
#' if (dir.exists(demo)) unlink(demo, recursive = TRUE)
#' dir.create(file.path(demo, "R"), recursive = TRUE)
#' file.create(file.path(demo, "R", "hello.R"))
#'
#' html <- tree_to_html(demo, title = "Demo project")
#' out <- tempfile(fileext = ".html")
#' tree_to_html(demo, file = out)
#'
#' # Inside a Git checkout, git_colors = TRUE adds status badges
#' # next to modified, untracked, and staged files
tree_to_html <- function(path = NULL, title = NULL, file = NULL, git_colors = FALSE, ...) {
  check_title(title)
  tree <- diagram_build_tree(path, want_git = git_colors, ...)
  if (is.null(title)) title <- "Directory tree"

  doc <- paste(
    c(
      "<!DOCTYPE html>",
      '<html lang="en">',
      "<head>",
      '<meta charset="utf-8">',
      '<meta name="viewport" content="width=device-width, initial-scale=1">',
      sprintf("<title>%s</title>", html_escape(title)),
      "<style>",
      html_tree_css(),
      "</style>",
      "</head>",
      "<body>",
      sprintf("<h1>%s</h1>", html_escape(title)),
      sprintf('<p class="pt-counts">%s</p>',
              html_escape(tree_count_footer(tree$directories, tree$files))),
      html_tree_list(tree),
      "<script>",
      html_tree_js(),
      "</script>",
      "</body>",
      "</html>"
    ),
    collapse = "\n"
  )

  if (!is.null(file)) {
    writeLines(doc, file, useBytes = TRUE)
    return(invisible(file))
  }
  invisible(doc)
}

#' Diff two directory trees as a Mermaid flowchart
#'
#' Builds both trees with the same options and renders the difference as a
#' Mermaid flowchart: nodes only in `after` are green, nodes only in `before`
#' are red and dashed, and unchanged nodes are neutral. Nodes are matched by
#' their path relative to each tree root.
#'
#' @param before Character. Directory path for the "before" tree.
#' @param after Character. Directory path for the "after" tree.
#' @param direction Character. Flowchart direction: `"TD"` (top-down),
#'   `"LR"` (left-right), `"RL"`, or `"BT"`.
#' @param file Character or NULL. If given, the diagram text is written to this
#'   file with [writeLines()].
#' @param ... Additional arguments passed to `build_tree()` for both trees
#'   (e.g. `ignore`, `max_depth`, `show_hidden`, `prune`).
#'
#' @return Invisibly, the diagram text as a single string (or `file` when
#'   `file` is given).
#' @export
#'
#' @examples
#' old <- file.path(tempdir(), "printtree-diff-old")
#' new <- file.path(tempdir(), "printtree-diff-new")
#' for (d in c(old, new)) {
#'   if (dir.exists(d)) unlink(d, recursive = TRUE)
#'   dir.create(d, recursive = TRUE)
#' }
#' file.create(file.path(old, "gone.txt"))
#' file.create(file.path(new, "added.txt"))
#' file.create(file.path(old, "kept.txt"))
#' file.create(file.path(new, "kept.txt"))
#'
#' cat(tree_diff_mermaid(old, new))
#'
#' # Left-to-right layout
#' cat(tree_diff_mermaid(old, new, direction = "LR"))
tree_diff_mermaid <- function(before,
                              after,
                              direction = c("TD", "LR", "RL", "BT"),
                              file = NULL,
                              ...) {
  direction <- match.arg(direction)
  diff <- diff_node_table(before, after, ...)

  ids <- paste0("n", seq_len(nrow(diff)))
  labels <- vapply(diff$name, mermaid_escape, character(1), USE.NAMES = FALSE)
  shapes <- ifelse(
    diff$is_dir,
    sprintf('(["%s"])', labels),
    sprintf('["%s"]', labels)
  )
  node_defs <- sprintf("    %s%s", ids, shapes)

  parent_idx <- match(diff$parent_rel, diff$rel)
  keep <- !is.na(parent_idx)
  edges <- sprintf("    %s --> %s", ids[parent_idx[keep]], ids[keep])

  extras <- character(0)
  if (any(diff$status == "added")) {
    extras <- c(extras, "    classDef ptAdd fill:#dcfce7,stroke:#15803d;")
  }
  if (any(diff$status == "removed")) {
    extras <- c(extras,
      "    classDef ptDel fill:#fee2e2,stroke:#b91c1c,stroke-dasharray:5 5;")
  }
  cls <- ifelse(diff$status == "added", "ptAdd",
    ifelse(diff$status == "removed", "ptDel", NA_character_))
  if (any(!is.na(cls))) {
    extras <- c(extras,
      sprintf("    class %s %s;", ids[!is.na(cls)], cls[!is.na(cls)]))
  }

  text <- paste(
    c(paste0("flowchart ", direction), node_defs, extras, edges),
    collapse = "\n"
  )

  if (!is.null(file)) {
    writeLines(text, file, useBytes = TRUE)
    return(invisible(file))
  }
  invisible(text)
}

#' Diff two directory trees as a Graphviz DOT graph
#'
#' Builds both trees with the same options and renders the difference as a
#' Graphviz DOT graph: nodes only in `after` are filled green, nodes only in
#' `before` are filled red and dashed, and unchanged nodes are neutral. Nodes
#' are matched by their path relative to each tree root.
#'
#' @param before Character. Directory path for the "before" tree.
#' @param after Character. Directory path for the "after" tree.
#' @param rankdir Character. Graph direction: `"TB"` (top-bottom),
#'   `"LR"` (left-right), `"RL"`, or `"BT"`.
#' @param file Character or NULL. If given, the diagram text is written to this
#'   file with [writeLines()].
#' @param ... Additional arguments passed to `build_tree()` for both trees
#'   (e.g. `ignore`, `max_depth`, `show_hidden`, `prune`).
#'
#' @return Invisibly, the diagram text as a single string (or `file` when
#'   `file` is given).
#' @export
#'
#' @examples
#' old <- file.path(tempdir(), "printtree-diff-old")
#' new <- file.path(tempdir(), "printtree-diff-new")
#' for (d in c(old, new)) {
#'   if (dir.exists(d)) unlink(d, recursive = TRUE)
#'   dir.create(d, recursive = TRUE)
#' }
#' file.create(file.path(old, "gone.txt"))
#' file.create(file.path(new, "added.txt"))
#'
#' cat(tree_diff_dot(old, new))
#'
#' # Left-to-right layout
#' cat(tree_diff_dot(old, new, rankdir = "LR"))
tree_diff_dot <- function(before,
                          after,
                          rankdir = c("TB", "LR", "RL", "BT"),
                          file = NULL,
                          ...) {
  rankdir <- match.arg(rankdir)
  diff <- diff_node_table(before, after, ...)

  ids <- paste0("n", seq_len(nrow(diff)))
  shapes <- ifelse(diff$is_dir, "folder", "note")
  labels <- vapply(diff$name, dot_escape, character(1), USE.NAMES = FALSE)
  attrs <- sprintf('[label="%s", shape=%s]', labels, shapes)

  fills <- ifelse(diff$status == "added", "#dcfce7",
    ifelse(diff$status == "removed", "#fee2e2", NA_character_))
  hit <- !is.na(fills)
  style <- ifelse(diff$status == "removed", "filled,dashed", "filled")
  attrs[hit] <- sprintf(
    '[label="%s", shape=%s, style="%s", fillcolor="%s"]',
    labels[hit], shapes[hit], style[hit], fills[hit]
  )

  node_defs <- sprintf('  "%s" %s;', ids, attrs)

  parent_idx <- match(diff$parent_rel, diff$rel)
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

#' Build a tree for diagram export, optionally forcing Git status
#'
#' Thin wrapper around [build_tree()] that turns `git = TRUE` on when a
#' diagram feature needs Git status (e.g. `git_colors = TRUE`) unless the
#' caller set `git` explicitly.
#'
#' @keywords internal
diagram_build_tree <- function(path, want_git = FALSE, ...) {
  dots <- list(...)
  if (isTRUE(want_git) && is.null(dots$git)) dots$git <- TRUE
  do.call(build_tree, c(list(path = path), dots))
}

#' Git status class for each Mermaid node
#'
#' Maps `git_label()` output to Mermaid class names: `" M"` becomes `ptMod`
#' (amber), `" ?"` becomes `ptNew` (green), and `" +"` becomes `ptAdd`
#' (blue). Nodes without a status get `NA`.
#'
#' @keywords internal
mermaid_git_classes <- function(tree) {
  nodes <- tree$nodes
  if (!length(tree$git_status)) return(rep(NA_character_, nrow(nodes)))
  labs <- vapply(nodes$path,
    function(p) git_label(p, tree$root, tree$git_status), character(1))
  ifelse(labs == " M", "ptMod",
    ifelse(labs == " ?", "ptNew",
      ifelse(labs == " +", "ptAdd", NA_character_)))
}

#' Git status fill color for each DOT node
#'
#' Same mapping as [mermaid_git_classes()] but returning hex fill colors for
#' Graphviz, or `NA` for nodes without a status.
#'
#' @keywords internal
dot_git_fills <- function(tree) {
  nodes <- tree$nodes
  if (!length(tree$git_status)) return(rep(NA_character_, nrow(nodes)))
  labs <- vapply(nodes$path,
    function(p) git_label(p, tree$root, tree$git_status), character(1))
  ifelse(labs == " M", "#fef3c7",
    ifelse(labs == " ?", "#dcfce7",
      ifelse(labs == " +", "#dbeafe", NA_character_)))
}

#' Extra Mermaid lines: class definitions, classes, and click targets
#'
#' Emits `classDef`/`class` lines for `git_colors = TRUE` and `click` lines
#' for `repo_url`, to be placed after the node definitions.
#'
#' @keywords internal
mermaid_extras <- function(tree, ids, git_colors, repo_url, repo_branch) {
  out <- character(0)
  if (isTRUE(git_colors)) {
    classes <- mermaid_git_classes(tree)
    if (any(!is.na(classes))) {
      out <- c(out,
        "    classDef ptMod fill:#fef3c7,stroke:#b45309;",
        "    classDef ptNew fill:#dcfce7,stroke:#15803d;",
        "    classDef ptAdd fill:#dbeafe,stroke:#1d4ed8;",
        sprintf("    class %s %s;", ids[!is.na(classes)], classes[!is.na(classes)]))
    }
  }
  if (!is.null(repo_url)) {
    urls <- diagram_node_urls(tree, repo_url, repo_branch)
    hit <- !is.na(urls)
    if (any(hit)) {
      out <- c(out, sprintf('    click %s "%s"', ids[hit], urls[hit]))
    }
  }
  out
}

#' Percent-encode URL path segments (RFC 3986)
#'
#' Splits each path on `/`, encodes every segment as UTF-8, and rejoins them.
#' Unreserved characters (`A-Z a-z 0-9 - _ . ~`) pass through; everything else
#' -- spaces, `#`, `%`, `?`, non-ASCII names, etc. -- becomes `%HH`.
#'
#' @keywords internal
url_encode_path <- function(paths) {
  vapply(paths, function(p) {
    segs <- strsplit(p, "/", fixed = TRUE)[[1L]]
    enc <- vapply(segs, function(s) {
      raw <- charToRaw(enc2utf8(s))
      is_unreserved <- as.integer(raw) %in% c(45L, 46L, 48:57, 65:90, 95L, 97:122, 126L)
      chars <- rawToChar(raw, multiple = TRUE)
      chars[!is_unreserved] <- sprintf("%%%02X", as.integer(raw[!is_unreserved]))
      paste(chars, collapse = "")
    }, character(1))
    paste(enc, collapse = "/")
  }, character(1), USE.NAMES = FALSE)
}

#' Repository URLs for every diagram node
#'
#' Builds `blob` (files) and `tree` (directories) URLs from a repository base
#' URL and branch, using each node's path relative to the tree root. The root
#' itself links to the branch's tree root.
#'
#' @keywords internal
diagram_node_urls <- function(tree, repo_url, repo_branch) {
  nodes <- tree$nodes
  base <- sub("/+$", "", repo_url)
  rel <- substring(nodes$path, nchar(tree$root) + 2L)
  enc <- url_encode_path(rel)
  # Branches are refs too: encode them the same way (slashes separate
  # hierarchical branch names like feature/foo).
  branch <- url_encode_path(repo_branch)
  kind <- ifelse(nodes$is_dir, "tree", "blob")
  urls <- sprintf("%s/%s/%s/%s", base, kind, branch, enc)
  urls[nodes$path == tree$root] <- sprintf("%s/tree/%s", base, branch)
  urls
}

#' Emit one directory (and its children) as a Mermaid subgraph block
#'
#' Recursive helper for `subgraph = TRUE`: every directory becomes
#' `subgraph <id>["<label>"] ... end`, nested to mirror the tree. Files are
#' plain node definitions inside their parent's block.
#'
#' @keywords internal
mermaid_subgraph_block <- function(tree, ids, labels) {
  nodes <- tree$nodes
  is_dir_of <- stats::setNames(nodes$is_dir, ids)
  label_of <- stats::setNames(labels, ids)

  child_ids <- function(did) {
    dpath <- nodes$path[match(did, ids)]
    ids[!is.na(nodes$parent) & nodes$parent == dpath]
  }

  emit <- function(did, indent) {
    pad <- strrep(" ", indent)
    kid_pad <- strrep(" ", indent + 4L)
    lines <- sprintf('%ssubgraph %s["%s"]', pad, did, label_of[[did]])
    for (cid in child_ids(did)) {
      if (isTRUE(is_dir_of[[cid]])) {
        lines <- c(lines, emit(cid, indent + 4L))
      } else {
        lines <- c(lines, sprintf('%s%s["%s"]', kid_pad, cid, label_of[[cid]]))
      }
    }
    c(lines, sprintf("%send", pad))
  }

  emit(ids[is.na(nodes$parent)][[1L]], 4L)
}

#' Depth-first row order for hierarchical diagram layouts
#'
#' The node table records directories after their children (post-order), which
#' suits flat flowcharts but not nested layouts. This returns row indices in
#' pre-order so each parent directly precedes its children.
#'
#' @keywords internal
mindmap_order <- function(tree) {
  nodes <- tree$nodes
  order <- integer(0)
  visit <- function(row) {
    order <<- c(order, row)
    kids <- which(!is.na(nodes$parent) & nodes$parent == nodes$path[row])
    for (kid in kids) {
      if (nodes$is_dir[kid]) visit(kid) else order <<- c(order, kid)
    }
  }
  visit(which(is.na(nodes$parent))[[1L]])
  order
}

#' Compare two node tables by root-relative path
#'
#' Builds both trees with the same options and returns one combined data
#' frame with `rel` (root-relative path), `name`, `is_dir`, `parent_rel`,
#' and `status` (`"same"`, `"added"`, or `"removed"`).
#'
#' @keywords internal
diff_node_table <- function(before, after, ...) {
  bt <- build_tree(path = before, ...)
  at <- build_tree(path = after, ...)
  bn <- bt$nodes
  an <- at$nodes

  bn$rel <- rel_path(bn$path, bt$root)
  bn$parent_rel <- rel_path(bn$parent, bt$root)
  an$rel <- rel_path(an$path, at$root)
  an$parent_rel <- rel_path(an$parent, at$root)

  an$status <- ifelse(an$rel %in% bn$rel, "same", "added")
  gone <- bn[!bn$rel %in% an$rel, , drop = FALSE]
  gone$status <- rep("removed", nrow(gone))

  cols <- c("rel", "name", "is_dir", "parent_rel", "status")
  combo <- rbind(an[, cols, drop = FALSE], gone[, cols, drop = FALSE])
  rownames(combo) <- NULL
  combo
}

#' Root-relative path, or NA for missing parents
#'
#' The tree root itself maps to `""` so parent matching in diff tables works
#' with plain `match()`.
#'
#' @keywords internal
rel_path <- function(paths, root) {
  out <- ifelse(is.na(paths), NA_character_, substring(paths, nchar(root) + 2L))
  out[!is.na(paths) & paths == root] <- ""
  out
}

#' Build the nested HTML list for a tree
#'
#' Recursive helper for [tree_to_html()]: directories become collapsible
#' `<span class="caret">` entries wrapping a `<ul class="nested">` of their
#' children; files are plain list items. Git status badges are added when the
#' tree was built with `git = TRUE`.
#'
#' @keywords internal
html_tree_list <- function(tree) {
  nodes <- tree$nodes
  use_git <- length(tree$git_status) > 0L

  item <- function(row) {
    name <- html_escape(nodes$name[row])
    badge <- ""
    if (use_git) {
      gl <- trimws(git_label(nodes$path[row], tree$root, tree$git_status))
      if (nzchar(gl)) {
        cls <- switch(gl, "M" = "git-m", "?" = "git-q", "+" = "git-p", "git-m")
        badge <- sprintf(' <span class="git %s">%s</span>', cls, gl)
      }
    }
    if (nodes$is_dir[row]) {
      kids <- which(!is.na(nodes$parent) & nodes$parent == nodes$path[row])
      inner <- paste(vapply(kids, item, character(1)), collapse = "\n")
      sprintf(paste0('<li><span class="caret">%s%s</span>\n',
                     '<ul class="nested">\n%s\n</ul></li>'),
              name, badge, inner)
    } else {
      sprintf('<li><span class="file">%s%s</span></li>', name, badge)
    }
  }

  root_row <- which(is.na(nodes$parent))[[1L]]
  sprintf("<ul class=\"pt-tree\">\n%s\n</ul>", item(root_row))
}

#' CSS for the collapsible HTML tree
#'
#' @keywords internal
html_tree_css <- function() {
  paste(
    c(
      "body{font-family:-apple-system,\"Segoe UI\",Helvetica,Arial,sans-serif;",
      "  color:#1f2328;max-width:900px;margin:2rem auto;padding:0 1rem;line-height:1.6}",
      ".pt-counts{color:#57606a}",
      ".pt-tree,.pt-tree ul{list-style:none;margin:0;padding-left:1.25rem}",
      ".pt-tree{padding-left:0}",
      ".caret{cursor:pointer;user-select:none;font-weight:600}",
      ".caret::before{content:\"\\25B6\";display:inline-block;margin-right:.4rem;",
      "  font-size:.75em;color:#57606a}",
      ".caret-down::before{content:\"\\25BC\"}",
      ".nested{display:none}",
      ".nested.active{display:block}",
      ".git{font-size:.72em;font-weight:700;border-radius:.4rem;",
      "  padding:.05rem .45rem;margin-left:.45rem;vertical-align:.1em}",
      ".git-m{background:#fef3c7;color:#92400e}",
      ".git-q{background:#dcfce7;color:#166534}",
      ".git-p{background:#dbeafe;color:#1e40af}"
    ),
    collapse = "\n"
  )
}

#' JavaScript for the collapsible HTML tree
#'
#' Toggles the nested list beneath each clicked directory label.
#'
#' @keywords internal
html_tree_js <- function() {
  paste(
    c(
      "document.querySelectorAll('.caret').forEach(function(el){",
      "  el.addEventListener('click',function(){",
      "    var n=this.parentElement.querySelector('.nested');",
      "    if(n){n.classList.toggle('active');this.classList.toggle('caret-down');}",
      "  });",
      "});"
    ),
    collapse = "\n"
  )
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

#' Escape a node label for Mermaid mindmap text
#'
#' Square brackets and parentheses steer the mindmap parser (they select node
#' shapes), so they are replaced with visually similar fullwidth characters.
#' Newlines are replaced with spaces.
#'
#' @keywords internal
mindmap_escape <- function(x) {
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("[", "\uFF3B", x, fixed = TRUE)
  x <- gsub("]", "\uFF3D", x, fixed = TRUE)
  x <- gsub("(", "\uFF08", x, fixed = TRUE)
  x <- gsub(")", "\uFF09", x, fixed = TRUE)
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

#' Escape text for HTML output
#'
#' Escapes `&`, `<`, `>`, and `"` so file names cannot break out of the
#' generated HTML document.
#'
#' @keywords internal
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}
