#' Print an R Project or Directory Tree
#'
#' Prints a directory tree for a given path. Optionally, detects an RStudio project
#' (`.Rproj`) and can print from a project root.
#'
#' @param path Character. Directory path, project name, or `.Rproj` file. If NULL, uses current directory.
#' @param ignore Character vector. Basenames to exclude (e.g., ".git", "renv").
#' @param max_depth Integer. Maximum depth to traverse. NULL for unlimited.
#' @param show_hidden Logical (TRUE/FALSE). Whether to include hidden files/directories (starting with ".").
#' @param project One of "auto", "root", "none".
#'   - "auto": use `path` as-is (no upward search)
#'   - "root": walk upward from `path` to find a project root (via `root_markers`) and use it if found
#'   - "none": never attempt root detection; print the tree from `path`
#' @param search_paths Character vector. Used only when `path` is not an existing directory
#'   (treated as a project name). Paths are searched in order.
#' @param root_markers Character vector. Markers used when `project = "root"` to detect a root directory.
#'   Special value ".Rproj" means "any file ending in .Rproj". Common markers include "DESCRIPTION"
#'   (R package root) and "_quarto.yml" (Quarto project root).
#' @param format One of "ascii" or "unicode". "ascii" is portable for all terminals.
#' @param return_lines Logical. If TRUE, invisibly return the printed character vector of lines.
#' @param snapshot Logical. If TRUE, gives a visual snapshot of tree.
#' @param snapshot_file Text. Snapshot PNG name if snapshot is set as TRUE.
#' @param snapshot_width Integer. Default set at 800.
#' @param snapshot_bg Either white or black for snapshot background. If white, tree text appears black and vice.
#' @param snapshot_path Character. If snapshot_path is provided, the file is saved there.
#'
#' @return Invisible NULL, or a character vector of printed lines if `return_lines = TRUE`.
#' @export
#'
#' @examples
#' # Create a small example directory tree
#' demo <- file.path(tempdir(), "printtree-demo")
#' if (dir.exists(demo)) unlink(demo, recursive = TRUE)
#' dir.create(demo, recursive = TRUE)
#' dir.create(file.path(demo, "R"))
#' file.create(file.path(demo, "R", "hello.R"))
#' file.create(file.path(demo, "README.md"))
#'
#' # Print the tree
#' print_rtree(demo)
#'
#' # Limit depth
#' print_rtree(demo, max_depth = 1)
#'
#' # Save a PNG snapshot to a temporary file
#' png_file <- tempfile(fileext = ".png")
#' print_rtree(demo, snapshot = TRUE, snapshot_file = png_file)
print_rtree <- function(
    path = NULL,
    ignore = c("renv", ".git", ".Rproj.user", "__pycache__", ".DS_Store", "node_modules", ".Rhistory"),
    max_depth = NULL,
    show_hidden = FALSE,
    project = c("auto", "root", "none"),
    search_paths = c(".", "..", "~/Documents", "~/Projects"),
    root_markers = c(".Rproj", "DESCRIPTION"),
    format = c("ascii", "unicode"),
    return_lines = FALSE,
    snapshot = FALSE,
    snapshot_file = "tree.png",
    snapshot_width = 800,
    snapshot_bg = c("white", "black"),
    snapshot_path = "."
) {
  project <- match.arg(project)
  format  <- match.arg(format)
  snapshot_bg <- match.arg(snapshot_bg)

  if (is.null(path)) {
    path <- getwd()
  }

  root <- resolve_tree_path(
    path,
    project = project,
    search_paths = search_paths,
    root_markers = root_markers
  )

  if (!dir.exists(root)) {
    stop("Directory does not exist: ", root, call. = FALSE)
  }

  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  glyph <- tree_glyphs(format)

  # Build output lines
  lines <- character(0)
  lines <- c(lines, paste0(basename(root), "/"))

  lines <- c(lines, rtree_walk(
    path = root,
    prefix = "",
    ignore = ignore,
    max_depth = max_depth,
    show_hidden = show_hidden,
    depth = 0,
    visited = root,
    glyph = glyph
  ))


  if (isTRUE(snapshot)) {
    snapshot_path <- path.expand(snapshot_path)

    if (!dir.exists(snapshot_path)) {
      stop("snapshot_path does not exist: ", snapshot_path, call. = FALSE)
    }

    # If snapshot_file is not an absolute path, combine with snapshot_path
    out_file <- if (grepl("^(/|[A-Za-z]:)", snapshot_file)) {
      snapshot_file
    } else {
      file.path(snapshot_path, snapshot_file)
    }

    write_tree_png(
      lines = lines,
      file = out_file,
      width = snapshot_width,
      bg = snapshot_bg
    )
  }


  cat(paste(lines, collapse = "\n"), "\n")

  if (isTRUE(return_lines)) return(invisible(lines))
  invisible(NULL)
}


#' @keywords internal
tree_glyphs <- function(format = c("ascii", "unicode")) {
  format <- match.arg(format)

  if (format == "ascii") {
    list(
      mid   = "|-- ",
      last  = "`-- ",
      pipe  = "|   ",
      blank = "    "
    )
  } else {
    # Unicode escapes only
    list(
      mid   = "\u251c\u2500\u2500 ", # ├──
      last  = "\u2514\u2500\u2500 ", # └──
      pipe  = "\u2502   ",           # │
      blank = "    "
    )
  }
}

#' @keywords internal
rtree_walk <- function(path, prefix, ignore, max_depth, show_hidden, depth, visited, glyph) {
  # Depth limit: depth counts directories below the root
  if (!is.null(max_depth) && depth >= max_depth) return(character(0))

  items <- safe_list(path, show_hidden = show_hidden)
  if (!length(items)) return(character(0))

  bn <- basename(items)

  # ignore exact basenames
  keep <- !(bn %in% ignore)

  # hidden filtering (in addition to list.files all.files)
  if (!show_hidden) {
    keep <- keep & !grepl("^\\.", bn)
  }

  items <- items[keep]
  if (!length(items)) return(character(0))

  is_dir <- dir.exists(items)
  ord <- order(!is_dir, tolower(basename(items)))
  items <- items[ord]
  is_dir <- is_dir[ord]

  out <- character(0)

  for (i in seq_along(items)) {
    item <- items[i]
    last <- (i == length(items))

    connector <- if (last) glyph$last else glyph$mid
    name <- basename(item)
    suffix <- if (is_dir[i]) "/" else ""

    out <- c(out, paste0(prefix, connector, name, suffix))

    if (is_dir[i]) {
      next_path <- normalizePath(item, winslash = "/", mustWork = FALSE)


      if (next_path %in% visited) next

      new_prefix <- paste0(prefix, if (last) glyph$blank else glyph$pipe)

      out <- c(out, rtree_walk(
        path = item,
        prefix = new_prefix,
        ignore = ignore,
        max_depth = max_depth,
        show_hidden = show_hidden,
        depth = depth + 1,
        visited = c(visited, next_path),
        glyph = glyph
      ))
    }
  }

  out
}
