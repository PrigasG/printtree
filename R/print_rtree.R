#' Print an R Project or Directory Tree
#'
#' Prints a directory tree for a given path. Optionally, detects an RStudio project
#' (`.Rproj`) and can print from a project root.
#'
#' @param path Character. Directory path, project name, or `.Rproj` file. If NULL, uses current directory.
#' @param ignore Character vector. Basenames to exclude (e.g., ".git", "renv").
#'   With `ignore_type = "auto"`, entries containing wildcard characters are
#'   treated as glob patterns, so values such as `"*.log"` or `"test_*"` work.
#' @param ignore_type One of "auto", "fixed", "glob", or "regex". Controls how
#'   `ignore` is matched against basenames.
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
#' @param quiet Logical. If TRUE, suppress console output. Useful with `return_lines = TRUE`.
#' @param count_footer Logical. If TRUE, append a summary like "3 directories, 12 files".
#' @param git Logical. If TRUE, annotate files and directories with porcelain
#'   `git status` markers when `path` is inside a Git work tree.
#' @param git_legend Logical. If TRUE and `git = TRUE`, append a short legend
#'   explaining the Git status markers.
#' @param prune Logical. If TRUE, omit directories with no displayable children.
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
    ignore_type = c("auto", "fixed", "glob", "regex"),
    max_depth = NULL,
    show_hidden = FALSE,
    project = c("auto", "root", "none"),
    search_paths = c(".", "..", "~/Documents", "~/Projects"),
    root_markers = c(".Rproj", "DESCRIPTION"),
    format = c("ascii", "unicode"),
    return_lines = FALSE,
    quiet = FALSE,
    count_footer = TRUE,
    git = FALSE,
    git_legend = TRUE,
    prune = FALSE,
    snapshot = FALSE,
    snapshot_file = "tree.png",
    snapshot_width = 800,
    snapshot_bg = c("white", "black"),
    snapshot_path = "."
) {
  snapshot_bg <- match.arg(snapshot_bg)

  tree <- build_tree(
    path = path,
    ignore = ignore,
    ignore_type = ignore_type,
    max_depth = max_depth,
    show_hidden = show_hidden,
    project = project,
    search_paths = search_paths,
    root_markers = root_markers,
    format = format,
    count_footer = count_footer,
    git = git,
    git_legend = git_legend,
    prune = prune
  )

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
      lines = tree$lines,
      file = out_file,
      width = snapshot_width,
      bg = snapshot_bg
    )
  }


  if (!isTRUE(quiet)) {
    cat(paste(tree$lines, collapse = "\n"), "\n")
  }

  if (isTRUE(return_lines)) return(invisible(tree$lines))
  invisible(NULL)
}

#' Write a Directory Tree to a Text or Markdown File
#'
#' Builds a directory tree with the same options as [print_rtree()] and writes it
#' to a plain text or Markdown file.
#'
#' @param path Character. Directory path, project name, or `.Rproj` file. If NULL, uses current directory.
#' @param file Character. Output file path.
#' @param format One of "txt" or "md".
#' @param title Optional Markdown heading used when `format = "md"`.
#' @param create_dirs Logical. If TRUE, create the output file's parent
#'   directory when it does not exist.
#' @param ... Additional arguments passed to [print_rtree()], such as `ignore`,
#'   `max_depth`, `git`, or `prune`.
#'
#' @return Invisibly returns the output file path.
#' @export
#'
#' @examples
#' demo <- file.path(tempdir(), "printtree-write-demo")
#' if (dir.exists(demo)) unlink(demo, recursive = TRUE)
#' dir.create(demo, recursive = TRUE)
#' file.create(file.path(demo, "README.md"))
#'
#' out <- tempfile(fileext = ".md")
#' write_tree(demo, out, format = "md")
write_tree <- function(path = NULL,
                       file,
                       format = c("txt", "md"),
                       title = NULL,
                       create_dirs = TRUE,
                       ...) {
  format <- match.arg(format)
  tree <- build_tree(path = path, ...)

  dir <- dirname(file)
  if (!dir.exists(dir)) {
    if (isTRUE(create_dirs)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (!dir.exists(dir)) {
      stop("Output directory does not exist: ", dir, call. = FALSE)
    }
  }

  output <- if (format == "md") {
    heading <- if (is.null(title)) character(0) else c(paste0("# ", title), "")
    c(heading, "```", tree$lines, "```")
  } else {
    tree$lines
  }

  writeLines(output, file, useBytes = TRUE)
  invisible(file)
}

#' @keywords internal
build_tree <- function(path = NULL,
                       ignore = c("renv", ".git", ".Rproj.user", "__pycache__", ".DS_Store", "node_modules", ".Rhistory"),
                       ignore_type = c("auto", "fixed", "glob", "regex"),
                       max_depth = NULL,
                       show_hidden = FALSE,
                       project = c("auto", "root", "none"),
                       search_paths = c(".", "..", "~/Documents", "~/Projects"),
                       root_markers = c(".Rproj", "DESCRIPTION"),
                       format = c("ascii", "unicode"),
                       count_footer = TRUE,
                       git = FALSE,
                       git_legend = TRUE,
                       prune = FALSE) {
  project <- match.arg(project)
  format <- match.arg(format)
  ignore_type <- match.arg(ignore_type)

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
  git_status <- if (isTRUE(git)) git_status_map(root) else character(0)
  counts <- new.env(parent = emptyenv())
  counts$dirs <- 0L
  counts$files <- 0L

  lines <- c(
    paste0(basename(root), "/", git_label(root, root, git_status)),
    rtree_walk(
      path = root,
      root = root,
      prefix = "",
      ignore = ignore,
      ignore_type = ignore_type,
      max_depth = max_depth,
      show_hidden = show_hidden,
      depth = 0,
      visited = root,
      glyph = glyph,
      git_status = git_status,
      prune = prune,
      counts = counts
    )
  )

  footer <- character(0)
  if (isTRUE(count_footer)) {
    footer <- c(footer, tree_count_footer(counts$dirs, counts$files))
  }
  if (isTRUE(git) && isTRUE(git_legend) && length(git_status)) {
    footer <- c(footer, git_status_legend())
  }
  if (length(footer)) {
    lines <- c(lines, "", footer)
  }

  list(root = root, lines = lines, directories = counts$dirs, files = counts$files)
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
rtree_walk <- function(path, root, prefix, ignore, ignore_type, max_depth, show_hidden,
                       depth, visited, glyph, git_status, prune, counts) {
  # Depth limit: depth counts directories below the root
  if (!is.null(max_depth) && depth >= max_depth) return(character(0))

  items <- safe_list(path, show_hidden = show_hidden)
  if (!length(items)) return(character(0))

  bn <- basename(items)

  keep <- !ignored_basenames(bn, ignore, ignore_type)

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
    name <- paste0(basename(item), git_label(item, root, git_status))
    suffix <- if (is_dir[i]) "/" else ""

    if (is_dir[i]) {
      next_path <- normalizePath(item, winslash = "/", mustWork = FALSE)

      if (next_path %in% visited) next

      new_prefix <- paste0(prefix, if (last) glyph$blank else glyph$pipe)

      child <- rtree_walk(
        path = item,
        root = root,
        prefix = new_prefix,
        ignore = ignore,
        ignore_type = ignore_type,
        max_depth = max_depth,
        show_hidden = show_hidden,
        depth = depth + 1,
        visited = c(visited, next_path),
        glyph = glyph,
        git_status = git_status,
        prune = prune,
        counts = counts
      )

      if (isTRUE(prune) && !length(child)) next

      counts$dirs <- counts$dirs + 1L
      out <- c(out, paste0(prefix, connector, name, suffix), child)
    } else {
      counts$files <- counts$files + 1L
      out <- c(out, paste0(prefix, connector, name, suffix))
    }
  }

  out
}

#' @keywords internal
ignored_basenames <- function(bn, ignore, ignore_type = c("auto", "fixed", "glob", "regex")) {
  ignore_type <- match.arg(ignore_type)
  if (!length(ignore)) return(rep(FALSE, length(bn)))

  patterns <- switch(
    ignore_type,
    fixed = paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", ignore), "$"),
    glob = utils::glob2rx(ignore),
    regex = ignore,
    auto = vapply(ignore, function(x) {
      if (grepl("[*?[]", x)) utils::glob2rx(x) else paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x), "$")
    }, character(1))
  )

  vapply(
    bn,
    function(x) any(vapply(patterns, grepl, logical(1), x = x)),
    logical(1)
  )
}

#' @keywords internal
git_status_map <- function(root) {
  git_root <- tryCatch(
    system2("git", c("-C", root, "rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE),
    warning = function(e) character(0),
    error = function(e) character(0)
  )

  if (!length(git_root) || !nzchar(git_root[[1]])) return(character(0))

  status <- tryCatch(
    system2("git", c("-C", git_root[[1]], "status", "--porcelain", "-uall"), stdout = TRUE, stderr = FALSE),
    warning = function(e) character(0),
    error = function(e) character(0)
  )

  if (!length(status)) return(character(0))

  paths <- substring(status, 4)
  paths <- sub("^.* -> ", "", paths)
  codes <- substring(status, 1, 2)
  labels <- vapply(codes, git_status_label, character(1))
  full_paths <- normalizePath(file.path(git_root[[1]], paths), winslash = "/", mustWork = FALSE)
  stats::setNames(labels, full_paths)
}

#' @keywords internal
git_status_label <- function(code) {
  index <- substr(code, 1, 1)
  worktree <- substr(code, 2, 2)

  if (code == "??") return(" ?")
  if (index != " " && worktree == " ") return(" +")
  if (worktree != " ") return(" M")
  if (index != " ") return(" +")
  ""
}

#' @keywords internal
git_label <- function(path, root, git_status) {
  if (!length(git_status)) return("")

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  direct <- unname(git_status[path])
  if (!is.na(direct)) return(direct)

  prefix <- paste0(path, "/")
  nested <- git_status[startsWith(names(git_status), prefix)]
  if (length(nested)) return(" M")

  ""
}

#' @keywords internal
tree_count_footer <- function(dirs, files) {
  sprintf(
    "%s %s, %s %s",
    dirs,
    if (dirs == 1L) "directory" else "directories",
    files,
    if (files == 1L) "file" else "files"
  )
}

#' @keywords internal
git_status_legend <- function() {
  "Git status: ? untracked, M modified, + staged"
}
