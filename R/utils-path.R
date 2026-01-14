#' @keywords internal
resolve_tree_path <- function(path,
                              project = c("auto", "root", "none"),
                              search_paths = c(".", ".."),
                              root_markers = c(".Rproj", "DESCRIPTION")) {
  project <- match.arg(project)

  path <- path.expand(path)
  search_paths <- path.expand(search_paths)

  # If user passed an existing .Rproj file, treat it as its containing directory
  if (file.exists(path) && grepl("\\.Rproj$", basename(path), ignore.case = TRUE)) {
    path <- dirname(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  # If not a directory, treat as "project name" and try to locate it
  if (!dir.exists(path)) {
    path <- find_project_by_name(path, search_paths = search_paths)
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (project == "none") return(path)
  if (project == "auto") return(path)

  # project == "root": walk upward for .Rproj and/or other root markers
  find_project_root_up(path, root_markers = root_markers)
}

#' @keywords internal
find_project_by_name <- function(name, search_paths = c(".", "..")) {
  name <- path.expand(name)
  search_paths <- path.expand(search_paths)
  search_paths <- search_paths[dir.exists(search_paths)]

  # If the user passed an existing directory, use it
  if (dir.exists(name)) return(name)

  # If the user passed an existing .Rproj file, use its directory
  if (file.exists(name) && grepl("\\.Rproj$", basename(name), ignore.case = TRUE)) {
    return(dirname(normalizePath(name, winslash = "/", mustWork = TRUE)))
  }

  # Normalize whether the user included ".Rproj" or not
  is_rproj_name <- grepl("\\.Rproj$", name, ignore.case = TRUE)
  rproj_file <- if (is_rproj_name) name else paste0(name, ".Rproj")

  # Search search_paths for either:
  #    - a directory named `name`
  #    - a `.Rproj` file named `name.Rproj` (or exact rproj file name supplied)
  for (base in search_paths) {
    # directory match
    dir_candidate <- file.path(base, name)
    if (dir.exists(dir_candidate)) return(dir_candidate)

    # rproj file match (in base)
    rproj_candidate <- file.path(base, rproj_file)
    if (file.exists(rproj_candidate)) {
      return(dirname(normalizePath(rproj_candidate, winslash = "/", mustWork = TRUE)))
    }

    # rproj file match inside a folder of the same name
    # e.g., ~/Projects/myproj/myproj.Rproj
    rproj_inside_dir <- file.path(base, name, basename(rproj_file))
    if (file.exists(rproj_inside_dir)) {
      return(dirname(normalizePath(rproj_inside_dir, winslash = "/", mustWork = TRUE)))
    }
  }

  stop(
    "Could not find directory or .Rproj for '", name, "' in search_paths.\n",
    "Try an explicit path or set search_paths.",
    call. = FALSE
  )
}

#' @keywords internal
has_root_marker <- function(dir, root_markers) {
  if (!length(root_markers)) return(FALSE)

  for (m in root_markers) {
    # Special marker ".Rproj" means "any file ending in .Rproj"
    if (identical(m, ".Rproj")) {
      if (length(list.files(dir, pattern = "\\.Rproj$", full.names = TRUE)) > 0) return(TRUE)
    } else {
      if (file.exists(file.path(dir, m))) return(TRUE)
    }
  }

  FALSE
}

#' @keywords internal
find_project_root_up <- function(path, root_markers = c(".Rproj", "DESCRIPTION")) {
  current <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (has_root_marker(current, root_markers)) return(current)

    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  # No marker found; return original
  path
}

#' @keywords internal
safe_list <- function(path, show_hidden = FALSE) {
  tryCatch(
    list.files(path, full.names = TRUE, all.files = show_hidden, no.. = TRUE),
    error = function(e) character(0)
  )
}
