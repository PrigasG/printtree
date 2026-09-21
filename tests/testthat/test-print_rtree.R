test_that("print_rtree returns lines and respects ignore", {
  td <- withr::local_tempdir()

  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))
  dir.create(file.path(td, ".git"))
  file.create(file.path(td, "demo.Rproj"))

  lines <- print_rtree(td, project = "root", return_lines = TRUE)

  expect_true(any(grepl("^.+/$", lines)))             # root line ends with /
  expect_true(any(grepl("R/$", lines)))               # directory printed
  expect_true(any(grepl("a\\.R$", lines)))            # file printed
  expect_false(any(grepl("\\.git", lines)))           # ignored by default
  expect_true(any(grepl("1 directory, 2 files", lines, fixed = TRUE)))
})

test_that("max_depth limits traversal", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "A", "B"), recursive = TRUE)
  file.create(file.path(td, "A", "B", "x.txt"))

  lines1 <- print_rtree(td, max_depth = 1, return_lines = TRUE)
  expect_true(any(grepl("A/$", lines1)))
  expect_false(any(grepl("B/$", lines1)))
})

test_that("ignore supports glob and regex patterns", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "keep.txt"))
  file.create(file.path(td, "debug.log"))
  file.create(file.path(td, "test_cache"))

  glob_lines <- print_rtree(td, ignore = "*.log", return_lines = TRUE)
  expect_true(any(grepl("keep\\.txt$", glob_lines)))
  expect_false(any(grepl("debug\\.log$", glob_lines)))

  regex_lines <- print_rtree(td, ignore = "^test_", ignore_type = "regex", return_lines = TRUE)
  expect_false(any(grepl("test_cache$", regex_lines)))
})

test_that("prune hides directories without displayable children", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "empty"))
  dir.create(file.path(td, "logs"))
  file.create(file.path(td, "logs", "debug.log"))
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))

  lines <- print_rtree(td, ignore = "*.log", prune = TRUE, return_lines = TRUE)
  expect_false(any(grepl("empty/$", lines)))
  expect_false(any(grepl("logs/$", lines)))
  expect_true(any(grepl("R/$", lines)))
})

test_that("write_tree writes text and markdown files", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "README.md"))

  txt <- tempfile(fileext = ".txt")
  md <- file.path(tempdir(), "printtree-nested-output", "tree.md")

  expect_identical(write_tree(td, txt, format = "txt"), txt)
  expect_identical(write_tree(td, md, format = "md", title = "Project Tree"), md)

  expect_true(any(grepl("README\\.md", readLines(txt))))
  md_lines <- readLines(md)
  expect_identical(md_lines[1], "# Project Tree")
  expect_true(any(md_lines == "```"))
})

test_that("print_rtree can return lines quietly", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "README.md"))

  expect_silent(lines <- print_rtree(td, return_lines = TRUE, quiet = TRUE))
  expect_true(any(grepl("README\\.md$", lines)))
})

test_that("safe_list excludes Windows hidden attribute entries", {
  skip_on_os(c("linux", "mac", "solaris"))

  td <- withr::local_tempdir()
  hidden_dir <- file.path(td, "hidden-dir")
  visible_dir <- file.path(td, "visible-dir")
  dir.create(hidden_dir)
  dir.create(visible_dir)

  system2("attrib", c("+h", hidden_dir), stdout = FALSE, stderr = FALSE)
  withr::defer(system2("attrib", c("-h", hidden_dir), stdout = FALSE, stderr = FALSE))

  lines <- print_rtree(td, return_lines = TRUE)
  expect_false(any(grepl("hidden-dir/$", lines)))
  expect_true(any(grepl("visible-dir/$", lines)))
})

# Initializes a scratch git repo with a test user identity. Returns TRUE on
# success; every git command's exit status is checked so CRAN machines with
# a broken git setup skip instead of failing.
git_test_init <- function(git, td) {
  isTRUE(tryCatch({
    system2(git, c("-C", td, "init"), stdout = FALSE, stderr = FALSE) == 0L &&
      system2(git, c("-C", td, "config", "user.email", "test@example.com"),
              stdout = FALSE, stderr = FALSE) == 0L &&
      system2(git, c("-C", td, "config", "user.name", "Test User"),
              stdout = FALSE, stderr = FALSE) == 0L
  }, warning = function(e) FALSE, error = function(e) FALSE))
}

# Stages and commits everything in the scratch repo. Returns TRUE on success.
git_test_commit <- function(git, td, message = "initial") {
  isTRUE(tryCatch({
    system2(git, c("-C", td, "add", "-A"), stdout = FALSE, stderr = FALSE) == 0L &&
      system2(git, c("-C", td, "commit", "-m", message),
              stdout = FALSE, stderr = FALSE) == 0L
  }, warning = function(e) FALSE, error = function(e) FALSE))
}

test_that("git mode annotates porcelain status", {
  skip_on_cran()
  git <- Sys.which("git")
  skip_if(!nzchar(git), "git is not installed")

  td <- withr::local_tempdir()
  skip_if_not(git_test_init(git, td), "could not initialize a git repository")

  file.create(file.path(td, "tracked.txt"))
  skip_if_not(git_test_commit(git, td), "could not commit test files")

  writeLines("changed", file.path(td, "tracked.txt"))
  file.create(file.path(td, "new.txt"))

  lines <- print_rtree(td, git = TRUE, return_lines = TRUE)
  expect_true(any(grepl("tracked\\.txt M$", lines)))
  expect_true(any(grepl("new\\.txt \\?$", lines)))
  expect_true(any(grepl("Git status:", lines, fixed = TRUE)))

  no_legend <- print_rtree(td, git = TRUE, git_legend = FALSE, return_lines = TRUE, quiet = TRUE)
  expect_false(any(grepl("Git status:", no_legend, fixed = TRUE)))
})

test_that("max_depth is validated", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "A"))

  expect_error(print_rtree(td, max_depth = "2"), "max_depth")
  expect_error(print_rtree(td, max_depth = -1), "max_depth")
  expect_error(print_rtree(td, max_depth = 1.5), "max_depth")
  expect_error(print_rtree(td, max_depth = c(1, 2)), "max_depth")
  expect_error(print_rtree(td, max_depth = NA_real_), "max_depth")
  expect_error(write_tree(td, tempfile(), max_depth = -1), "max_depth")

  expect_no_error(print_rtree(td, max_depth = 2, return_lines = TRUE, quiet = TRUE))
  expect_no_error(print_rtree(td, max_depth = NULL, return_lines = TRUE, quiet = TRUE))
})

test_that("snapshot arguments are validated before writing", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "a.txt"))

  expect_error(
    print_rtree(td, snapshot = TRUE, snapshot_path = file.path(td, "nope")),
    "snapshot_path"
  )
  expect_error(
    print_rtree(td, snapshot = TRUE, snapshot_width = 0),
    "snapshot_width"
  )
  expect_error(
    print_rtree(td, snapshot = TRUE, snapshot_width = "wide"),
    "snapshot_width"
  )
  expect_error(
    print_rtree(td, snapshot = TRUE, snapshot_width = 0.5),
    "snapshot_width"
  )
  expect_error(
    print_rtree(td, snapshot = TRUE, snapshot_width = 100000),
    "snapshot_width"
  )
})

test_that("project 'auto' is an alias of 'none'", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "sub"))

  auto <- print_rtree(td, project = "auto", return_lines = TRUE, quiet = TRUE)
  none <- print_rtree(td, project = "none", return_lines = TRUE, quiet = TRUE)
  expect_identical(auto, none)
})

test_that("git directory labels reflect nested status", {
  skip_on_cran()
  git <- Sys.which("git")
  skip_if(!nzchar(git), "git is not installed")

  td <- withr::local_tempdir()
  skip_if_not(git_test_init(git, td), "could not initialize a git repository")

  dir.create(file.path(td, "sub"))
  file.create(file.path(td, "sub", "new.txt"))

  lines <- print_rtree(td, git = TRUE, return_lines = TRUE, quiet = TRUE)
  expect_true(any(grepl("sub/ \\?$", lines)))
  expect_true(any(grepl("new\\.txt \\?$", lines)))
})

test_that("git status parsing handles quoted, unicode, and renamed paths", {
  skip_on_cran()
  git <- Sys.which("git")
  skip_if(!nzchar(git), "git is not installed")

  td <- withr::local_tempdir()
  skip_if_not(git_test_init(git, td), "could not initialize a git repository")

  file.create(file.path(td, "space name.txt"))
  file.create(file.path(td, "unicod\u00e9.txt"))
  file.create(file.path(td, "oldname.txt"))
  skip_if_not(git_test_commit(git, td), "could not commit test files")

  # Staged rename plus worktree modifications; porcelain -z must keep
  # every path literal (quoted "space name.txt", octal-escaped unicode).
  if (system2(git, c("-C", td, "mv", "oldname.txt", "newname.txt"),
              stdout = FALSE, stderr = FALSE) != 0L) {
    skip("could not rename test file")
  }
  writeLines("changed", file.path(td, "space name.txt"))
  writeLines("changed", file.path(td, "unicod\u00e9.txt"))

  lines <- print_rtree(td, git = TRUE, return_lines = TRUE, quiet = TRUE)
  expect_true(any(grepl("space name\\.txt M$", lines)))
  expect_true(any(grepl("unicod\u00e9\\.txt M$", lines)))
  expect_true(any(grepl("newname\\.txt \\+$", lines)))
})
