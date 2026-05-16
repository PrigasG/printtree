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

test_that("git mode annotates porcelain status", {
  git <- Sys.which("git")
  skip_if(!nzchar(git), "git is not installed")

  td <- withr::local_tempdir()
  system2(git, c("-C", td, "init"), stdout = FALSE, stderr = FALSE)
  system2(git, c("-C", td, "config", "user.email", "test@example.com"))
  system2(git, c("-C", td, "config", "user.name", "Test User"))

  file.create(file.path(td, "tracked.txt"))
  system2(git, c("-C", td, "add", "tracked.txt"))
  system2(git, c("-C", td, "commit", "-m", "initial"), stdout = FALSE, stderr = FALSE)

  writeLines("changed", file.path(td, "tracked.txt"))
  file.create(file.path(td, "new.txt"))

  lines <- print_rtree(td, git = TRUE, return_lines = TRUE)
  expect_true(any(grepl("tracked\\.txt M$", lines)))
  expect_true(any(grepl("new\\.txt \\?$", lines)))
  expect_true(any(grepl("Git status:", lines, fixed = TRUE)))

  no_legend <- print_rtree(td, git = TRUE, git_legend = FALSE, return_lines = TRUE, quiet = TRUE)
  expect_false(any(grepl("Git status:", no_legend, fixed = TRUE)))
})
