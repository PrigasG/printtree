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
})

test_that("max_depth limits traversal", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "A", "B"), recursive = TRUE)
  file.create(file.path(td, "A", "B", "x.txt"))

  lines1 <- print_rtree(td, max_depth = 1, return_lines = TRUE)
  expect_true(any(grepl("A/$", lines1)))
  expect_false(any(grepl("B/$", lines1)))
})
