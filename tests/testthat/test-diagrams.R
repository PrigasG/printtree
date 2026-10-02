test_that("tree_to_mermaid produces a valid flowchart", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))
  file.create(file.path(td, "README.md"))

  mm <- tree_to_mermaid(td)
  expect_type(mm, "character")
  expect_length(mm, 1)

  lines <- strsplit(mm, "\n", fixed = TRUE)[[1L]]
  expect_true(grepl("^flowchart TD$", lines[1]))

  # root + 1 dir + 2 files = 4 node definitions
  expect_equal(sum(grepl("^    n[0-9]+(\\(|\\[)", lines)), 4)

  # directories use the stadium shape, files use rectangles
  expect_true(any(grepl('n[0-9]+\\(\\["R/"\\]\\)', lines)))
  expect_true(any(grepl('n[0-9]+\\["a\\.R"\\]', lines)))
  expect_true(any(grepl('n[0-9]+\\["README\\.md"\\]', lines)))

  # every non-root node has exactly one incoming edge
  expect_equal(sum(grepl("-->", lines, fixed = TRUE)), 3)
})

test_that("tree_to_mermaid respects direction and filtering", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "A", "B"), recursive = TRUE)
  file.create(file.path(td, "A", "B", "x.txt"))

  mm <- tree_to_mermaid(td, direction = "LR", max_depth = 1)
  lines <- strsplit(mm, "\n", fixed = TRUE)[[1L]]
  expect_true(grepl("^flowchart LR$", lines[1]))
  expect_true(any(grepl('"A/"', lines, fixed = TRUE)))
  expect_false(any(grepl('"B/"', lines, fixed = TRUE)))
  expect_false(any(grepl('"x.txt"', lines, fixed = TRUE)))
})

test_that("mermaid labels escape quotes and hashes", {
  expect_identical(mermaid_escape('quo"te.md'), "quo#quot;te.md")
  expect_identical(mermaid_escape("a#b"), "a#35;b")
  expect_identical(mermaid_escape("plain.md"), "plain.md")
})

test_that("tree_to_dot produces a valid digraph", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))
  file.create(file.path(td, "README.md"))

  dot <- tree_to_dot(td)
  lines <- strsplit(dot, "\n", fixed = TRUE)[[1L]]

  expect_true(grepl("^digraph printtree \\{$", lines[1]))
  expect_true(any(grepl("rankdir=TB;", lines, fixed = TRUE)))
  expect_true(any(grepl('label="R/", shape=folder', lines, fixed = TRUE)))
  expect_true(any(grepl('label="a.R", shape=note', lines, fixed = TRUE)))
  expect_equal(sum(grepl("->", lines, fixed = TRUE)), 3)
  expect_true(grepl("^\\}$", lines[length(lines)]))
})

test_that("tree_to_dot respects rankdir", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "a.md"))

  dot <- tree_to_dot(td, rankdir = "LR")
  expect_true(grepl("rankdir=LR;", dot, fixed = TRUE))
})

test_that("dot labels escape quotes and backslashes", {
  expect_identical(dot_escape('quo"te.md'), 'quo\\"te.md')
  expect_identical(dot_escape("a\\b"), "a\\\\b")
})

test_that("diagram functions write to file when requested", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "README.md"))

  mm_file <- file.path(td, "tree.mmd")
  out <- tree_to_mermaid(td, file = mm_file)
  expect_identical(out, mm_file)
  expect_true(any(grepl("^flowchart TD$", readLines(mm_file))))

  dot_file <- file.path(td, "tree.dot")
  out <- tree_to_dot(td, file = dot_file)
  expect_identical(out, dot_file)
  expect_true(any(grepl("^digraph", readLines(dot_file))))
})

test_that("write_tree supports diagram and quarto formats", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "README.md"))

  mm_file <- file.path(td, "tree.mmd")
  write_tree(td, mm_file, format = "mermaid")
  expect_true(any(grepl("^flowchart TD$", readLines(mm_file))))

  dot_file <- file.path(td, "tree.dot")
  write_tree(td, dot_file, format = "dot")
  expect_true(any(grepl("^digraph", readLines(dot_file))))

  qmd_file <- file.path(td, "tree.qmd")
  write_tree(td, qmd_file, format = "qmd", title = "Demo tree", direction = "LR")
  qmd <- readLines(qmd_file)
  expect_true(any(grepl('title: "Demo tree"', qmd, fixed = TRUE)))
  expect_true(any(grepl("^```\\{mermaid\\}$", qmd)))
  expect_true(any(grepl("^flowchart LR$", qmd)))
  expect_true(any(grepl("^```$", qmd)))
})

test_that("build_tree nodes match the printed tree", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "A", "B"), recursive = TRUE)
  file.create(file.path(td, "A", "B", "x.txt"))
  file.create(file.path(td, "top.md"))
  dir.create(file.path(td, "empty"))

  tree <- build_tree(td, prune = TRUE)
  nodes <- tree$nodes

  # pruned empty dir is absent from both lines and nodes
  expect_false(any(grepl("empty", tree$lines, fixed = TRUE)))
  expect_false(any(nodes$name == "empty/"))

  # every non-root node resolves to its parent
  kids <- nodes[!is.na(nodes$parent), ]
  expect_true(all(kids$parent %in% nodes$path))

  # node count matches directory + file counts (+1 for root)
  expect_equal(nrow(nodes), tree$directories + tree$files + 1L)
  expect_equal(sum(!nodes$is_dir), tree$files)
})
