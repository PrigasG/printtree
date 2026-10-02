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
  expect_true(any(grepl("title: 'Demo tree'", qmd, fixed = TRUE)))
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

test_that("git_colors tints nodes by git status", {
  skip_on_cran()
  git <- Sys.which("git")
  skip_if(!nzchar(git), "git is not installed")

  td <- withr::local_tempdir()
  skip_if_not(git_test_init(git, td), "could not initialize a git repository")

  file.create(file.path(td, "tracked.txt"))
  skip_if_not(git_test_commit(git, td), "could not commit test files")

  writeLines("changed", file.path(td, "tracked.txt"))
  file.create(file.path(td, "new.txt"))

  # git_colors implies git = TRUE; no explicit git argument passed
  mm <- tree_to_mermaid(td, git_colors = TRUE)
  lines <- strsplit(mm, "\n", fixed = TRUE)[[1L]]
  expect_true(any(grepl("classDef ptMod", lines, fixed = TRUE)))
  expect_true(any(grepl("classDef ptNew", lines, fixed = TRUE)))
  expect_true(any(grepl("class n[0-9]+ ptMod", lines)))
  expect_true(any(grepl("class n[0-9]+ ptNew", lines)))

  dot <- tree_to_dot(td, git_colors = TRUE)
  expect_true(any(grepl('fillcolor="#fef3c7"', strsplit(dot, "\n", fixed = TRUE)[[1L]], fixed = TRUE)))
  expect_true(any(grepl('fillcolor="#dcfce7"', strsplit(dot, "\n", fixed = TRUE)[[1L]], fixed = TRUE)))

  # without git_colors there are no class or fillcolor annotations
  plain <- tree_to_mermaid(td)
  expect_false(any(grepl("classDef", strsplit(plain, "\n", fixed = TRUE)[[1L]], fixed = TRUE)))
})

test_that("subgraph wraps directories in nested subgraph blocks", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R", "inner"), recursive = TRUE)
  file.create(file.path(td, "R", "a.R"))
  file.create(file.path(td, "R", "inner", "b.R"))

  mm <- tree_to_mermaid(td, subgraph = TRUE)
  lines <- strsplit(mm, "\n", fixed = TRUE)[[1L]]

  # root, R, inner => three subgraph blocks, each closed
  expect_equal(sum(grepl("^\\s*subgraph n[0-9]+", lines)), 3)
  expect_equal(sum(grepl("^\\s*end$", lines)), 3)

  # inner's block is nested inside R's block
  r_open <- grep("subgraph n[0-9]+\\[\"R/\"\\]", lines)
  i_open <- grep("subgraph n[0-9]+\\[\"inner/\"\\]", lines)
  ends <- grep("^\\s*end$", lines)
  expect_true(i_open > r_open)
  i_close <- ends[ends > i_open][1]
  r_close <- ends[ends > r_open][2]
  expect_true(i_close < r_close)

  # files are defined once, inside their parent's block
  expect_equal(sum(grepl('n[0-9]+\\["a\\.R"\\]', lines)), 1)

  # edges are unchanged by subgraph mode
  expect_equal(sum(grepl("-->", lines, fixed = TRUE)), 4)
})

test_that("repo_url adds clickable nodes", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))

  mm <- tree_to_mermaid(td, repo_url = "https://github.com/u/repo", repo_branch = "main")
  expect_true(grepl('click n[0-9]+ "https://github.com/u/repo/blob/main/R/a\\.R"', mm))
  expect_true(grepl('click n[0-9]+ "https://github.com/u/repo/tree/main/R"', mm))
  expect_true(grepl('click n1 "https://github.com/u/repo/tree/main"', mm, fixed = TRUE))

  dot <- tree_to_dot(td, repo_url = "https://github.com/u/repo")
  expect_true(grepl('URL="https://github.com/u/repo/blob/main/R/a.R"', dot, fixed = TRUE))
})

test_that("repo_url encodes reserved characters, spaces, and Unicode", {
  td <- withr::local_tempdir()
  # Valid filenames that break naive URL building
  file.create(file.path(td, "a#b.txt"))
  file.create(file.path(td, "100%.txt"))
  file.create(file.path(td, "sp ace.txt"))

  mm <- tree_to_mermaid(td, repo_url = "https://github.com/u/repo", repo_branch = "main")
  expect_true(grepl("blob/main/a%23b.txt", mm, fixed = TRUE))
  expect_true(grepl("blob/main/100%25.txt", mm, fixed = TRUE))
  expect_true(grepl("blob/main/sp%20ace.txt", mm, fixed = TRUE))
  # No raw fragment delimiters in file links
  expect_false(grepl("blob/main/a#b.txt", mm, fixed = TRUE))

  # url_encode_path handles reserved chars and Unicode directly
  # (tested directly: ? is illegal in Windows filenames and some
  # filesystems cannot create Unicode names)
  expect_identical(url_encode_path("q?uestion.txt"), "q%3Fuestion.txt")
  expect_identical(url_encode_path("caf\u00e9.txt"), "caf%C3%A9.txt")

  # repo_branch is encoded too (slashes preserved for hierarchical names)
  mm2 <- tree_to_mermaid(td, repo_url = "https://github.com/u/repo",
                         repo_branch = "release#1")
  expect_true(grepl("blob/release%231/a%23b.txt", mm2, fixed = TRUE))
  mm3 <- tree_to_mermaid(td, repo_url = "https://github.com/u/repo",
                         repo_branch = "feature/foo")
  expect_true(grepl("blob/feature/foo/a%23b.txt", mm3, fixed = TRUE))
  expect_identical(url_encode_path("R/a-b_c.R"), "R/a-b_c.R")
  expect_identical(url_encode_path("a/b/c"), "a/b/c")
})

test_that("qmd titles are validated and sanitized for YAML", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "a.txt"))

  # Newlines collapse to spaces so the YAML front matter stays valid
  out <- tempfile(fileext = ".qmd")
  write_tree(td, out, format = "qmd", title = "A\nB")
  lines <- readLines(out)
  expect_true(any(grepl("title: 'A B'", lines, fixed = TRUE)))

  # Single-quoted scalar: apostrophes double, backslashes and double
  # quotes stay literal
  out2 <- tempfile(fileext = ".qmd")
  write_tree(td, out2, format = "qmd", title = "It's C:\\proj \"ok\"")
  expect_true(any(grepl("title: 'It''s C:\\proj \"ok\"'", readLines(out2), fixed = TRUE)))

  # Non-string titles are rejected
  expect_error(write_tree(td, tempfile(fileext = ".qmd"), format = "qmd", title = 123))
  expect_error(write_tree(td, tempfile(fileext = ".qmd"), format = "qmd", title = NA_character_))
  expect_error(write_tree(td, tempfile(fileext = ".qmd"), format = "qmd", title = c("a", "b")))
})

test_that("tree_to_html accepts git_colors", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "a.txt"))
  # git_colors is accepted (no unused-argument error) even outside a repo
  html <- tree_to_html(td, git_colors = TRUE)
  expect_true(grepl("<html", html, fixed = TRUE))
})

test_that("tree_to_html validates title", {
  td <- withr::local_tempdir()
  file.create(file.path(td, "a.txt"))
  expect_error(tree_to_html(td, title = c("A", "B")))
  expect_error(tree_to_html(td, title = 123))
  expect_error(tree_to_html(td, title = NA_character_))
  # valid titles work
  expect_true(grepl("<title>Hi</title>", tree_to_html(td, title = "Hi"), fixed = TRUE))
})

test_that("tree_diff_mermaid marks added and removed nodes", {
  old <- withr::local_tempdir()
  new <- withr::local_tempdir()
  file.create(file.path(old, "gone.txt"))
  file.create(file.path(old, "kept.txt"))
  file.create(file.path(new, "kept.txt"))
  file.create(file.path(new, "added.txt"))

  mm <- tree_diff_mermaid(old, new)
  lines <- strsplit(mm, "\n", fixed = TRUE)[[1L]]

  expect_true(any(grepl("classDef ptAdd", lines, fixed = TRUE)))
  expect_true(any(grepl("classDef ptDel", lines, fixed = TRUE)))

  node_id <- function(label) {
    def <- grep(sprintf('["%s"]', label), lines, fixed = TRUE, value = TRUE)
    sub('^    (n[0-9]+).*$', '\\1', def)
  }

  added_id <- node_id("added.txt")
  expect_true(any(grepl(sprintf("class %s ptAdd", added_id), lines, fixed = TRUE)))

  gone_id <- node_id("gone.txt")
  expect_true(any(grepl(sprintf("class %s ptDel", gone_id), lines, fixed = TRUE)))

  kept_id <- node_id("kept.txt")
  expect_false(any(grepl(sprintf("class %s ", kept_id), lines, fixed = TRUE)))

  # identical trees produce no diff classes at all
  same <- tree_diff_mermaid(old, old)
  expect_false(grepl("classDef", same, fixed = TRUE))
})

test_that("tree_diff_dot fills added and removed nodes", {
  old <- withr::local_tempdir()
  new <- withr::local_tempdir()
  file.create(file.path(old, "gone.txt"))
  file.create(file.path(new, "added.txt"))

  dot <- tree_diff_dot(old, new)
  dlines <- strsplit(dot, "\n", fixed = TRUE)[[1L]]
  expect_true(any(grepl('fillcolor="#dcfce7"', dlines, fixed = TRUE)))
  expect_true(any(grepl('fillcolor="#fee2e2"', dlines, fixed = TRUE)))
  expect_true(any(grepl('style="filled,dashed"', dlines, fixed = TRUE)))
})

test_that("tree_to_mindmap produces nested mindmap text", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))

  mm <- tree_to_mindmap(td)
  lines <- strsplit(mm, "\n", fixed = TRUE)[[1L]]

  expect_identical(lines[1], "mindmap")
  expect_true(grepl("^  root\\(\\(", lines[2]))

  r_line <- which(lines == "    R/")
  a_line <- which(lines == "      a.R")
  expect_equal(length(r_line), 1)
  expect_equal(length(a_line), 1)
  expect_true(a_line > r_line)

  out <- file.path(td, "tree.mmd")
  expect_identical(tree_to_mindmap(td, file = out), out)
  expect_identical(readLines(out)[1], "mindmap")
})

test_that("mindmap labels escape brackets and parens", {
  expect_identical(mindmap_escape("a[1](2).R"), "a［1］（2）.R")
})

test_that("tree_to_html produces a self-contained collapsible page", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))
  file.create(file.path(td, "README.md"))

  html <- tree_to_html(td, title = "Demo")
  expect_true(grepl("<!DOCTYPE html>", html, fixed = TRUE))
  expect_true(grepl("<title>Demo</title>", html, fixed = TRUE))
  expect_true(grepl("<h1>Demo</h1>", html, fixed = TRUE))
  expect_true(grepl('class="caret"', html, fixed = TRUE))
  expect_true(grepl('class="nested"', html, fixed = TRUE))
  expect_true(grepl("a\\.R", html))

  # no external dependencies: everything is inline
  expect_false(grepl("<script src=", html, fixed = TRUE))
  expect_false(grepl('rel="stylesheet"', html, fixed = TRUE))

  out <- file.path(td, "tree.html")
  expect_identical(tree_to_html(td, file = out), out)
  expect_true(any(grepl("<!DOCTYPE html>", readLines(out), fixed = TRUE)))
})

test_that("html labels escape markup", {
  expect_identical(html_escape("<a>&\""), "&lt;a&gt;&amp;&quot;")
})

test_that("write_tree supports mindmap and html formats", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "README.md"))

  mm_file <- file.path(td, "tree.mmd")
  write_tree(td, mm_file, format = "mindmap")
  expect_identical(readLines(mm_file)[1], "mindmap")

  html_file <- file.path(td, "tree.html")
  write_tree(td, html_file, format = "html", title = "T")
  expect_true(any(grepl("<title>T</title>", readLines(html_file), fixed = TRUE)))
})

test_that("view_mermaid produces HTML with export buttons", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "R"))
  file.create(file.path(td, "R", "a.R"))

  html <- view_mermaid(td)
  expect_true(grepl("<!DOCTYPE html>", html, fixed = TRUE))
  expect_true(grepl('class="mermaid"', html, fixed = TRUE))
  expect_true(grepl("flowchart", html, fixed = TRUE))
  # Export buttons present
  expect_true(grepl("Download SVG", html, fixed = TRUE))
  expect_true(grepl("Download PNG", html, fixed = TRUE))
  expect_true(grepl("Download JPEG", html, fixed = TRUE))
  # Pan/zoom library included by default
  expect_true(grepl("svg-pan-zoom", html, fixed = TRUE))

  # file= writes the HTML
  out <- tempfile(fileext = ".html")
  res <- view_mermaid(td, file = out)
  expect_identical(res, out)
  expect_true(file.exists(out))

  # pan_zoom = FALSE omits the library
  html2 <- view_mermaid(td, pan_zoom = FALSE)
  expect_false(grepl("svg-pan-zoom", html2, fixed = TRUE))

  # title validation applies
  expect_error(view_mermaid(td, title = c("A", "B")))
})

test_that("view_mermaid hints at large diagrams", {
  td <- withr::local_tempdir()
  # Create a tree with >100 lines of diagram output
  for (i in 1:40) {
    dir.create(file.path(td, paste0("d", i)))
    file.create(file.path(td, paste0("d", i), "f.txt"))
  }
  html <- view_mermaid(td)
  expect_true(grepl("Large diagram", html, fixed = TRUE))
  expect_true(grepl("max_depth", html, fixed = TRUE))
})
