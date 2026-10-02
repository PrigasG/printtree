# Initializes a scratch git repo with a test user identity. Returns TRUE on
# success; every git command's exit status is checked so CRAN machines with
# a broken git setup skip instead of failing.
git_test_init <- function(git, td) {
  isTRUE(tryCatch({
    system2(git, c("-C", git_path_arg(td), "init"), stdout = FALSE, stderr = FALSE) == 0L &&
      system2(git, c("-C", git_path_arg(td), "config", "user.email", "test@example.com"),
              stdout = FALSE, stderr = FALSE) == 0L &&
      system2(git, c("-C", git_path_arg(td), "config", "user.name", "Test User"),
              stdout = FALSE, stderr = FALSE) == 0L
  }, warning = function(e) FALSE, error = function(e) FALSE))
}

# Stages and commits everything in the scratch repo. Returns TRUE on success.
git_test_commit <- function(git, td, message = "initial") {
  isTRUE(tryCatch({
    system2(git, c("-C", git_path_arg(td), "add", "-A"), stdout = FALSE, stderr = FALSE) == 0L &&
      system2(git, c("-C", git_path_arg(td), "commit", "-m", message),
              stdout = FALSE, stderr = FALSE) == 0L
  }, warning = function(e) FALSE, error = function(e) FALSE))
}
