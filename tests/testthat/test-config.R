test_that("local, custom config roundtrip", {
  repo <- git_init(tempfile("gert-tests-config"))
  on.exit(unlink(repo, recursive = TRUE))

  orig <- git_config_set("aaa.bbb", "ccc", repo)
  expect_null(orig)
  cfg <- git_config(repo)
  expect_equal(cfg$value[cfg$name == "aaa.bbb"], "ccc")

  orig <- git_config_set("aaa.bbb", NULL, repo)
  expect_equal(orig, "ccc")
  cfg <- git_config(repo)
  expect_equal(cfg$value[cfg$name == "aaa.bbb"], character())
})

test_that("multivar, local, custom config roundtrip", {
  repo <- git_init(tempfile("gert-tests-config-multivar"))
  on.exit(unlink(repo, recursive = TRUE))

  git_config_set("aaa.bbb", "ccc", repo = repo, add = TRUE)
  git_config_set("aaa.bbb", "ddd", repo = repo, add = TRUE)
  git_config_set("aaa.bbb", "eee", repo = repo, add = TRUE)
  cfg <- git_config(repo)
  expect_equal(cfg$value[cfg$name == "aaa.bbb"], c("ccc", "ddd", "eee"))

  git_config_set("aaa.bbb", "ddd", repo = repo, unset = TRUE)
  cfg <- git_config(repo)
  expect_equal(cfg$value[cfg$name == "aaa.bbb"], c("ccc", "eee"))

  git_config_set("aaa.bbb", "^[a-z]{3}$", repo = repo, unset = TRUE)
  cfg <- git_config(repo)
  expect_equal(cfg$value[cfg$name == "aaa.bbb"], character())
})
