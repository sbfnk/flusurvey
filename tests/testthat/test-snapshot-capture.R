test_that("snapshot tables retain values, classes and ordering", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  original <- data.frame(id = c(2L, 1L), value = factor(c("b", "a")))
  capture_snapshot_table(original, "example", path)
  expect_identical(readRDS(file.path(path, "example.rds")), original)
})
