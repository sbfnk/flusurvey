test_that("merge_data turns t/f columns from every survey into factors", {
  data <- lapply(simulated_data[["2013"]], data.table::copy)
  ## columns found only in the background survey, which is not the last
  ## survey processed
  background_only <- setdiff(
    colnames(data[["background"]]),
    colnames(data[[length(data)]])
  )
  tf_columns <- background_only[vapply(background_only, function(x) {
    all(c("t", "f") %in% data[["background"]][[x]])
  }, logical(1))]
  expect_gt(length(tf_columns), 0)
  for (column in tf_columns) {
    data[["background"]][, (column) := as.character(get(column))]
  }

  dt <- merge_data(data, clean = NULL)
  for (column in tf_columns) {
    expect_true(is.factor(dt[[column]]), info = column)
  }
})

test_that("merge_data skips every cleaning step for clean = NULL", {
  raw <- merge_data(simulated_data[["2013"]], clean = NULL)
  first_only <- merge_data(simulated_data[["2013"]], clean = "remove.first")
  expect_gt(nrow(raw), nrow(first_only))
  expect_identical(
    nrow(merge_data(simulated_data[["2013"]], clean = character(0))),
    nrow(raw)
  )
})
