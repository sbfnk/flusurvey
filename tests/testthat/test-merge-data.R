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

test_that("only.symptoms keeps participants with a missing no.symptoms answer", {
  symptom <- data.table::data.table(
    global_id = c("a", "a", "b", "b", "c"),
    date = as.Date(c(
      "2013-01-03", "2013-01-10", "2013-01-03", "2013-01-10", "2013-01-03"
    )),
    no.symptoms = c("t", NA, "t", "f", NA)
  )
  dt <- merge_data(list(symptom = symptom), clean = "only.symptoms")
  expect_setequal(dt$global_id, c("a", "b"))
  expect_equal(nrow(dt), 4)
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
