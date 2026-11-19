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

test_that("the driving survey retains same-day reports through joins", {
  data <- lapply(simulated_data[["2013"]], data.table::copy)
  for (name in c("background", "contact", "symptom")) {
    data[[name]] <- data.table::rbindlist(list(data[[name]][1], data[[name]][1]))
  }
  for (surveys in list("background", c("background", "contact"),
                       c("background", "contact", "symptom"))) {
    if (length(surveys) == 3) {
      expect_warning(result <- merge_data(data[surveys], clean = NULL))
    } else {
      result <- merge_data(data[surveys], clean = NULL)
    }
    expect_equal(nrow(result), 2)
  }
})

test_that("minimum symptom reports counts reporting days", {
  report <- data.table::copy(simulated_data[["2013"]][["symptom"]][1])
  reports <- data.table::rbindlist(rep(list(report), 6))
  reports[, global_id := rep(c("a", "b"), each = 3)]
  reports[global_id == "b", date := date + 0:2]

  result <- merge_data(list(symptom = reports), clean = "n.reports")
  expect_equal(as.character(result$global_id), rep("b", 3))
  expect_equal(result$nReports, rep(3L, 3))

  unfiltered <- merge_data(list(symptom = reports), clean = NULL)
  expect_equal(unfiltered[global_id == "a", nReports], rep(1L, 3))
})
