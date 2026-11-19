## participants with several reports, so each keeps a row when the first
## report is removed
pick_ids <- function(data, n) {
  counts <- table(data$symptom$global_id)
  head(intersect(data$background$global_id, names(counts)[counts > 2]), n)
}

test_that("nb.household handles unticked, uncounted and implausible bands", {
  data <- copy(simulated_data[["2013"]])
  bg <- data$background
  ind <- grep("^household\\.", colnames(bg), value = TRUE)
  ids <- pick_ids(data, 3)
  bg[global_id == ids[1], (ind) := "f"]
  bg[
    global_id == ids[1],
    c("household.0.4", "nb.household.0.4") := list("t", NA)
  ]
  bg[global_id == ids[2], (ind) := "f"]
  bg[
    global_id == ids[3],
    c("household.19.44", "nb.household.19.44") := list("t", 500)
  ]
  dt <- merge_data(data, clean = NULL)
  hh <- unique(dt[global_id %in% ids, list(global_id, nb.household)])
  expect_equal(hh[global_id == ids[1], nb.household], 1)
  expect_true(is.na(hh[global_id == ids[2], nb.household]))
  expect_true(is.na(hh[global_id == ids[3], nb.household]))
})

test_that("max.band.count sets the limit on a band", {
  data <- copy(simulated_data[["2013"]])
  bg <- data$background
  ind <- grep("^household\\.", colnames(bg), value = TRUE)
  pid <- pick_ids(data, 1)
  bg[global_id == pid, (ind) := "f"]
  bg[
    global_id == pid,
    c("household.19.44", "nb.household.19.44") := list("t", 6)
  ]
  small <- merge_data(data, clean = NULL, max.band.count = 5)
  large <- merge_data(data, clean = NULL, max.band.count = 6)
  expect_gt(nrow(small[global_id == pid]), 0)
  expect_true(all(is.na(small[global_id == pid, nb.household])))
  expect_true(all(large[global_id == pid, nb.household] == 6))
})
