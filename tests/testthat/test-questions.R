test_that("Q2 is mapped to same in every year's symptom survey from 2012", {
  for (year in as.character(2012:2020)) {
    expect_equal(
      unname(flusurvey::questions[[year]][["symptom"]][["Q2"]]), "same",
      info = year
    )
  }
})

test_that("same has a not_applicable option", {
  expect_equal(
    unname(flusurvey::options[["same"]]),
    c("yes", "no", "dont_know", "not_applicable")
  )
})
