make_symptoms <- function() {
  data.table::data.table(
    symptoms.suddenly = c("yes", "dont_know", NA, "no"),
    fever = "t", tired = "f", headache = "f", cough = "t",
    sore.throat = "f", shortness.breath = "f"
  )
}

test_that("unknown sudden onset counts as sudden by default", {
  res <- aggregate_symptoms(make_symptoms())
  expect_equal(res$ili, c("t", "t", "t", "f"))
  expect_equal(res$ili.fever, c("t", "t", "t", "f"))
})

test_that("ili.sudden.unknown = 0 counts unknown onset as not sudden", {
  res <- aggregate_symptoms(make_symptoms(), ili.sudden.unknown = 0)
  expect_equal(res$ili, c("t", "f", "f", "f"))
})

test_that("ili.sudden.unknown = NA leaves unknown onset missing", {
  res <- aggregate_symptoms(make_symptoms(), ili.sudden.unknown = NA)
  expect_equal(res$ili, c("t", NA, NA, "f"))
})
