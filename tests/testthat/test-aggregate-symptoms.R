make_symptoms <- function() {
  data.table::data.table(
    symptoms.suddenly = c("yes", "dont_know", NA, "no", "dont_know"),
    fever = "t", tired = "f", headache = "f",
    cough = c("t", "t", "t", "t", "f"),
    sore.throat = "f", shortness.breath = "f"
  )
}

test_that("onset records explicit unknown apart from no answer", {
  res <- aggregate_symptoms(make_symptoms())
  expect_equal(
    res$onset,
    c("sudden", "dont_know", "not.answered", "not.sudden", "dont_know")
  )
})

test_that("unknown onset leaves ili indeterminate by default", {
  res <- aggregate_symptoms(make_symptoms())
  expect_equal(res$ili, c("t", NA, NA, "f", "f"))
  expect_equal(res$ili.fever, c("t", NA, NA, "f", "f"))
  expect_equal(res$suddenly, c("t", NA, NA, "f", NA))
})

test_that("ili.sudden.unknown = 1 counts unknown onset as sudden", {
  res <- aggregate_symptoms(make_symptoms(), ili.sudden.unknown = 1)
  expect_equal(res$ili, c("t", "t", "t", "f", "f"))
  expect_equal(res$ili.fever, c("t", "t", "t", "f", "f"))
})

test_that("ili.sudden.unknown = 0 counts unknown onset as not sudden", {
  res <- aggregate_symptoms(make_symptoms(), ili.sudden.unknown = 0)
  expect_equal(res$ili, c("t", "f", "f", "f", "f"))
})

test_that("the interpretation does not change the recorded onset", {
  symptoms <- make_symptoms()
  onset <- aggregate_symptoms(symptoms)$onset
  for (unknown in c(0, 1, NA)) {
    res <- aggregate_symptoms(symptoms, ili.sudden.unknown = unknown)
    expect_equal(res$onset, onset)
    expect_equal(res$symptoms.suddenly, symptoms$symptoms.suddenly)
  }
})

test_that("a missing onset question is treated as unknown onset", {
  symptoms <- make_symptoms()[, symptoms.suddenly := NULL]
  expect_equal(aggregate_symptoms(symptoms)$onset, rep("no.question", 5))
  expect_equal(
    aggregate_symptoms(symptoms)$ili, c(NA, NA, NA, NA, "f")
  )
  expect_equal(
    aggregate_symptoms(symptoms, ili.sudden.unknown = 1)$ili,
    c("t", "t", "t", "t", "f")
  )
  expect_equal(
    aggregate_symptoms(symptoms, ili.sudden.unknown = 0)$ili,
    rep("f", 5)
  )
})

test_that("fever questions settle onset where they answer it", {
  symptoms <- make_symptoms()
  symptoms[, fever.quickly := c(NA, "yes", "no", "yes", NA)]
  res <- aggregate_symptoms(symptoms)
  expect_equal(
    res$onset, c("sudden", "sudden", "not.sudden", "sudden", "dont_know")
  )
  expect_equal(res$ili, c("t", "t", "f", "t", "f"))
})

test_that("merge_data and extract_data pass on ili.sudden.unknown", {
  ## Copy the table, because := would change the packaged data in place.
  data <- simulated_data[["2013"]]
  data$symptom <- data.table::copy(data$symptom)
  data$symptom[, symptoms.suddenly := rep(
    c("yes", "dont_know", NA, "no"), length.out = .N
  )]
  by_default <- merge_data(data["symptom"], clean = NULL)
  expect_true(all(c("dont_know", "not.answered") %in% by_default$onset))
  unknown <- by_default$onset %in% c("dont_know", "not.answered") &
    by_default$ili.symptoms == "t"
  expect_true(any(unknown))
  expect_true(all(is.na(by_default$ili[unknown])))
  as_sudden <- merge_data(data["symptom"], clean = NULL, ili.sudden.unknown = 1)
  expect_true(all(as_sudden$ili[unknown] == "t"))
  as_not <- extract_data(
    simulated_data, years = 2013, surveys = "symptom", clean = NULL,
    ili.sudden.unknown = 0
  )
  expect_true(all(
    as_not$ili[as_not$onset %in% c("dont_know", "not.answered")] == "f"
  ))
})
