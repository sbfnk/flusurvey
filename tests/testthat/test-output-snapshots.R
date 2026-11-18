## These tests record the output as it stands, so that a change to it shows up
## as a changed snapshot in review. A full serialised copy of each table runs to
## many megabytes, so the snapshot holds the shape, the classes and one hash per
## column instead: a change names the columns it touched. Contact ranges are
## sampled at random, hence the seed before every extraction.
extract_seeded <- function(...) {
  set.seed(20150101)
  extract_data(simulated_data, years = 2013:2014, ...)
}

summarise_table <- function(x) {
  x <- as.data.frame(x)
  list(
    dim = dim(x),
    classes = vapply(x, function(column) class(column)[1], ""),
    hashes = vapply(x, rlang::hash, "")
  )
}

expect_unchanged <- function(x) {
  expect_snapshot_value(summarise_table(x), style = "json2")
}

test_that("extract_data output with the default cleaning is unchanged", {
  expect_unchanged(extract_seeded())
})

cleaning_options <- c(
  "remove.first", "remove.bad.symptom.dates", "remove.bad.health.score",
  "limit.season", "remove.postcodes", "n.reports", "unsuccessful.join",
  "only.symptoms"
)

for (option in cleaning_options) {
  test_that(paste("extract_data output with only", option, "is unchanged"), {
    expect_unchanged(extract_seeded(clean = option))
  })
}

test_that("extract_data output with clean = NULL is unchanged", {
  expect_unchanged(extract_seeded(clean = NULL))
})

test_that("bouts_of_illness output on the default extract is unchanged", {
  dt <- extract_seeded()
  invisible(capture.output(res <- bouts_of_illness(dt)))
  expect_unchanged(res)
})

test_that("get_incidence output on the default extract is unchanged", {
  dt <- extract_seeded()
  expect_unchanged(
    get_incidence(dt, incidence.columns = "ili", aggregation = "week")
  )
  expect_unchanged(get_incidence(dt, by = "agegroup"))
})
