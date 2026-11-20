## These tests record the current output, so a change to it appears as a changed
## snapshot in review. A full serialised copy of each table would take many
## megabytes, so the snapshot stores the dimensions, the column classes and one
## hash per column. A change then shows which columns it touched. Contact ranges
## are sampled at random, so we set the seed before every extraction.
extract_seeded <- function(...) {
  set.seed(20150101)
  extract_data(simulated_data, years = 2013:2014, ...)
}

## Rows are sorted with radix ordering before hashing. Radix ordering is the
## same in every locale and for any join order, so the hashes depend only on
## the values.
sort_rows <- function(x) {
  keys <- intersect(c("global_id", "participant_id", "date"), colnames(x))
  if (length(keys) == 0) return(x)
  x[do.call(order, c(unname(as.list(x[keys])), list(method = "radix"))), ,
    drop = FALSE]
}

summarise_table <- function(x) {
  x <- sort_rows(as.data.frame(x))
  list(
    dim = dim(x),
    classes = vapply(x, function(column) class(column)[1], ""),
    ## Hash the text form of the values instead of the object. R serialises
    ## attributes differently across platforms and versions, which would change
    ## the hashes.
    hashes = vapply(x, function(column) rlang::hash(as.character(column)), "")
  )
}

expect_unchanged <- function(x, name) {
  capture_snapshot_table(x, name)
  expect_snapshot_value(summarise_table(x), style = "json2")
}

test_that("extract_data output with the default cleaning is unchanged", {
  expect_unchanged(extract_seeded(), "extract-default")
})

cleaning_options <- c(
  "remove.first", "remove.bad.symptom.dates", "remove.bad.health.score",
  "limit.season", "remove.postcodes", "n.reports", "unsuccessful.join",
  "only.symptoms"
)

for (option in cleaning_options) {
  test_that(paste("extract_data output with only", option, "is unchanged"), {
    expect_unchanged(extract_seeded(clean = option), paste0("extract-", option))
  })
}

test_that("extract_data output with clean = NULL is unchanged", {
  expect_unchanged(extract_seeded(clean = NULL), "extract-uncleaned")
})

test_that("bouts_of_illness output on the default extract is unchanged", {
  dt <- extract_seeded()
  invisible(capture.output(res <- bouts_of_illness(dt)))
  expect_unchanged(res, "bouts")
})

test_that("get_incidence output on the default extract is unchanged", {
  dt <- extract_seeded()
  expect_unchanged(
    get_incidence(dt, incidence.columns = "ili", aggregation = "week"),
    "incidence"
  )
  expect_unchanged(get_incidence(dt, by = "agegroup"), "incidence-agegroup")
})
