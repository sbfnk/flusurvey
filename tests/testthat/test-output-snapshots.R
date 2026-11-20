## These tests record the current output, so a change to it appears as a changed
## snapshot in review. A full serialised copy of each table would take many
## megabytes, so the snapshot stores the dimensions, the column classes and one
## hash per column. A change then shows which columns it touched. Contact ranges
## are sampled at random, so we set the seed before every extraction.
extract_seeded <- function(...) {
  set.seed(20150101)
  expect_contact_warning(extract_data(simulated_data, years = 2013:2014, ...))
}

## Values are converted to text before hashing. Objects are serialised
## differently across platforms and R versions, and as.character() on doubles
## has changed between R versions, so doubles are written with ten significant
## digits instead.
as_text <- function(column) {
  if (is.double(column)) {
    out <- sprintf("%.10g", column)
    out[is.na(column)] <- NA_character_
    out
  } else {
    as.character(column)
  }
}

## Rows are sorted with radix ordering on the keys and then on every column.
## Radix ordering is the same in every locale, and the full ordering means that
## rows sharing a key come out the same whatever the join order.
summarise_table <- function(x) {
  x <- as.data.frame(x)
  classes <- vapply(x, function(column) class(column)[1], "")
  text <- lapply(x, as_text)
  keys <- intersect(c("global_id", "participant_id", "date"), names(text))
  rows <- do.call(order, c(unname(text[keys]),
                           unname(text[setdiff(names(text), keys)]),
                           list(method = "radix")))
  list(
    dim = dim(x),
    classes = classes,
    hashes = vapply(text, function(column) rlang::hash(column[rows]), "")
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
