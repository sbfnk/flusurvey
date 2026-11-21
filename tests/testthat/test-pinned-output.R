## These pin what read_data(), merge_data() and extract_data() return for the
## simulated data, so that restructuring them can be checked against it.

test_that("merge_data with default cleaning gives the pinned table", {
  set.seed(1)
  dt <- expect_contact_warning(merge_data(simulated_data[["2013"]]))
  expect_identical(
    fingerprint(dt),
    list(
      dim = c(2898L, 243L), key = c("date", "global_id"),
      classes = c(character = 3L, Date = 8L, factor = 163L, integer = 64L,
                  numeric = 5L),
      missing = 168572, numeric_total = 7578352, factor_levels = 471L,
      factor_codes = 456641, names_checksum = 928461132
    )
  )
})

test_that("merge_data with only the first reports removed gives the pinned table", {
  set.seed(1)
  dt <- expect_contact_warning(
    merge_data(simulated_data[["2013"]], clean = "remove.first")
  )
  expect_identical(
    fingerprint(dt),
    list(
      dim = c(3068L, 245L), key = c("date", "global_id"),
      classes = c(character = 5L, Date = 8L, factor = 163L, integer = 64L,
                  numeric = 5L),
      missing = 182595, numeric_total = 8035196, factor_levels = 471L,
      factor_codes = 480645, names_checksum = 938163011
    )
  )
})

test_that("merge_data with a function for ranges gives the pinned table", {
  set.seed(1)
  dt <- expect_contact_warning(
    merge_data(simulated_data[["2014"]], clean = "n.reports",
               ranges = "mean", min.reports = 2)
  )
  expect_identical(
    fingerprint(dt),
    list(
      dim = c(3325L, 245L), key = c("date", "global_id"),
      classes = c(character = 5L, Date = 8L, factor = 158L, integer = 69L,
                  numeric = 5L),
      missing = 202398, numeric_total = 8829829, factor_levels = 461L,
      factor_codes = 498858, names_checksum = 945860811
    )
  )
})

test_that("extract_data gives the pinned table for both seasons", {
  set.seed(1)
  dt <- expect_contact_warning(extract_data(simulated_data, years = 2013:2014))
  expect_identical(
    fingerprint(dt),
    list(
      dim = c(5734L, 253L), key = "global_id",
      classes = c(character = 7L, Date = 8L, factor = 166L, integer = 67L,
                  numeric = 5L),
      missing = 381935, numeric_total = 2590409, factor_levels = 477L,
      factor_codes = 886668, names_checksum = 997430436
    )
  )
})

test_that("extract_data without joining returns one table per season", {
  set.seed(1)
  res <- extract_data(simulated_data, years = 2013, surveys = "symptom",
                      join = FALSE)
  expect_named(res, "2013")
  expect_identical(
    fingerprint(res[["2013"]]),
    list(
      dim = c(2932L, 82L), key = c("date", "global_id"),
      classes = c(character = 1L, Date = 6L, factor = 69L, integer = 5L,
                  numeric = 1L),
      missing = 122081, numeric_total = 5258973, factor_levels = 209L,
      factor_codes = 101150, names_checksum = 113647443
    )
  )
})

test_that("extract_data warns about years and surveys that do not exist", {
  expect_warning(
    extract_data(simulated_data, years = c(2013, 1999), surveys = "symptom",
                 join = FALSE),
    "1999"
  )
  expect_warning(
    extract_data(simulated_data, years = 2013, surveys = c("symptom", "x"),
                 join = FALSE),
    "'x'"
  )
  expect_error(
    suppressWarnings(extract_data(simulated_data, years = 1999)),
    "No years"
  )
  expect_error(extract_data(simulated_data, surveys = character()),
               "No surveys")
})

## writes a survey of the simulated data back to a csv file in the form of the
## raw files: question names as columns, 0/1 for yes/no and option codes
raw_file <- function(season, survey) {
  dt <- data.table::copy(simulated_data[[season]][[survey]])
  codes <- flusurvey::questions[[season]][[survey]]
  for (col in colnames(dt)) {
    x <- dt[[col]]
    if (is.factor(x) && identical(levels(x), c("f", "t"))) {
      data.table::set(dt, j = col, value = as.integer(x == "t"))
    } else if (col %in% names(flusurvey::options)) {
      data.table::set(
        dt, j = col,
        value = names(flusurvey::options[[col]])[
          match(as.character(x), flusurvey::options[[col]])
        ]
      )
    } else if (inherits(x, "Date")) {
      data.table::set(dt, j = col, value = as.character(x))
    }
  }
  dt[, timestamp := paste(date, "10:00:00")]
  dt[, date := NULL]
  data.table::setnames(dt, codes, names(codes))
  path <- tempfile(fileext = ".csv")
  utils::write.csv(dt, path, row.names = FALSE, na = "")
  path
}

test_that("read_data restores the simulated data from its raw files", {
  for (season in names(simulated_data)) {
    files <- lapply(
      stats::setNames(nm = names(simulated_data[[season]])),
      function(survey) raw_file(season, survey)
    )
    res <- read_data(files, as.integer(season))
    expect_named(res, names(simulated_data[[season]]))
    for (survey in names(res)) {
      expected <- simulated_data[[season]][[survey]]
      expect_identical(key(res[[survey]]), key(expected))
      expect_identical(
        res[[survey]][, setdiff(colnames(res[[survey]]), "timestamp"),
                      with = FALSE],
        expected[, setdiff(colnames(expected), "timestamp"), with = FALSE]
      )
    }
  }
})

test_that("read_data takes uid or user_id as the global id when it is missing", {
  for (id_name in c("uid", "user_id")) {
    files <- lapply(
      stats::setNames(nm = c("background", "symptom")),
      function(survey) {
        path <- raw_file("2013", survey)
        raw <- utils::read.csv(path, check.names = FALSE)
        colnames(raw)[colnames(raw) == "global_id"] <- id_name
        utils::write.csv(raw, path, row.names = FALSE, na = "")
        path
      }
    )
    res <- read_data(files, 2013)
    expect_true(all(vapply(res, function(x) "global_id" %in% colnames(x),
                           TRUE)))
    expect_false(any(vapply(res, function(x) id_name %in% colnames(x), TRUE)))
  }
})

test_that("read_data warns when the surveys share no id", {
  files <- list(background = raw_file("2013", "background"),
                symptom = raw_file("2013", "symptom"))
  raw <- utils::read.csv(files$symptom, check.names = FALSE)
  colnames(raw)[colnames(raw) == "global_id"] <- "other_id"
  utils::write.csv(raw, files$symptom, row.names = FALSE, na = "")
  expect_warning(read_data(files, 2013), "No global ID")
})

test_that("bouts_of_illness handles missing values in no.symptoms", {
  set.seed(1)
  dt <- expect_contact_warning(extract_data(simulated_data, years = 2013:2014))
  invisible(capture.output(complete <- bouts_of_illness(data.table::copy(dt))))
  expect_identical(dim(complete), c(217L, 261L))
  dt[sample(.N, 300), no.symptoms := NA]
  invisible(capture.output(res <- bouts_of_illness(dt)))
  expect_identical(dim(res), c(205L, 261L))
})
