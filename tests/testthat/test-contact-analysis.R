test_that("contact plots use the saved contact population", {
  path <- test_path("..", "..", "analysis", "contacts.R")
  if (!file.exists(path)) skip("Contact analysis is not included")
  expressions <- parse(path)
  saved <- which(vapply(expressions, function(x) {
    is.call(x) && identical(x[[1]], as.name("saveRDS"))
  }, logical(1)))
  expect_length(saved, 1)
  expect_identical(expressions[[saved]][[2]], as.name("contacts"))
  plot_symbols <- all.names(as.call(c(list(as.name("{")),
                                      as.list(expressions[-seq_len(saved)]))))
  expect_true("contacts" %in% plot_symbols)
  expect_false(any(c("dt_contacts", "dt_back_contacts") %in% plot_symbols))
})

test_that("contact extraction retains reports outside the influenza season", {
  path <- test_path("..", "..", "analysis", "contacts.R")
  if (!file.exists(path)) skip("Contact analysis is not included")
  expressions <- parse(path)
  assignment <- expressions[[which(vapply(expressions, function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("dt_back_contacts"))
  }, logical(1)))]]
  call <- assignment[[3]]
  data <- lapply(simulated_data[["2013"]], data.table::copy)
  report <- data[["contact"]][1]
  data[["contact"]] <- data.table::rbindlist(list(report, report, report))
  data[["contact"]][, date := as.Date(c("2013-10-31", "2014-01-15", "2014-04-02"))]
  data[["background"]] <- data[["background"]][global_id == report$global_id]
  data[["background"]][, date := as.Date("2013-10-01")]
  call[[2]] <- quote(list(`2013` = data))
  call[["years"]] <- 2013L
  result <- eval(call)
  expect_equal(sort(result$date), as.Date(c("2013-10-31", "2014-01-15", "2014-04-02")))
})
