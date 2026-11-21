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
