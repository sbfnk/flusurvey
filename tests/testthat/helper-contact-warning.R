expect_contact_warning <- function(code) {
  count <- 0L
  result <- withCallingHandlers(code, warning = function(w) {
    if (startsWith(conditionMessage(w),
                   "The contact and symptom surveys are joined by report date")) {
      count <<- count + 1L
      invokeRestart("muffleWarning")
    }
  })
  expect_gt(count, 0L)
  result
}
