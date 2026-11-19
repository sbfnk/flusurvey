## A compact description of a table: enough that a change to any column, to
## the column order or to the missing values shows up as a different result.
fingerprint <- function(dt) {
  is_num <- vapply(dt, is.numeric, TRUE)
  is_fct <- vapply(dt, is.factor, TRUE)
  names_string <- paste(colnames(dt), collapse = " ")
  class_table <- table(vapply(dt, function(x) class(x)[1], ""))
  ## Order the names without regard to case or locale, which table() does not
  class_table <- class_table[order(tolower(names(class_table)),
                                   method = "radix")]
  list(
    dim = dim(dt),
    key = data.table::key(dt),
    classes = stats::setNames(as.vector(class_table), names(class_table)),
    missing = sum(vapply(dt, function(x) sum(is.na(x)), 1)),
    numeric_total = sum(vapply(dt[, is_num, with = FALSE], sum, 1,
                               na.rm = TRUE)),
    factor_levels = sum(vapply(dt[, is_fct, with = FALSE], nlevels, 1L)),
    factor_codes = sum(vapply(
      dt[, is_fct, with = FALSE],
      function(x) sum(as.integer(x), na.rm = TRUE), 1
    )),
    names_checksum = sum(
      as.numeric(utf8ToInt(names_string)) * seq_len(nchar(names_string))
    )
  )
}
