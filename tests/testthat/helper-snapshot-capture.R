capture_snapshot_table <- function(x, name, path = Sys.getenv("SNAPSHOT_TABLES")) {
  if (!nzchar(path)) return(invisible(NULL))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, file.path(path, paste0(name, ".rds")), version = 2)
}
