# Generated demonstration-data path restoration for the public code release.
# The published files have generic names data/data-1.*, data/data-2.*, ...
# Dataset identifiers and relative paths inside reference indexes remain unchanged.
# This function recreates those paths as links (copies where links are unavailable).
psdb_restore_data <- function(root = ".", verbose = TRUE) {
  root <- normalizePath(root, mustWork = TRUE)
  mapping_file <- file.path(root, "data_manifest.csv")
  if (!file.exists(mapping_file)) stop("Missing data_manifest.csv: ", mapping_file)
  m <- utils::read.csv(mapping_file, stringsAsFactors = FALSE, check.names = FALSE,
                       fileEncoding = "UTF-8")
  needed <- c("original_path", "data_file", "size_bytes")
  if (!all(needed %in% names(m))) stop("Invalid data_manifest.csv columns")
  created <- 0L
  for (i in seq_len(nrow(m))) {
    from <- file.path(root, m$data_file[i])
    to <- file.path(root, m$original_path[i])
    if (!file.exists(from)) stop("Missing bundled data: ", m$data_file[i])
    if (!isTRUE(file.info(from)$size == as.numeric(m$size_bytes[i]))) {
      stop("Bundled data has unexpected size: ", m$data_file[i])
    }
    if (file.exists(to)) {
      if (!isTRUE(file.info(to)$size == file.info(from)$size))
        stop("A conflicting file already exists: ", m$original_path[i])
      next
    }
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    # Relative paths in the lookup .rds files expect exactly these original names.
    linked <- isTRUE(suppressWarnings(file.symlink(from, to)))
    if (!linked) linked <- isTRUE(suppressWarnings(file.link(from, to)))
    if (!linked) linked <- isTRUE(file.copy(from, to, overwrite = FALSE))
    if (!linked || !file.exists(to)) stop("Cannot restore reference path: ", to)
    created <- created + 1L
  }
  if (verbose) message(sprintf("[PerturbSigDB demo] Data paths ready: %d new aliases; %d total assets",
                               created, nrow(m)))
  invisible(TRUE)
}
