# Stream the exported tables and check every pair without loading them in full.
# Run from the repository root after generating all levels.
levels <- commandArgs(trailingOnly = TRUE)
if (!length(levels)) levels <- c("005", "020", "050", "gt", "ht", "htg")
stopifnot(all(levels %in% c("005", "020", "050", "gt", "ht", "htg")))
for (scale in levels) {
  m <- as.matrix(read.csv(paste0("output/", scale, "/ed_matrix_", scale, ".csv.gz"),
                          row.names = 1, check.names = FALSE))
  stopifnot(identical(rownames(m), colnames(m)), all(is.finite(m)),
            all(m >= 0), all(diag(m) == 0), isTRUE(all.equal(m, t(m))))
  sums <- numeric(length(m))
  counts <- integer(length(m))
  component_counts <- numeric()
  path <- paste0("output/", scale, "/ed_decomposition_", scale, ".csv.gz")
  con <- gzfile(path, "rt")
  header <- readLines(con, n = 1)
  stopifnot(identical(header, '"type_a","type_b","component","contribution","definition_a","definition_b"'))
  repeat {
    lines <- readLines(con, n = 50000)
    if (!length(lines)) break
    x <- read.csv(text = lines, header = FALSE,
                   col.names = c("type_a", "type_b", "component", "contribution", "definition_a", "definition_b"),
                   colClasses = c("character", "character", "character", "numeric", "character", "character"))
    observed <- table(x$component)
    component_counts[setdiff(names(observed), names(component_counts))] <- 0
    component_counts[names(observed)] <- component_counts[names(observed)] + as.numeric(observed)
    i <- match(x$type_a, rownames(m))
    j <- match(x$type_b, colnames(m))
    stopifnot(!anyNA(i), !anyNA(j), !anyNA(x$component), all(nzchar(x$component)),
              all(is.finite(x$contribution)),
              all(x$contribution[x$component != "defining_LEC_correlation_adjustment"] >= 0),
              all(x$contribution[x$component == "defining_LEC_correlation_adjustment"] <= 0),
              !any(x$component == "defining_LEC_difference"),
              all(x$contribution[x$component == "no_difference"] == 0))
    defining <- startsWith(x$component, "defining_LEC:")
    stopifnot(!anyNA(x$definition_a[defining]), !anyNA(x$definition_b[defining]),
              all(nzchar(x$definition_a[defining])), all(nzchar(x$definition_b[defining])),
              all(x$definition_a[defining] != x$definition_b[defining]),
              all(x$contribution[defining] > 0))
    ids <- i + (j - 1L) * nrow(m)
    totals <- rowsum(cbind(value = x$contribution, count = 1), ids, reorder = FALSE)
    indices <- as.integer(rownames(totals))
    sums[indices] <- sums[indices] + totals[, "value"]
    counts[indices] <- counts[indices] + totals[, "count"]
  }
  close(con)
  # Decimal serialization and the matrix's one-decimal rounding can differ at ties.
  stopifnot(all(counts > 0), max(abs(sums - as.numeric(m))) <= 0.05 + 1e-10,
            all(sums[seq.int(1, length(m), nrow(m) + 1L)] == 0))
  catalogue <- read.csv(paste0("output/", scale, "/ed_components_", scale, ".csv.gz"))
  stopifnot(setequal(catalogue$component, names(component_counts)),
            identical(catalogue$row_count, as.integer(component_counts[catalogue$component])),
            all(catalogue$family %in% c("lec", "criterion", "correlated_lecs", "defining_lec", "adjustment", "penalty", "zero")))
  cat(scale, ": checked ", length(m), " ordered pairs and ", sum(counts),
      " component rows\n", sep = "")
}
