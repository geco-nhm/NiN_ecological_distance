# Export distance matrices, decompositions, and component inventories ----
write.ed.outputs <- function(units, lec, ht_specific_list, hlec_dlec, hlec,
                             dlec, dlec_without, criteria_data, correlation_lec,
                             ht_specific_number_minor_types, ht_specific_number_mu,
                             matrix_path, decomposition_path, workers = 1L, component_path = NULL) {
  if (!is.numeric(workers) || length(workers) != 1L || !is.finite(workers) ||
      workers < 1 || workers != as.integer(workers)) {
    stop("workers must be a positive integer")
  }
  if (.Platform$OS.type == "windows" && workers > 1L) {
    stop("Use workers = 1 on Windows")
  }
  codes <- as.character(units$code)
  if (anyNA(codes) || anyDuplicated(codes)) stop("Type codes must be unique and non-missing")
  # Numeric matrices avoid repeatedly performing data-frame arithmetic per pair.
  units <- as.matrix(units)
  lec <- as.matrix(lec)
  criteria_data <- cbind(code = 0, ht = 0, as.matrix(criteria_data[, -(1:2)]))
  storage.mode(criteria_data) <- "numeric"
  ht_specific_number_minor_types <- as.matrix(ht_specific_number_minor_types)
  ht_specific_number_mu <- as.matrix(ht_specific_number_mu)
  n <- length(codes)
  if (!n) stop("No types to compare")
  matrix <- matrix(0, n, n, dimnames = list(codes, codes))
  component_counts <- numeric()
  matrix_tmp <- tempfile("ed-matrix-", tmpdir = dirname(matrix_path))
  decomposition_tmp <- tempfile("ed-components-", tmpdir = dirname(decomposition_path))
  con <- if (endsWith(decomposition_path, ".gz")) {
    gzfile(decomposition_tmp, "wt")
  } else {
    file(decomposition_tmp, "wt")
  }
  on.exit({
    if (!is.null(con)) close(con)
    unlink(c(matrix_tmp, decomposition_tmp))
  }, add = TRUE)

  calculate_row <- function(i) {
    pairs <- lapply(seq_len(n), function(j) {
      result <- compute.ed.ultimate(
        i, j, units, lec, ht_specific_list, hlec_dlec, hlec, dlec,
        dlec_without, criteria_data, correlation_lec,
        ht_specific_number_minor_types, ht_specific_number_mu, decompose = TRUE
      )
      if (!is.finite(result$total) || result$total < 0 ||
          any(!is.finite(result$contributions)) ||
          !isTRUE(all.equal(sum(result$contributions), result$total, tolerance = 1e-12))) {
        stop("Invalid decomposition for ", codes[i], " / ", codes[j])
      }
      result$definition_a <- result$definitions$definition_a[
        match(names(result$contributions), result$definitions$component)]
      result$definition_b <- result$definitions$definition_b[
        match(names(result$contributions), result$definitions$component)]
      result
    })
    contributions <- lapply(pairs, `[[`, "contributions")
    list(
      total = vapply(pairs, `[[`, numeric(1), "total"),
      table = data.frame(
        type_a = codes[i],
        type_b = rep(codes, lengths(contributions)),
        component = unlist(lapply(contributions, names), use.names = FALSE),
        contribution = unlist(contributions, use.names = FALSE),
        definition_a = unlist(lapply(pairs, `[[`, "definition_a"), use.names = FALSE),
        definition_b = unlist(lapply(pairs, `[[`, "definition_b"), use.names = FALSE)
      )
    )
  }

  # Stream small batches so the complete long table need not fit in memory.
  batches <- split(seq_len(n), ceiling(seq_len(n) / max(1L, workers * 4L)))
  for (indices in batches) {
    rows <- if (workers == 1L) lapply(indices, calculate_row) else {
      parallel::mclapply(indices, calculate_row, mc.cores = workers)
    }
    for (k in seq_along(indices)) {
      i <- indices[k]
      if (inherits(rows[[k]], "try-error")) stop(rows[[k]])
      matrix[i, ] <- rows[[k]]$total
      observed <- table(rows[[k]]$table$component)
      missing <- setdiff(names(observed), names(component_counts))
      component_counts[missing] <- 0
      component_counts[names(observed)] <- component_counts[names(observed)] + as.numeric(observed)
      write.table(rows[[k]]$table, con, sep = ",", row.names = FALSE,
                  col.names = i == 1L, quote = TRUE)
    }
    message(basename(matrix_path), ": ", max(indices), "/", n, " rows")
  }
  close(con)
  con <- NULL
  matrix_con <- if (endsWith(matrix_path, ".gz")) gzfile(matrix_tmp, "wt") else file(matrix_tmp, "wt")
  tryCatch(write.csv(round(as.data.frame(matrix), 1), matrix_con),
           finally = close(matrix_con))
  if (!file.rename(decomposition_tmp, decomposition_path)) stop("Cannot save decomposition table")
  if (!file.rename(matrix_tmp, matrix_path)) stop("Cannot save distance matrix")
  if (!is.null(component_path)) {
    components <- sort(names(component_counts))
    family <- rep("lec", length(components))
    family[components %in% colnames(criteria_data)[-(1:2)]] <- "criterion"
    family[startsWith(components, "correlated_LECs:")] <- "correlated_lecs"
    family[startsWith(components, "defining_LEC:")] <- "defining_lec"
    family[components == "defining_LEC_correlation_adjustment"] <- "adjustment"
    family[components == "no_shared_LECs"] <- "penalty"
    family[components == "no_difference"] <- "zero"
    lec_codes <- sub("^(correlated_LECs:|defining_LEC:)", "", components)
    lec_codes[!family %in% c("lec", "correlated_lecs", "defining_lec")] <- NA_character_
    component_con <- if (endsWith(component_path, ".gz")) gzfile(component_path, "wt") else file(component_path, "wt")
    tryCatch(write.csv(data.frame(component = components, family = family, lec_codes = lec_codes,
                         row_count = as.numeric(component_counts[components])),
              component_con, row.names = FALSE), finally = close(component_con))
  }
  invisible(matrix)
}

# Look up distances for paired type vectors ----
lookup.ed.decomposition <- function(type_a, type_b, decomposition) {
  type_a <- as.character(type_a)
  type_b <- as.character(type_b)
  if (length(type_a) != length(type_b)) stop("Type vectors must have equal lengths")
  if (anyNA(type_a) || anyNA(type_b)) stop("Type vectors must not contain missing codes")
  required <- c("type_a", "type_b", "component", "contribution")
  if (!is.data.frame(decomposition) || !all(required %in% names(decomposition))) {
    stop("decomposition must contain: ", paste(required, collapse = ", "))
  }
  columns <- c(required, intersect(c("definition_a", "definition_b"), names(decomposition)))
  result <- data.frame(pair_id = integer(), decomposition[FALSE, columns, drop = FALSE])
  if (!length(type_a)) return(result)
  key <- function(a, b) paste(a, b, sep = "\r")
  requested <- key(type_a, type_b)
  available <- key(decomposition$type_a, decomposition$type_b)
  # Subset before splitting; maps often request far fewer pairs than the full table.
  keep <- which(available %in% requested)
  groups <- split(keep, available[keep])
  missing <- which(!requested %in% names(groups))
  if (length(missing)) {
    i <- missing[1]
    stop("Pair not found: ", type_a[i], " / ", type_b[i])
  }
  rows <- lapply(seq_along(requested), function(i) {
    x <- decomposition[groups[[requested[i]]], columns, drop = FALSE]
    data.frame(pair_id = i, x, row.names = NULL)
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}
