# Build the existing major-type adapted reference once per major type and LEC.
prepare.ht.references <- function(units, lec, lec_steps) {
  references <- vector("list", nrow(units))
  for (ht in unique(units$ht)) {
    indices <- which(units$ht == ht)
    reference <- setNames(vector("list", ncol(lec)), colnames(lec))
    for (category in colnames(lec)) {
      steps <- find.ht.steps(lec[indices, category])
      steps <- sort(as.character(remove.common.bs(steps)))
      if (length(steps)) reference[[category]] <- add.border.bs(lec_steps[category], steps)
    }
    for (i in indices) references[[i]] <- reference
  }
  names(references) <- as.character(units$ht)
  references
}

# Count major-type adapted steps ----
count.ht.steps <- function(units, lec) {
  major_types <- unique(units$ht)
  counts <- do.call(rbind, lapply(major_types, identify.ht.specific.number, units, lec))
  dimnames(counts) <- list(major_types, colnames(lec))
  as.data.frame(counts)
}

# Prepare minor types and mapping units ----
prepare.ed.level <- function(level, base, lec_steps, minor_counts = NULL) {
  if (!level %in% c("005", "020", "050", "gt")) stop("Unsupported level: ", level)
  if (is.null(minor_counts)) minor_counts <- count.ht.steps(base$units, base$lec)
  if (level == "gt") {
    result <- base
  } else {
    aggregation <- paste0("scale_", level)
    groups <- base$units[[aggregation]]
    keep <- !is.na(groups) & nzchar(groups) & !grepl("[*#]", groups)
    units <- base$units[keep, , drop = FALSE]
    if (!nrow(units)) stop("No mapping units defined for ", level)
    lecs <- base$lec[keep, , drop = FALSE]
    criteria <- base$criteria_data[keep, , drop = FALSE]
    major_types <- unique(units$ht)
    bind <- function(fun, ...) do.call(rbind, lapply(major_types, fun, ...))
    result <- list(
      units = bind(function(ht) aggregate.scale.units(units, aggregation, ht)),
      lec = bind(function(ht) aggregate.scale.lecs(units, lecs, aggregation, ht)),
      criteria_data = bind(function(ht) aggregate.scale.criteria(units, criteria, aggregation, ht)),
      sources = bind(function(ht) aggregate.profile.sources(units, base$sources[keep, , drop = FALSE], aggregation, ht)),
      correlation_lec = base$correlation_lec
    )
    for (name in c("hlec_dlec", "hlec", "dlec_without", "dlec")) {
      values <- base[[name]][keep]
      fun <- if (name == "dlec") aggregate.lec.list else aggregate.list
      result[[name]] <- do.call(c, lapply(major_types, function(ht) fun(units, values, aggregation, ht)))
    }
  }
  result$ht_specific_list <- prepare.ht.references(result$units, result$lec, lec_steps)
  result$ht_specific_number_minor_types <- minor_counts
  result$ht_specific_number_mu <- if (level == "gt") minor_counts else count.ht.steps(result$units, result$lec)
  result
}


# Prepare major types and major-type groups from their own definitions ----
prepare.ed.hierarchy <- function(level, base, dlec_ht, lec_htg, criteria_ht, lec_steps) {

  # Select the codes represented in the current ground/bottom-system dataset
  {
    if (!level %in% c("ht", "htg")) stop("Choose ht or htg")
    codes <- unique(base$units[[level]])
    units <- base$units[match(codes, base$units[[level]]), , drop = FALSE]
    units$code <- codes
    units$ht <- codes
    definitions <- if (level == "ht") dlec_ht[codes] else lec_htg[codes]
  }

  # Create empty ecological profiles; do not inherit minor-type hLECs
  {
    lec <- matrix(NA_character_, nrow = length(codes), ncol = ncol(base$lec),
                  dimnames = list(codes, colnames(base$lec)))
    sources <- lec
    hlec <- setNames(rep(list(character()), length(codes)), codes)
    dlec <- hlec
    dlec_without <- hlec
  }

  # Assign only defining LECs at the requested level
  {
    for (i in seq_along(codes)) {
      current_definitions <- definitions[[i]]
      current_definitions <- trimws(current_definitions)
      current_definitions <- current_definitions[!is.na(current_definitions) &
                                                  grepl("_", current_definitions)]
      dlec[[i]] <- unique(current_definitions)
      dlec_without[[i]] <- unique(sub("_.*", "", current_definitions))

      # Major types also inherit the major-type group's ranges as a fallback
      inherited <- if (level == "ht") lec_htg[[units$htg[i]]] else character()
      for (origin in c("major_type_group", "major_type")) {
        current <- if (origin == "major_type_group") inherited else current_definitions
        if (level == "htg" && origin == "major_type") origin <- "major_type_group"
        for (definition in current) {
          if (is.na(definition) || !grepl("_", definition)) next
          category <- sub("_.*", "", trimws(definition))
          if (!category %in% colnames(lec)) next
          lec[i, category] <- sub(".*_", "", trimws(definition))
          sources[i, category] <- origin
        }
      }
    }
  }

  # Retain major-type criteria; group criteria are unknown at this level
  {
    criteria_data <- data.frame(code = codes, ht = codes)
    for (category in setdiff(names(criteria_ht), "ht")) {
      criteria_data[[category]] <- if (level == "ht") {
        criteria_ht[[category]][match(codes, criteria_ht$ht)]
      } else {
        rep(NA_real_, length(codes))
      }
    }

  }

  # Use the existing distance engine without within-major-type variation
  {
    # Include basic steps present only in coarse definitions in local references
    hierarchy_steps <- lec_steps
    for (category in colnames(lec)) {
      ranges <- lec[, category]
      steps <- unique(unlist(strsplit(ranges[!is.na(ranges)], "")))
      hierarchy_steps[category] <- paste(sort(unique(c(steps, strsplit(lec_steps[[category]], "")[[1]]))), collapse = "")
    }
    references <- prepare.ht.references(units, lec, hierarchy_steps)
    counts <- matrix(1, nrow = length(codes), ncol = ncol(lec),
                     dimnames = list(codes, colnames(lec)))
    result <- list(units = units, lec = as.data.frame(lec), sources = sources,
                   criteria_data = criteria_data, hlec = hlec,
                   hlec_dlec = dlec_without, dlec = dlec, dlec_without = dlec_without,
                   correlation_lec = base$correlation_lec,
                   ht_specific_list = references,
                   ht_specific_number_minor_types = counts,
                   ht_specific_number_mu = counts)
  }

  return(result)
}
