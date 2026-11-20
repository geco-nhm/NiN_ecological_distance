# Shared positions are 1-based ranks of observed gradient basic steps, ordered
# 0, a, ..., z. Uppercase factor levels are never assigned gradient positions.
# Create the shared basic-step reference ----
create.shared.reference <- function(lec_steps) {
  rows <- lapply(names(lec_steps), function(category) {
    steps <- strsplit(lec_steps[[category]], "")[[1]]
    if (!any(steps %in% letters)) return(NULL)
    ordered <- c("0", letters)
    ordered <- ordered[ordered %in% steps]
    data.frame(category = category, basic_step = ordered,
               position = seq_along(ordered))
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(category = character(), basic_step = character(), position = numeric()))
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

# Aggregate range provenance for mapping units ----
aggregate.profile.sources <- function(units, sources, agg_unit, ht) {
  indices <- which(units$ht == ht & !grepl("[*#]", units[[agg_unit]]))
  groups <- units[[agg_unit]][indices]
  if (anyNA(groups)) stop("Missing aggregation group for ", ht)
  result <- lapply(unique(groups), function(group) {
    apply(sources[indices[groups == group], , drop = FALSE], 2, function(x) {
      x <- sort(unique(x[!is.na(x)]))
      if (length(x)) paste(x, collapse = ";") else NA_character_
    })
  })
  if (!length(result)) return(as.matrix(sources[FALSE, , drop = FALSE]))
  do.call(rbind, result)
}

# Create ecological profiles for reclassification ----
create.type.profiles <- function(units, lec, sources, criteria_data, ht_specific_list,
                                 shared_reference, ht_specific_number_minor_types,
                                 ht_specific_number_mu, gradient_categories = c("KA")) {
  lec <- as.matrix(lec)
  sources <- as.matrix(sources)
  normalize <- function(x) sub("-0", "-", as.character(x))
  if (nrow(units) != nrow(lec) || !identical(dim(lec), dim(sources)) ||
      !identical(colnames(lec), colnames(sources)) ||
      length(ht_specific_list) != nrow(units) || anyDuplicated(units$code) ||
      !identical(normalize(units$code), normalize(criteria_data$code)) ||
      !identical(as.character(units$ht), as.character(criteria_data$ht))) {
    stop("Type profile inputs are not aligned")
  }
  if (any(is.na(sources) & !is.na(lec))) stop("A defined range has no recorded source")
  minor_counts <- as.matrix(ht_specific_number_minor_types)
  mapping_counts <- as.matrix(ht_specific_number_mu)
  rows <- lapply(colnames(lec), function(category) {
    ranges <- as.character(lec[, category])
    shared <- shared_reference[shared_reference$category == category, , drop = FALSE]
    shared <- shared[order(shared$position), , drop = FALSE]
    kind <- rep(if (nrow(shared)) "gradient" else "factor", nrow(units))
    if (category %in% gradient_categories) {
      kind[] <- "gradient"
    } else {
      kind[!is.na(ranges) & grepl("[A-Z]", ranges)] <- "factor"
    }
    local_position <- shared_position <- adjustment <- rep(NA_real_, nrow(units))
    local_status <- shared_status <- rep("undefined", nrow(units))
    local_steps <- rep(NA_character_, nrow(units))
    for (i in seq_len(nrow(units))) {
      reference <- ht_specific_list[[i]][[category]]
      if (length(reference)) local_steps[i] <- paste(reference, collapse = "|")
      if (is.na(ranges[i])) next
      if (kind[i] == "factor") {
        local_status[i] <- shared_status[i] <- "factor"
        next
      }
      if (grepl("[A-Z]", ranges[i])) {
        local_status[i] <- shared_status[i] <- "invalid_range"
        next
      }
      # Coarse definitions can include basic steps absent from the shared reference
      range_steps <- strsplit(ranges[i], "")[[1]]
      if (all(range_steps %in% shared$basic_step)) {
        shared_position[i] <- gradient.positions(shared$basic_step, ranges[i], require_all = TRUE)
        shared_status[i] <- "ok"
      } else {
        shared_status[i] <- "outside_reference"
      }
      if (!length(reference)) next
      if (any(grepl("[A-Z]", reference))) {
        local_status[i] <- if (category %in% gradient_categories) "invalid_reference" else "factor_reference"
        next
      }
      local_position[i] <- gradient.positions(reference, ranges[i], require_all = TRUE)
      local_status[i] <- "ok"
      adjustment[i] <- minor_counts[units$ht[i], category] /
        mapping_counts[units$ht[i], category]
    }
    data.frame(
      type = as.character(units$code), major_type = as.character(units$ht),
      category = category, kind = kind, range = ranges, value = NA_real_,
      source = as.character(sources[, category]),
      position_major_type = local_position, position_shared = shared_position,
      major_type_adjustment = adjustment, major_type_steps = local_steps,
      major_type_status = local_status, shared_status = shared_status
    )
  })
  criterion_rows <- lapply(setdiff(names(criteria_data), c("code", "ht")), function(category) {
    values <- as.numeric(criteria_data[[category]])
    data.frame(
      type = as.character(units$code), major_type = as.character(units$ht),
      category = category, kind = "criterion", range = NA_character_, value = values,
      source = "major_type_criteria", position_major_type = NA_real_, position_shared = NA_real_,
      major_type_adjustment = NA_real_, major_type_steps = NA_character_,
      major_type_status = "not_applicable", shared_status = "not_applicable"
    )
  })
  result <- do.call(rbind, c(rows, criterion_rows))
  rownames(result) <- NULL
  result
}

# Reclassify a vector of type codes ----
reclassify.types <- function(types, category, profiles,
                             reference = c("shared", "major_type"),
                             representation = c("position", "range")) {
  reference <- match.arg(reference)
  representation <- match.arg(representation)
  if (length(category) != 1L || is.na(category)) stop("Choose one category")
  required <- c("type", "category", "kind", "range", "value",
                "position_major_type", "position_shared")
  if (!is.data.frame(profiles) || !all(required %in% names(profiles))) {
    stop("Invalid type profile table")
  }
  selected <- profiles[profiles$category == category, , drop = FALSE]
  if (!nrow(selected)) stop("Unknown category: ", category)
  if (anyDuplicated(selected$type)) stop("Duplicate profile rows for ", category)
  input_names <- names(types)
  types <- as.character(types)
  indices <- match(types, selected$type)
  if (any(!is.na(types) & is.na(indices))) {
    stop("Unknown type: ", types[which(!is.na(types) & is.na(indices))[1]])
  }
  values <- if (representation == "range") {
    ifelse(selected$kind == "criterion", as.character(selected$value), selected$range)
  } else {
    ifelse(selected$kind == "criterion", selected$value,
           selected[[paste0("position_", reference)]])
  }
  result <- values[indices]
  names(result) <- input_names
  result
}
