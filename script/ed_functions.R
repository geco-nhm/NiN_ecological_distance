#Expand "and lower" ranges, including legacy tokens without an underscore.
expand.lower.steps <- function(definitions) {
  pattern <- "([A-Z]{2})_?([a-z])\\([\u2012\u2013\u2212-]\\)"
  vapply(definitions, function(definition) {
    if (is.na(definition)) return(NA_character_)
    matches <- gregexpr(pattern, definition, perl = TRUE)
    tokens <- regmatches(definition, matches)[[1]]
    replacements <- vapply(tokens, function(token) {
      lec <- sub(pattern, "\\1", token, perl = TRUE)
      upper <- sub(pattern, "\\2", token, perl = TRUE)
      paste0(lec, "_0", paste(letters[seq_len(match(upper, letters))], collapse = ""))
    }, character(1))
    regmatches(definition, matches) <- list(replacements)
    definition
  }, character(1), USE.NAMES = FALSE)
}


#Define function to identify major-type specific steps
find.mt.steps <- function(lec_vector) {
  
  #Identify all unique basic steps
  all_steps <- unique(unlist(strsplit(paste(lec_vector, collapse = ""), "")))
  
  #Map each basic step to the indices of the mapping unit containing it
  step_indices <- lapply(all_steps, function(basic_steps) {
    which(sapply(lec_vector, function(x) grepl(basic_steps, x)))
  })
  
  #Rename list elements with basic steps
  names(step_indices) <- all_steps
  
  #Create list
  minimal_strings <- list()
  
  #For each character, find the string with the smallest length containing that character
  for (i in all_steps) {
    
    #Identify the index of the mapping units containing the character
    mu_indices <- which(sapply(lec_vector, function(x) grepl(i, x)))
    
    #Find the index of the string with the smallest length among those that contain the character
    min_length <- mu_indices[which.min(sapply(lec_vector[mu_indices], nchar))]
    
    #Store the basic steos in the list
    step_indices[[i]] <- lec_vector[min_length]
  }
  
  #Find the unique elements
  selected_indices <- unique(unlist(step_indices))
  
  #Return the sorted indices
  return(sort(selected_indices))
}

#Define function that creates a nested list
create.template.list <- function(n_inner, n_outer, outer_names = NULL, inner_names = NULL) {
  
  #Create inner list
  main_list <- vector("list", n_inner)
  
  #Assign the names to the outer list
  if (!is.null(outer_names) && length(outer_names) == n_inner) {
    names(main_list) <- outer_names
  }
  
  #Populate each inner list with outer lists and assign names
  for (i in seq_len(n_inner)) {
    
    #Create outer list
    main_list[[i]] <- vector("list", n_outer)
    
    #Assign name to the inner list
    if (!is.null(inner_names) && length(inner_names) == n_outer) {
      names(main_list[[i]]) <- inner_names
    }
  }
  
  #Return the entire list
  return(main_list)
}

#Function to identify the number of major-type specific steps
identify.mt.specific.number <- function(mt, units, lecs) {
  
  #Select the major type
  current_mt <- mt
  
  #Identify the mapping units for the major type
  current_elements <- which(units$mt %in% current_mt)
  
  #Create a data frame for the major type
  current_data <- lecs[current_elements,]
  
  #Create vector to store the number of major-type specific steps
  mt_specific_number <- numeric()
  
  #Loop over all LECs
  for (j in 1:ncol(current_data)) {
    
    #Select LEC
    current_lec <- colnames(current_data)[j]
    
    #Select LEC steps for the mapping unit
    current_lec_values <- current_data[,j]
    
    #Identify the major-type specific steps for the LEC
    mt_specific_steps <- find.mt.steps(current_lec_values)
    
    #Remove the letters in common from the step with most symbols
    mt_specific_steps <- as.character(remove.common.bs(mt_specific_steps))
    
    #Sort the elements aplhabetically
    mt_specific_steps <- sort(mt_specific_steps)
    
    #If there are any major-type specific steps
    if(length(mt_specific_steps) > 0) {
      
      #Store the number of major-type specific steps
      mt_specific_number[j] <- length(mt_specific_steps)
      
    } else {
      
      #Store NA
      mt_specific_number[j] <- NA
    }
  }
  
  #Return the number of major-type specific steps for the LEC
  return(mt_specific_number)
}

#Define function that keeps elements that have underscores (dLECs)
retain.dlecs <- function(data) {
  
  #Apply function to every element
  filtered_vector <- sapply(data$def, function(x) {
    
    #Split the string separated by "," into individual words 
    words <- unlist(strsplit(x, " "))
    
    #Keep words containing "_"
    with_underscore <- grep("_", words, value = TRUE)
    
    #Merge the words into a single string
    return(paste(with_underscore, collapse = " "))
  })
  
  #Rename the vector
  names(filtered_vector) <- data$code
  
  #Return the vector
  return(filtered_vector)
}

#Define a function to remove characters within brackets (bLECs)
remove.blec <- function(lec_vector) {
  Filter(function(x) !grepl("\\[|\\]", x), lec_vector)
}

#Define function that merges, sorts, and only keeps unique basic steps 
bs.unique.sorted <- function(lec_vector) {
  
  #Remove missing elements
  lec_vector <- lec_vector[!is.na(lec_vector)]
  
  #Collapse all bs into one string
  lec_collapsed <- paste(lec_vector, collapse = "")
  
  #Split into individual sorted unique basic steps
  bs_unique_sorted <- sort(unique(unlist(strsplit(lec_collapsed, ""))))
  
  #Merge into a single string
  lec_new <- paste(bs_unique_sorted, collapse = "")
  
  #Return NA if the vector is empty
  if(lec_new == "") {
    
    #Return NA
    return(NA)
    
  } else {
    
    #Otherwise return the new LECs
    return(lec_new)
  }
}

#Define function to aggregate LEC steps
aggregate.lec.vector <- function(lec_vector) {
  
  #Remove missing elements
  lec_vector <- lec_vector[!is.na(lec_vector)]
  
  #Unlist vector
  lec_unlisted <- unlist(lec_vector)
  
  #Identify LEC steps by removing underscores
  lec_steps <- sub(".*_", "", lec_unlisted)
  
  #Identify LECs
  lec_vector <- sub("_.*", "", lec_unlisted)
  
  #Identify unique LECs
  lec_unique <- unique(lec_vector)
  
  #Create vector for storing new LEC values
  new_vector <- numeric()
  
  #Loop over all unique LEC steps
  for (i in seq_along(lec_unique)) {
    
    #Identify the current LEC
    current_lec <- which(lec_vector == lec_unique[i])
    
    #Only keep unique basic steps and sort them
    new_lec <- bs.unique.sorted(lec_steps[current_lec])
    
    #Paste the LEC with the basic steps and store it in the vector
    new_vector[i] <- paste0(lec_unique[i], "_", new_lec)
  }
  
  #Return the new LECs
  return(new_vector)
}

#Function to add basic steps that are below or above the basic steps for the mapping unit
add.border.bs <- function(all_bs, lec_vector) {
  
  #Find unique basic steps
  unique_bs_all <- unique(unlist(strsplit(all_bs[1], "")))
  
  #Find unique basic steps for the major type
  unique_bs_mt <- unique(unlist(strsplit(paste(lec_vector, collapse = ""), "")))
  
  #Create vectors to store the basic steps coming after and before
  prepend_letters <- c()
  append_letters <- c()
  
  #Identify basic steps to prepend or append
  for (i in unique_bs_all) {
    
    #Check if the basic step is already in the major type
    if (!(i %in% unique_bs_mt)) {
      
      #Check order
      if (length(unique_bs_mt) == 0 || i < min(unique_bs_mt)) {
        
        #Collect basic steps to prepend
        prepend_letters <- c(prepend_letters, i)
        
      } else if (i > max(unique_bs_mt)) {
        
        #Collect basic steps to append
        append_letters <- c(append_letters, i)
        
      }
    }
  }
  
  #Concatenate basic steps to prepend and append
  if (length(prepend_letters) > 0) {
    lec_vector <- c(paste(prepend_letters, collapse = ""), lec_vector)
  }
  if (length(append_letters) > 0) {
    lec_vector <- c(lec_vector, paste(append_letters, collapse = ""))
  }
  
  #Return the updated major-type specific steps
  return(lec_vector)
}

#Function to extract LEC names from a list
extract.lecs <- function(lec_list) {
  
  #Create a vector to store the LEC names
  extracted_lecs <- c()
  
  #Loop over all elements in the list
  for (i in names(lec_list)) {
    
    #Select the basic steps
    basic_steps <- lec_list[[i]]
    
    #Add LEC if it has a defined ecological space
    if (!is.null(basic_steps) && length(basic_steps) > 0) {
      extracted_lecs <- c(extracted_lecs, i)
    }
  }
  
  #Return the LEC names
  return(extracted_lecs)
}

# Repair the missing separator in LC01-05 and apply the existing KA_w exclusion
# to a whole token, including when it occurs at the start of a definition.
clean.lec.definitions <- function(definitions) {
  vapply(definitions, function(definition) {
    if (is.na(definition) || !grepl("[KA_w]", definition, fixed = TRUE)) return(definition)
    definition <- gsub("[KA_w] VT_C", "[KA_w], VT_C", definition, fixed = TRUE)
    tokens <- trimws(strsplit(definition, ",", fixed = TRUE)[[1]])
    paste(tokens[tokens != "[KA_w]"], collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

#Function to convert from character-based basic steps to numerical values
convert.bs <- function(basic_steps, index) {
  
  #Number of basic steps
  n <- length(basic_steps)
  
  #Create a vector to store the values
  values <- numeric(n)
  
  #Calculate adjustment factor for basic steps deviating from the midpoint
  adjustment <- n / 5
  
  #CHeck if the length of the vector is odd
  if (n %% 2 == 1) {
    
    #Find the index of the basic step in the middle
    middle_index <- (n + 1) / 2
    
    #Loop over all basic steps
    for (i in 1:n) {
      
      #Calculate the new value by adjusting the index value depending on its distance from the middle
      if (i < middle_index) {
        values[i] <- index - adjustment
      } else if (i == middle_index) {
        values[i] <- index
      } else {
        values[i] <- index + adjustment
      }
    }
    
  } else {
    
    #Find the index of the basic steps in the middle
    middle_index1 <- n / 2
    middle_index2 <- middle_index1 + 1
    
    #Loop over all basic steps
    for (i in 1:n) {
      
      #Calculate the new value by adjusting the index value depending on its distance from the middle
      if (i < middle_index1) {
        values[i] <- index - adjustment
      } else if (i == middle_index1) {
        values[i] <- index - 0.2
      } else if (i == middle_index2) {
        values[i] <- index + 0.2
      } else {
        values[i] <- index + adjustment
      }
    }
  }
  
  #Return the values
  return(unlist(values))
}

#Function to selectively remove characters from longer strings
remove.common.bs <- function(lec_vector) {
  
  #Sort LEC steps by length
  length_order_index <- order(nchar(lec_vector))
  length_order <- lec_vector[length_order_index]
  
  #Create vector for preserved basic steps
  preserve_bs <- character()
  x <- length_order[1]
  # Process each string, starting with the shortest
  modified_lec <- sapply(length_order, function(x) {
    
    #Split the LEC step into basic steps
    bs <- strsplit(x, NULL)[[1]]
    
    #Add basic steps that are not yet preserved
    unique_bs <- bs[!(bs %in% preserve_bs)]
    
    #Only keep unique basic steps and collapse them into one element
    preserve_bs <<- unique(c(preserve_bs, unique_bs))
    paste(unique_bs, collapse = "")
  })
  
  #Return the LEC to the original order
  modified_lec <- as.character(modified_lec[order(length_order_index)])
  
  #Return the modified LEC
  return(modified_lec)
}

# Positions on a chosen gradient reference, using the existing basic-step spacing.
# Strict coverage is useful for lookups; compute.ed retains its intersection rule.
gradient.positions <- function(reference_steps, ranges, require_all = FALSE) {
  if (!length(reference_steps) || anyNA(reference_steps) ||
      any(grepl("[A-Z]", reference_steps))) return(rep(NA_real_, length(ranges)))
  split_steps <- strsplit(reference_steps, "")
  basic_steps <- unlist(split_steps, use.names = FALSE)
  positions <- unlist(lapply(seq_along(split_steps), function(i) {
    convert.bs(split_steps[[i]], i)
  }), use.names = FALSE)
  vapply(ranges, function(range) {
    if (is.na(range)) return(NA_real_)
    selected <- strsplit(range, "")[[1]]
    if (require_all && !all(selected %in% basic_steps)) {
      stop("Range ", range, " is not covered by the gradient reference")
    }
    mean(positions[basic_steps %in% selected])
  }, numeric(1), USE.NAMES = FALSE)
}

#Function to compute ED between two mapping units for one LEC
compute.ed <- function(mt2_steps, mu1, mu2) {
  
  #If any of the mapping units have not been defined with regards to the LEC
  if(any(is.na(c(mu1, mu2)))) {
    
    #Return zero ED
    return(0)
    
  } else {
    
    #Split the vector into vectors with single characters in a list
    split_vector <- strsplit(mt2_steps, "")
    
    #Check if the LEC is a factor
    if(any(grepl("[A-Z]", split_vector))) {
      
      #If the two mapping units have different LEC steps assign one ecological distance unit
      ed_distance <- as.numeric(mu1 != mu2)
      
    } else {
      
      #Use the same position calculation that is exported in type profiles.
      positions <- gradient.positions(mt2_steps, c(mu1, mu2))
      ed_distance <- abs(positions[2] - positions[1])

    }
    
    #Return the ecological distance
    return(ed_distance)
  }
}

# Attribute the existing defining-LEC set penalty without changing its value.
decompose.defining.lecs <- function(definition_a, definition_b, adjusted_total) {
  definition_a <- as.character(definition_a)
  definition_b <- as.character(definition_b)
  all_definitions <- unique(c(definition_a, definition_b))
  codes <- unique(sub("_.*", "", all_definitions))
  matched <- definition_a[definition_a %in% definition_b]
  values <- vapply(codes, function(code) {
    sum(sub("_.*", "", all_definitions) == code) -
      sum(sub("_.*", "", matched) == code)
  }, numeric(1))
  names(values) <- if (length(codes)) paste0("defining_LEC:", codes) else character()
  keep <- values != 0
  values <- values[keep]
  codes <- codes[keep]
  describe <- function(definitions, code) {
    selected <- unique(definitions[sub("_.*", "", definitions) == code])
    if (length(selected)) paste(selected, collapse = "; ") else "absent"
  }
  definitions <- data.frame(
    component = names(values),
    definition_a = vapply(codes, function(code) describe(definition_a, code), character(1)),
    definition_b = vapply(codes, function(code) describe(definition_b, code), character(1)),
    row.names = NULL
  )
  # Keep the existing joint correction separate; assigning it to a LEC would be arbitrary.
  correction <- adjusted_total - sum(values)
  if (correction != 0) {
    values <- c(values, defining_LEC_correlation_adjustment = correction)
  }
  list(contributions = values, definitions = definitions)
}

#Define function to compute ED for any pair of mapping units
compute.ed.ultimate <- function(row, col, units, lec, mt_specific_list, hlec_dlec, hlec, dlec, dlec_without, criteria_data, correlation_lec, mt_specific_number_minor_types, mt_specific_number_mu, decompose = FALSE) {
  
  #Selecting the relevant lists for hLECs, dLECs, and criteria
  {
    current_row_hlec_dlec <- hlec_dlec[[row]]
    current_col_hlec_dlec <- hlec_dlec[[col]]
    current_row_hlec <- hlec[[row]]
    current_col_hlec <- hlec[[col]]
    current_row_dlec <- dlec[[row]]
    current_col_dlec <- dlec[[col]]
    current_row_dlec_without <- dlec_without[[row]]
    current_col_dlec_without <- dlec_without[[col]]
    current_row_criteria <- criteria_data[row, 3:ncol(criteria_data)]
    current_col_criteria <- criteria_data[col, 3:ncol(criteria_data)]
    nested_row <- mt_specific_list[[row]]
    nested_col <- mt_specific_list[[col]]
    row_adjustment <- mt_specific_number_minor_types[units[row,"mt"],] / mt_specific_number_mu[units[row,"mt"],]
    col_adjustment <- mt_specific_number_minor_types[units[col,"mt"],] / mt_specific_number_mu[units[col,"mt"],]
  }
  
  #Identifying the criteria that are shared by bot mapping units
  shared_criteria <- unique(c(which(!is.na(current_col_criteria)), which(!is.na(current_row_criteria))))
  
  #Selecting only the criteria that are shared by both mapping units
  current_row_criteria <- current_row_criteria[shared_criteria]
  current_col_criteria <- current_col_criteria[shared_criteria]
  
  #Currently setting the ecoloigical distance to zero
  ed_distance <- 0
  components <- numeric()
  definitions <- data.frame(component = character(), definition_a = character(),
                            definition_b = character())
  
  #Calculate ED between the mapping units within major types
  if(units[row,"mt"] == units[col,"mt"]) {
    
    #Increase the ecological distance with the calculated increment
    within <- calculate.ed.within(row, col, lec, mt_specific_list, correlation_lec, row_adjustment, col_adjustment, decompose = decompose)
    ed_distance <- ed_distance + sum(within, na.rm = TRUE)
    if (decompose) components <- c(components, within)
    
  }
  
  #Calculate ED between mapping units within major types
  if(units[row,"mt"] != units[col,"mt"]) {
    
    #Check if the mapping units have different SSGs, there can only be one ED for SSG
    if(sum(abs(current_col_criteria[c("ssg_helophytes","ssg_trees")] - current_row_criteria[c("ssg_helophytes","ssg_trees")])) > 1) {
      
      #Set one SSG to zero
      current_col_criteria["ssg_helophytes"] <- 0
      current_row_criteria["ssg_helophytes"] <- 0
    }
    
    #Do not take SSG into account if it has been removed
    if(current_col_criteria["ssg_removed"] == 1 || current_row_criteria["ssg_removed"] == 1) {
      
      #Set SSG to zero
      current_col_criteria[c("ssg_helophytes","ssg_trees")] <- 0
      current_row_criteria[c("ssg_helophytes","ssg_trees")] <- 0
    }
    
    #Filter out forest line criterion if one of the mapping units can occur both above or below
    relevant_criteria <- complete.cases(t(rbind(current_row_criteria,current_col_criteria)))
    
    #Add one ED unit for any criterion that differ
    criterion_ed <- abs(current_row_criteria[relevant_criteria] - current_col_criteria[relevant_criteria])
    ed_distance <- ed_distance + sum(criterion_ed)
    if (decompose) {
      components <- c(components, setNames(as.numeric(unlist(criterion_ed)), names(criterion_ed)))
    }
    
    #Extract the names from the list
    nested_col_names <- extract.lecs(nested_col)
    nested_row_names <- extract.lecs(nested_row)
    
    #Identify groups of correlated LECs
    current_correlation_list <- lapply(correlation_lec, function(x, mt_specific_col, mt_specific_row) {
      
      #Select the LECs from the two mapping units
      lec_col <- names(mt_specific_col)
      lec_row <- names(mt_specific_row)
      
      #Identify the mapping units that are strongly correlating with another LEC 
      unique_index <- unique(c(which(lec_row %in% x), which(lec_col %in% x)))
      
      #Return the vector
      return(unique_index)
    }, nested_col_names, nested_row_names)
    
    #Identify unique LECs
    unique_lec <- unique(unlist(current_correlation_list))
    
    #Create a matrix that shows which of the LECs that are correlated
    correlation_matrix <- matrix(data = 0, nrow = length(unique_lec), ncol = length(unique_lec))
    
    #Convert to indices in the unique LECs
    current_correlation_list <- lapply(current_correlation_list, function(x) which(unique_lec %in% x))
    
    #Apply the function for all LECs
    correlation_matrix <- lapply(current_correlation_list, function(x){
      
      #Add one to the matrix if LECs are correlated
      correlation_matrix[x, x] <- 1
      
      #Return the correlation matrix
      return(correlation_matrix)
    })
    
    #Sum the matrices element-wise
    correlation_matrix <- Reduce(`+`, correlation_matrix)
    
    #Find the number of matching dLECs
    matching_dlec <- length(which(current_row_dlec %in% current_col_dlec))
    
    #Find the total number of dLECs
    total_dlec <- length(unique(c(current_row_dlec, current_col_dlec)))
    
    #If there are no correlating LECs
    if(length(correlation_matrix) == 0) {
      
      #Add the ED from number of different dLECs
      add_distance <- total_dlec - matching_dlec
      
    } else {
      
      #Reset the diagonal
      diag(correlation_matrix) <- 0
      
      #Find the minimum number of LECs any LEC is correlated with
      number_correlation <- min(colSums(correlation_matrix))
      
      #Add the ED from number of different dLECs and subtract for number of correlating LECs
      add_distance <- total_dlec - matching_dlec - number_correlation
      
      #If the result is less than zero, reset the added ED
      add_distance <- max(0, add_distance)
    }
    
    #Add ED to the total calculation
    ed_distance <- ed_distance + add_distance
    if (decompose) {
      defining <- decompose.defining.lecs(current_row_dlec, current_col_dlec, add_distance)
      components <- c(components, defining$contributions)
      definitions <- defining$definitions
    }
    
    #Identify the shared LECs
    shared_lecs <- current_row_hlec_dlec[current_row_hlec_dlec %in% current_col_hlec_dlec]
    
    #If there are no shared LECs
    if(length(shared_lecs) == 0) {
      
      #Add one ED unit
      ed_distance <- ed_distance + 1
      if (decompose) components <- c(components, no_shared_LECs = 1)
    } 
    
    #Add ED between major types
    between <- calculate.ed.between(row, col, lec, mt_specific_list, current_row_hlec, current_col_hlec, correlation_lec, row_adjustment, col_adjustment, decompose = decompose)
    ed_distance <- ed_distance + sum(between, na.rm = TRUE)
    if (decompose) components <- c(components, between)
    
  }
  
  #Keep unrounded contributions; the matrix is rounded only when exported.
  if (decompose) {
    components <- components[!is.na(components) & components != 0]
    if (!length(components)) components <- c(no_difference = 0)
    return(list(total = ed_distance, contributions = components, definitions = definitions))
  }
  #Return ED
  return(ed_distance)
}

#Define function to calculate ecological distance between two mapping units
calculate.ed.between <- function(row, col, lec, mt_specific_list, current_row_lecs, current_col_lecs, correlation_lec, row_adjust, col_adjust, decompose = FALSE) {
  
  #Create a vector to store the calculated ecological distance
  total_ed <- numeric()
  
  #Extract LECs for the relevant mapping units
  row_data <- lec[row, ]
  col_data <- lec[col, ]
  
  #Identify the shared LECs
  shared_lec <- current_row_lecs[current_row_lecs %in% current_col_lecs]
  
  #Remove missing values
  shared_lec <- shared_lec[complete.cases(shared_lec)]
  
  #Return zero ED units if there are no shared LECs
  if(length(shared_lec) == 0) {
    
    #Return zero
    return(if (decompose) numeric() else 0)
    
  } else {
    
    #Select only the LECs that are shared between the mapping units
    row_data <- as.character(row_data[shared_lec])
    col_data <- as.character(col_data[shared_lec])
    
    #Select the relevant major-type specific steps
    nested_row <- mt_specific_list[[row]][shared_lec]
    nested_col <- mt_specific_list[[col]][shared_lec]
    
    #Identify groups of correlated LECs
    current_correlation_list <- lapply(correlation_lec, function(x, mt_specific_col, mt_specific_row) {
      
      #Select the LECs from the two mapping units
      lec_col <- names(mt_specific_col)
      lec_row <- names(mt_specific_row)
      
      #Identify the mapping units that are strongly correlating with another LEC 
      unique_index <- unique(c(which(lec_row %in% x), which(lec_col %in% x)))
      
      #Return the vector
      return(unique_index)
    }, nested_col, nested_row)
    
    #Find unique elements across the list
    unique_elements <- unique(unlist(current_correlation_list))
    
    #Remove elements from all but one list if they occur in several elements
    current_correlation_list <- lapply(current_correlation_list, function(x) x[x %in% unique_elements])
    
    #Remove duplicated elements
    current_correlation_list <- lapply(current_correlation_list, function(x) {
      
      #Identify the index of the LECs 
      unique_values <- x[x %in% unique_elements]
      
      #Exclude the elements that are already used to calculate ecological distances
      unique_elements <<- setdiff(unique_elements, unique_values)
      unique_values
    })
    
    #Check if there are LECs that correlate strongly
    if(length(unlist(current_correlation_list)) > 1) {
      
      #Calculate the ED
      total_ed <- aggregate.correlated.lecs(current_correlation_list, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust, decompose = decompose)
      
    } else {
      
      #Loop through each column
      for (i in seq_along(shared_lec)) {
        
        #Compute the mean difference by putting the first mapping unit into the major type of the second mapping unit
        ed_mu12 <- compute.ed(nested_row[[i]], col_data[i], row_data[i]) * as.numeric(row_adjust[shared_lec[i]])
        
        #Compute the mean difference by putting the second mapping unit into the major type of the first mapping unit
        ed_mu21 <- compute.ed(nested_col[[i]], row_data[i], col_data[i]) * as.numeric(col_adjust[shared_lec[i]])
        
        #Calculate the mean ED between the two mapping units
        total_ed[i] <- mean(c(as.numeric(ed_mu12), as.numeric(ed_mu21)), na.rm = TRUE)
      }
    }
    
    #The correlated branch already returns named group contributions.
    if (decompose) {
      if (is.null(names(total_ed))) names(total_ed) <- shared_lec
      return(total_ed)
    }
    #Return the sum of ED units for all LECs
    return(sum(total_ed, na.rm = TRUE))
  }
}

#Define function to calculate ecological distance between two mapping units
calculate.ed.within <- function(row, col, lec, mt_specific_list, correlation_lec, row_adjust, col_adjust, decompose = FALSE) {
  
  #Create a vector to store the calculated ecological distance
  total_ed <- numeric()
  
  #Extract LECs for the relevant mapping units
  row_data <- lec[row, ]
  col_data <- lec[col, ]
  
  #Identify the shared LECs
  shared_lec <- which(complete.cases(row_data))[(which(complete.cases(row_data)) %in% which(complete.cases(col_data)))]
  
  #Select only the LECs that are shared between the mapping units
  row_data <- as.character(row_data[shared_lec])
  col_data <- as.character(col_data[shared_lec])
  
  #Select the relevant the major-type specific steps
  nested_row <- mt_specific_list[[row]][shared_lec]
  nested_col <- mt_specific_list[[col]][shared_lec]
  
  #Identify groups of correlated LECs
  current_correlation_list <- lapply(correlation_lec, function(x, mt_specific_col, mt_specific_row) {
    
    #Select the LECs from the two mapping units
    lec_col <- names(mt_specific_col)
    lec_row <- names(mt_specific_row)
    
    #Identify the mapping units that are strongly correlating with another LEC 
    unique_index <- unique(c(which(lec_row %in% x), which(lec_col %in% x)))
    
    #Return the vector
    return(unique_index)
  }, nested_col, nested_row)
  
  #Find unique elements across the list
  unique_elements <- unique(unlist(current_correlation_list))
  
  #Remove elements from all but one list if they occur in several elements
  current_correlation_list <- lapply(current_correlation_list, function(x) x[x %in% unique_elements])
  
  #Remove duplicated elements
  current_correlation_list <- lapply(current_correlation_list, function(x) {
    
    #Identify the index of the LECs 
    unique_values <- x[x %in% unique_elements]
    
    #Exclude the elements that are already used to calculate ecological distances
    unique_elements <<- setdiff(unique_elements, unique_values)
    unique_values
  })
  
  #Check if there are LECs that correlate strongly
  if(length(unlist(current_correlation_list)) > 1) {
    
    #Calculate the ED
    total_ed <- aggregate.correlated.lecs(current_correlation_list, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust, decompose = decompose)
    
  } else {
    
    #Loop through each column
    for (i in seq_along(shared_lec)) {
      
      #Compute the mean difference by putting the first mapping unit into the major type of the second mapping unit
      ed_mu12 <- compute.ed(nested_row[[i]], col_data[i], row_data[i]) * row_adjust[shared_lec[i]]
      
      #Compute the mean difference by putting the second mapping unit into the major type of the first mapping unit
      ed_mu21 <- compute.ed(nested_col[[i]], row_data[i], col_data[i]) * col_adjust[shared_lec[i]]
      
      #Calculate the mean ED between the two mapping units
      total_ed[i] <- mean(c(as.numeric(ed_mu12), as.numeric(ed_mu21)), na.rm = TRUE)
    }
  }
  
  if (decompose) {
    if (is.null(names(total_ed))) names(total_ed) <- colnames(lec)[shared_lec]
    return(total_ed)
  }
  #Return the sum of ED units for all LECs
  return(sum(total_ed, na.rm = TRUE))
}

#Define function to aggregate ED from correlated LECs
aggregate.correlated.lecs <- function(correlated_lec, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust, decompose = FALSE) {
  
  #Create a vector to store ED
  current_ed <- numeric()
  
  #Compute the ED between the mapping units with regard to different LECs, but only keep the ED unit for the maximally distant mapping units
  current_ed <- lapply(correlated_lec, aggregate.max, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust)
  
  #Sum the ED units for the evaluated LECs
  group_values <- unlist(current_ed, use.names = FALSE)
  if (decompose) {
    names(group_values) <- vapply(correlated_lec, function(indices) {
      paste0("correlated_LECs:", paste(names(nested_row)[indices], collapse = "+"))
    }, character(1))
  }
  current_ed <- sum(group_values)
  
  #Identify the LEC steps for remaining LECs
  remaining_lec <- which(!seq_along(row_data) %in%  unlist(correlated_lec))
  
  #If there are remaining LECs 
  if(length(remaining_lec) > 0) {
    
    #Create a vector to store the additional potential ED units
    added_ed <- numeric()
    
    #Select the relevant LEC values
    current_row_data <- row_data[remaining_lec]
    current_col_data <- col_data[remaining_lec]
    
    #Select the relevant major-type specific steps
    current_nested_row <- nested_row[remaining_lec]
    current_nested_col <- nested_col[remaining_lec]
    
    #Loop through each LEC
    for (i in seq_along(remaining_lec)) {
      
      #Compute the mean difference by putting the first mapping unit into the major type of the second mapping unit
      ed_mu12 <- compute.ed(current_nested_row[[i]], current_col_data[i], current_row_data[i]) * as.numeric(row_adjust[remaining_lec[i]])
      
      #Compute the mean difference by putting the second mapping unit into the major type of the first mapping unit
      ed_mu21 <- compute.ed(current_nested_col[[i]], current_row_data[i], current_col_data[i]) * as.numeric(col_adjust[remaining_lec[i]])
      
      #Select the mean ED
      added_ed[i] <- mean(c(as.numeric(ed_mu12), as.numeric(ed_mu21)), na.rm = TRUE)
    }
    
    #Sum the ED units for all LECs
    if (decompose) {
      names(added_ed) <- names(nested_row)[remaining_lec]
      return(c(group_values, added_ed))
    }
    added_ed <- sum(added_ed, na.rm = TRUE)
    
    #Add the ED to the ED for correlated LECs
    total_ed <- current_ed + added_ed
    
  } else {
    
    #If there are no more LECs, just return the ED already calculated
    total_ed <- current_ed
    if (decompose) return(group_values)
  }
  
  #Return the ED
  return(total_ed)
}

#Define function to aggregate ED across LECs based on maximum values
aggregate.max <- function(corr_lec, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust) {
  
  #Create a vector to store additional ED
  current_ed <- numeric()
  
  #Check if there are any correlated LECs
  if(length(corr_lec) == 0) {
    
    #If there are none, update additional ED to be zero
    current_ed <- 0
    
    #Return the ecological distance
    return(current_ed)
    
  } else {
    
    #Select the relevant LECs
    {
      current_row_data <- row_data[corr_lec]
      current_col_data <- col_data[corr_lec]
      current_nested_row <- nested_row[corr_lec]
      current_nested_col <- nested_col[corr_lec]
    }
    
    #Retain the LECs with values
    row_adjust <- row_adjust[which(!is.na(row_adjust))]
    col_adjust <- col_adjust[which(!is.na(col_adjust))]
    
    #Loop over all LECs
    for (i in seq_along(corr_lec)) {
      
      #Compute the mean difference by putting the first mapping unit into the major type of the second mapping unit
      ed_mu12 <- compute.ed(current_nested_row[[i]], current_col_data[i], current_row_data[i]) * as.numeric(row_adjust[corr_lec[i]])
      
      #Compute the mean difference by putting the second mapping unit into the major type of the first mapping unit
      ed_mu21 <- compute.ed(current_nested_col[[i]], current_row_data[i], current_col_data[i]) * as.numeric(col_adjust[corr_lec[i]])
      
      #Compute the mean ED for both mapping units
      current_ed[i] <- mean(c(as.numeric(ed_mu12), as.numeric(ed_mu21)), na.rm = TRUE)
    }
    
    #Select the maximum ED over all the LECs
    current_ed <- max(current_ed, na.rm = TRUE)
    
    #Return ED
    return(current_ed)
  }
}

#Define function to aggregate data frame with codes, mapping units, major types, and major-type groups
aggregate.scale.units <- function(units, agg_unit, mt) {
  
  #Choose major type
  current_mt <- which(units$mt == mt)
  
  #Choose the current aggregation
  current_agg <- units[current_mt, agg_unit]
  
  #Identify elements with * or #
  exclude_agg <- grepl("[*#]", current_agg)
  
  #Exclude the elements 
  current_mt <- current_mt[!exclude_agg]
  
  #Exclude the elements 
  current_agg <- current_agg[!exclude_agg]
  
  #Identify the mapping units within the major type
  unique_mu <- unique(current_agg)
  
  #Add mapping unit string
  mu_string <- as.character(seq_along(unique_mu))
  
  #Add zero as the first character to numbers below 10
  mu_string <- unlist(lapply(X = mu_string, FUN = function(x) { 
    
    #Check if the character is a digit between 1 and 9
    if (grepl("^([1-9])$", x)) {
      
      #Add zero as the first character
      return(x <- paste0("0", x))
      
    } else {
      
      #Return the character unchanged
      return(x)
    }
    
  }))
  
  #Create a vector with mapping units for the major type
  code <- paste(mt, "-", mu_string, sep = "")
  
  #Create a column for major type group
  mtg <- substr(x = code, start = 1, stop = 1)
  
  #Create data frame
  unit_data <- data.frame(code, mtg, mt, mu = seq_along(unique(unique_mu)))
  
  #Return the data frame
  return(unit_data)
}

#Define function to aggregate data frame with ecological spaces
aggregate.scale.lecs <- function(units, lecs, agg_unit, mt) {
  
  #Choose major type
  current_mt <- which(units$mt == mt)
  
  #Choose the current aggregation
  current_agg <- units[current_mt, agg_unit]
  
  #Identify elements with * or #
  exclude_agg <- grepl("[*#]", current_agg)
  
  #Exclude the elements 
  current_mt <- current_mt[!exclude_agg]
  
  #Exclude the elements 
  current_agg <- current_agg[!exclude_agg]
  
  #Identify the mapping units within the major type
  unique_mu <- unique(current_agg)
  
  #Create an empty data frame
  new_def_data <- data.frame()
  
  #Loop over all mapping units within the major type
  for (i in seq_along(unique_mu)) {
    
    #Choose mapping unit
    first_mu <- current_agg == unique_mu[i]
    
    #Choose the LEC data
    current_lec <- lecs[current_mt[first_mu], ]
    
    #Aggregate the LEC steps
    new_def <- apply(X = current_lec, MARGIN = 2, FUN = bs.unique.sorted)
    
    #Bind the data frames
    new_def_data <- rbind(new_def_data, new_def)
    
  }
  
  #Rename columns
  colnames(new_def_data) <- colnames(lecs)
  
  #Return the data frame
  return(new_def_data)
}

#Define function to aggregate data frame with criteria
aggregate.scale.criteria <- function(units, criteria, agg_unit, mt) {
  
  #Choose major type
  current_mt <- which(units$mt == mt)
  
  #Choose the current aggregation
  current_agg <- units[current_mt, agg_unit]
  
  #Identify elements with * or #
  exclude_agg <- grepl("[*#]", current_agg)
  
  #Exclude the elements 
  current_mt <- current_mt[!exclude_agg]
  
  #Exclude the elements 
  current_agg <- current_agg[!exclude_agg]
  
  #Identify the mapping units within the major type
  unique_mu <- unique(current_agg)
  
  #Create an empty data frame
  new_def_data <- data.frame()
  
  #Loop over all mapping units within the major type
  for (i in seq_along(unique_mu)) {
    
    #Choose mapping unit
    first_mu <- current_agg == unique_mu[i]
    
    #Choose the LEC data
    current_lec <- criteria[current_mt[first_mu], ]
    
    #Select the dLEC columns
    current_lec <- current_lec[3:ncol(current_lec)]
    
    #Aggregate the ecological spaces
    new_def <- apply(X = current_lec, MARGIN = 2, FUN = function(x) median(x, na.rm = TRUE))
    
    #Bind the data frames
    new_def_data <- rbind(new_def_data, new_def)
  }
  
  #Rename columns
  colnames(new_def_data) <- colnames(criteria)[3:ncol(criteria)]
  
  #Create vector for major type
  mt_bind <- rep(mt, nrow(new_def_data))
  
  #Create vector for code
  code_bind <- paste0(mt_bind, "-", seq(1, nrow(new_def_data)))
  
  #Add major type and code to the data frame
  new_def_data <- cbind(code = code_bind, mt = mt_bind, new_def_data)
  
  #Return the data frame
  return(new_def_data)
}

#Define function to aggregate lists with LEC steps
aggregate.lec.list <- function(units, lec_list, agg_unit, mt) {
  
  #Choose major type
  current_mt <- which(units$mt == mt)
  
  #Choose the current aggregation
  current_agg <- units[current_mt, agg_unit]
  
  #Identify elements with * or #
  exclude_agg <- grepl("[*#]", current_agg)
  
  #Exclude the elements 
  current_mt <- current_mt[!exclude_agg]
  
  #Exclude the elements 
  current_agg <- current_agg[!exclude_agg]
  
  #Identify the mapping units within the major type
  unique_mu <- unique(current_agg)
  
  #Create an empty list
  current_lec_list <- list()
  
  #Loop over all mapping units within the major type
  for (i in seq_along(unique_mu)) {
    
    #Choose mapping unit
    first_mu <- current_agg == unique_mu[i]
    
    #Choose the LEC data
    current_lec <- lec_list[current_mt[first_mu]]
    
    #Find unique elements in the list
    current_lec_list[[i]] <- unique(unlist(current_lec))
    
    #If there elements that contain values
    if(any(!is.na(current_lec))) {
      
      #Aggregate the LEC steps
      current_lec_list[[i]] <- aggregate.lec.vector(list(current_lec_list[[i]]))
    }
  }
  
  #Rename list
  names(current_lec_list) <- paste0(mt, "-", seq_along(unique_mu))
  
  #Return the data frame
  return(current_lec_list)
}

#Define function to aggregate lists with LECs
aggregate.list <- function(units, lec_list, agg_unit, mt) {
  
  #Choose major type
  current_mt <- which(units$mt == mt)
  
  #Choose the current aggregation
  current_agg <- units[current_mt, agg_unit]
  
  #Identify elements with * or #
  exclude_agg <- grepl("[*#]", current_agg)
  
  #Exclude the elements 
  current_mt <- current_mt[!exclude_agg]
  
  #Exclude the elements 
  current_agg <- current_agg[!exclude_agg]
  
  #Identify the mapping units within the major type
  unique_mu <- unique(current_agg)
  
  #Create an empty list
  current_lec_list <- list()
  
  #Loop over all mapping units within the major type
  for (i in seq_along(unique_mu)) {
    
    #Choose mapping unit
    first_mu <- current_agg == unique_mu[i]
    
    #Choose the LEC data
    current_lec <- lec_list[current_mt[first_mu]]
    
    #Find unique elements in th list
    current_lec_list[[i]] <- unique(unlist(current_lec))
  }
  
  #Rename list
  names(current_lec_list) <- paste0(mt, "-", seq_along(unique_mu))
  
  #Return the data frame
  return(current_lec_list)
}
