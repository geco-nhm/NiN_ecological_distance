

# Import data, create functions, and clean master data frame ----
{
  
  #Import library
  library(readxl)
  library(pbapply)
  library(parallel)
  
  #Set working directory
  setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Tolga/NiN_ecological_distance/NiN_version_3/excel_files")
  
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
        
        #Converting the characters to numbers
        mt2_values <- unlist(lapply(seq_along(split_vector), function(i) convert.bs(split_vector[[i]], i)))
        
        #Split the vector into vectors with single characters in a list
        split_mu <- unlist(strsplit(mu2, ""))
        
        #Identifying the major type specific steps the first mapping unit falls in
        mu_index <- which(unlist(split_vector) %in% split_mu)
        
        #Taking the mean of the value(s)
        mt2 <- mean(mt2_values[mu_index])
        
        #Split the vector into vectors with single characters in a list
        split_mu <- unlist(strsplit(mu1, ""))
        
        #Identifying the major type specific steps the second mapping unit falls in
        mu_index <- which(unlist(split_vector) %in% split_mu)
        
        #Taking the mean of the value(s)
        mt1 <- mean(mt2_values[mu_index])
        
        #Compute the absolute difference between
        ed_distance <- abs(mt2 - mt1)
        
      }
      
      #Return the ecological distance
      return(ed_distance)
    }
  }
  
  #Define function to compute ED for any pair of mapping units
  compute.ed.ultimate <- function(row, col, units, lec, mt_specific_list, hlec_dlec, hlec, dlec, dlec_without, criteria_data, correlation_lec, mt_specific_number_minor_types, mt_specific_number_mu) {
    
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
    
    #Calculate ED between the mapping units within major types
    if(units[row,"mt"] == units[col,"mt"]) {
      
      #Increase the ecological distance with the calculated increment
      ed_distance <- ed_distance + calculate.ed.within(row, col, lec, mt_specific_list, correlation_lec, row_adjustment, col_adjustment)
      
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
      ed_distance <- ed_distance + sum(abs(current_row_criteria[relevant_criteria] - current_col_criteria[relevant_criteria]))
      
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
      
      #Identify the shared LECs
      shared_lecs <- current_row_hlec_dlec[current_row_hlec_dlec %in% current_col_hlec_dlec]
      
      #If there are no shared LECs
      if(length(shared_lecs) == 0) {
        
        #Add one ED unit
        ed_distance <- ed_distance + 1
      } 
      
      #Add ED between major types
      ed_distance <- ed_distance + calculate.ed.between(row, col, lec, mt_specific_list, current_row_hlec, current_col_hlec, correlation_lec, row_adjustment, col_adjustment)
      
    }
    
    #Return ED
    return(ed_distance)
  }
  
  #Define function to calculate ecological distance between two mapping units
  calculate.ed.between <- function(row, col, lec, mt_specific_list, current_row_lecs, current_col_lecs, correlation_lec, row_adjust, col_adjust) {
    
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
      return(0)
      
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
        total_ed <- aggregate.correlated.lecs(current_correlation_list, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust)
        
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
      
      #Return the sum of ED units for all LECs
      return(sum(total_ed, na.rm = TRUE))
    }
  }
  
  #Define function to calculate ecological distance between two mapping units
  calculate.ed.within <- function(row, col, lec, mt_specific_list, correlation_lec, row_adjust, col_adjust) {
    
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
      total_ed <- aggregate.correlated.lecs(current_correlation_list, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust)
      
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
    
    #Return the sum of ED units for all LECs
    return(sum(total_ed, na.rm = TRUE))
  }
  
  #Define function to aggregate ED from correlated LECs
  aggregate.correlated.lecs <- function(correlated_lec, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust) {
    
    #Create a vector to store ED
    current_ed <- numeric()
    
    #Compute the ED between the mapping units with regard to different LECs, but only keep the ED unit for the maximally distant mapping units
    current_ed <- lapply(correlated_lec, aggregate.max, row_data, col_data, nested_row, nested_col, row_adjust, col_adjust)
    
    #Sum the ED units for the evaluated LECs
    current_ed <- do.call(sum, current_ed)
    
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
      added_ed <- sum(added_ed, na.rm = TRUE)
      
      #Add the ED to the ED for correlated LECs
      total_ed <- current_ed + added_ed
      
    } else {
      
      #If there are no more LECs, just return the ED already calculated
      total_ed <- current_ed
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
    
    #Create a vector with mapping units for the major type
    code <- paste(mt, "-", seq_along(unique(unique_mu)), sep = "")
    
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
  
  
  
  
  #Import master data file
  data_0005 <- read_xlsx(path = "NiN3.0_SplitVar.xlsx", sheet = "Typer")
  
  #Import data for criteria
  criteria_mt <- read.csv("NiN3_categorical_variables.csv")
  
  #Convert to data frame
  data_0005 <- as.data.frame(data_0005)
  
  #Fix error
  data_0005[which(data_0005[,"6 kat3"] == "NB"),"6 kat3"] <- "MB"
  
  #Choose ground and bottom systems
  data_0005 <- data_0005[data_0005[,"6 kat3"] == "MB",]
  
  #Remove empty rows
  data_0005 <- data_0005[!is.na(data_0005$kortkode),]
  
  #Create a column for major type group
  data_0005$mtg <- substr(x = data_0005$kortkode, start = 1, stop = 1)
  
  #Create a column for process category
  data_0005$pc <- substr(x = data_0005$kortkode, start = 2, stop = 2)
  
  #Create a column for major type
  data_0005$mt <- sub(pattern = "-.*", replacement = "", x = data_0005$kortkode)
  
  #Create a column for NiN 2 major type
  data_0005$nin2_mt <- sub(pattern = "-.*", replacement = "", x = data_0005$`NiN 2 kode`)
  
  #Create a column for NiN 2 major type group
  data_0005$nin2_mtg <- substr(x = data_0005$`NiN 2 kode`, start = 1, stop = 1)
  
  #Create a column for NiN 2 mapping unit
  data_0005$nin2_mu <- sub(pattern = ".*-", replacement = "", x = data_0005$`NiN 2 kode`)
  
  #Rename columns
  colnames(data_0005) <- c("long_code","code","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","def","17","nin2_code",
                      "18","19","mu","scale_005","20","21","scale_020","22","23","scale_050","24","25",
                      "comments","mtg","pc","mt","nin2_mt","nin2_mtg","nin2_mu")
  
  #Remove KA_w from the definitions
  data_0005$def <- gsub(", \\[KA_w\\]", "", data_0005$def)
  
  #Create data frame for dLECs
  data_0005_dlec <- data_0005[grepl(pattern = "NA-", x = data_0005$code), ]
  
  #Remove major type group and major type rows
  data_0005 <- data_0005[!grepl(pattern = "NA-", x = data_0005$code),]
  
  #Retain the desired columns
  units_0005 <- data_0005[,c("code","mtg","mt","mu","pc","scale_005","scale_020","scale_050","nin2_mtg","nin2_mt","nin2_mu")]
  
  #Remove zeros that come right before another number
  units_0005[,c("scale_005","scale_020","scale_050")] <- as.data.frame(lapply(units_0005[,c("scale_005","scale_020","scale_050")], function(x) gsub("0(?=[0-9])", "", x, perl = TRUE)))
}


#Master data frame
data_0005

#Data frame for criteria
criteria_mt

#Data frame with all units and aggregations
units_0005



# Create data frames and lists for LEC values ----
{
  
  #Only retain what is inside brackets
  blec_0005 <- gsub(".*\\[([^]]+)\\].*", "\\1", data_0005$def)
  
  #Split LECs into strings for each mapping unit
  blec_0005 <- strsplit(x = blec_0005, split = ",")
  
  # Apply function to remove everything after "_" in each element
  blec_0005 <- lapply(blec_0005, function(x) sub("_.*", "", x))
  
  # Apply function to remove everything after "_" in each element
  blec_0005 <- lapply(blec_0005, function(x) sub(" ", "", x))
  
  #Rename list
  names(blec_0005) <- units_0005$code
  
  
  
  
  
  #Split LECs into strings for each mapping unit
  hlec_0005 <- strsplit(x = data_0005$def, split = ",")
  
  #Remove bLECs
  hlec_0005 <- lapply(hlec_0005, remove.blec)
  
  #Apply function to replace "-" with "_"
  hlec_0005 <- lapply(hlec_0005, function(x) sub("-", "_", x))
    
  #Apply function to remove everything after "_" in each element
  hlec_0005 <- lapply(hlec_0005, function(x) sub("_.*", "", x))
  
  #Apply function to remove spaces
  hlec_0005 <- lapply(hlec_0005, function(x) sub(" ", "", x))
  
  #Rename list
  names(hlec_0005) <- units_0005$code
  
  
  #Split LECs separated by ","
  lec_0005 <- strsplit(x = data_0005$def, split = ",")
  
  #Apply function to remove spaces
  lec_0005 <- lapply(lec_0005, function(x) sub(" ", "", x))
  
  #Apply function to remove everything square bracket
  lec_0005 <- lapply(lec_0005, function(x) sub("]", "", x))
  
  #Apply function to remove square bracket
  lec_0005 <- lapply(lec_0005, function(x) sub("\\[", "", x))
  
  #Rename list
  names(lec_0005) <- units_0005$code
  
  
  
  #Create a vector from the list
  lec_strings <- unlist(lec_0005)
  
  #Remove everything after the underscore
  lec_strings <- sub(pattern = "_.*", replacement = "", x = lec_strings)
  
  #Remove everything after the hyphen
  lec_strings <- sub(pattern = "-.*", replacement = "", x = lec_strings)
  
  #Remove brackets
  lec_strings <- sub(pattern = "\\[", replacement = "", x = lec_strings)
  
  #Remove incomplete data
  lec_strings <- lec_strings[complete.cases(lec_strings)]
  
  #Identify unique LECs
  unique_lec <- unique(lec_strings)
  
  
  
  
  #Apply the function
  dlec_mt <- retain.dlecs(data_0005_dlec)
  
  #Remove "NA-" from the string
  names(dlec_mt) <- sub(pattern = "NA-", replacement = "", x = names(dlec_mt))
  
  #Keep only elements with one character
  lec_mtg <- dlec_mt[nchar(names(dlec_mt)) == 1]
  
  #Create list with elements corresponding to major types
  lec_mtg <- strsplit(x = lec_mtg, split = ",")
  
  #Apply function to remove everything after "_" in each element
  lec_mtg <- lapply(lec_mtg, function(x) sub(" ", "", x))
  
  #Remove elements with only one character (major-type groups)
  dlec_mt <- dlec_mt[nchar(names(dlec_mt)) > 1]
  
  #Create list with elements corresponding to major types
  dlec_mt <- strsplit(x = dlec_mt, split = ",")
  
  #Apply function to remove spaces
  dlec_mt <- lapply(dlec_mt, function(x) sub(" ", "", x))
  
  #Create a vector from the list
  dlec_strings <- unlist(dlec_mt)
  
  #Remove spaces
  dlec_strings <- sub(pattern = " ", replacement = "", x = dlec_strings)
  
  #Remove everything after the underscore
  dlec_strings <- sub(pattern = "_.*", replacement = "", x = dlec_strings)
  
  #Identify unique LECs
  unique_dlec <- unique(dlec_strings)
  
  #Identify overall unique LECs
  unique_strings <- unique(c(unique_lec, unique_dlec))
  
  
  
  #Identify unique major types
  unique_mt <- unique(units_0005$mt)
  
  #Apply function to remove everything after "_" in each element
  dlec_lec <- lapply(dlec_mt, function(x) sub("_.*", "", x))
  
  #Apply the function for each major type
  dlec_0005 <- lapply(seq_along(unique_mt), function(i) {
    
    #Select the current major type, hLECs, and dLECs
    current_mt <- which(units_0005$mt %in% unique_mt[i])
    current_hlec <- hlec_0005[current_mt]
    current_dlec <- dlec_mt[unique_mt[i]]
    
    #Merge the hLECs with the dLECs
    current_hlec_dlec <- lapply(current_hlec, function(x) current_dlec[[1]])
    
    #Return the combined hLECs and dLECs
    return(current_hlec_dlec)
  })
  
  #Bind the nested lists together into a single list
  dlec_0005 <- unlist(dlec_0005, recursive = FALSE)
  
  #Apply function to remove everything after "_" in each element
  dlec_without_0005 <- lapply(dlec_0005, function(x) sub("_.*", "", x))
  
  
  
  #Apply function to merge hLECs and dLECs for each major type
  hlec_dlec_0005 <- lapply(seq_along(unique_mt), function(i) {
    
    #Select the current major type, hLECs, and dLECs
    current_mt <- which(units_0005$mt %in% unique_mt[i])
    current_hlec <- hlec_0005[current_mt]
    current_dlec <- dlec_lec[unique_mt[i]]
    
    #Merge the hLECs with the dLECs
    current_hlec_dlec <- lapply(current_hlec, function(x) c(x, current_dlec[[1]]))
    
    #Return the combined hLECs and dLECs 
    return(current_hlec_dlec)
  })
  
  #Bind the nested lists together into a single list
  hlec_dlec_0005 <- unlist(hlec_dlec_0005, recursive = FALSE)
  
  #Apply function to remove everything after "_" in each element
  hlec_dlec_0005 <- lapply(hlec_dlec_0005, unique)
  
  #Remove duplicates and NAs from each element in the list
  hlec_dlec_0005 <- lapply(hlec_dlec_0005, function(x) unique(na.omit(x)))
  
  
  
  #Create list to store criteria for mapping units within every major type
  criteria_list <- lapply(1:nrow(criteria_mt), function(i) {
    
    #Find the relevnat major type
    current_mt <- which(units_0005[,"mt"] %in% criteria_mt[i,"mt"])
    
    #Create a matrix with the criteria for the relevant major type
    current_data <- matrix(data = criteria_mt[i,], 
                           nrow = length(current_mt), 
                           ncol = ncol(criteria_mt), 
                           byrow = TRUE)
    
    #Convert the matrix to a data frame
    current_data <- as.data.frame(current_data)
    
    #Rename the columns
    colnames(current_data) <- colnames(criteria_mt)
    
    #Return the data frame
    return(current_data)
  })
  
  #Bind lists together
  criteria_0005 <- do.call(rbind, criteria_list)
  
  #Add mapping unit codes to the data frame
  criteria_0005 <- cbind(code = units_0005$code, criteria_0005)
  
  #Change the criteria variables into numeric
  for (i in 3:ncol(criteria_0005)) {
    criteria_0005[,i] <- as.numeric(criteria_0005[,i])
  }
  
  #Create list with correlating LECs
  cor_lec <- list(c("HA","HM","HH"),
                  c("HR","HH"))
}

#Data frame with all criteria
criteria_0005

#List of bLECs for each mapping unit (not to be taken into account between major types)
blec_0005

#List of hLECs for each mapping unit (not to be taken into account between major types)
hlec_0005

#List of dLECs for each mapping unit (taken into account between major types)
dlec_0005

#List of hLECs and dLECs combined for each mapping unit (taken into account between major types)
hlec_dlec_0005

#List of dLECs for each major type (to be taken into account between major types)
dlec_without_0005

#List of hLECs, tLECs, and bLECs for each mapping unit (to be taken into account within major types)
lec_0005

#List of LECs for major-type groups (not to be taken into account between major types within the same major type or within major types)
lec_mtg

#List of dLECs with steps for each major type (to be taken into account between major types)
dlec_mt


#All LECs
unique_strings

#Correlating LECs
cor_lec



# Create data frame with LECs for mapping units ----
{
  
  #Create a data frame with mapping units as rows and LECs as columns
  lecs_0005 <- data.frame(matrix(data = NA, nrow = nrow(units_0005), ncol = length(unique_strings)))
  
  #Rename the columns
  colnames(lecs_0005) <- unique_strings
  
  #For hLECs and tLECs
  #Loop through each element in the list
  for (i in seq_along(lec_0005)) {
    
    #Only for elements that contain values
    if(!all(is.na(lec_0005[[i]]))) {
      
      #Identify the relevant LECs
      #Remove spaces
      current_lec <- sub(pattern = " ", replacement = "", x = lec_0005[[i]])
      
      #Replace hyphens with underscores
      current_lec <- gsub("-", "_", current_lec)
      
      #Remove bracket
      current_lec <- sub(pattern = "\\[", replacement = "", x = current_lec)
      
      #Remove bracket
      current_lec <- sub(pattern = "]", replacement = "", x = current_lec)
      
      #Remove everything after the underscore
      duplicated_lec <- sub(pattern = "_.*", replacement = "", x = current_lec)
      
      #Identify duplicated LECs
      first_duplicate <- duplicated(x = duplicated_lec)
      
      #Identify the second duplicate
      second_duplicate <- match(x = duplicated_lec[first_duplicate], table = duplicated_lec)
      
      #Create a new vector with both duplicates
      duplicates <- unique(current_lec[c(second_duplicate, which(first_duplicate))])
      
      #Remove everything before the underscore
      duplicates <- sub(pattern = ".*_", replacement = "", x = duplicates)
      
      #Collapse the string
      duplicates <- paste(duplicates, collapse = "")
      
      #Remove the second duplicate
      current_lec <- current_lec[!first_duplicate]
      
      #Remove everything before the underscore
      current_lec_values <- sub(pattern = ".*_", replacement = "", x = current_lec)
      
      #Give the new aggregated value to the duplicated LEC
      current_lec_values[second_duplicate] <- duplicates
      
      #Remove everything after the hyphen
      current_lec <- sub(pattern = "_.*", replacement = "", x = current_lec)
      
      #Assign the LEC values to the appropriate columns in the data frame
      lecs_0005[i, current_lec] <- current_lec_values
    }
  }
  
  #For dLECs
  #Loop through each element in the list
  for (i in seq_along(dlec_mt)) {
    
    #Identify the current major type
    current_mt <- names(dlec_mt)[i]
    
    #Identify the current mapping units
    current_mu <- which(units_0005$mt %in% current_mt)
    
    #Identify the relevant LECs
    #Remove spaces
    current_lec <- sub(pattern = " ", replacement = "", x = dlec_mt[[i]])
    
    #Remove everything before the underscore
    current_lec_values <- sub(pattern = ".*_", replacement = "", x = current_lec)
    
    #Remove everything after the underscore
    current_lec <- sub(pattern = "_.*", replacement = "", x = current_lec)
    
    #Identify the relevant data
    current_data <- lecs_0005[current_mu, current_lec]
    
    #If there are more than one dLECs it will be a data frame
    if(class(current_data) == "data.frame" && ncol(current_data) > 0) {
      
      #Loop over all LECs
      for (j in 1:ncol(current_data)) {
        
        if(any(is.na(current_data[,j]))) {
          
          #Assign the new LEC values
          current_data[,j][is.na(current_data[,j])] <- current_lec_values[j]
        }
        
      }
      
      #Add the LEC values the data frame
      lecs_0005[current_mu, current_lec] <- current_data
      
    } else {
      
      #Assign the new LEC values
      current_data[!complete.cases(current_data)] <- current_lec_values
      
      #Add the LEC values the data frame
      lecs_0005[current_mu, current_lec] <- current_data
    }
  }
  
  #For major_type groups
  #Loop through each element in the list
  for (i in seq_along(lec_mtg)) {
    
    #Identify the current major type
    current_mtg <- names(lec_mtg)[i]
    
    #Identify the current mapping units
    current_mu <- which(units_0005$mtg %in% current_mtg)
    
    #Identify the relevant LECs
    #Remove spaces
    current_lec <- sub(pattern = " ", replacement = "", x = lec_mtg[[i]])
    
    #Remove everything before the underscore
    current_lec_values <- sub(pattern = ".*_", replacement = "", x = current_lec)
    
    #Remove everything after the underscore
    current_lec <- sub(pattern = "_.*", replacement = "", x = current_lec)
    
    #Identify the relvant data
    current_data <- lecs_0005[current_mu, current_lec]
    
    #If there are more than one LEC it will be a data frame
    if(class(current_data) == "data.frame" && ncol(current_data) > 0) {
      
      #Loop over all LECs
      for (j in 1:ncol(current_data)) {
        
        if(any(is.na(current_data[,j]))) {
          
          #Assign the new LEC values
          current_data[,j][is.na(current_data[,j])] <- current_lec_values[j]
        }
        
      }
      
      #Add the LECs to the data frame
      lecs_0005[current_mu, current_lec] <- current_data
      
    } else {
      
      #Assign the new LEC values
      current_data[!complete.cases(current_data)] <- current_lec_values
      
      #Add the LECs to the data frame
      lecs_0005[current_mu, current_lec] <- current_data
    }
  }
  
  
  
  #Create a vector that contains the entire gradient for each LEC
  #Create a data frame with mapping units as rows and LECs as columns
  lec_steps <- character(length(unique_strings))
  
  #Rename the vector
  names(lec_steps) <- unique_strings
  
  #Loop over all mapping units
  for (i in 1:length(lec_steps)) {
    
    #Remove incomplete rows
    current_lec <- lecs_0005[,i][complete.cases(lecs_0005[,i])]
    
    #If there are any LECs
    if(length(current_lec) > 0) {
      
      #Retain only unique characters
      current_lec <- unique(unlist(strsplit(x = current_lec, split = "")))
      
      #Sort the characters alphabetically
      current_lec <- sort(x = current_lec[current_lec != " "], decreasing = FALSE)
      
      #Sort the characters alphabetically
      lec_steps[i] <- paste(x = current_lec, collapse = "")
    }
  }
  
  #Identify factor LECs
  factor_lec <- grep("[A-Z]", lec_steps)
  
}


#Data frame with ecologial space for every mapping unit
lecs_0005

#Vector with ecological space for each LEC
lec_steps

#Index vector with all factor LECs
factor_lec



# Aggregate from minor types to 1.5000 mapping units ----
{
  
  #Select the desired scale, other possibilities are "scale_020" and "scale_050"
  aggregation_scale <- "scale_005"
  
  #Identify major types
  unique_mt <- unique(units_0005$mt)
  
  #Aggregate the units
  units_005 <- lapply(unique_mt, function(mt) aggregate.scale.units(units_0005, aggregation_scale, mt))
  
  #Create a list from the data frame
  units_005 <- do.call(rbind, units_005) 
  
  #Aggregate the LEC steps
  lecs_005 <- lapply(unique_mt, function(mt) aggregate.scale.lecs(units_0005, lecs_0005, aggregation_scale, mt))
  
  #Create a list from the data frame
  lecs_005 <- do.call(rbind, lecs_005)
  
  #Aggregate the criteria
  criteria_005 <- lapply(unique_mt, function(mt) aggregate.scale.criteria(units_0005, criteria_0005, aggregation_scale, mt))
  
  #Create a list from the data frame
  criteria_005 <- do.call(rbind, criteria_005)
  
  
  #Aggregate lists to 1.5000
  #Aggregate dLECs
  dlec_without_005 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, dlec_without_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  dlec_without_005 <- do.call(c, dlec_without_005)
  
  #Aggregate combined hLECs and dLECs
  hlec_dlec_005 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, hlec_dlec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  hlec_dlec_005 <- do.call(c, hlec_dlec_005)
  
  #Aggregate hLECs
  hlec_005 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, hlec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  hlec_005 <- do.call(c, hlec_005)
  
  #Aggregate bLECs
  blec_005 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, blec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  blec_005 <- do.call(c, blec_005)
  
  
  
  
  #Aggregate dLECs
  dlec_005 <- lapply(unique_mt, function(mt) aggregate.lec.list(units_0005, dlec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  dlec_005 <- do.call(c, dlec_005)
  
  #Aggregate LECs
  lec_005 <- lapply(unique_mt, function(mt) aggregate.lec.list(units_0005, lec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  lec_005 <- do.call(c, lec_005)

}

#Codes for mapping units, major types, and major-type groups for 1.5000
units_005

#LEC values for each mapping unit in 1.5000
lecs_005


# Aggregate from minor types to 1.20000 mapping units ----
{
  
  #Select the desired scale, other possibilities are "scale_020" and "scale_050"
  aggregation_scale <- "scale_020"
  
  #Identify major types
  unique_mt <- unique(units_0005$mt)
  
  #Aggregate the units
  units_020 <- lapply(unique_mt, function(mt) aggregate.scale.units(units_0005, aggregation_scale, mt))
  
  #Create a list from the data frame
  units_020 <- do.call(rbind, units_020) 
  
  #Aggregate the LEC steps
  lecs_020 <- lapply(unique_mt, function(mt) aggregate.scale.lecs(units_0005, lecs_0005, aggregation_scale, mt))
  
  #Create a list from the data frame
  lecs_020 <- do.call(rbind, lecs_020)
  
  #Aggregate the criteria
  criteria_020 <- lapply(unique_mt, function(mt) aggregate.scale.criteria(units_0005, criteria_0005, aggregation_scale, mt))
  
  #Create a list from the data frame
  criteria_020 <- do.call(rbind, criteria_020)
  
  
  #Aggregate lists to 1.5000
  #Aggregate dLECs
  dlec_without_020 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, dlec_without_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  dlec_without_020 <- do.call(c, dlec_without_020)
  
  #Aggregate combined hLECs and dLECs
  hlec_dlec_020 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, hlec_dlec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  hlec_dlec_020 <- do.call(c, hlec_dlec_020)
  
  #Aggregate hLECs
  hlec_020 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, hlec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  hlec_020 <- do.call(c, hlec_020)
  
  #Aggregate bLECs
  blec_020 <- lapply(unique_mt, function(mt) aggregate.list(units_0005, blec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  blec_020 <- do.call(c, blec_020)
  
  
  
  
  #Aggregate dLECs
  dlec_020 <- lapply(unique_mt, function(mt) aggregate.lec.list(units_0005, dlec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  dlec_020 <- do.call(c, dlec_020)
  
  #Aggregate LECs
  lec_020 <- lapply(unique_mt, function(mt) aggregate.lec.list(units_0005, lec_0005, aggregation_scale, mt))
  
  #Unlist the list by one level
  lec_020 <- do.call(c, lec_020)
  
}


#Codes for mapping units, major types, and major-type groups for 1.20000
units_020

#LEC values for each mapping unit in 1.20000
lecs_020

# Identify major type specific groups for each 1.5000 mapping unit ----
{
  
  #Create nested list
  mt_specific_005 <- create.template.list(length(units_005$mt), ncol(lecs_005), units_005$mt, colnames(lecs_005))
  
  #Identify unique mapping units
  unique_codes <- unique(units_005$mt)
  
  #Loop over all major types
  for (k in 1:length(unique_codes)) {
    
    #Select the major type
    current_mt <- unique_codes[k]
    
    #Identify the mapping units for the major type
    current_elements <- which(units_005$mt %in% current_mt)
    
    #Create a data frame for the major type
    current_data <- lecs_005[current_elements,]
    
    #Loop over all LECs
    for (j in 1:ncol(current_data)) {
      
      #Loop over all mapping units
      for (i in current_elements) {
        
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
          
          #Identify the major-type specific steps for the LEC
          mt_specific_005[[i]][[current_lec]] <- add.border.bs(lec_steps[current_lec], mt_specific_steps)
        }
      }
    }
    
    #Print progress bar
    print(k/length(unique_codes))
  }
  
  #Find the number of major-type specific steps for each major type
  mt_specific_number_0005 <- lapply(unique_mt, identify.mt.specific.number, units_0005, lecs_0005)
  
  #Bind the lists to a matrix
  mt_specific_number_0005 <- do.call(rbind, mt_specific_number_0005)
  
  #Convert to data frame
  mt_specific_number_0005 <- as.data.frame(mt_specific_number_0005)
  
  #Rename columns
  colnames(mt_specific_number_0005) <- unique_strings
  
  #Rename rows
  rownames(mt_specific_number_0005) <- unique_mt
  
  
  
  #Find the number of major-type specific steps for each major type
  mt_specific_number_005 <- lapply(unique_mt, identify.mt.specific.number, units_005, lecs_005)
  
  #Bind the lists to a matrix
  mt_specific_number_005 <- do.call(rbind, mt_specific_number_005)
  
  #Convert to data frame
  mt_specific_number_005 <- as.data.frame(mt_specific_number_005)
  
  #Rename columns
  colnames(mt_specific_number_005) <- unique_strings
  
  #Rename rows
  rownames(mt_specific_number_005) <- unique_mt
}

#Nested list with major-type specific groups for each LEC within each major type
mt_specific_005

#Data frame with number of major-type specific steps for 1.500 each major type
mt_specific_number_0005

#Data frame with number of major-type specific steps for 1.5000 for each major type
mt_specific_number_005

# Identify major type specific groups for each 1.20000 mapping unit ----
{
  
  #Create nested list
  mt_specific_020 <- create.template.list(length(units_020$mt), ncol(lecs_020), units_020$mt, colnames(lecs_020))
  
  #Identify unique mapping units
  unique_codes <- unique(units_020$mt)
  
  #Loop over all major types
  for (k in 1:length(unique_codes)) {
    
    #Select the major type
    current_mt <- unique_codes[k]
    
    #Identify the mapping units for the major type
    current_elements <- which(units_020$mt %in% current_mt)
    
    #Create a data frame for the major type
    current_data <- lecs_020[current_elements,]
    
    #Loop over all LECs
    for (j in 1:ncol(current_data)) {
      
      #Loop over all mapping units
      for (i in current_elements) {
        
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
          
          #Identify the major-type specific steps for the LEC
          mt_specific_020[[i]][[current_lec]] <- add.border.bs(lec_steps[current_lec], mt_specific_steps)
        }
      }
    }
    
    #Print progress bar
    print(k/length(unique_codes))
  }
  
  
  #Find the number of major-type specific steps for each major type
  mt_specific_number_020 <- lapply(unique_mt, identify.mt.specific.number, units_020, lecs_020)
  
  #Bind the lists to a matrix
  mt_specific_number_020 <- do.call(rbind, mt_specific_number_020)
  
  #Convert to data frame
  mt_specific_number_020 <- as.data.frame(mt_specific_number_020)
  
  #Rename columns
  colnames(mt_specific_number_020) <- unique_strings
  
  #Rename rows
  rownames(mt_specific_number_020) <- unique_mt
}

#Nested list with major-type specific groups for each LEC within each major type
mt_specific_020

#Data frame with number of major-type specific steps for 1.20000 for each major type
mt_specific_number_020


# Calculate ED for 1.5000 ----

#Find the number of mapping units
n <- nrow(lecs_005)

#Extract data before computing ED to save time
lecs_005_matrix <- as.matrix(lecs_005)
mt_specific_005_matrix <- lapply(1:n, function(i) mt_specific_005[[i]])

#Create a matrix to store ED units for all mapping units
ed_matrix <- matrix(0, nrow = n, ncol = n)

#Loop over all mapping units
for (i in 1:n) {
  for (j in 1:n) {
    
    #Compute ED for all pairs of mapping units
    ed_matrix[i,j] <- compute.ed.ultimate(i, j, units_005, lecs_005_matrix, mt_specific_005, hlec_dlec_005, hlec_005, dlec_005, dlec_without_005, criteria_005, cor_lec, mt_specific_number_0005, mt_specific_number_0005)
    
    #Print progress bar
    print(paste("j =", j, ", i =", i))
  }
}

#Convert to data frame
ed_data_frame <- as.data.frame(ed_matrix)

#Rename columns
colnames(ed_data_frame) <- units_005$code

#Rename rows
rownames(ed_data_frame) <- units_005$code

#Round down to integer values
#ed_data_frame <- floor(ed_data_frame)

# Save ED data frame for 1.5000 to disk ----

#Save csv file
write.csv(ed_data_frame, "ed_matrix_005.csv")


# Calculate ED for 1.20000 ----

#Find the number of mapping units
n <- nrow(lecs_020)

#Extract data before computing ED to save time
lecs_020_matrix <- as.matrix(lecs_020)
mt_specific_020_matrix <- lapply(1:n, function(i) mt_specific_020[[i]])

#Create a matrix to store ED units for all mapping units
ed_matrix <- matrix(0, nrow = n, ncol = n)

#Loop over all mapping units
for (i in 1:n) {
  for (j in 1:n) {
    
    #Compute ED for all pairs of mapping units
    ed_matrix[i,j] <- compute.ed.ultimate(i, j, units_020, lecs_020_matrix, mt_specific_020, hlec_dlec_020, hlec_020, dlec_020, dlec_without_020, criteria_020, cor_lec, mt_specific_number_005, mt_specific_number_020)
    
    #Print progress bar
    print(paste("j =", j, ", i =", i))
  }
}

#Convert to data frame
ed_data_frame <- as.data.frame(ed_matrix)

#Rename columns
colnames(ed_data_frame) <- units_020$code

#Rename rows
rownames(ed_data_frame) <- units_020$code

#Round down to integer values
#ed_data_frame <- floor(ed_data_frame)


# Save ED data frame for 1.20000 to disk ----

#Save csv file
write.csv(ed_data_frame, "ed_matrix_020.csv")



# Paralellization (option) ----

#Compute ED for all pairs of mapping units
#ed_vector <- pbmapply(compute.ed.ultimate, 
#                      row = rep(1:n, each = n), 
#                      col = rep(1:n, times = n),
#                      MoreArgs = list(units_005, lecs_005_matrix, mt_specific_005_matrix, hlec_dlec_005, hlec_005, dlec_005, dlec_without_005, criteria_005, cor_lec, mt_specific_number_0005, mt_specific_number_0005))

#Populate the matrix with the computed ED units
#ed_matrix <- matrix(ed_vector, nrow = n, ncol = n, byrow = TRUE)


#Set up the cluster
#num_cores <- detectCores() - 1
#cl <- makeCluster(num_cores)
#pboptions(type = "timer")

#Export necessary objects and functions to each worker in the cluster
#clusterExport(cl, varlist = c("compute.ed.ultimate", "units_005", "lecs_005_matrix", 
#                              "mt_specific_005_matrix", "hlec_dlec_005", "hlec_005", 
#                              "dlec_005", "dlec_without_005", "criteria_005", "cor_lec", 
#                              "mt_specific_number_0005"))

#Compute ED values for all pairs of mapping units
#ed_vector <- pbmapply(
#  compute.ed.ultimate, 
#  row = rep(1:n, each = n), 
#  col = rep(1:n, times = n),
#  MoreArgs = list(
#    units_005, lecs_005_matrix, mt_specific_005_matrix, hlec_dlec_005,
#    hlec_005, dlec_005, dlec_without_005, criteria_005, cor_lec, 
#    mt_specific_number_0005,mt_specific_number_0005
#  )
#)

#Reshape the result into the matrix
#ed_matrix <- matrix(ed_vector, nrow = n, ncol = n, byrow = TRUE)

#Stop the cluster after computation
#stopCluster(cl)
