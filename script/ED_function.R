###################################################
# Function for computing Ecological Distance (ED) #
###################################################

# by Adam E. Naas, revised by Eva Lieungh

# check your working directory:
# it should be <your local path>/NiN_ecological_distance/script
getwd()

#Function that calculates the absolute difference between mapping units
absolute.distance <- function(data) {
  
  #Compute the Manhattan distance (absolute distances) between all mapping units
  if(nrow(data) > 1) {
    distance <- dist(x = data, method = "manhattan", diag = TRUE, upper = TRUE)
  }
  #Create a data frame with one column for major types with only one mapping unit
  else {
    distance <- data.frame(0)
    colnames(distance) <- 1
  }
  
  #Convert the crossdist object to a matrix
  distance <- as.matrix(distance)
  
  #Convert the crossdist object to a data frame
  distance <- as.data.frame(distance)
  
  return(distance)
}

#Function that calculates the absolute difference between mapping units
custom.distance <- function(x, y, exclude_irrelevant) {
  
  if(exclude_irrelevant == TRUE) {
    
    #Identify "irrelevant columns" (i.e., LCE columns that sum to zero)
    non_zero_x <- which(colSums(x) != 0)
    non_zero_y <- which(colSums(y) != 0)
    
    #Identify columns that are relevant to both mapping units
    matching_cols <- match(non_zero_x, non_zero_y)
    
    #Remove NA's
    matching_cols <- matching_cols[!is.na(matching_cols)]
    
    # If any columns are relevant, calculate the absolute difference for those mapping units
    if (is.numeric(matching_cols)) {
      distance <- proxy::dist(x[,matching_cols], y[,matching_cols], method = "manhattan", diag = TRUE, upper = TRUE)
    }
    # If all columns are zero, return a distance of 0
    else {
      distance <- 0 
    }
  }
  else {
    distance <- proxy::dist(x, y, method = "manhattan", diag = TRUE, upper = TRUE)
  }
  
  #Convert the crossdist object to a data frame
  distance <- as.data.frame.matrix(distance)
  
  #Replace NA with zero (may happen if there are only irrelevant columns)
  if(any(is.na(distance))) {
    distance[is.na(distance)] <- 0
  }
  
  #Rename the columns
  colnames(distance) <- 1:ncol(distance)
  
  #Return the data frame
  return(distance)
}

#Function that calculates ED between one major type and the rest
compute.ed <- function(majortype_indices, ed_within_df) {
  
  #Generate indices to apply the distance calculation to
  index_pairs <- expand.grid(1:n_mtypes, majortype_indices)
  
  #Apply the distance calculation to the indexed elements in the list (LCE with zero values are irrelevant)
  ed_bs <- mapply(function(i, j) {
    custom.distance(x = bs_list[[i]], y = bs_list[[j]], exclude_irrelevant = TRUE)
  }, index_pairs[[1]], index_pairs[[2]], SIMPLIFY = FALSE)
  
  #Apply the distance calculation to the indexed elements in the list 
  ed_dlce <- mapply(function(i, j) {
    custom.distance(x = dlce_list[[i]], y = dlce_list[[j]], exclude_irrelevant = FALSE)
  }, index_pairs[[1]], index_pairs[[2]], SIMPLIFY = FALSE)
  
  # Add ED from basic steps and defining LCEs
  ed_between <- mapply(function(x, y) {
    x + y
  }, ed_bs, ed_dlce, SIMPLIFY = FALSE)
  
  #Add 2 ED units if the major types belong to different major-type groups
  ed_between <- lapply(seq_along(ed_between), function(i) {
    if (major_type_group[majortype_indices] != major_type_group[i]) {
      ed_between[[i]] <- ed_between[[i]] + 2
      
      #If one of the major types are terrestrial subtract ED units if the mapping unit is influenced by water
      if(major_type_group[majortype_indices] == 1 || major_type_group[i] == 1) {
        
        #Subtract ED units if the mapping unit is influenced by spring water, inundation, or water disturbance
        subtraction <- rowSums(dlce_list[[i]][,c(4,6,11)])
        
        #Subtract one ED unit at maximum 
        ed_between[[i]] <- ed_between[[i]] - pmin(1, subtraction)
      }
    }
    return(ed_between[[i]])
  })
  
  #Add 2 ED units per anthropogenic influence step
  ed_between <- lapply(seq_along(ed_between), function(i) {
    
    #Calculate the absolute number of steps of anthropogenic influence
    difference <- abs(anthropogenic_influence[majortype_indices] - anthropogenic_influence[i])
    
    #Add the number of steps times two
    ed_between[[i]] <- ed_between[[i]] + difference * 2
    
    return(ed_between[[i]])
  })
  
  #Add 2 ED units if the structuring species groups differ between major types
  ed_between <- lapply(seq_along(ed_between), function(i) {
    if (structuring_species[majortype_indices] != structuring_species[i]) {
      ed_between[[i]] <- ed_between[[i]] + 2
    }
    return(ed_between[[i]])
  })
  
  #Add 1 ED unit if the major types are conditional on different LCEs
  ed_between <- lapply(seq_along(ed_between), function(i) {
    if (factor_LCE[majortype_indices] != factor_LCE[i]) {
      ed_between[[i]] <- ed_between[[i]] + 1
    }
    return(ed_between[[i]])
  })
  
  #Add 1 ED unit if strongly modified major types are conditional on different LCEs
  ed_between <- lapply(seq_along(ed_between), function(i) {
    
    #If statement to first check that the major type is strongly modified 
    if(!is.na(strong_LCE[majortype_indices]) && !is.na(strong_LCE[i])) {
      
      #Then check whether major types are conditional on different LCEs
      if (strong_LCE[majortype_indices] != strong_LCE[i]) {
        ed_between[[i]] <- ed_between[[i]] + 1
      }
    }
    
    return(ed_between[[i]])  # Return the modified or unmodified data frame
  })
  
  #Replace the EDs computed within the majortype with the already computed ones
  ed_between[[majortype_indices]] <- ed_within_df[[majortype_indices]]
  
  #Bind the data frames together
  data <- do.call(rbind, ed_between)
  
  #Reset the row names
  rownames(data) <- NULL
  
  return(data)
}

# save function object as RData
saveRDS(absolute.distance, "../absolute_distance_function.RData")

# save function object as RData
saveRDS(custom.distance, "../custom_distance_function.RData")

# save function object as RData
saveRDS(compute.ed, "../compute_ED_function.RData")
