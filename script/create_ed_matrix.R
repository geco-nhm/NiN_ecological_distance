

# Import data, create functions, and clean master data frame ----
{
  
  #Import library
  library(readxl)
  library(pbapply)
  library(parallel)

  source("script/ed_functions.R")
  source("script/ed_decomposition.R")
  source("script/type_profiles.R")

  #Run this script from the repository root
  dir.create("output", showWarnings = FALSE)

  #Import master data file
  data_0005 <- read_xlsx(path = "input/NiN_3.0.1_Master_Clean.xlsx", sheet = "Typer", col_types = "text")
  
  #Import data for criteria
  criteria_mt <- read.csv("input/NiN3_categorical_variables.csv")
  
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
  data_0005$nin2_mt <- sub(pattern = "-.*", replacement = "", x = data_0005$`F3MR-VS`)
  
  #Create a column for NiN 2 major type group
  data_0005$nin2_mtg <- substr(x = data_0005$`F3MR-VS`, start = 1, stop = 1)
  
  #Create a column for NiN 2 mapping unit
  data_0005$nin2_mu <- sub(pattern = ".*-", replacement = "", x = data_0005$`F3MR-VS`)
  
  #Rename columns
  colnames(data_0005) <- c("long_code","code","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","def","17","nin2_code",
                      "18","19","mu","scale_005","20","21","scale_020","22","23","scale_050","24","25",
                      "comments","mtg","pc","mt","nin2_mt","nin2_mtg","nin2_mu")
  
  #Repair the LC01-05 separator and exclude KA_w wherever its token occurs.
  data_0005$def <- clean.lec.definitions(data_0005$def)

  #Convert workbook shorthand to explicit basic steps before parsing LECs.
  data_0005$def <- expand.lower.steps(data_0005$def)
  
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
  
  #Record the source of each effective range before filling inherited values.
  lec_sources_0005 <- as.data.frame(ifelse(
    is.na(as.matrix(lecs_0005)), NA_character_, "individual"
  ), stringsAsFactors = FALSE, check.names = FALSE)

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
  
  newly_defined <- is.na(as.matrix(lec_sources_0005)) & !is.na(as.matrix(lecs_0005))
  lec_sources_0005[newly_defined] <- "major_type"

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
  
  
  
  newly_defined <- is.na(as.matrix(lec_sources_0005)) & !is.na(as.matrix(lecs_0005))
  lec_sources_0005[newly_defined] <- "major_type_group"

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


# Export type profiles, and optionally calculate distances and decompositions ----
shared_reference <- create.shared.reference(lec_steps)
write.csv(shared_reference, "output/lec_reference_steps.csv", row.names = FALSE)
profiles_only <- "--profiles-only" %in% commandArgs(trailingOnly = TRUE)
for (scale in c("005", "020")) {
  profile_sources <- do.call(rbind, lapply(unique_mt, function(mt) {
    aggregate.profile.sources(units_0005, lec_sources_0005, paste0("scale_", scale), mt)
  }))
  profiles <- create.type.profiles(
    units = get(paste0("units_", scale)), lec = get(paste0("lecs_", scale)),
    sources = profile_sources, criteria_data = get(paste0("criteria_", scale)),
    mt_specific_list = get(paste0("mt_specific_", scale)),
    shared_reference = shared_reference,
    mt_specific_number_minor_types = mt_specific_number_0005,
    mt_specific_number_mu = get(paste0("mt_specific_number_", scale))
  )
  write.csv(profiles, paste0("output/type_profiles_", scale, ".csv"), row.names = FALSE)
  if (!profiles_only) write.ed.outputs(
    units = get(paste0("units_", scale)),
    lec = get(paste0("lecs_", scale)),
    mt_specific_list = get(paste0("mt_specific_", scale)),
    hlec_dlec = get(paste0("hlec_dlec_", scale)),
    hlec = get(paste0("hlec_", scale)),
    dlec = get(paste0("dlec_", scale)),
    dlec_without = get(paste0("dlec_without_", scale)),
    criteria_data = get(paste0("criteria_", scale)),
    correlation_lec = cor_lec,
    mt_specific_number_minor_types = mt_specific_number_0005,
    mt_specific_number_mu = get(paste0("mt_specific_number_", scale)),
    matrix_path = paste0("output/ed_matrix_", scale, ".csv"),
    decomposition_path = paste0("output/ed_decomposition_", scale, ".csv"),
    workers = as.integer(Sys.getenv("NIN_WORKERS", "1"))
  )
}
