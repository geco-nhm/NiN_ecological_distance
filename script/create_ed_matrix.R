

# Import data, create functions, and clean master data frame ----
{
  
  #Import library
  library(readxl)
  library(pbapply)
  library(parallel)

  source("script/ed_functions.R")
  source("script/ed_decomposition.R")
  source("script/type_profiles.R")
  source("script/ed_levels.R")

  #Run this script from the repository root
  dir.create("output", showWarnings = FALSE)

  #Import master data file
  data_0005 <- read_xlsx(path = "input/NiN_3.0.1_Master_Clean.xlsx", sheet = "Typer", col_types = "text")
  
  #Import data for criteria
  criteria_ht <- read.csv("input/NiN3_categorical_variables.csv")
  
  #Convert to data frame
  data_0005 <- as.data.frame(data_0005)
  
  #Fix error
  data_0005[which(data_0005[,"6 kat3"] == "NB"),"6 kat3"] <- "MB"
  
  #Choose ground and bottom systems
  data_0005 <- data_0005[data_0005[,"6 kat3"] == "MB",]
  
  #Remove empty rows
  data_0005 <- data_0005[!is.na(data_0005$kortkode),]
  
  #Create a column for major type group
  data_0005$htg <- substr(x = data_0005$kortkode, start = 1, stop = 1)
  
  #Create a column for process category
  data_0005$pc <- substr(x = data_0005$kortkode, start = 2, stop = 2)
  
  #Create a column for major type
  data_0005$ht <- sub(pattern = "-.*", replacement = "", x = data_0005$kortkode)
  
  #Create a column for NiN 2 major type
  data_0005$nin2_ht <- sub(pattern = "-.*", replacement = "", x = data_0005$`F3MR-VS`)
  
  #Create a column for NiN 2 major type group
  data_0005$nin2_htg <- substr(x = data_0005$`F3MR-VS`, start = 1, stop = 1)
  
  #Create a column for NiN 2 mapping unit
  data_0005$nin2_mu <- sub(pattern = ".*-", replacement = "", x = data_0005$`F3MR-VS`)
  
  #Rename columns
  colnames(data_0005) <- c("long_code","code","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","def","17","nin2_code",
                      "18","19","mu","scale_005","20","21","scale_020","22","23","scale_050","24","25",
                      "comments","htg","pc","ht","nin2_ht","nin2_htg","nin2_mu")
  
  #Repair the LC01-05 separator and exclude KA_w wherever its token occurs.
  data_0005$def <- clean.lec.definitions(data_0005$def)

  #Convert workbook shorthand to explicit basic steps before parsing LECs.
  data_0005$def <- expand.lower.steps(data_0005$def)
  
  #Create data frame for dLECs
  data_0005_dlec <- data_0005[grepl(pattern = "NA-", x = data_0005$code), ]
  
  #Remove major type group and major type rows
  data_0005 <- data_0005[!grepl(pattern = "NA-", x = data_0005$code),]
  
  #Retain the desired columns
  units_0005 <- data_0005[,c("code","htg","ht","mu","pc","scale_005","scale_020","scale_050","nin2_htg","nin2_ht","nin2_mu")]
  
  #Remove zeros that come right before another number
  units_0005[,c("scale_005","scale_020","scale_050")] <- as.data.frame(lapply(units_0005[,c("scale_005","scale_020","scale_050")], function(x) gsub("0(?=[0-9])", "", x, perl = TRUE)))
}


#Master data frame
data_0005

#Data frame for criteria
criteria_ht

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
  dlec_ht <- retain.dlecs(data_0005_dlec)
  
  #Remove "NA-" from the string
  names(dlec_ht) <- sub(pattern = "NA-", replacement = "", x = names(dlec_ht))
  
  #Keep only elements with one character
  lec_htg <- dlec_ht[nchar(names(dlec_ht)) == 1]
  
  #Create list with elements corresponding to major types
  lec_htg <- strsplit(x = lec_htg, split = ",")
  
  #Apply function to remove everything after "_" in each element
  lec_htg <- lapply(lec_htg, function(x) sub(" ", "", x))
  
  #Remove elements with only one character (major-type groups)
  dlec_ht <- dlec_ht[nchar(names(dlec_ht)) > 1]
  
  #Create list with elements corresponding to major types
  dlec_ht <- strsplit(x = dlec_ht, split = ",")
  
  #Apply function to remove spaces
  dlec_ht <- lapply(dlec_ht, function(x) sub(" ", "", x))
  
  #Create a vector from the list
  dlec_strings <- unlist(dlec_ht)
  
  #Remove spaces
  dlec_strings <- sub(pattern = " ", replacement = "", x = dlec_strings)
  
  #Remove everything after the underscore
  dlec_strings <- sub(pattern = "_.*", replacement = "", x = dlec_strings)
  
  #Identify unique LECs
  unique_dlec <- unique(dlec_strings)
  
  #Identify overall unique LECs
  unique_strings <- unique(c(unique_lec, unique_dlec))
  
  
  
  #Identify unique major types
  unique_ht <- unique(units_0005$ht)
  
  #Apply function to remove everything after "_" in each element
  dlec_lec <- lapply(dlec_ht, function(x) sub("_.*", "", x))
  
  #Apply the function for each major type
  dlec_0005 <- lapply(seq_along(unique_ht), function(i) {
    
    #Select the current major type, hLECs, and dLECs
    current_ht <- which(units_0005$ht %in% unique_ht[i])
    current_hlec <- hlec_0005[current_ht]
    current_dlec <- dlec_ht[unique_ht[i]]
    
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
  hlec_dlec_0005 <- lapply(seq_along(unique_ht), function(i) {
    
    #Select the current major type, hLECs, and dLECs
    current_ht <- which(units_0005$ht %in% unique_ht[i])
    current_hlec <- hlec_0005[current_ht]
    current_dlec <- dlec_lec[unique_ht[i]]
    
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
  criteria_list <- lapply(1:nrow(criteria_ht), function(i) {
    
    #Find the relevnat major type
    current_ht <- which(units_0005[,"ht"] %in% criteria_ht[i,"ht"])
    
    #Create a matrix with the criteria for the relevant major type
    current_data <- matrix(data = criteria_ht[i,],
                           nrow = length(current_ht),
                           ncol = ncol(criteria_ht),
                           byrow = TRUE)
    
    #Convert the matrix to a data frame
    current_data <- as.data.frame(current_data)
    
    #Rename the columns
    colnames(current_data) <- colnames(criteria_ht)
    
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
lec_htg

#List of dLECs with steps for each major type (to be taken into account between major types)
dlec_ht


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
  for (i in seq_along(dlec_ht)) {
    
    #Identify the current major type
    current_ht <- names(dlec_ht)[i]
    
    #Identify the current mapping units
    current_mu <- which(units_0005$ht %in% current_ht)
    
    #Identify the relevant LECs
    #Remove spaces
    current_lec <- sub(pattern = " ", replacement = "", x = dlec_ht[[i]])
    
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
  for (i in seq_along(lec_htg)) {
    
    #Identify the current major type
    current_htg <- names(lec_htg)[i]
    
    #Identify the current mapping units
    current_mu <- which(units_0005$htg %in% current_htg)
    
    #Identify the relevant LECs
    #Remove spaces
    current_lec <- sub(pattern = " ", replacement = "", x = lec_htg[[i]])
    
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



# Export all supported levels through the same preparation/calculation path ----
base <- list(units = units_0005, lec = lecs_0005, sources = lec_sources_0005,
             criteria_data = criteria_0005, hlec_dlec = hlec_dlec_0005,
             hlec = hlec_0005, dlec = dlec_0005, dlec_without = dlec_without_0005,
             correlation_lec = cor_lec)
minor_counts <- count.ht.steps(base$units, base$lec)
shared_reference <- create.shared.reference(lec_steps)
dir.create("output/shared", recursive = TRUE, showWarnings = FALSE)
reference_con <- gzfile("output/shared/lec_reference_steps.csv.gz", "wt")
tryCatch(write.csv(shared_reference, reference_con, row.names = FALSE), finally = close(reference_con))
arguments <- commandArgs(trailingOnly = TRUE)
profiles_only <- "--profiles-only" %in% arguments
selection <- grep("^--levels=", arguments, value = TRUE)
levels <- if (length(selection)) strsplit(sub("^--levels=", "", selection[1]), ",")[[1]] else c("005", "020", "050", "gt", "ht", "htg")
if (!length(levels) || any(!levels %in% c("005", "020", "050", "gt", "ht", "htg"))) stop("Choose levels from 005,020,050,gt,ht,htg")
for (level in unique(levels)) {
  output_dir <- file.path("output", level)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Prepare the information available at the requested classification level
  if (level %in% c("ht", "htg")) {
    data <- prepare.ed.hierarchy(level, base, dlec_ht, lec_htg, criteria_ht, lec_steps)
  } else {
    data <- prepare.ed.level(level, base, lec_steps, minor_counts)
  }
  message("Level ", level, ": ", nrow(data$units), " types")
  profiles <- create.type.profiles(
    units = data$units, lec = data$lec, sources = data$sources,
    criteria_data = data$criteria_data, ht_specific_list = data$ht_specific_list,
    shared_reference = shared_reference,
    ht_specific_number_minor_types = data$ht_specific_number_minor_types,
    ht_specific_number_mu = data$ht_specific_number_mu
  )
  profile_con <- gzfile(file.path(output_dir, paste0("type_profiles_", level, ".csv.gz")), "wt")
  tryCatch(write.csv(profiles, profile_con, row.names = FALSE), finally = close(profile_con))
  if (!profiles_only) {
    do.call(write.ed.outputs, c(data[setdiff(names(data), "sources")], list(
      matrix_path = file.path(output_dir, paste0("ed_matrix_", level, ".csv.gz")),
      decomposition_path = file.path(output_dir, paste0("ed_decomposition_", level, ".csv.gz")),
      component_path = file.path(output_dir, paste0("ed_components_", level, ".csv.gz")),
      workers = as.integer(Sys.getenv("NIN_WORKERS", "1"))
    )))
  }
}
