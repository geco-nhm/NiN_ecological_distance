###################################
# ecological distance calculation #
###################################

# by Adam E. Naas, revised by Eva Lieungh

# this script ...

# check your working directory:
# it should be <your local path>/NiN_ecological_distance/script
getwd()

# Import libraries
library(readxl)
library(proxy)

#Specify the number of major types
n_mtypes <- 60

# Make vectors for different rules to apply
{
  #Structuring species group
  structuring_species <- numeric(n_mtypes)
  structuring_species[c(4,30,47,53)] <- 1 # trees
  structuring_species[59] <- 2 # helophytes
  
  #Anthropogenic influence
  anthropogenic_influence <- numeric(n_mtypes)
  anthropogenic_influence[c(1:30,46:53,59:60)] <- 1 # natural / weak influence
  anthropogenic_influence[c(31:34,54:55)] <- 2 # semi-natural / clear influence
  anthropogenic_influence[c(35:45,56:58)] <- 3 # strongly modified
  
  #Major type group
  major_type_group <- numeric(n_mtypes)
  major_type_group[1:45] <- 1 # non-wetland
  major_type_group[46:58] <- 2 # wetland
  major_type_group[59:60] <- 3 # limnic
  
  #Factor LCE
  factor_LCE <- numeric(n_mtypes)
  factor_LCE[c(25:29,48,53)] <- 1:7
  
  #Clearly modified LCE
  clear_LCE <- rep(NA, n_mtypes)
  clear_LCE[c(31,34)] <- c(1,2) # boreal and coastal heaths
  
  #Strongly modified LCE
  strong_LCE <- rep(NA, n_mtypes)
  strong_LCE[c(35:45,56:58)] <- c(5,6,7,5,8,9,10,11,11,12,12,13,14,15)
}

# read in function for computing ED
compute.mode <- readRDS("../compute_mode_function.RData")

# read in function for calculating the absolute difference between mapping units
distance.between <- readRDS("../distance_between_function.RData")

# read in function for computing ED
compute.ed.major <- readRDS("../compute_ED_major_function.RData")

#Create lists for storing excel sheets with LCE values for the mapping units
dlce_list <- list()

#Loop through all excel sheets
for (i in 1:n_mtypes) {

  #Import data frames for defining LCEs, soil and forest line
  dlce_list[[i]] <- as.data.frame(read_xlsx("../excel_files/sLKM5.xlsx", sheet = i))
  
  #Compute the mode column-wise to create data frames for dLCEs for each major type
  dlce_list[[i]] <- apply(X = dlce_list[[i]], MARGIN = 2, FUN = compute.mode)
}

#Compute ED between major types
list_ed_major <- lapply(1:n_mtypes, function(i) compute.ed.major(i))

#Bind data frames together
ed_major <- do.call(cbind, list_ed_major)

#Round down to the nearest integer
ed_major <- floor(ed_major)

#Rename columns
typenames <- c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16","T17","T18","T19","T20","T21","T22","T23","T24","T25","T26",
               "T27","T28","T29","T30","T31","T32","T33","T34","T35","T36","T37","T38","T39","T40","T41","T42","T43","T44","T45","V1","V2","V3","V4","V5","V6","V7",
               "V8","V9","V10","V11","V12","V13","L4","L")
colnames(ed_major) <- typenames
rownames(ed_major) <- typenames

#Save ED matrix
write.csv(ed_major,"../matrices/ED_major.csv")

#Load old ED matrix
ed_old <- as.data.frame(read_xlsx("../matrices/MT.xlsx"))

#Compare results
ed_old - ed_major
