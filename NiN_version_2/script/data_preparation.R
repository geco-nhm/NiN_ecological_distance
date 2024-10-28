####################
# data preparation #
####################

# by Adam E. Naas, Eva Lieungh

#Import libraries
library(readxl)
library(writexl)

# check your working directory:
# it should be <your local path>/NiN_ecological_distance/script
getwd()

# Make vectors for different type levels
{
  #Structuring species group
  structuring_species <- numeric(n_mtypes)
  structuring_species[c(4,30,47,53)] <- 1
  structuring_species[59] <- 2
  
  #Anthropogenic influence
  anthropogenic_influence <- numeric(n_mtypes)
  anthropogenic_influence[c(1:30,46:53,59:60)] <- 1
  anthropogenic_influence[c(31:34,54:55)] <- 2
  anthropogenic_influence[c(35:45,56:58)] <- 3
  
  #Major type group
  major_type_group <- numeric(n_mtypes)
  major_type_group[1:45] <- 1
  major_type_group[46:58] <- 2
  major_type_group[59:60] <- 3
  
  #Factor LCE
  factor_LCE <- numeric(n_mtypes)
  factor_LCE[c(25:29,48,53)] <- 1:7
  
  #Strongly modified LCE
  strong_LCE <- rep(NA, n_mtypes)
  strong_LCE[c(35:45,56:58)] <- c(5,6,7,5,8,9,10,11,11,12,12,13,14,15)
}

# Uploading lists with the mapping units and their midpoints 
# along gradients and specifications for principles

## specify path to files (1:5000 scale)
MT_sheet <- "../excel_files/HT5.xlsx" # how were these made?
bT_sheet <- "../excel_files/bT5.xlsx"
sLKM_sheet <- "../excel_files/sLKM5.xlsx"

# ## ALTERNATIVE: specify path to files (1:20 000 scale)
# MT_sheet <- "../excel_files/HT20.xlsx"
# bT_sheet <- "../excel_files/bT20.xlsx"
# sLKM_sheet <- "../excel_files/sLKM20.xlsx"

## make some placeholders for list
MT_list <- numeric()
bT_list <- numeric()
sLKM_list <- numeric()
ED <- list(numeric(),numeric())

## specify number of ecosystem types 
## (60 for 45 terrestrial + 13 wetland + 2 'limnic' 
## types allowed during mapping) - !!! Adam, stemmer dette?^ Nesten
#Specify the number of major types
n_mtypes <- 60


# save data 
saveRDS(ED, "../ED_list.RData")
