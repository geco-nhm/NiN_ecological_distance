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
library(writexl)
library(proxy)

#Specify the number of major types
n_mtypes <- 60

# Make vectors for different rules to apply
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

# read in function for calculating the absolute difference within mapping units
absolute.distance <- readRDS("../absolute_distance_function.RData")

# read in function for calculating the absolute difference between mapping units
custom.distance <- readRDS("../custom_distance_function.RData")

# read in function for computing ED
compute.ed <- readRDS("../compute_ED_function.RData")

#Create lists for storing excel sheets with LCE values for the mapping units
mt_list <- list()
bs_list <- list()
dlce_list <- list()

#Loop through all excel sheets
for (i in 1:n_mtypes) {
  
  #Import data frames for major types
  mt_list[[i]] <- as.data.frame(read_xlsx("../excel_files/HT5.xlsx", sheet = i))
  
  #Import data frames for basic steps (apply weight 1/2)
  bs_list[[i]] <- as.data.frame(read_xlsx("../excel_files/bT5.xlsx", sheet = i)) / 2
  
  #Import data frames for defining LCEs, soil and forest line
  dlce_list[[i]] <- as.data.frame(read_xlsx("../excel_files/sLKM5.xlsx", sheet = i))
}

#Compute ED within major types
ed_within <- lapply(mt_list, absolute.distance)

#Compute ED between major types
list_ed_between <- lapply(1:n_mtypes, function(i) compute.ed(i, ed_within))

#Bind data frames together
ed_between <- do.call(cbind, list_ed_between)

#Round down to the nearest integer
ed_between <- floor(ed_between)

#Rename columns
colnames(ed_between) <- c("T1-C-1",	"T1-C-2",	"T1-C-3",	"T1-C-4",	"T1-C-5",	"T1-C-6",	"T1-C-7",	"T1-C-8",	"T1-C-9",	"T1-C-10",	"T1-C-11",	"T1-C-12",	"T2-C-1",	"T2-C-2",	"T2-C-3",	"T2-C-4",	"T2-C-5",	"T2-C-6",	"T2-C-7",	"T2-C-8",	"T3-C-1",	"T3-C-2",	"T3-C-3",	"T3-C-4",	"T3-C-5",	"T3-C-6",	"T3-C-7",	"T3-C-8",	"T3-C-9",	"T3-C-10",	"T3-C-11",	"T3-C-12",	"T3-C-13",	"T3-C-14",	"T4-C-1",	"T4-C-2",	"T4-C-3",	"T4-C-4",	"T4-C-5",	"T4-C-6",	"T4-C-7",	"T4-C-8",	"T4-C-9",	"T4-C-10",	"T4-C-11",	"T4-C-12",	"T4-C-13",	"T4-C-14",	"T4-C-15",	"T4-C-16",	"T4-C-17",	"T4-C-18",	"T4-C-19",	"T4-C-20",	"T5-C-1",	"T5-C-2",	"T5-C-3",	"T5-C-4",	"T5-C-5",	"T5-C-6",	"T5-C-7",	"T6-C-1",	"T6-C-2",	"T7-C-1",	"T7-C-2",	"T7-C-3",	"T7-C-4",	"T7-C-5",	"T7-C-6",	"T7-C-7",	"T7-C-8",	"T7-C-9",	"T7-C-10",	"T7-C-11",	"T7-C-12",	"T7-C-13",	"T7-C-14",	"T8-C-1",	"T8-C-2",	"T8-C-3",	"T9-C-1",	"T9-C-2",	"T10-C-1",	"T11-C-1",	"T11-C-2",	"T12-C-1",	"T12-C-2",	"T13-C-1",	"T13-C-2",	"T13-C-3",	"T13-C-4",	"T13-C-5",	"T13-C-6",	"T13-C-7",	"T13-C-8",	"T13-C-9",	"T13-C-10",	"T13-C-11",	"T13-C-12",	"T13-C-13",	"T13-C-14",	"T13-C-15",	"T14-C-1",	"T14-C-2",	"T15-C-1",	"T15-C-2",	"T16-C-1",	"T16-C-2",	"T16-C-3",	"T16-C-4",	"T16-C-5",	"T16-C-6",	"T16-C-7",	"T17-C-1",	"T17-C-2",	"T17-C-3",	"T18-C-1",	"T18-C-2",	"T18-C-3",	"T18-C-4",	"T19-C-1",	"T19-C-2",	"T20-C-1",	"T20-C-2",	"T21-C-1",	"T21-C-2",	"T21-C-3",	"T21-C-4",	"T22-C-1",	"T22-C-2",	"T22-C-3",	"T22-C-4",	"T23-C-1",	"T24-C-1",	"T24-C-2",	"T25-C-1",	"T25-C-2",	"T25-C-3",	"T26-C-1",	"T26-C-2",	"T26-C-3",	"T26-C-4",	"T27-C-1",	"T27-C-2",	"T27-C-3",	"T27-C-4",	"T27-C-5",	"T27-C-6",	"T27-C-7",	"T28-C-1",	"T28-C-2",	"T28-C-3",	"T29-C-1",	"T29-C-2",	"T29-C-3",	"T29-C-4",	"T29-C-5",	"T29-C-6",	"T30-C-1",	"T30-C-2",	"T30-C-3",	"T30-C-4",	"T31-C-1",	"T31-C-2",	"T31-C-3",	"T31-C-4",	"T31-C-5",	"T31-C-6",	"T31-C-7",	"T31-C-8",	"T31-C-9",	"T31-C-10",	"T31-C-11",	"T31-C-12",	"T31-C-13",	"T31-C-14",	"T32-C-1",	"T32-C-2",	"T32-C-3",	"T32-C-4",	"T32-C-5",	"T32-C-6",	"T32-C-7",	"T32-C-8",	"T32-C-9",	"T32-C-10",	"T32-C-11",	"T32-C-12",	"T32-C-13",	"T32-C-14",	"T32-C-15",	"T32-C-16",	"T32-C-17",	"T32-C-18",	"T32-C-19",	"T32-C-20",	"T32-C-21",	"T33-C-1",	"T33-C-2",	"T34-C-1",	"T34-C-2",	"T34-C-3",	"T34-C-4",	"T34-C-5",	"T34-C-6",	"T35-C-1",	"T35-C-2",	"T35-C-3",	"T36-C-1",	"T36-C-2",	"T36-C-3",	"T37-C-1",	"T37-C-2",	"T37-C-3",	"T38-C-1",	"T39-C-1",	"T39-C-2",	"T39-C-3",	"T39-C-4",	"T40-C-1",	"T41-C-1",	"T42-C-1",	"T43-C-1",	"T44-C-1",	"T45-C-1",	"T45-C-2",	"T45-C-3",	"V1-C-1",	"V1-C-2",	"V1-C-3",	"V1-C-4",	"V1-C-5",	"V1-C-6",	"V1-C-7",	"V1-C-8",	"V1-C-9",	"V2-C-1",	"V2-C-2",	"V2-C-3",	"V3-C-1",	"V3-C-2",	"V4-C-1",	"V4-C-2",	"V4-C-3",	"V4-C-4",	"V4-C-5",	"V5-C-1",	"V5-C-2",	"V6-C-1",	"V6-C-2",	"V6-C-3",	"V6-C-4",	"V6-C-5",	"V6-C-6",	"V6-C-7",	"V6-C-8",	"V6-C-9",	"V7-C-1",	"V7-C-2",	"V8-C-1",	"V8-C-2",	"V8-C-3",	"V9-C-1",	"V9-C-2",	"V9-C-3",	"V10-C-1",	"V10-C-2",	"V10-C-3",	"V11-C-1",	"V11-C-2",	"V12-C-1",	"V12-C-2",	"V12-C-3",	"V13-C-1",	"V13-C-2",	"V13-C-3",	"V13-C-4",	"L4-C-1",	"L4-C-2",	"L4-C-3",	"L")

#Save ED matrix
write.csv(ed_between,"../matrices/ED5_test.xlsx")

#Load old ED matrix
ed <- as.data.frame(read_xlsx("../matrices/ED5.xlsx"))

#Compare results
ed - ed_between
