####################
# data preparation #
####################

# by Adam E. Naas, Eva Lieungh

library("readxl")
library("writexl")

# check your working directory:
# it should be <your local path>/NiN_ecological_distance/script
getwd()

# Make vectors for different type levels
## Major type groups
{
  firm <- 1:45
  wet <- 46:58
  limn <- 59:60
  MX <- 31
  HR <- 34
  all <- 1:60
}

# natural factor LCE's
fLKM <- c(25:29,48,53)

# LCE for anthropogenic influence
SX <- matrix(0,2,14)
SX[1,] <- c(35:45,56:58)
SX[2,] <- c(5,6,7,5,8,9,10,11,11,12,12,13,14,15)

# anthropogenic influence
{
  natural <- c(1:30,46:53,59:60)
  semi <- c(31:34,54:55)
  strong <- c(35:45,56:58)
}

# ecosystem engineering species
A <- matrix(0,2,60)
A[1,] <- 1:60

## Trees
A[2,c(4,30,47,53)] <- 1

## Helophytes
A[2,59] <- 2

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
## (60 for 45 terrestrial + 13 wetland + 1 'limnic' + 1 'marine' 
## types allowed during mapping) - !!! Adam, stemmer dette?^
n <- 60

# Uploading data to fill lists
for (l in 1:n) {
  MT_list[l] <- list(as.data.frame(read_xlsx(MT_sheet, sheet = l)))
  bT_list[l] <- list(as.data.frame(read_xlsx(bT_sheet, sheet = l))/2) 
  sLKM_list[l] <- list(as.data.frame(read_xlsx(sLKM_sheet, sheet = l)))
}

# Making ecological distance matrix [[1]] and criteria matrix [[2]]
for (l in 1:n) {
  for (m in 1:n) {
    ED[[1]][[(l-1)*n+m]] <- list(matrix(0,nrow(MT_list[[l]]),nrow(MT_list[[m]])))
    ED[[1]][[(l-1)*n+m]] <- as.data.frame(ED[[1]][[(l-1)*n+m]])
    ED[[2]][[(l-1)*n+m]] <- list(array(0,dim=c(nrow(MT_list[[l]]),nrow(MT_list[[m]]),7)))
  }
}

# save data 
saveRDS(ED, "../ED_list.RData")
