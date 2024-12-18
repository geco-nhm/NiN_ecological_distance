#####################################
# find the Ecological Distance (ED) #
# between evaluation and individual #
#     mapping unit assignments      #
#           in points               #
#####################################

# started 2024-10-07
# by Eva Lieungh, Adam E. Naas

## The Ecological Distancd (ED) between each mapping unit is saved in
## https://github.com/geco-nhm/NiN_ecological_distance/NiN_version_2/ 
## as a csv file with mapping units ("V1-C-5" etc.) as row & column names. 
## This script makes a new data frame with ED between a "true" evaluation 
## assessment (termed "consensus" in this script) and individual type 
## assignments in points, using the consensus and individual
## assignments as coordinates to find the ED value from the matrix.

# check your working directory to avoid changing all paths manually.
# Should be <local>/NiN_ecological_distance/NiN_version_2/script/
getwd()

# how many points are in the data set?
npoints = 400

# how many individual mappers are in the data set?
nmappers = 15

# make empty data frame to be filled with ED values
ED <- data.frame(matrix(nrow = npoints, # points
                        ncol = nmappers)) # mappers

# name mappers A, B, C etc
mappers = LETTERS[1:nmappers]
colnames(ED) <- mappers
ED$point <- 1:npoints

# read in the ED lookup matrix (1:5000 scale)
EDmatrix <- read.csv("../matrices/ED5000.csv", 
                     row.names = 1, # read 1st column as row names
                     header = TRUE)
colnames(EDmatrix) <- rownames(EDmatrix) # change because somehow the column names were read with . instead of - 
EDmatrix[1:5,1:10] # check that it looks correct

## The code below assumes there are separate data files for the consensus points
## and the individual assignments. It also uses individual data stored in
## list format, where each mapper has a separate data frame containing 
## the point ID and mapping_unit

# read in your own data - change this to your local directory
localdatapath_consensus <- "../../data/consensus-points.csv"
localdatapath_individual <- "../../data/point-data-individual-list.RData" # NB .RData format

# set the path to where your ED data should be stored
localoutputpath <- "../../data_processed/ED_per_mapper.csv"

# read and format point data set with consensus assignments
consensus <- read.csv(localdatapath)
head(consensus) # check if it looka correct

# If missing, add a column with the point numbers
consensus$point400 <- 1:npoints
# If mapping units are given as "T4-C10", change the format to match ED matrix
consensus$mapping_unit <- sub("C", "C-", consensus$mapping_unit) # add "-" after C

# read and format point data set with individual assignments
individual <- readRDS(localdatapath_individual)

# in a loop, find the ED between the consensus and each mapper's assignments
for (mapper in mappers) {
  for (i in 1:npoints) {
    type_consensus <- 
      consensus$mapping_unit[consensus$point400 == i]
    type_individual <- 
      individual[[mapper]]$mapping_unit[individual[[mapper]]$temp_id == i] # temp_id = point ID
    
    # Fetch the value from ED matrix using the types
    ED_value <- EDmatrix[type_consensus, type_individual]
    
    # Populate the ED dataframe with the fetched value
    ED[[mapper]][i] <- ED_value
  }
}

# check if it looks correct
head(ED)

# format the data longer
ed_long <- ED %>%
  pivot_longer(!point,
               names_to = "mapper")

# save in local directory
write.csv(ed_long, localoutputpath, row.names = FALSE)

# check that it is correct! Make sure mapper identity, order of points, etc, 
# have been kept in the correct order...

