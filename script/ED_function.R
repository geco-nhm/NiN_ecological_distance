###################################################
# Function for computing Ecological Distance (ED) #
###################################################

# by Adam E. Naas, revised by Eva Lieungh

# Function for computing ED based on the lists (HT, bT and sLKM)
compute_ED <- function(ED_matrix,
                       criteria_matrix,
                       ED_list,
                       between_major_types,
                       between_all_units)
{
  if (between_major_types == 1)
  {
    for (l in 1:n) {
      for (m in 1:n) {
        if (l != m) #Only between different major types
        {
          for (j in 1:nrow(ED_list[[l]])) {
            for (k in 1:nrow(ED_list[[m]])) {
              for (i in 1:length(ED_list[[l]])) {
                ED_matrix[[(l-1)*n+m]][j,k] <- floor(abs(sum(ED_list[[l]][j,i]) - sum(ED_list[[m]][k,i]))) + ED_matrix[[(l-1)*n+m]][j,k]
                if (floor(abs(sum(ED_list[[l]][j,i]) - sum(ED_list[[m]][k,i]))) > 0){                  
                  if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] == colnames(ED_list[[l]][i]) ||
                      criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] == colnames(ED_list[[l]][i]) ||
                      criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] == colnames(ED_list[[l]][i]) ||
                      criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] == colnames(ED_list[[l]][i]) ||
                      criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] == colnames(ED_list[[l]][i]) ||
                      criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] == colnames(ED_list[[l]][i]) ||
                      criteria_matrix[[(l-1)*n+m]][[1]][j,k,7] == colnames(ED_list[[l]][i])) {""} else {
                        if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] <- colnames(ED_list[[l]][i])
                        } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] <- colnames(ED_list[[l]][i])
                        } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] <- colnames(ED_list[[l]][i])
                        } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] <- colnames(ED_list[[l]][i])
                        } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] <- colnames(ED_list[[l]][i])
                        } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] <- colnames(ED_list[[l]][i])
                        } else {criteria_matrix[[(l-1)*n+m]][[1]][j,k,7] <- colnames(ED_list[[l]][i])} 
                      }
                }
                else {""}
              }
            }
          }
        }
      }
    }
  }
  else
  {
    for (l in 1:n) {
      for (m in 1:n) {
        for (j in 1:nrow(ED_list[[l]])) {
          for (k in 1:nrow(ED_list[[m]])) {
            for (i in 1:length(ED_list[[l]])) {
              if (ED_list[[l]][j,i] == 0 || ED_list[[m]][k,i] == 0) {""}
              else{
                if (between_all_units == 0) {
                  if (m == l) #only within major types
                  {
                    ED_matrix[[(l-1)*n+m]][j,k] <- floor(abs(sum(ED_list[[l]][j,i]) - sum(ED_list[[m]][k,i]))) + ED_matrix[[(l-1)*n+m]][j,k]
                    if (floor(abs(sum(ED_list[[l]][j,i]) - sum(ED_list[[m]][k,i]))) > 0){                  
                      if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,7] == colnames(ED_list[[l]][i])) {""} else {
                            if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] <- colnames(ED_list[[l]][i])
                            } else {criteria_matrix[[(l-1)*n+m]][[1]][j,k,7] <- colnames(ED_list[[l]][i])} 
                          }
                    }
                  }
                }
                else #only between different major types
                { if (m != l)
                {
                  if (ED_list[[l]][j,i] == 0 || ED_list[[m]][k,i] == 0) {""}
                  else{
                    ED_matrix[[(l-1)*n+m]][j,k] <- floor(abs(sum(ED_list[[l]][j,i]) - sum(ED_list[[m]][k,i]))) + ED_matrix[[(l-1)*n+m]][j,k] #beregner ?A innenfor hovedtyper vha HT
                    if (floor(abs(sum(ED_list[[l]][j,i]) - sum(ED_list[[m]][k,i]))) > 0){                  
                      if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] == colnames(ED_list[[l]][i]) ||
                          criteria_matrix[[(l-1)*n+m]][[1]][j,k,7] == colnames(ED_list[[l]][i])) {""} else {
                            if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,1] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,2] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,3] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,4] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,5] <- colnames(ED_list[[l]][i])
                            } else if (criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] == 0){criteria_matrix[[(l-1)*n+m]][[1]][j,k,6] <- colnames(ED_list[[l]][i])
                            } else {criteria_matrix[[(l-1)*n+m]][[1]][j,k,7] <- colnames(ED_list[[l]][i])} 
                          }
                    }
                    else {""}
                  }
                }
                }
              }
            }
          }
        }
      }
    }
  }
  return(list(ED_matrix,criteria_matrix))
}



# save function object as RData
saveRDS(compute_ED, "../compute_ED_function.RData")