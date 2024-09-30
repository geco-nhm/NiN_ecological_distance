###################################
# ecological distance calculation #
###################################

# by Adam E. Naas, revised by Eva Lieungh

# this script ...

# read in data prepared in data_preparation script
ED <- readRDS("../ED_list.RData")

# read in function for computing ED
compute_ED <- readRDS("../compute_ED_function.RData")

# 
ED <- compute_ED(ED[[1]],ED[[2]],bT_list,between_major_types = 0,between_all_units = 1)
ED <- compute_ED(ED[[1]],ED[[2]],sLKM_list,between_major_types = 1, between_all_units = 1)
ED <- compute_ED(ED[[1]],ED[[2]],MT_list,between_major_types = 0, between_all_units = 0)

#Less ED between firm ground types influenced by spring water, water disturbance or drought and wetland or freshwater types
for (l in firm) {
  for (m in firm) {
    if (m == l) {""}
    else {
      for (j in 1:nrow(sLKM_list[[l]])) {
      if (m == limn || l == limn) {ED[[(l-1)*n+m]][j,] <- floor(ED[[(l-1)*n+m]][j,] - (sLKM_list[[l]][4][j,1] + sLKM_list[[l]][6][j,1] + sLKM_list[[l]][11][j,1]))}
      else {""}
      if (m == wet || l == wet) {ED[[(l-1)*n+m]][j,] <- floor(ED[[(l-1)*n+m]][j,] - (sLKM_list[[l]][4][j,1] + sLKM_list[[l]][6][j,1]))}
      else {""}
      }
      for (k in 1:nrow(sLKM_list[[m]])) {
        for (i in 1:nrow(sLKM_list[[l]])) {
          #symmetry
          if (m == limn || l == limn) {ED[[(l-1)*n+m]][i,k] <- floor(ED[[(l-1)*n+m]][i,k] - (sLKM_list[[m]][4][k,1] + sLKM_list[[m]][6][k,1] + sLKM_list[[m]][11][k,1]))}
          else {""}
          if (m == wet || l == wet) {ED[[(l-1)*n+m]][i,k] <- floor(ED[[(l-1)*n+m]][i,k] - (sLKM_list[[m]][4][k,1] + sLKM_list[[m]][6][k,1]))}
          else {""} 
        }
      }
    }
  }      
}

#Function to compute ED based on principles (e.g. degrees of anthropogenic influence)
principles <- function(group1,group2,ED_matrix,criteria_matrix,confusion_type,penalising)
{
for (l in group1) {
  for (m in group2) {
    if (m==l) {""}
else {
    ED_matrix[[(l-1)*n+m]] <- (ED_matrix[[(l-1)*n+m]])+penalising
    if (criteria_matrix[[(l-1)*n+m]][[1]][,,1] == confusion_type ||
        criteria_matrix[[(l-1)*n+m]][[1]][,,2] == confusion_type ||
        criteria_matrix[[(l-1)*n+m]][[1]][,,3] == confusion_type ||
        criteria_matrix[[(l-1)*n+m]][[1]][,,4] == confusion_type ||
        criteria_matrix[[(l-1)*n+m]][[1]][,,5] == confusion_type ||
        criteria_matrix[[(l-1)*n+m]][[1]][,,6] == confusion_type ||
        criteria_matrix[[(l-1)*n+m]][[1]][,,7] == confusion_type) {""} 
    else {if (criteria_matrix[[(l-1)*n+m]][[1]][,,1] == 0){criteria_matrix[[(l-1)*n+m]][[1]][,,1] <- confusion_type
    } else if (criteria_matrix[[(l-1)*n+m]][[1]][,,2] == 0){criteria_matrix[[(l-1)*n+m]][[1]][,,2] <- confusion_type
    } else if (criteria_matrix[[(l-1)*n+m]][[1]][,,3] == 0){criteria_matrix[[(l-1)*n+m]][[1]][,,3] <- confusion_type
    } else if (criteria_matrix[[(l-1)*n+m]][[1]][,,4] == 0){criteria_matrix[[(l-1)*n+m]][[1]][,,4] <- confusion_type
    } else if (criteria_matrix[[(l-1)*n+m]][[1]][,,5] == 0){criteria_matrix[[(l-1)*n+m]][[1]][,,5] <- confusion_type
    } else if (criteria_matrix[[(l-1)*n+m]][[1]][,,6] == 0){criteria_matrix[[(l-1)*n+m]][[1]][,,6] <- confusion_type
    } else {criteria_matrix[[(l-1)*n+m]][[1]][,,7] <- confusion_type} 
    }
  }
}
}
  return(list(ED_matrix,criteria_matrix))
}

ED <- principles(limn,firm,ED[[1]],ED[[2]],"LIMTER",2)
ED <- principles(firm,limn,ED[[1]],ED[[2]],"LIMTER",2)
ED <- principles(limn,wet,ED[[1]],ED[[2]],"LIMWET",2)
ED <- principles(wet,limn,ED[[1]],ED[[2]],"LIMWET",2)
ED <- principles(firm,wet,ED[[1]],ED[[2]],"WETTER",2)
ED <- principles(wet,firm,ED[[1]],ED[[2]],"WETTER",2)
ED <- principles(natural,semi,ED[[1]],ED[[2]],"NATSEM",2)
ED <- principles(semi,natural,ED[[1]],ED[[2]],"NATSEM",2)
ED <- principles(strong,semi,ED[[1]],ED[[2]],"SEMSTR",2)
ED <- principles(semi,strong,ED[[1]],ED[[2]],"SEMSTR",2)
ED <- principles(strong,natural,ED[[1]],ED[[2]],"NATSTR",4)
ED <- principles(natural,strong,ED[[1]],ED[[2]],"NATSTR",4)
ED <- principles(MX,c(semi[-c(MX)]),ED[[1]],ED[[2]],"MX",1)
ED <- principles(c(semi[-c(MX)]),MX,ED[[1]],ED[[2]],"MX",1)
ED <- principles(HR,c(semi[-c(HR)]),ED[[1]],ED[[2]],"HR",1)
ED <- principles(c(semi[-c(HR)]),HR,ED[[1]],ED[[2]],"HR",1)
ED <- principles(fLKM,all,ED[[1]],ED[[2]],"fLKM",1)
ED <- principles(all,fLKM,ED[[1]],ED[[2]],"fLKM",1)

#1 ED between different categories of strongly modified types
for (l in SX[1,]) {
  for (m in SX[1,]) {
    if (SX[2,which(SX[1,] == l)] == SX[2,which(SX[1,] == m)]) {""}
    else {
      ED[[1]][[(l-1)*n+m]] <- (ED[[1]][[(l-1)*n+m]])+1
      if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == "SX" ||
          ED[[2]][[(l-1)*n+m]][[1]][,,2] == "SX" ||
          ED[[2]][[(l-1)*n+m]][[1]][,,3] == "SX" ||
          ED[[2]][[(l-1)*n+m]][[1]][,,4] == "SX" ||
          ED[[2]][[(l-1)*n+m]][[1]][,,5] == "SX" ||
          ED[[2]][[(l-1)*n+m]][[1]][,,6] == "SX" ||
          ED[[2]][[(l-1)*n+m]][[1]][,,7] == "SX") {""} 
      else {if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,1] <- "SX"
      } else if (ED[[2]][[(l-1)*n+m]][[1]][,,2] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,2] <- "SX"
      } else if (ED[[2]][[(l-1)*n+m]][[1]][,,3] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,3] <- "SX"
      } else if (ED[[2]][[(l-1)*n+m]][[1]][,,4] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,4] <- "SX"
      } else if (ED[[2]][[(l-1)*n+m]][[1]][,,5] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,5] <- "SX"
      } else if (ED[[2]][[(l-1)*n+m]][[1]][,,6] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,6] <- "SX"
      } else {ED[[2]][[(l-1)*n+m]][[1]][,,7] <- "SX"} 
      }
    }
  }
}

#2 ED between types with different ecosystem engineering species or their absence
for (l in A[1,]) {
  for (m in A[1,]) {
    if (A[2,which(A[1,] == l)] == A[2,which(A[1,] == m)]) {""}
    else {
      ED[[1]][[(l-1)*n+m]] <- (ED[[1]][[(l-1)*n+m]])+2
      if ((A[2,which(A[1,] == l)] == 1 && A[2,which(A[1,] == m)] == 2) || (A[2,which(A[1,] == l)] == 2 && A[2,which(A[1,] == m)] == 1))
      {if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == "HeloTree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,2] == "HeloTree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,3] == "HeloTree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,4] == "HeloTree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,5] == "HeloTree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,6] == "HeloTree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,7] == "HeloTree") {""} 
        else {if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,1] <- "HeloTree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,2] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,2] <- "HeloTree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,3] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,3] <- "HeloTree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,4] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,4] <- "HeloTree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,5] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,5] <- "HeloTree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,6] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,6] <- "HeloTree"
        } else {ED[[2]][[(l-1)*n+m]][[1]][,,7] <- "HeloTree"} 
        }
      }
      else if ((A[2,which(A[1,] == l)] == 1 && A[2,which(A[1,] == m)] == 0) || (A[2,which(A[1,] == l)] == 0 && A[2,which(A[1,] == m)] == 1))
      {if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == "Tree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,2] == "Tree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,3] == "Tree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,4] == "Tree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,5] == "Tree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,6] == "Tree" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,7] == "Tree") {""} 
        else {if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,1] <- "Tree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,2] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,2] <- "Tree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,3] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,3] <- "Tree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,4] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,4] <- "Tree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,5] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,5] <- "Tree"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,6] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,6] <- "Tree"
        } else {ED[[2]][[(l-1)*n+m]][[1]][,,7] <- "Tree"} 
        }
      }
      else if ((A[2,which(A[1,] == l)] == 2 && A[2,which(A[1,] == m)] == 0) || (A[2,which(A[1,] == l)] == 0 && A[2,which(A[1,] == m)] == 2))
      {if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == "Helo" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,2] == "Helo" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,3] == "Helo" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,4] == "Helo" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,5] == "Helo" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,6] == "Helo" ||
           ED[[2]][[(l-1)*n+m]][[1]][,,7] == "Helo") {""} 
        else {if (ED[[2]][[(l-1)*n+m]][[1]][,,1] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,1] <- "Helo"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,2] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,2] <- "Helo"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,3] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,3] <- "Helo"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,4] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,4] <- "Helo"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,5] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,5] <- "Helo"
        } else if (ED[[2]][[(l-1)*n+m]][[1]][,,6] == 0){ED[[2]][[(l-1)*n+m]][[1]][,,6] <- "Helo"
        } else {ED[[2]][[(l-1)*n+m]][[1]][,,7] <- "Helo"} 
        }
      }
    }
  }
}

#binding columns and rows together
bind_ED_matrix <- function(ED_matrix,names_units)
{
q <- numeric(1)
q[1] <- 1
for (i in 1:length(ED_matrix)) {
  q[i+1] <- (n*i)+1
}  
r <- numeric(1)
for (i in 1:length(ED_matrix)) {
  r[i] <- n*i
}  

x <- numeric(1)
for (j in 1:n) {
  m <- matrix(0,nrow(ED_matrix[[q[j]]]),0)
  x[[j]] <- list(m)
  for (i in q[j]:r[j]) {
    x[[j]] <- cbind(x[[j]], ED_matrix[[i]])
  }
}
for (i in 1:n) {
  colnames(x[[i]]) <- colnames(x[[1]]) #error-fix
}
v <- matrix(0, 0, ncol(x[[1]]))
for (j in 1:n) {
  v <- rbind(v, x[[j]])
}
colnames(v) <- names_units
rownames(v) <- names_units
return(v)
}
bind_criteria_matrix <- function(ED_matrix,criteria_matrix,names_units)
{
  q <- numeric(1)
  q[1] <- 1
  for (i in 1:length(ED_matrix)) {
    q[i+1] <- (n*i)+1
  }  
  r <- numeric(1)
  for (i in 1:length(ED_matrix)) {
    r[i] <- n*i
  }  
  
  x <- numeric(1)
  for (j in 1:n) {
    m <- matrix(0,nrow(ED_matrix[[q[j]]]),0)
    x[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      x[[j]] <- cbind(x[[j]], ED_matrix[[i]])
    }
  }
  
#lists for criteria
criterion1 <- list()
criterion2 <- list()
criterion3 <- list()
criterion4 <- list()
criterion5 <- list()
criterion6 <- list()
criterion7 <- list()

for (j in 1:n) {
  for (i in q[j]:r[j]) {
    criterion1[[i]] <- criteria_matrix[[i]][[1]][,,1]
    criterion2[[i]] <- criteria_matrix[[i]][[1]][,,2]
    criterion3[[i]] <- criteria_matrix[[i]][[1]][,,3]
    criterion4[[i]] <- criteria_matrix[[i]][[1]][,,4]
    criterion5[[i]] <- criteria_matrix[[i]][[1]][,,5]
    criterion6[[i]] <- criteria_matrix[[i]][[1]][,,6]
    criterion7[[i]] <- criteria_matrix[[i]][[1]][,,7]
  }
}

a <- numeric(1)
for (j in 1:n) {
  if (length(nrow(criterion1[[q[j]]])) == 0) {
    m <- matrix(0,1,0)
    a[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      a[[j]][[1]] <- cbind(a[[j]][[1]], t(criterion1[[i]]))
    }
  }
  else {
    m <- matrix(0,nrow(criterion1[[q[j]]]),0)
    a[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      a[[j]][[1]] <- cbind(a[[j]][[1]], criterion1[[i]])
    }
  }
}
b <- numeric(1)
for (j in 1:n) {
  if (length(nrow(criterion2[[q[j]]])) == 0) {
    m <- matrix(0,1,0)
    b[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      b[[j]][[1]] <- cbind(b[[j]][[1]], t(criterion2[[i]]))
    }
  }
  else {
    m <- matrix(0,nrow(criterion2[[q[j]]]),0)
    b[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      b[[j]][[1]] <- cbind(b[[j]][[1]], criterion2[[i]])
    }
  }
}
c <- numeric(1)
for (j in 1:n) {
  if (length(nrow(criterion3[[q[j]]])) == 0) {
    m <- matrix(0,1,0)
    c[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      c[[j]][[1]] <- cbind(c[[j]][[1]], t(criterion3[[i]]))
    }
  }
  else {
    m <- matrix(0,nrow(criterion3[[q[j]]]),0)
    c[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      c[[j]][[1]] <- cbind(c[[j]][[1]], criterion3[[i]])
    }
  }
}
d <- numeric(1)
for (j in 1:n) {
  if (length(nrow(criterion4[[q[j]]])) == 0) {
    m <- matrix(0,1,0)
    d[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      d[[j]][[1]] <- cbind(d[[j]][[1]], t(criterion4[[i]]))
    }
  }
  else {
    m <- matrix(0,nrow(criterion4[[q[j]]]),0)
    d[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      d[[j]][[1]] <- cbind(d[[j]][[1]], criterion4[[i]])
    }
  }
}
A <- numeric(1)
for (j in 1:n) {
  if (length(nrow(criterion5[[q[j]]])) == 0) {
    m <- matrix(0,1,0)
    A[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      A[[j]][[1]] <- cbind(A[[j]][[1]], t(criterion5[[i]]))
    }
  }
  else {
    m <- matrix(0,nrow(criterion5[[q[j]]]),0)
    A[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      A[[j]][[1]] <- cbind(A[[j]][[1]], criterion5[[i]])
    }
  }
}
B <- numeric(1)
for (j in 1:n) {
  if (length(nrow(criterion6[[q[j]]])) == 0) {
    m <- matrix(0,1,0)
    B[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      B[[j]][[1]] <- cbind(B[[j]][[1]], t(criterion6[[i]]))
    }
  }
  else {
    m <- matrix(0,nrow(criterion6[[q[j]]]),0)
    B[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      B[[j]][[1]] <- cbind(B[[j]][[1]], criterion6[[i]])
    }
  }
}
C <- numeric(1)
for (j in 1:n) {
  if (length(nrow(criterion7[[q[j]]])) == 0) {
    m <- matrix(0,1,0)
    C[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      C[[j]][[1]] <- cbind(C[[j]][[1]], t(criterion7[[i]]))
    }
  }
  else {
    m <- matrix(0,nrow(criterion7[[q[j]]]),0)
    C[[j]] <- list(m)
    for (i in q[j]:r[j]) {
      C[[j]][[1]] <- cbind(C[[j]][[1]], criterion7[[i]])
    }
  }
}

e <- matrix(0, 0, ncol(a[[1]][[1]]))
for (j in 1:n) {
  e <- rbind(e, a[[j]][[1]])
}
f <- matrix(0, 0, ncol(b[[1]][[1]]))
for (j in 1:n) {
  f <- rbind(f, b[[j]][[1]])
}
g <- matrix(0, 0, ncol(c[[1]][[1]]))
for (j in 1:n) {
  g <- rbind(g, c[[j]][[1]])
}
h <- matrix(0, 0, ncol(d[[1]][[1]]))
for (j in 1:n) {
  h <- rbind(h, d[[j]][[1]])
}

E <- matrix(0, 0, ncol(A[[1]][[1]]))
for (j in 1:n) {
  E <- rbind(E, A[[j]][[1]])
}
G <- matrix(0, 0, ncol(B[[1]][[1]]))
for (j in 1:n) {
  G <- rbind(G, B[[j]][[1]])
}
H <- matrix(0, 0, ncol(C[[1]][[1]]))
for (j in 1:n) {
  H <- rbind(H, C[[j]][[1]])
}

colnames(e) <- names_units
colnames(f) <- names_units
colnames(g) <- names_units
colnames(h) <- names_units
colnames(E) <- names_units
colnames(G) <- names_units
colnames(H) <- names_units
criteria <- list(as.data.frame(e),as.data.frame(f),as.data.frame(g),as.data.frame(h),as.data.frame(E),as.data.frame(G),as.data.frame(H))
}

names5 <- c("T1-C-1",	"T1-C-2",	"T1-C-3",	"T1-C-4",	"T1-C-5",	"T1-C-6",	"T1-C-7",	"T1-C-8",	"T1-C-9",	"T1-C-10",	"T1-C-11",	"T1-C-12",	"T2-C-1",	"T2-C-2",	"T2-C-3",	"T2-C-4",	"T2-C-5",	"T2-C-6",	"T2-C-7",	"T2-C-8",	"T3-C-1",	"T3-C-2",	"T3-C-3",	"T3-C-4",	"T3-C-5",	"T3-C-6",	"T3-C-7",	"T3-C-8",	"T3-C-9",	"T3-C-10",	"T3-C-11",	"T3-C-12",	"T3-C-13",	"T3-C-14",	"T4-C-1",	"T4-C-2",	"T4-C-3",	"T4-C-4",	"T4-C-5",	"T4-C-6",	"T4-C-7",	"T4-C-8",	"T4-C-9",	"T4-C-10",	"T4-C-11",	"T4-C-12",	"T4-C-13",	"T4-C-14",	"T4-C-15",	"T4-C-16",	"T4-C-17",	"T4-C-18",	"T4-C-19",	"T4-C-20",	"T5-C-1",	"T5-C-2",	"T5-C-3",	"T5-C-4",	"T5-C-5",	"T5-C-6",	"T5-C-7",	"T6-C-1",	"T6-C-2",	"T7-C-1",	"T7-C-2",	"T7-C-3",	"T7-C-4",	"T7-C-5",	"T7-C-6",	"T7-C-7",	"T7-C-8",	"T7-C-9",	"T7-C-10",	"T7-C-11",	"T7-C-12",	"T7-C-13",	"T7-C-14",	"T8-C-1",	"T8-C-2",	"T8-C-3",	"T9-C-1",	"T9-C-2",	"T10-C-1",	"T11-C-1",	"T11-C-2",	"T12-C-1",	"T12-C-2",	"T13-C-1",	"T13-C-2",	"T13-C-3",	"T13-C-4",	"T13-C-5",	"T13-C-6",	"T13-C-7",	"T13-C-8",	"T13-C-9",	"T13-C-10",	"T13-C-11",	"T13-C-12",	"T13-C-13",	"T13-C-14",	"T13-C-15",	"T14-C-1",	"T14-C-2",	"T15-C-1",	"T15-C-2",	"T16-C-1",	"T16-C-2",	"T16-C-3",	"T16-C-4",	"T16-C-5",	"T16-C-6",	"T16-C-7",	"T17-C-1",	"T17-C-2",	"T17-C-3",	"T18-C-1",	"T18-C-2",	"T18-C-3",	"T18-C-4",	"T19-C-1",	"T19-C-2",	"T20-C-1",	"T20-C-2",	"T21-C-1",	"T21-C-2",	"T21-C-3",	"T21-C-4",	"T22-C-1",	"T22-C-2",	"T22-C-3",	"T22-C-4",	"T23-C-1",	"T24-C-1",	"T24-C-2",	"T25-C-1",	"T25-C-2",	"T25-C-3",	"T26-C-1",	"T26-C-2",	"T26-C-3",	"T26-C-4",	"T27-C-1",	"T27-C-2",	"T27-C-3",	"T27-C-4",	"T27-C-5",	"T27-C-6",	"T27-C-7",	"T28-C-1",	"T28-C-2",	"T28-C-3",	"T29-C-1",	"T29-C-2",	"T29-C-3",	"T29-C-4",	"T29-C-5",	"T29-C-6",	"T30-C-1",	"T30-C-2",	"T30-C-3",	"T30-C-4",	"T31-C-1",	"T31-C-2",	"T31-C-3",	"T31-C-4",	"T31-C-5",	"T31-C-6",	"T31-C-7",	"T31-C-8",	"T31-C-9",	"T31-C-10",	"T31-C-11",	"T31-C-12",	"T31-C-13",	"T31-C-14",	"T32-C-1",	"T32-C-2",	"T32-C-3",	"T32-C-4",	"T32-C-5",	"T32-C-6",	"T32-C-7",	"T32-C-8",	"T32-C-9",	"T32-C-10",	"T32-C-11",	"T32-C-12",	"T32-C-13",	"T32-C-14",	"T32-C-15",	"T32-C-16",	"T32-C-17",	"T32-C-18",	"T32-C-19",	"T32-C-20",	"T32-C-21",	"T33-C-1",	"T33-C-2",	"T34-C-1",	"T34-C-2",	"T34-C-3",	"T34-C-4",	"T34-C-5",	"T34-C-6",	"T35-C-1",	"T35-C-2",	"T35-C-3",	"T36-C-1",	"T36-C-2",	"T36-C-3",	"T37-C-1",	"T37-C-2",	"T37-C-3",	"T38-C-1",	"T39-C-1",	"T39-C-2",	"T39-C-3",	"T39-C-4",	"T40-C-1",	"T41-C-1",	"T42-C-1",	"T43-C-1",	"T44-C-1",	"T45-C-1",	"T45-C-2",	"T45-C-3",	"V1-C-1",	"V1-C-2",	"V1-C-3",	"V1-C-4",	"V1-C-5",	"V1-C-6",	"V1-C-7",	"V1-C-8",	"V1-C-9",	"V2-C-1",	"V2-C-2",	"V2-C-3",	"V3-C-1",	"V3-C-2",	"V4-C-1",	"V4-C-2",	"V4-C-3",	"V4-C-4",	"V4-C-5",	"V5-C-1",	"V5-C-2",	"V6-C-1",	"V6-C-2",	"V6-C-3",	"V6-C-4",	"V6-C-5",	"V6-C-6",	"V6-C-7",	"V6-C-8",	"V6-C-9",	"V7-C-1",	"V7-C-2",	"V8-C-1",	"V8-C-2",	"V8-C-3",	"V9-C-1",	"V9-C-2",	"V9-C-3",	"V10-C-1",	"V10-C-2",	"V10-C-3",	"V11-C-1",	"V11-C-2",	"V12-C-1",	"V12-C-2",	"V12-C-3",	"V13-C-1",	"V13-C-2",	"V13-C-3",	"V13-C-4",	"L4-C-1",	"L4-C-2",	"L4-C-3",	"L")
names20 <- c("T1-E-1", "T1-E-2",	"T2-E-1",	"T2-E-2",	"T2-E-3",	"T2-E-4",	"T3-E-1",	"T3-E-2",	"T3-E-3",	"T3-E-4",	"T3-E-5",	"T3-E-6",	"T3-E-7",	"T4-E-1",	"T4-E-2",	"T4-E-3",	"T4-E-4",	"T4-E-5",	"T4-E-6",	"T5-E-1",	"T5-E-2",	"T5-E-3",	"T5-E-4",	"T6-E-1",	"T6-E-2",	"T7-E-1",	"T7-E-2",	"T7-E-3",	"T7-E-4",	"T7-E-5",	"T7-E-6",	"T8-E-1",	"T8-E-2",	"T9-E-1",	"T9-E-2",	"T10-E-1",	"T11-E-1",	"T12-E-1", "T13-E-1",	"T13-E-2",	"T13-E-3",	"T13-E-4",	"T13-E-5",	"T13-E-6",	"T13-E-7",	"T13-E-8",	"T13-E-9",	"T14-E-1",	"T14-E-2",	"T15-E-1",	"T15-E-2",	"T16-E-1",	"T17-E-1",	"T17-E-2",	"T17-E-3",	"T18-E-1",	"T18-E-2",	"T18-E-3",	"T19-E-1",	"T19-E-2",	"T20-E-1",	"T20-E-2",	"T21-E-1",	"T22-E-1",	"T22-E-2",	"T23-E-1",	"T24-E-1",	"T25-E-1",	"T25-E-2",	"T25-E-3",	"T26-E-1",	"T27-E-1",	"T27-E-2",	"T27-E-3",	"T28-E-1",	"T28-E-2",	"T28-E-3",	"T29-E-1",	"T30-E-1",	"T31-E-1",	"T31-E-2",	"T31-E-3",	"T31-E-4",	"T31-E-5",	"T31-E-6",	"T31-E-7",	"T32-E-1",	"T32-E-2",	"T32-E-3",	"T32-E-4",	"T33-E-1",	"T34-E-1",	"T34-E-2",	"T34-E-3",	"T34-E-4",	"T35-E-1",	"T35-E-2",	"T35-E-3",	"T36-E-1",	"T36-E-2",	"T36-E-3",	"T37-E-1",	"T37-E-2",	"T37-E-3",	"T38-E-1",	"T39-E-1",	"T39-E-2",	"T39-E-3",	"T39-E-4",	"T40-E-1",	"T41-E-1",	"T42-E-1",	"T43-E-1",	"T44-E-1",	"T45-E-1",	"T45-E-2",	"V1-E-1",	"V1-E-2",	"V1-E-3",	"V2-E-1",	"V2-E-2",	"V2-E-3",	"V3-E-1",	"V4-E-1",	"V4-E-2",	"V4-E-3",	"V4-E-4",	"V5-E-1",	"V5-E-2",	"V6-E-1",	"V6-E-2",	"V6-E-3",	"V7-E-1",	"V7-E-2",	"V8-E-1",	"V8-E-2",	"V8-E-3",	"V9-E-1",	"V9-E-2",	"V9-E-3",	"V10-E-1",	"V10-E-2",	"V11-E-1",	"V11-E-2",	"V12-E-1",	"V12-E-2",	"V12-E-3",	"V13-E-1",	"V13-E-2",	"V13-E-3",	"V13-E-4",	"L4-E-1",	"L4-E-2",	"L4-E-3",	"L")
names_major <- c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16","T17","T18","T19","T20","T21","T22","T23","T24","T25","T26",
                 "T27","T28","T29","T30","T31","T32","T33","T34","T35","T36","T37","T38","T39","T40","T41","T42","T43","T44","T45","V1","V2","V3","V4","V5","V6","V7",
                 "V8","V9","V10","V11","V12","V13","L4","L")

ED5 <- bind_ED_matrix(ED[[1]],names5)
criteria5 <- bind_criteria_matrix(ED[[1]],ED[[2]],names5)

#write_xlsx(ED5,"ED/ED5.xlsx")
#write_xlsx(criteria5,"ED/ED5_criteria.xlsx")

#ED20 <- bind_ED_matrix(ED[[1]],names20)
#criteria20 <- bind_criteria_matrix(ED[[1]],ED[[2]],names20)

#write_xlsx(ED20,"ED/ED20.xlsx")
#write_xlsx(criteria20,"ED/ED20_criteria.xlsx")

#Aggregating tables to major types only
aggregate_matrix <- function(ED_matrix,criteria_matrix,conversion_matrix,names_units)
{
major_type_conversion <- as.data.frame(read_xlsx(conversion_matrix))
g <- numeric()
for (i in 1:length(names_units)) {
  g[i] <- which(major_type_conversion == i)[1]
}
MT <- ED_matrix[g,g]
MT_criteria <- list()
for (i in 1:length(criteria_matrix)) {
  MT_criteria[[i]] <- criteria_matrix[[i]][g,g]
  colnames(MT_criteria[[i]]) <- names_units
  rownames(MT_criteria[[i]]) <- names_units
}
rownames(MT) <- names_units
colnames(MT) <- names_units
return(list(MT,MT_criteria))
}

#No distance between T35 and T38
MT <- aggregate_matrix(ED5,criteria5,"R/conversion_tables/conversion_major_types_5.xlsx",names_major)

#write_xlsx(MT[[1]],"ED/MT.xlsx")
#write_xlsx(MT[[2]],"ED/MT_criteria.xlsx")
