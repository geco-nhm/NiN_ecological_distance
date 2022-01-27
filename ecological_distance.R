library(readxl)
library(writexl)

#Lister med trinn for LKM'er og andre regler
t <- numeric(58)
n <- numeric(1)
s <- numeric(1)
o <- numeric(1)
p <- numeric(1)
d <- numeric(1)

#Liste med ecological distance
ed <- numeric(1)

#ED mellom og innen hovedtyper basert på bT
for (l in 1:length(t)) {
  t[l] <- list(read_xlsx("C:/Users/adamen/Documents/Doktorgrad/Artikkel 1/ED/HT.xlsx", sheet = l)) #lister over HT
  t[[l]] <- as.data.frame(t[[l]])
  n[l] <- list(read_xlsx("C:/Users/adamen/Documents/Doktorgrad/Artikkel 1/ED/bT.xlsx", sheet = l)) #lister over bT
  n[[l]] <- as.data.frame(n[[l]])/2 # bør kanskje heller gå for eksponentiell vekting?
  for (m in 1:length(t)) {
    s[m] <- list(read_xlsx("C:/Users/adamen/Documents/Doktorgrad/Artikkel 1/ED/HT.xlsx", sheet = m)) #lister over HT
    s[[m]] <- as.data.frame(s[[m]])
    o[m] <- list(read_xlsx("C:/Users/adamen/Documents/Doktorgrad/Artikkel 1/ED/bT.xlsx", sheet = m)) #lister over bT
    o[[m]] <- as.data.frame(o[[m]])/2 # bør kanskje heller gå for eksponentiell vekting? veldig stor ØA 
    #f.eks mellom typer som har S1 som LKM (opptil 11 basistrinn mellom c og j)
    #ED innenfor hovedtyper
    ed[[(l-1)*length(t)+m]] <- list(matrix(0,nrow(t[[l]]),nrow(s[[m]]))) #ed=liste med ecological distance
    ed[[(l-1)*length(t)+m]] <- as.data.frame(ed[[(l-1)*length(t)+m]]) #j=T1 Kartleggingsenheter, k=T1 Kartleggingsenheter, i= Antall LKM'er
    if (m == l) {
      for (j in 1:nrow(t[[l]])) {
        for (k in 1:nrow(s[[m]])) {
          for (i in 1:length(t[[l]])) {
            if (t[[l]][j,i] == 0 || s[[m]][k,i] == 0) {""}
            else{ed[[(l-1)*length(t)+m]][j,k] <- floor(abs(sum(t[[l]][j,i]) - sum(s[[m]][k,i]))) + ed[[(l-1)*length(t)+m]][j,k]} #beregner ØA innen hovedtyper vha HT
          }
        }
      }
    }
    #ED for "normalvariasjon" mellom hovedtyper
    else {for (j in 1:nrow(n[[l]])) {
      for (k in 1:nrow(o[[m]])) {
        for (i in 1:length(n[[l]])) {
          if (n[[l]][j,i] == 0 || o[[m]][k,i] == 0) {""}
          else{ed[[(l-1)*length(t)+m]][j,k] <- floor(abs(sum(n[[l]][j,i]) - sum(o[[m]][k,i]))) + ed[[(l-1)*length(t)+m]][j,k]} #beregner ØA mellom hovedtyper vha bT
        }
      }
    }
  }
}
}

for (l in 1:length(t)) {
  p[l] <- list(read_xlsx("C:/Users/adamen/Documents/Doktorgrad/Artikkel 1/ED/sLKM.xlsx", sheet = l)) #lister over sLKM
  p[[l]] <- as.data.frame(p[[l]])
  for (m in 1:length(t)) {
    d[m] <- list(read_xlsx("C:/Users/adamen/Documents/Doktorgrad/Artikkel 1/ED/sLKM.xlsx", sheet = m)) #lister over sLKM
    d[[m]] <- as.data.frame(d[[m]])
    #ED mellom hovedtyper beregnet for spesifikke regler
      if (m == l) {""}
    else {for (j in 1:nrow(p[[l]])) {
      for (k in 1:nrow(d[[m]])) {
        for (i in 1:length(p[[l]])) {ed[[(l-1)*length(t)+m]][j,k] <- floor(abs(sum(p[[l]][j,i]) - sum(d[[m]][k,i]))) + ed[[(l-1)*length(t)+m]][j,k]} #beregner ØA mellom hovedtypers sLKM
        }
      }
    }
  }
}


#binder sammen kolonner
q <- numeric(1)
q[1] <- 1
for (i in 1:length(ed)) {
  q[i+1] <- (length(t)*i)+1
}  
r <- numeric(1)
for (i in 1:length(ed)) {
  r[i] <- length(t)*i
}  

x <- numeric(1)
for (j in 1:length(t)) {
  m <- matrix(0,nrow(ed[[q[j]]]),0)
  x[[j]] <- list(m)
  for (i in q[j]:r[j]) {
    x[[j]] <- cbind(x[[j]], ed[[i]])
  }
}

#binder sammen rader
for (i in 1:length(t)) {
  colnames(x[[i]]) <- colnames(x[[1]]) #error-fix
}
v <- matrix(0, 0, ncol(x[[1]]))
for (j in 1:length(t)) {
  v <- rbind(v, x[[j]])
}

write_xlsx(v,"C:/Users/adamen/Documents/Doktorgrad/Artikkel 1/ED/ED.xlsx")
