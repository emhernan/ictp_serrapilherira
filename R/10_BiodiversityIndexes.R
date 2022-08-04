comm <- read.csv("data/raw/cestes/comm.csv")

# Which are the 5 most abundant species overall in the dataset?
sort(colSums(comm), decreasing = TRUE)[2:6]

# How many species are there in each site? (Richness)

comm_binary=comm[,-1]
comm_binary[comm_binary > 0] <- 1

SpeciesPerSites <- data.frame( Sites = paste("Site", 1:nrow(comm_binary), sep = ""), 
    NumberOfSpecies= rowSums(comm_binary[,2:ncol(comm_binary)]))

# Which are the most abundant species that in each site?

Specie <- apply(X = comm[,-1], MARGIN = 1, FUN = which.max)

data.frame(Sites =1:nrow(comm), 
           MostAbundantSpecied = Specie)


# Shannon diversity index
# Try to think about the most basic structure (vector)
# Your functions must function with vectors and not with
# other complex structures. 

# Check vegan::diversity()
Shannon_calculus <- function(matrix){
  P <- matriz/rowSums(matriz)
  logP <- log(matriz/rowSums(matrix))
  H <- P*logP
  H[is.na(H)] <- 0
  df <- data.frame(
    Sites = 1:nrow(matrix),
    ShannonI = -rowSums(H))
}

result <- Shannon_calculus(comm[,-1])
