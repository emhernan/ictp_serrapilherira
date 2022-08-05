# Importing the libraries

library(cluster)
library(FD)
library(vegan)
library(SYNCSA)
library(taxize)
# Reading the data: tables comm and traits
comm <- read.table(file = "data/raw/cestes/comm.csv", sep = ",", header = TRUE)
traits <- read.table(file = "data/raw/cestes/traits.csv", sep = ",", header = TRUE)

# Exploring our dataframes

head(comm)[,1:6]
head(traits)[,1:6]

# Our row names are not so nice, let's try to 
# put our first column as the row's names. 

rownames(comm)[1:6]
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

# Let's do the same for the traits df
head(traits)[,1:6]
rownames(traits) <- traits$Sp
traits <- traits[, -1]
head(traits)

# Species richness
# Species richness can be calculated with the vegan package:
# specnumber, computes the Shannon, Simpson and Fischer diversity
# indices and species richness. 
# For default, it computes the Sannon index. 

richness <- vegan::specnumber(comm) # Calculate the number of species per site

# Taxonomic diversity
# Taxonomic measures can be calculated using diversity() function:

shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

# Taxonomic diversity indices are based on the assumption that species belong to one species or the other.
# This can be thought as a distance matrix between individuals, where individuals of the same species have a 
# distance of zero between them, individuals of different species have a distance of 1 between them.
# When analyzing functional traits the distance between individuals is no longer determined by their belonging to a 
# species, but to their position in the trait space.
# These traits can be continuous, but vary in different scales, or they can be categorical, and appropriate distance 
#measures have to be used to deal with this difference. 
# Gower distance is a common distance metric used in trait-based ecology.

gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)

#implementations in R vary and the literature reports extensions and modifications

identical(gow, gow2) #not the same but why?

# Because they are different classes
class(gow) 
class(gow2)

# However both have the same values
plot(gow, gow2, asp = 1) 

# Rao’s quadratic entropy calculations in R
# Using package SYNCSA
# Rao's quadratic entropy (Rao 1982) is a 
# measure of diversity in ecological communities that can optionally 
# take species differences (e.g. phylogenetic dissimilarity) into account. 

tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)
# Simpson index is overestimating the diversity. 

#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)

#We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)


## If we want to compute diversity including Phylogeniric distances

splist <- read.table(file = "data/raw/cestes/splist.csv", sep = ",", header = TRUE)

classification_data <- taxize::classification(splist$TaxonName, db = "ncbi")

str(classification)
length(classification_data)
classification_data[[1]]
classification_data[[1]]

result <- sapply(X = classification_data, FUN = function(df) {
  if(length(df)>1){
    row <- df[df$rank == "family",]
    return(row[1])
  }}, simplify = TRUE)


unlist(result)
