library(vegan)
data(dune)
data(dune.env)
table(dune.env$Management)

# We calculate two dissimlairty indices between sites: 
# Bray-Curtis distance and Chord distance



bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

# decostand, standarize data. 

# We perform the cluster analysis. Which is the default clustering method?
# Let’s use "average", which will link clusters. 

library(cluster)
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")

# Let’s plot them side to side

par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1, 1))

# Ok, that’s a little bit ugly

par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
par(mfrow = c(1, 1))

# hang -1 it will drive to the lengths to be at the same level in y
# axis. 

########################################################################
########################################################################

# Bray distance --> PCoA
# Euclidean distance --> PCA
is(chord_distance)
normalize <- decostand(dune, "norm")
pca <- rda(normalize)
dim(dune)
plot(pca)
summary(pca)

library(dplyr)

dune.env$A1 <- as.numeric(dune.env$A1)
dune.env$Moisture <- as.numeric(dune.env$Moisture)
dune.env$Manure <- as.numeric(dune.env$Moisture)

pca_env <- rda(dune.env[, c("A1", "Moisture", "Manure")])
plot(pca_env)
cor(dune.env[, c("A1", "Moisture", "Manure")])
