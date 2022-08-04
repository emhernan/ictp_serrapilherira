library(vegan)
library(ggplot2)
CommunityA <- c(10,6,4,1)
CommunityB <- c(17, rep(1,7))

diversity(CommunityA, "shannon")
diversity(CommunityB, "shannon")
diversity(CommunityA, "invsimpson")
diversity(CommunityB, "invsimpson")

barplot(CommunityA, col = "blue")
barplot(CommunityB, col = "red")

?renyi

renyi_comA <- renyi(CommunityA)
renyi_comB <- renyi(CommunityB)
ren_AB <- rbind(renyi_comA, renyi_comB)

matplot(t(ren_AB), type = "l",  axes = F, ylab = "Renyi Diversity")

box()
axis(side = 2)
axis(side = 1, labels = c(0,0.25,0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11)
legend("topright", 
       legend = c("Communitity A", "Community B"), 
       lty = c(1,2), 
       col = c(1,2))
