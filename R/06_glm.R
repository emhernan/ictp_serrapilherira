# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Inplementing glm and glmm in R
# First version 2022-07-20
# --------------------------------------------------#

# Loading needed packages
library(dplyr)
library(lme4)
library(merTools)
library(ggplot2)
#library(bbmle)

# Going back to lm to fit a Gaussian glm ---------------------------------------
cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")

h1 <- glm(Beg ~ Mass, data = cuckoo,
          family = poisson(link = log))

h2 <- glm(Beg ~ Mass + Species, data = cuckoo,
          family = poisson(link = log))

h3 <- glm(Beg ~ Mass * Species, data = cuckoo,
          family = poisson(link = log))

h0 <- glm(Beg ~ 0, data = cuckoo,
          family = poisson(link = log))


## -Cuckoo glm--- ---------------------------------------------------------------cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")
summary(h2)

AIC(h0)
AIC(h1)
AIC(h2)

bbmle::AICtab(h0, h1, h2, h3, base = TRUE, weights = TRUE)

# Calculating the predicted values
newdata <- expand.grid(Mass = seq(min(cuckoo$Mass), max(cuckoo$Mass), length.out = 200),
                       Species = unique(cuckoo$Species))
newdata$Beg <- predict(h3, newdata, type = 'response')

summary(exp(predict(h3, newdata)))
## explore ?predict.glm

p <- ggplot(mapping = aes(x = Mass, y = Beg, colour = Species)) +
  geom_point(data = cuckoo) +  geom_line(data = newdata) +
  theme_classic()




# Bacterial growth -------------------------------------------------------------
#ac <- read.csv("data/raw/valletta_bac.csv")

bac$media <- as.factor(bac$media)
bac$cabinet <- as.factor(bac$cabinet)

bac_lm <- lm(growth ~ media, data = bac)

summary(bac_lm)


ac_lm2 <- lm(growth ~ media + cabinet, data = bac)

summary(bac_lm2)


# Building the mixed model with cabined as a random effect#ac_lmer <- lmer(growth ~ media + (1 | cabinet), data = bac)

summary(bac_lmer)


## -Simulating the confidence interval of the residualsfeEx <- FEsim(bac_lmer, 1000)


fe <- plotFEsim(feEx) +
  theme_bw() + labs(title = "Coefficient Plot",
                    x = "Median Effect Estimate", y = "Evaluation Rating")


fe

