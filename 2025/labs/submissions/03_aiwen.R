#############
### SETUP ###
#############

rm(list=ls())
# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(data.table)

# set working directory to wherever your data is
# setwd("filepath")

##############
### PART 1 ###
##############

# load data
nba_four_factors = as.data.table(read_csv("../data/03_nba-four-factors.csv"))
names(nba_four_factors)

# task 1 eda

setnames(nba_four_factors, old = c("EFG%", "OPP EFG%", "OREB%", "DREB%", "TOV%", "OPP TOV %", "FT Rate", "OPP FT Rate"), 
         new = c("efg", "opp_efg", "oreb", "dreb", "tov", "opp_tov", "ft_rate", "opp_ft_rate"))

nba_four_factors <- nba_four_factors %>% 
  mutate(x1 = efg - opp_efg,
         x2 = oreb - dreb,
         x3 = tov - opp_tov,
         x4 = ft_rate - opp_ft_rate)

summary(nba_four_factors)
sd(nba_four_factors$W)
sd(nba_four_factors$x1)
sd(nba_four_factors$x2)
sd(nba_four_factors$x3)
sd(nba_four_factors$x4)

ggplot(nba_four_factors, aes(x = W)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x1)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x2)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x3)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x4)) +
  geom_histogram()

corr_matrix <- nba_four_factors %>%
  select(W, x1, x2, x3, x4) %>%
  cor()
corr_matrix

# task 2 modeling

model_1 <- lm(W ~ x1 + x2 + x3 + x4, data = nba_four_factors)
summary(model_1)

nba_four_factors <- nba_four_factors %>% 
  mutate(x1_std = (x1 - mean(x1)) / sd(x1),
         x2_std = (x2 - mean(x2)) / sd(x2),
         x3_std = (x3 - mean(x3)) / sd(x3),
         x4_std = (x4 - mean(x4)) / sd(x4))

model_2 <- lm(W ~ x1_std + x2_std + x3_std + x4_std, data = nba_four_factors)
summary(model_2)

# standardized model (model_2) is better because all the x variables are on the same scale
coefs <- coef(summary(model_2))[-1, 1] # coefficients of each standardized x variable
ordered_coefs <- sort(abs(coefs), decreasing = TRUE)
ordered_coefs

# comparing two models
n <- dim(nba_four_factors)[1]
train_indices <- sample(1:n, size = floor(0.5 * n))

train <- nba_four_factors[train_indices, ]
test <- nba_four_factors[-train_indices, ]
names(train)

mod1 <- lm(W ~ x1 + x2 + x3 + x4, data = train)
mod2 <- lm(W ~ x1 + x2 + x3 + x4, data = train_std)

##############
### PART 2 ###
##############

# load data
punts <- read_csv("../data/03_punts.csv")
names(punts)

# scatterplots

