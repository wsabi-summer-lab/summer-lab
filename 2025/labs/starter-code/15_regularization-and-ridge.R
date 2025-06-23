#############
### SETUP ###
#############

# install.packages(c("ggplot2", "glmnet", "tidyverse"))
library(ggplot2)
library(glmnet)
library(tidyverse)

# set seed
set.seed(15)

####################
### NBA LINEUPS ###
####################

# load NBA lineups data
nba_lineups = readRDS("../data/15_nba-lineups.rds")
# preview data
head(nba_lineups)