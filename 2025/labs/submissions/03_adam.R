#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set working directory to wherever your data is
setwd("filepath")

##############
### PART 1 ###
##############

# load data
mlb_team_seasons = read_csv("../data/03_nba-four-factors.csv")

##############
### PART 2 ###
##############

# load data
mlb_payrolls = read_csv("../data/03_punts.csv")

#start