#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("../data/03_nba-four-factors.csv")

##############
### PART 2 ###
##############

# load data
punts = read_csv("../data/03_punts.csv")