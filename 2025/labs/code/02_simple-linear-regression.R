#############
### SETUP ###
#############

# install.packages("tidyverse")
library(tidyverse)

# set working directory to wherever your data is
setwd("filepath")

##############
### PART 1 ###
##############

# load data
mlb_team_seasons = read_csv("mlb_team_seasons.csv")

##############
### PART 2 ###
##############

# load data
mlb_payrolls = read_csv("mlb_payrolls.csv")