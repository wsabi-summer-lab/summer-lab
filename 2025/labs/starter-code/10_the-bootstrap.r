#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(10)

####################
### HORSE RACING ###
####################

# number of races
n_races = 1000
# set true win probabilities
p_1 = 0.3
p_2 = 0.5
p_3 = 0.2