#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(9)

################
### SKITTLES ###
################

# number of each color
n_red = 10
n_orange = 10
n_yellow = 10
n_green = 10
n_purple = 10
# total number of skittles
n_total = n_red + n_orange + n_yellow + n_green + n_purple

# construct a vector of colors
skittles = c(rep("red", n_red), rep("orange", n_orange), rep("yellow", n_yellow), rep("green", n_green), rep("purple", n_purple))

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/09_nba-free-throws.csv", delim = ";")