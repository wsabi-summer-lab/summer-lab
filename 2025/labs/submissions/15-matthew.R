#############
### SETUP ###
#############

# install.packages(c("ggplot2", "glmnet", "tidyverse"))
library(ggplot2)
library(glmnet)
library(tidyverse)

set.seed(15)

####################
### NBA LINEUPS ###
####################

# load NBA lineups data
nba_lineups <- readRDS("../data/15_nba-lineups.rds")

# preview data
head(nba_lineups)

############################
### ONE-HOT ENCODING ###
############################

# Get list of all players
all_players <- unique(unlist(nba_lineups[, c("OP1", "OP2", "OP3", "OP4", "OP5", "DP1", "DP2", "DP3", "DP4", "DP5")]))
m <- length(all_players)
player_index <- setNames(seq_len(m), all_players)

# Design matrix X and outcome y
n <- nrow(nba_lineups)
X <- matrix(0, nrow = n, ncol = 1 + 2 * m)  # intercept + offensive + defensive
colnames(X) <- c("Intercept", paste0("Off_", all_players), paste0("Def_", all_players))

# Fill X matrix
for (i in 1:n) {
  X[i, "Intercept"] <- 1
  for (p in paste0("OP", 1:5)) {
    player <- nba_lineups[[p]][i]
    if (!is.na(player)) X[i, paste0("Off_", player)] <- 1
  }
  for (p in paste0("DP", 1:5)) {
    player <- nba_lineups[[p]][i]
    if (!is.na(player)) X[i, paste0("Def_", player)] <- 1
  }
}

y <- nba_lineups$points

############################
### FIT OLS MODEL (APM) ###
############################

ols_fit <- lm(y ~ X - 1)  # remove intercept term since we included it manually
ols_coefs <- coef(ols_fit)

###############################
### FIT RIDGE MODEL (RAPM) ###
###############################

lambdas <- 10^seq(-3, 3, by = 0.2)
ridge_model <- cv.glmnet(x = X, y = y, alpha = 0, lambda = lambdas,
                         family = "gaussian", standardize = FALSE, nfolds = 5)

ridge_coefs <- as.vector(coef(ridge_model, s = "lambda.min"))

##################################
### VISUALIZATION & COMPARISON ###
##################################

# Extract player names from coefficient names (excluding intercept)
player_names <- all_players

ols_off <- ols_coefs[paste0("XOff_", player_names)]
ols_def <- ols_coefs[paste0("XDef_", player_names)]

ridge_off <- ridge_coefs[paste0("Off_", player_names)]
ridge_def <- ridge_coefs[paste0("Def_", player_names)]

# Build data frame for plotting
coef_df <- data.frame(
  Player = rep(player_names, times = 2),
  Type = rep(c("Offense", "Defense"), each = m),
  OLS = c(ols_off, ols_def),
  Ridge = c(ridge_off, ridge_def)
)

# Reshape for ggplot
coef_long <- coef_df %>% 
  pivot_longer(cols = c("OLS", "Ridge"), names_to = "Model", values_to = "Coefficient")

# Plot offensive and defensive impact separately
p1 <- ggplot(filter(coef_long, Type == "Offense"),
             aes(x = reorder(Player, Coefficient), y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Offensive Impact: OLS vs Ridge", y = "Coefficient", x = "Player") +
  theme_minimal()

p2 <- ggplot(filter(coef_long, Type == "Defense"),
             aes(x = reorder(Player, Coefficient), y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Defensive Impact: OLS vs Ridge", y = "Coefficient", x = "Player") +
  theme_minimal()

# Show plots
print(p1)
print(p2)
