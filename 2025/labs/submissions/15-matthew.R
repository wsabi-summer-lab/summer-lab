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
### ONE-HOT ENCODING FIX ###
############################

# Split lineup strings into player IDs
lineup_team_list <- strsplit(nba_lineups$lineup_team, ",\\s*")
lineup_opp_list  <- strsplit(nba_lineups$lineup_opp, ",\\s*")

# Get all unique players
all_players <- sort(unique(unlist(c(lineup_team_list, lineup_opp_list))))
m <- length(all_players)

# Create index
player_index <- setNames(seq_len(m), all_players)

# Initialize X matrix: intercept + m offense + m defense
n <- nrow(nba_lineups)
X <- matrix(0, nrow = n, ncol = 1 + 2 * m)
colnames(X) <- c("Intercept", paste0("Off_", all_players), paste0("Def_", all_players))

# Fill in the design matrix
for (i in 1:n) {
  X[i, "Intercept"] <- 1
  for (player in lineup_team_list[[i]]) {
    if (!is.na(player)) {
      X[i, paste0("Off_", player)] <- 1
    }
  }
  for (player in lineup_opp_list[[i]]) {
    if (!is.na(player)) {
      X[i, paste0("Def_", player)] <- 1
    }
  }
}

# Outcome variable
y <- nba_lineups$pts_poss

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

ridge_off <- ridge_coefs[match(paste0("Off_", player_names), rownames(coef(ridge_model)))]
ridge_def <- ridge_coefs[match(paste0("Def_", player_names), rownames(coef(ridge_model)))]

# Build data frame for plotting
coef_df <- data.frame(
  Player = rep(player_names, times = 2),
  Type = rep(c("Offense", "Defense"), each = m),
  OLS = c(ols_off, ols_def),
  Ridge = c(ridge_off, ridge_def)
)

# Sample 25 players for plotting
sample_players <- sample(player_names, 25)
coef_df_sample <- coef_df %>% filter(Player %in% sample_players)

# Reshape for ggplot
coef_long <- coef_df_sample %>% 
  pivot_longer(cols = c("OLS", "Ridge"), names_to = "Model", values_to = "Coefficient")

# Sort players by true offensive or defensive coefficients for better comparison
coef_long <- coef_long %>% group_by(Type) %>% mutate(Player = fct_reorder(Player, Coefficient)) %>% ungroup()

# Plot offensive and defensive impact separately
p1 <- ggplot(filter(coef_long, Type == "Offense"),
             aes(x = Player, y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Offensive Impact (Sample of 25): OLS vs Ridge", y = "Coefficient", x = "Player") +
  theme_minimal()

p2 <- ggplot(filter(coef_long, Type == "Defense"),
             aes(x = Player, y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Defensive Impact (Sample of 25): OLS vs Ridge", y = "Coefficient", x = "Player") +
  theme_minimal()

# Show plots
print(p1)
print(p2)
