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
head(nba_lineups$lineup_team, n=2)
head(nba_lineups$lineup_opp, n=2)
nba_lineups


players_df <- nba_lineups %>%                       # your lineup-level data
  select(lineup_team, lineup_opp) %>%               # keep the two roster columns
  pivot_longer(everything(), values_to = "raw") %>% # stack team & opp into one column
  separate_rows(raw, sep = ",") %>%                 # one player per row
  mutate(raw = str_trim(raw)) %>%                   # trim leading/trailing spaces
  separate(                                          # split “ID Name” → ID | Name
    raw,
    into = c("player_id", "player_name"),
    sep  = " ",
    extra = "merge",                                # keep the rest of the string as name
    fill  = "right"
  ) %>% 
  mutate(player_id = as.integer(player_id)) %>%     # numeric ID is handy later
  distinct(player_id, player_name) %>%              # de-duplicate
  arrange(player_name) 

nrow(players_df)

print(players_df, n=572)

library(caret)

only_names = players_df %>%
  select(player_name) %>%
  arrange(player_name)

# Create a dummy variable for each player
players_dummies <- dummyVars(" ~ .", data=only_names)
players_matrix <- predict(players_dummies, newdata=only_names)
players_matrix <- as.data.frame(players_matrix)

off_cols <- paste0(players_df$player_name, "_off")
def_cols <- paste0(players_df$player_name, "_def")
all_cols <- c(off_cols, def_cols)

players_df <- nba_lineups %>%                       
  select(lineup_team, lineup_opp) %>%               
  pivot_longer(everything(), values_to = "raw") %>% 
  separate_rows(raw, sep = ",") %>%                 
  mutate(raw = str_trim(raw),
         player_id   = str_extract(raw, "^[0-9]+") %>% as.integer(),
         player_name = str_remove(raw, "^[0-9]+ ") %>% str_trim()) %>% 
  distinct(player_id, player_name) %>%              
  arrange(player_name)

# ------------------------------------------------------------------
# 2. Long table of (row, player, role)
# ------------------------------------------------------------------
long_roles <- nba_lineups %>% 
  mutate(row_id = row_number()) %>%                               # track original row
  pivot_longer(c(lineup_team, lineup_opp),
               names_to  = "side",
               values_to = "raw") %>% 
  mutate(role = if_else(side == "lineup_team", "off", "def")) %>% # offense / defense
  separate_rows(raw, sep = ",") %>%                               # explode roster list
  mutate(raw         = str_trim(raw),
         player_name = str_remove(raw, "^[0-9]+ ") %>% str_trim(),
         role_label  = paste0(make.names(player_name), "_", role),
         value       = 1L) %>%                                    # indicator
  select(row_id, role_label, value)

# ------------------------------------------------------------------
# 3. Wide one-hot table: 1 = in lineup, 0 = otherwise
# ------------------------------------------------------------------
players_matrix_df <- long_roles %>% 
  pivot_wider(id_cols     = row_id,
              names_from  = role_label,
              values_from = value,
              values_fill = list(value = 0L)) %>% 
  arrange(row_id)

players_matrix_df

nba_lineups


X_df <- players_matrix_df %>% select(-row_id)        # data-frame for lm
X    <- as.matrix(X_df)   

y <- nba_lineups$pts_poss

apm_ols <- lm(y ~ ., data = cbind(y = y, X_df))

lambdas  <- 10^seq(-3, 3, by = 0.2)          # grid to search
ridge_cv <- cv.glmnet(
  x            = X,
  y            = y,
  alpha        = 0,                          # ridge
  lambda       = lambdas,
  nfolds       = 5,
  standardize  = FALSE,
  family       = "gaussian"
)

ridge_cv$lambda.min
plot(ridge_cv)

pred_ols   <- predict(apm_ols)
pred_ridge <- as.numeric(predict(ridge_cv, newx = X))

mse_insample <- tibble(
  Model = c("OLS (APM)", "Ridge (RAPM)"),
  MSE   = c(
    mean((y - pred_ols  )^2),
    mean((y - pred_ridge)^2)
  )
)

# Plot model performances using ggplot2 and mse_insample
ggplot(mse_insample, aes(x = Model, y = MSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "In-sample MSE of APM vs RAPM",
       x    = "Model",
       y    = "Mean Squared Error (MSE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
