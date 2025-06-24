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

nba_lineups_fast <- nba_lineups %>%
  mutate(
    home_ids = str_extract_all(lineup_team, "\\d{7}"),
    away_ids = str_extract_all(lineup_opp, "\\d{7}"),
    all_ids  = map2(home_ids, away_ids, ~ unique(c(.x, .y))),
    row_id   = row_number()
  )

long_ids <- nba_lineups_fast %>%
  select(row_id, all_ids) %>%
  unnest(all_ids, keep_empty = TRUE)

one_hot <- long_ids %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = all_ids, values_from = value, values_fill = 0, names_prefix = "player_")

nba_encoded <- nba_lineups_fast %>%
  left_join(one_hot, by = "row_id")

#OLS fit
nba_apm <- nba_lineups_fast %>%
  left_join(one_hot, by = "row_id") %>%
  mutate(
    y = if_else(pts_poss > 0,  1L, -1L))

df_apm <- nba_apm %>%
  select(
    y,
    starts_with("player_"))

apm_fit <- lm(y ~ . - 1, data = df_apm)

summary(apm_fit)

#Ridge Fit

X = as.matrix(one_hot)
# set lambdas to cross−validate
lambdas = 10^seq(-3, 3, by = 0.2)

ridge_model = cv.glmnet(x = X, y = nba_encoded$pts_poss, 
                        nfolds = 5, alpha = 0, family = "gaussian",
                        lambda = lambdas, standardize = FALSE)

ridge_model$lambda.min

plot(ridge_model)


#VISUALIZE
ols_vec <- coef(apm_fit)
ols_coefs <- data.frame(
  player = names(ols_vec),
  ols    = as.numeric(ols_vec),
  stringsAsFactors = FALSE
)

# 2) extract ridge coefficients at lambda.min
ridge_mat   <- as.matrix(coef(ridge_model, s="lambda.min"))
ridge_coefs <- data.frame(
  player = rownames(ridge_mat),
  ridge  = as.numeric(ridge_mat[,1]),
  stringsAsFactors = FALSE
)

ols_coefs   <- subset(ols_coefs,   player != "(Intercept)")
ridge_coefs <- subset(ridge_coefs, player != "(Intercept)")

coef_compare <- merge(ols_coefs, ridge_coefs, by = "player")

ggplot(coef_compare, aes(x = ols, y = ridge)) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_vline(xintercept = 0, color = "gray80") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.6) +
  labs(
    title    = "OLS vs Ridge Adjusted Plus–Minus",
    subtitle = paste0("Ridge λ_min = ", signif(ridge_model$lambda.min, 2)),
    x        = "OLS APM",
    y        = "Ridge APM"
  ) +
  theme_minimal()


#VISUAL 2
n         <- nrow(df_apm)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))
train_df  <- df_apm[train_idx, ]
test_df   <- df_apm[-train_idx, ]

###################
# REFIT OLS APM   #
###################

# (we refit on train so we can predict out‐of‐sample)
apm_fit_train <- lm(y ~ . - 1, data = train_df)

############################
# PREDICT ON TEST SET      #
############################

test_df <- test_df %>%
  mutate(
    y_true = y,
    y_pred = predict(apm_fit_train, newdata = test_df)
  )

##############################
# PLOT PREDICTED vs ACTUAL   #
##############################

plot = ggplot(test_df, aes(x = y_true, y = y_pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Out-of-Sample APM: Predicted vs. Actual",
    x     = "Actual y (±1)",
    y     = "Predicted y"
  ) +
  theme_minimal()

plot
