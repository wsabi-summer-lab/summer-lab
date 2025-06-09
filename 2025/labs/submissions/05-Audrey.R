
df <- read.csv(file.choose())

global_mean <- mean(df$INN_RUNS)
library(dplyr)

naive_park_effects <- df %>%
  group_by(PARK) %>%
  summarise(mean_runs = mean(INN_RUNS),
            naive_park_effect = mean_runs - global_mean)
print(naive_park_effects)

#ADJ. MOD
model <- lm(INN_RUNS ~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data = df)
park_effects <- coef(model)[grep("factor\\(pi\\)", names(coef(model)))]
park_effects_df <- data.frame(
  pi = gsub("factor\\(PARK\\)", "", names(park_effects)),
  adjusted_park_effect = park_effects
)
print(park_effects_df)

#COMPARE
set.seed(123)
train_idx <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]
naive_train_means <- train_df %>%
  group_by(PARK) %>%
  summarise(mean_runs = mean(INN_RUNS))
test_df <- test_df %>%
  left_join(naive_train_means, by = "PARK")
head(test_df
test_df$pred_naive <- test_df$mean_runs

model_adj <- lm(INN_RUNS ~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data = train_df)
test_df$pred_adj <- predict(model_adj, newdata = test_df)

rmse_naive <- sqrt(mean((test_df$INN_RUNS - test_df$pred_naive)^2, na.rm = TRUE))
rmse_adj <- sqrt(mean((test_df$INN_RUNS - test_df$pred_adj)^2, na.rm = TRUE))

cat("Naive RMSE:", rmse_naive, "\n")
cat("Adjusted RMSE:", rmse_adj, "\n")

comparison_df <- naive_park_effects %>%
  left_join(park_effects_df, by = c("PARK" = "pi"))


library(ggplot2)
ggplot(comparison_df, aes(x = naive_park_effect, y = adjusted_park_effect)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Naive vs Adj. Park Effects",
       x = "Naive Park Effect",
       y = "Adj.Park Effect")
