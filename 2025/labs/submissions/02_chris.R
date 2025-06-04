install.packages("tidyverse")

library(tidyverse)

###############
#### Part 1 ###
###############

# Load in csv
mlb_tem_seasons = read.csv("/Users/chrisbugs/Downloads/02_mlb-team-seasons.csv")

mlb_tem_seasons 

mlb_team_seasons <- mlb_team_seasons %>%
  mutate(
    log_WP = log(WP / (1 - WP)),
    X = log(RS) - log(RA)
  )

model = lm(log_WP ~ X, data = mlb_team_seasons)

alpha = coef(model)["X"]
alpha

mlb_team_seasons <- mlb_team_seasons %>% 
  mutate(
    WP_hat_fit = RS^alpha / (RS^alpha + RA^alpha),
    WP_hat_og = RS^2 / (RS^2 + RA^2),
    resid_fit = model$residuals,
    resid_og = WP - WP_hat_og
  )

re <- function(resid, response) {
  1 - ( sd(resid, na.rm = TRUE) / sd(response, na.rm = TRUE) )
}

re_fit = re(mlb_team_seasons$resid_fit, mlb_team_seasons$log_WP)
re_og = re(mlb_team_seasons$resid_og, mlb_team_seasons$WP)

re_fit
re_og

install.packages("ggplot2")

library(ggplot2)

# 1. Assemble a minimal data frame
re_df <- data.frame(
  Model = factor(
    c(sprintf("Fitted α = %.3f", alpha), "Classic α = 2"),
    levels = c(sprintf("Fitted α = %.3f", alpha), "Classic α = 2")
  ),
  RE = c(re_fit, re_og)
)

# 2. Plot
ggplot(re_df, aes(x = Model, y = RE)) +
  geom_col(width = 0.55, fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(RE, accuracy = 0.1)),
    vjust = -0.35,
    size  = 4
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format()
  ) +
  labs(
    title = "Reduction in Error: Fitted vs. Classic Pythagorean Model",
    y     = "Reduction in Error (RE)",
    x     = NULL
  ) +
  theme_minimal(base_size = 12)

###############
#### Part 2 ###
###############

# Load in csv
mlb_payrolls = read.csv("/Users/chrisbugs/Downloads/02_mlb-payrolls.csv")

mlb_payrolls
mlb_payrolls <- mlb_payrolls %>%
  filter(yearID != "2020")

# Scatterplot of winning percentage versus median payroll
ggplot(mlb_payrolls, aes(x = Payroll.Median, y = WP)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_point(data = filter(mlb_payrolls, teamID == "OAK"),
             colour = "forestgreen", size = 3) +
  geom_point(data = filter(mlb_payrolls, teamID == "NYA"),
             colour = "darkorange", size = 3) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "MLB Winning % vs. Relative Payroll",
    subtitle = "Green: OAK, Orange: NYY, Solid: payroll/median, Dashed: log(payroll/median)",
    x        = "Team Payroll over League-Median Payroll",
    y        = "Winning Percentage (WP)"
  ) +
  theme_minimal(base_size = 12)


# Scatterplot of winning percentage versus median payroll w/ normal regression line
ggplot(mlb_payrolls, aes(x = Payroll.Median, y = WP)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_point(data = filter(mlb_payrolls, teamID == "OAK"),
             colour = "forestgreen", size = 3) +
  geom_point(data = filter(mlb_payrolls, teamID == "NYA"),
             colour = "darkorange", size = 3) +
  geom_smooth(method  = "lm",
              formula = y ~ x,
              colour  = "steelblue",
              linewidth = 0.9,
              se = FALSE) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "MLB Winning % vs. Relative Payroll",
    subtitle = "Green: OAK, Orange: NYY, Line: payroll/median",
    x        = "Team Payroll over League-Median Payroll",
    y        = "Winning Percentage (WP)"
  ) +
  theme_minimal(base_size = 12)


# Scatterplot of winning percentage versus median payroll w/ log regression line
ggplot(mlb_payrolls, aes(x = Payroll.Median, y = WP)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_point(data = filter(mlb_payrolls, teamID == "OAK"),
             colour = "forestgreen", size = 3) +
  geom_point(data = filter(mlb_payrolls, teamID == "NYA"),
             colour = "darkorange", size = 3) +
  geom_smooth(method  = "lm",
              formula = y ~ log(x),
              colour  = "steelblue",
              linewidth = 0.9,
              se = FALSE) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "MLB Winning % vs. Relative Payroll",
    subtitle = "Green: OAK, Orange: NYY, Line: payroll/median",
    x        = "Team Payroll over League-Median Payroll",
    y        = "Winning Percentage (WP)"
  ) +
  theme_minimal(base_size = 12)

model_normal = lm(WP ~ Payroll.Median, data = mlb_payrolls)

model_log = lm(WP ~ Log..Payroll.Median., data = mlb_payrolls)

# Difference between actual WP and predicted WP
mlb_payrolls <- mlb_payrolls %>%
  mutate(
    WP_hat_normal = predict(model_normal),
    WP_hat_log = predict(model_log),
    diff_normal = WP - WP_hat_normal,
    diff_log = WP - WP_hat_log
  )

# Average distance between actual WP and predicted WP for each team
mlb_payrolls <- mlb_payrolls %>%
  group_by(teamID) %>%
  summarise(
    avg_diff_normal = mean(diff_normal, na.rm = TRUE),
    avg_diff_log = mean(diff_log, na.rm = TRUE)
  )

# Graph of average distance by team
mlb_payrolls %>% 
  arrange(desc(avg_diff_normal)) %>% 
  mutate(teamID = factor(teamID, levels = teamID)) %>% 
  ggplot(aes(teamID, avg_diff_normal)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x * 100)) +
  labs(title = "Linear Model – Average (Actual − Predicted) Winning %",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

# Graph of average distance by team with log 
mlb_payrolls %>% 
  arrange(desc(avg_diff_log)) %>% 
  mutate(teamID = factor(teamID, levels = teamID)) %>% 
  ggplot(aes(teamID, avg_diff_log)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x * 100)) +
  labs(title = "Log Model – Average (Actual − Predicted) Winning %",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

# Converted to wins
mlb_payrolls_wins <- mlb_payrolls %>%
  mutate(
    avg_diff_normal_wins = avg_diff_normal * 162,
    avg_diff_log_wins = avg_diff_log * 162
  )
  
# Graph of average distance by team (wins version)
mlb_payrolls_wins %>% 
  arrange(desc(avg_diff_normal_wins)) %>% 
  mutate(teamID = factor(teamID, levels = teamID)) %>% 
  ggplot(aes(teamID, avg_diff_normal_wins)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  labs(title = "Linear Model – Average (Actual − Predicted) Wins",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

# Graph of average distance by team with log (wins version)
mlb_payrolls_wins %>% 
  arrange(desc(avg_diff_log_wins)) %>% 
  mutate(teamID = factor(teamID, levels = teamID)) %>% 
  ggplot(aes(teamID, avg_diff_log_wins)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  labs(title = "Log Model – Average (Actual − Predicted) Wins",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())
