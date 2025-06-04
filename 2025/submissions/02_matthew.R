# PART 1

library(tidyverse)

mlb_team_seasons <- read.csv("C:/Users/matts/Downloads/02_mlb-team-seasons.csv")
head(mlb_team_seasons)

df <- mlb_team_seasons |>
  select(teamID, RS,RA,WP,WP_Pythag_2)|>
  mutate(
    logratio=log(RS/RA),
    wpratio=log((WP)/(1-WP))
  )|>
  select (teamID,logratio,wpratio)
head(df)
model <- lm(wpratio ~ logratio+0, data = df)
model

library(ggplot2)

ggplot(df, aes(x = logratio, y = wpratio)) +
  geom_point() +                              # scatter plot of data points
  geom_smooth(method = "lm", se = TRUE) +    # adds linear regression line with confidence interval
  theme_minimal() +
  labs(title = "Linear Model: wpratio vs logratio",
       x = "logratio",
       y = "wpratio")

alpha <- coef(model)["logratio"]
mlb_team_seasons <- mlb_team_seasons |>
  mutate(
    MyPrediction= RS^alpha/(RS^alpha+RA^alpha)
  )

# Plot WP vs WP_Pythag_2 with axis limits and diagonal line
ggplot(mlb_team_seasons, aes(x = WP_Pythag_2, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  # diagonal line
  coord_cartesian(xlim = c(.25,.75), ylim = c(.25,.75)) +
  labs(title = "WP vs WP_Pythag_2",
       x = "WP_Pythag_2",
       y = "WP")

# Plot WP vs MyPrediction with axis limits and diagonal line
ggplot(mlb_team_seasons, aes(x = MyPrediction, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  # diagonal line
  coord_cartesian(xlim = c(.25,.75), ylim = c(.25,.75)) +
  labs(title = "WP vs MyPrediction",
       x = "MyPrediction",
       y = "WP")

mlb_team_seasons<-mlb_team_seasons|>
  mutate(
    respythag=WP-WP_Pythag_2,
    resme=WP-MyPrediction
  )

Pythag=1-sd(mlb_team_seasons$respythag)/sd(mlb_team_seasons$WP)
Me=1-sd(mlb_team_seasons$resme)/sd(mlb_team_seasons$WP)

library(ggplot2)

# Create a data frame with your values
resid_df <- data.frame(
  Model = c("Pythag", "Me"),
  ResidualError = c(Pythag, Me)
)

# Plot the bar graph
ggplot(resid_df, aes(x = Model, y = ResidualError, fill = Model)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  labs(title = "Reduction in Error Comparison", x = "Model", y = "Residual Std Dev") +
  theme_minimal()

# PART 2

library(ggplot2)

df <- read.csv("C:/Users/matts/Downloads/02_mlb-payrolls.csv")
head(df)

df<-df|>
  filter(yearID!=2020)|>
  select(name,yearID,WP,Payroll.Median,Log..Payroll.Median.)

ggplot(data = df, aes(x = Payroll.Median, y = WP*162)) +
  geom_point() +
  labs(title = "WP vs Payroll/Median",
       x = "Payroll Median",
       y = "Winning Percentage")
ggplot(data = df, aes(x = Log..Payroll.Median., y = WP*162)) +
  geom_point() +
  labs(title = "WP vs Log Payroll/Median",
       x = "Payroll Median",
       y = "Winning Percentage")

unique(df$name)

df <- df |>
  mutate(team_color = case_when(
    name == "Oakland Athletics" ~ "Oakland",
    name == "New York Yankees" ~ "Yankees",
    TRUE ~ "Other"
  ))

ggplot(df, aes(x = Payroll.Median, y = WP*162, color = team_color)) +
  geom_point() +
  scale_color_manual(values = c(
    "Oakland" = "green",
    "Yankees" = "blue",
    "Other" = "gray"
  )) +
  theme_minimal()

ggplot(df, aes(x = Log..Payroll.Median., y = WP*162, color = team_color)) +
  geom_point() +
  scale_color_manual(values = c(
    "Oakland" = "green",
    "Yankees" = "blue",
    "Other" = "gray"
  )) +
  theme_minimal()

ggplot(df, aes(x = Payroll.Median, y = WP*162, color = team_color)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # single overall fit line in black
  scale_color_manual(values = c(
    "Oakland" = "green",
    "Yankees" = "blue",
    "Other" = "gray"
  )) +
  theme_minimal()

ggplot(df, aes(x = Log..Payroll.Median., y = WP*162, color = team_color)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # single overall fit line in black
  scale_color_manual(values = c(
    "Oakland" = "green",
    "Yankees" = "blue",
    "Other" = "gray"
  )) +
  theme_minimal()

model1 <- lm(WP ~ Payroll.Median, data = df)
model2 <- lm(WP ~ Log..Payroll.Median., data = df)
df$residuals_model1 <- residuals(model1)
df$residuals_model2 <- residuals(model2)

library(dplyr)

# Example: suppose you want average difference of residuals_model1 - residuals_model2 per team
df_summary <- df %>%
  group_by(name) %>%
  summarize(avg_diff1 = mean(residuals_model1, na.rm = TRUE),
            avg_diff2 = mean(residuals_model2, na.rm = TRUE))

# View result
print(df_summary, n=33)

library(ggplot2)
library(dplyr)

# Bar graph for avg_diff1, sorted descending
ggplot(df_summary %>% arrange(desc(avg_diff1)), aes(x = reorder(name, avg_diff1), y = avg_diff1)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # flip for easier reading
  labs(title = "Average Difference 1 by Team",
       x = "Team",
       y = "Average Difference 1") +
  theme_minimal()

# Bar graph for avg_diff2, sorted descending
ggplot(df_summary %>% arrange(desc(avg_diff2)), aes(x = reorder(name, avg_diff2), y = avg_diff2)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Average Difference 2 by Team",
       x = "Team",
       y = "Average Difference 2") +
  theme_minimal()