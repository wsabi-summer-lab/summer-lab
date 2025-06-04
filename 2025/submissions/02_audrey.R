library(ggplot2)
library(dplyr)

mlb_data <- read.csv(file.choose(), stringsAsFactors = FALSE)

print("Column names in your data:")
print(names(mlb_data))
print("First few rows:")
head(mlb_data)

names(mlb_data) <- make.names(names(mlb_data))
print("Cleaned column names:")
print(names(mlb_data))

mlb_clean <- mlb_data %>%
  filter(yearID != 2020,
         !is.na(WP),
         !is.na(Payroll.Median),
         !is.na(Log..Payroll.Median.))

mlb_clean <- mlb_data %>%
  filter(!is.na(WP) & !is.na(Payroll.Median) & !is.na(Log..Payroll.Median.))

print(paste("We have", nrow(mlb_clean), "observations"))
print(paste("Years:", min(mlb_clean$yearID), "to", max(mlb_clean$yearID)))

#Task 1

linear_model <- lm(WP ~ Payroll.Median, data = mlb_clean)
log_model <- lm(WP ~ Log..Payroll.Median., data = mlb_clean)

print(paste("Linear: WP =", round(coef(linear_model)[2], 4), "× Payroll.Median +", round(coef(linear_model)[1], 4)))
print(paste("Log: WP =", round(coef(log_model)[2], 4), "× Log(Payroll.Median) +", round(coef(log_model)[1], 4)))

plot1 <- ggplot(mlb_clean, aes(x = Payroll.Median, y = WP)) +
  
  geom_point(data = subset(mlb_clean, teamID != "OAK" & teamID != "NYA"), 
             color = "gray", alpha = 0.6, size = 2) +
  
  geom_point(data = subset(mlb_clean, teamID == "OAK"), 
             color = "red", size = 4, shape = 17) +
  geom_point(data = subset(mlb_clean, teamID == "NYA"), 
             color = "darkgreen", size = 4, shape = 15) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +
  labs(title = "Winning Percentage vs Payroll/Median",
       subtitle = "Red triangles = Oakland A's, Green squares = Yankees",
       x = "Payroll/Median", 
       y = "Winning Percentage") +
  theme_minimal()

print(plot1)
# Task 2

mlb_clean$predicted_linear <- predict(linear_model)
mlb_clean$predicted_log <- predict(log_model)
mlb_clean$residual_linear <- mlb_clean$WP - mlb_clean$predicted_linear
mlb_clean$residual_log <- mlb_clean$WP - mlb_clean$predicted_log

mlb_clean$wins_above_linear <- mlb_clean$residual_linear * 162
mlb_clean$wins_above_log <- mlb_clean$residual_log * 162

team_stats <- mlb_clean %>%
  group_by(teamID) %>%
  summarise(
    avg_wins_linear = mean(wins_above_linear),
    avg_wins_log = mean(wins_above_log),
    seasons = n()
  ) %>%
  arrange(desc(avg_wins_linear))

print("Top 5 over-performers (Linear Model):")
print(head(team_stats, 5))
print("Bottom 5 under-performers (Linear Model):")
print(tail(team_stats, 5))

plot2 <- ggplot(team_stats, aes(x = reorder(teamID, avg_wins_linear), y = avg_wins_linear)) +
  geom_col(aes(fill = avg_wins_linear > 0)) +
  scale_fill_manual(values = c("red", "darkgreen"), 
                    name = "Performance", 
                    labels = c("Below Expected", "Above Expected")) +
  coord_flip() +
  labs(title = "Team Performance vs Linear Model",
       subtitle = "Average Wins Above/Below Expected per Season",
       x = "Team", 
       y = "Wins Above/Below Expected") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)

print(plot2)


team_stats_log <- team_stats %>% arrange(desc(avg_wins_log))

plot3 <- ggplot(team_stats_log, aes(x = reorder(teamID, avg_wins_log), y = avg_wins_log)) +
  geom_col(aes(fill = avg_wins_log > 0)) +
  scale_fill_manual(values = c("red", "darkgreen"),
                    name = "Performance",
                    labels = c("Below Expected", "Above Expected")) +
  coord_flip() +
  labs(title = "Team Performance vs Log Model", 
       subtitle = "Average Wins Above/Below Expected per Season",
       x = "Team",
       y = "Wins Above/Below Expected") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)

print(plot3)

print("Summary")
print(paste("Linear Model R-squared:", round(summary(linear_model)$r.squared, 3)))
print(paste("Log Model R-squared:", round(summary(log_model)$r.squared, 3)))


