library(dplyr)
library(ggplot2)
library(nnet)       
library(splines)   

df <- read.csv(file.choose())
head(df)
str(df)

#1.1Line
df$pts_next_score <- factor(df$pts_next_score)
model1 <- multinom(pts_next_score ~ yardline_100, data = df)
yard_seq <- seq(1, 99, by = 1)
new_df1 <- data.frame(yardline_100 = yard_seq)
probs1 <- predict(model1, newdata = new_df1, type = "probs")
k_values <- as.numeric(as.character(levels(df$pts_next_score)))
ep1 <- as.vector(probs1 %*% k_values)

plot_df1 <- data.frame(
  yardline_100 = yard_seq,
  EP = ep1
)

ggplot(plot_df1, aes(x = yardline_100, y = EP)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "EP vs Yard Line Linear",
       x = "Yard Line",
       y = "Expected Points")

#1.2Spline
model2 <- multinom(pts_next_score ~ bs(yardline_100, df = 5), data = df)
probs2 <- predict(model2, newdata = new_df1, type = "probs")
ep2 <- as.vector(probs2 %*% k_values)
plot_df2 <- data.frame(
  yardline_100 = yard_seq,
  EP = ep2
)
ggplot(plot_df2, aes(x = yardline_100, y = EP)) +
  geom_line(color = "darkgreen", size = 1) +
  theme_minimal() +
  labs(title = "EP vs Yard Line Spline",
       x = "Yard Line",

#1.3Downs
df$down <- factor(df$down)
model3 <- multinom(pts_next_score ~ bs(yardline_100, df = 5) + down, data = df)
yard_seq <- seq(1, 99, by = 1)
down_levels <- levels(df$down)
new_df3 <- expand.grid(
  yardline_100 = yard_seq,
  down = down_levels
)
probs3 <- predict(model3, newdata = new_df3, type = "probs")
ep3 <- as.vector(probs3 %*% k_values)
plot_df3 <- data.frame(
  yardline_100 = new_df3$yardline_100,
  down = new_df3$down,
  EP = ep3
)

ggplot(plot_df3, aes(x = yardline_100, y = EP, color = down)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "EP vs Yard Line by Downs)",
       x = "Yard Line",
       y = "Expected Points")

#1.4Yards
names(df)
df$log_ydstogo <- log(df$ydstogo + 1)
model4 <- multinom(pts_next_score ~ bs(yardline_100, df = 5) + down + log_ydstogo, data = df)
yards_seq <- seq(1, 99, by = 1)
log_yds_seq <- log(c(1, 5, 10, 15, 20) + 1)  
down_levels <- levels(df$down)
new_df4 <- expand.grid(
  yardline_100 = yards_seq,
  down = down_levels,
  log_ydstogo = log_yds_seq
)
probs4 <- predict(model4, newdata = new_df4, type = "probs")
ep4 <- as.vector(probs4 %*% k_values)
plot_df4 <- data.frame(
  yardline_100 = new_df4$yardline_100,
  down = new_df4$down,
  log_ydstogo = new_df4$log_ydstogo,
  EP = ep4
)
ggplot(plot_df4, aes(x = yardline_100, y = EP, color = log_ydstogo)) +
  geom_line(size = 1) +
  facet_wrap(~ down) +
  theme_minimal() +
  labs(title = "EP vs Yard Line Yards to Go)",
       x = "Yard Line",
       y = "Expected Points",
       color = "log_ydstogo")

#1.5
model5 <- multinom(pts_next_score ~ bs(yardline_100, df = 5) + down + log_ydstogo + half_seconds_remaining, data = df)
time_seq <- seq(0, 1800, by = 100)

new_df5 <- expand.grid(
  yardline_100 = yards_seq,
  down = "1",         
  log_ydstogo = log(10 + 1),    
  half_seconds_remaining = time_seq
)
probs5 <- predict(model5, newdata = new_df5, type = "probs")
ep5 <- as.vector(probs5 %*% k_values)

plot_df5 <- data.frame(
  yardline_100 = new_df5$yardline_100,
  half_seconds_remaining = new_df5$half_seconds_remaining,
  EP = ep5
)
ggplot(plot_df5, aes(x = yardline_100, y = EP, color = half_seconds_remaining)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "EP vs Yard Line by Time Remaining",
       x = "Yard Line",
       y = "Expected Points",
       color = "Time Remaining (s)")

#2Quality
model_M <- multinom(pts_next_score ~ bs(yardline_100, df = 5) + down + log_ydstogo + half_seconds_remaining + posteam_spread, data = df)
new_dfM <- expand.grid(
  yardline_100 = yards_seq,
  down = "1",
  log_ydstogo = log(10 + 1),
  half_seconds_remaining = time_seq,
  posteam_spread = 0    # Set spread = 0
)
probsM <- predict(model_M, newdata = new_dfM, type = "probs")
epM <- as.vector(probsM %*% k_values)
comparison_df <- data.frame(
  yardline_100 = new_dfM$yardline_100,
  half_seconds_remaining = new_dfM$half_seconds_remaining,
  EP_M = epM,
  EP_no_spread = plot_df5$EP
)
ggplot(comparison_df, aes(x = EP_no_spread, y = EP_M)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "darkblue", linetype = "dashed") +
  theme_minimal() +
  labs(title = "EP w & w/o Adj.",
       x = "EP w/o spread",
       y = "EP w spread = 0")
