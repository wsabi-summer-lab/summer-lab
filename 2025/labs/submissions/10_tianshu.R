#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

p = c(0.6, 0.3, 0.1)
alpha = c(2, 3, 8)


# Simulate M = 100 times N = 1000 bets
M = 100
N = 1000
set.seed(42)

b = .Machine$integer.max
for (t in 1:3) {
  p_t = sum(p[1:t])
  sigma_t = sum((1/alpha)[1:t])
  b_t = (1 - p_t) / (1 - sigma_t)
  if (b_t > 0) b = min(b_t, b)
}
f_kelly = pmax(0, p - b / alpha)
# Two strategies: Kelly and Kelly but setting f_i = 0 for -EV
f_kelly_zero = f_kelly
f_kelly_zero[p*alpha<1] = 0

# Simulate M = 100 times N = 1000 bets
bankroll1 = matrix(0, nrow = M, ncol = N)
bankroll2 = matrix(0, nrow = M, ncol = N)
for (i in 1:M) {
  for (j in 1:N) {
    outcome = sample(1:3, size = 1, prob = p)
    bankroll1[i,j] = ifelse(j==1, 1, bankroll1[i,j-1]) * (1 - sum(f_kelly) + f_kelly[outcome] * alpha[outcome])
    bankroll2[i,j] = ifelse(j==1, 1, bankroll2[i,j-1]) * (1 - sum(f_kelly_zero) + f_kelly_zero[outcome] * alpha[outcome])
  }
}

avg1 = colMeans(bankroll1)
avg2 = colMeans(bankroll2)
avg = data.frame(
  kelly = log(avg1),
  kelly_zero = log(avg2)
)
# Plot {kelly, kelly_zero} vs {1:N}
ggplot(avg, aes(x = 1:N)) +
  geom_line(aes(y = kelly, color = "Kelly"), size = 1) +
  geom_line(aes(y = kelly_zero, color = "Kelly (bet 0 on -EV)"), size = 1) +
  labs(title = "Kelly Betting Strategy",
       x = "Number of Bets",
       y = "Log Bankroll") +
  scale_color_manual(values = c("Kelly" = "blue", "Kelly (bet 0 on -EV)" = "red")) +
  theme_minimal()
