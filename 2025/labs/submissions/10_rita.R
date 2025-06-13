p = c(0.58, 0.322, 0.1)    
alpha = c(2, 3, 8)

kelly = compute_kelly(p, alpha)
kelly$f

#TASK 1

compute_kelly = function(p, alpha) 
  {idx = order(p * alpha, decreasing = TRUE)
  p_s = p[idx]
  a_s = alpha[idx]
  p_cumulative = cumsum(p_s)
  sigma_cumulative  = cumsum(1 / a_s)
 
  b_cand  = (1 - p_cumulative) / (1 - sigma_cumulative)
  ok = which(b_cand > 0 & (1 - sigma_cumulative) > 0)
  b_opt = min(b_cand[ok])

  f_raw = p - b_opt / alpha
  f_pos = pmax(0, f_raw)
  list(f = f_pos, b = b_opt)}

kelly = compute_kelly(p, alpha)
f_full = kelly$f       # full Kelly fractions
b_full = kelly$b       # reserve fraction



ev = p * alpha - 1
f_plus = ifelse(ev >= 0, f_full, 0)
b_plus = 1 - sum(f_plus)

#SIMULATION 

M = 100 
N = 1000   

bank_full = matrix(NA, nrow = M, ncol = N + 1)
bank_plus = matrix(NA, nrow = M, ncol = N + 1)

set.seed(42)
for(i in 1:M) {
  Bf = Bp = numeric(N + 1)
  Bf[1] = Bp[1] = 1.0
  for(t in 1:N) {
    win = sample(1:3, 1, prob = p)
    Bf[t+1] = Bf[t] * (b_full + f_full[win]   * alpha[win])
    Bp[t+1] = Bp[t] * (b_plus + f_plus[win]   * alpha[win])
  }
  bank_full[i, ] = Bf
  bank_plus[i, ] = Bp
}

avg_full = colMeans(bank_full)
avg_plus = colMeans(bank_plus)

#PLOT

library(tidyr)
library(ggplot2)

df_kelly = data.frame(
  Bet = 0:N,
  FullKelly  = avg_full,
  KellyPosEV = avg_plus
) %>% pivot_longer(-Bet, names_to = "Strategy", values_to = "Bankroll")

ggplot(df, aes(x = Bet, y = Bankroll, color = Strategy)) +
  geom_line(size = 1) +
  labs(
    title = "Average Bankroll: Full Kelly vs. Kelly on +EV Only",
    x     = "Bet Number (t)",
    y     = "Average Bankroll"
  ) +
  theme_minimal()