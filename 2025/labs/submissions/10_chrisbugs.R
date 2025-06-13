library(ggplot2)
library(tidyverse)

set.seed(10)

# Win probabilities and odds
p      <- c(0.20, 0.30, 0.50)       
alpha  <- c( 6.0,  1.6,  7.0)       

ev     <- p*alpha - 1 

# Kelly allocation function
kelly_alloc <- function(p, alpha) {
  idx   <- order(p*alpha, decreasing = TRUE)      # Step 1
  p_ord <- p[idx];  a_ord <- alpha[idx]
  
  pt <- sigma <- b_vec <- c()
  for (t in seq_along(p_ord)) {
    pt[t]    <- sum(p_ord[1:t])
    sigma[t] <- sum(1/a_ord[1:t])
    cand     <- (1 - pt[t])/(1 - sigma[t])        # Step 2
    if (cand > 0) b_vec <- c(b_vec, cand)
  }
  b  <- min(b_vec)                                
  f_ord <- p_ord - b/a_ord                        # Step 3
  f_ord[f_ord < 0] <- 0
  f <- numeric(length(p));  f[idx] <- f_ord
  list(f = f, b = b)
}

alloc      <- kelly_alloc(p, alpha)

# Kelly allocations (Strategy 1)
f1         <- alloc$f                   
bet_frac1  <- sum(f1)                   

# Dont bet on negative EV horses (Strategy 2)
f2         <- ifelse(ev < 0, 0, f1)
bet_frac2  <- sum(f2)

# Simulation (task 2)
M <- 100       
N <- 1000 

# Simulation itself
sim_tbl <- map_dfr(1:M, function(sim_id) {
  bank1 <- bank2 <- 1
  tibble(race = 1:N) %>% 
    mutate(
      winner      = sample(1:3, N, replace = TRUE, prob = p),
      payoff1     = (1 - bet_frac1) + alpha[winner]*f1[winner],
      payoff2     = (1 - bet_frac2) + alpha[winner]*f2[winner],
      bank1       = accumulate(payoff1, `*`, .init = bank1)[-1],
      bank2       = accumulate(payoff2, `*`, .init = bank2)[-1],
      sim         = sim_id
    ) %>% 
    select(sim, race, bank1, bank2)
})

# Get average bankroll
avg_tbl <- sim_tbl %>% 
  group_by(race) %>% 
  summarise(across(bank1:bank2, mean), .groups = "drop") %>% 
  pivot_longer(-race, names_to = "strategy", values_to = "bankroll")

# Plot average bankroll using ggplot
ggplot(avg_tbl, aes(race, bankroll, colour = strategy)) +
  geom_line(size = 1) +
  scale_y_continuous(trans = "log10") +   # log scale for visibility
  labs(title   = "Average bankroll across 100 Kelly simulations",
       x       = "Race number",
       y       = "Mean bankroll (log-scale)",
       colour  = NULL) +
  theme_minimal()






