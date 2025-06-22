#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(13)

####################
### GOLF PUTTING ###
####################

# load training data (first half of season)
putts_train = read_csv(file.choose())
# preview data
head(putts_train)

# load training data (first half of season)
putts_test = read_csv(file.choose())
# preview data
head(putts_test)

# Basic variables
k <- nrow(putts_train)
first_half <- putts_train$X  
N <- putts_train$N               
true_half <- putts_test$X        
avg <- mean(first_half)

# METH 1: Average
meth1 <- rep(avg, k)

# METH 2: Individual (MLE)
meth2 <- first_half

# METH 3: Efron-Morris V1
C <- avg * (1 - avg)
se <- sqrt(C / N)
x_tilde <- first_half / se
x_bar <- mean(x_tilde)
ss <- sum((x_tilde - x_bar)^2)
shrink <- 1 - (k - 1) / ss
em1_est <- x_bar + shrink * (x_tilde - x_bar)
meth3 <- se * em1_est

# METH 4: Efron-Morris V2
made <- round(first_half * N)
arc_data <- asin(sqrt((made + 3/8) / (N + 3/4)))
nu <- 1 / (2 * sqrt(N))
x_std <- arc_data / nu
x_std_bar <- mean(x_std)
ss_std <- sum((x_std - x_std_bar)^2)
shrink2 <- 1 - (k - 1) / ss_std
em2_est <- x_std_bar + shrink2 * (x_std - x_std_bar)
meth4 <- sin(nu * em2_est)^2

# METH 5: Empirical Bayes V1
mu_hat <- avg
s2 <- var(first_half)
tau2 <- s2 - C * mean(1/N)
if(tau2 <= 0) tau2 <- 0.001
weights <- tau2 / (tau2 + C/N)
meth5 <- mu_hat + weights * (first_half - mu_hat)

# METH 6: Empirical Bayes V2
arc_eb <- arc_data
var_i <- 1 / (4 * N)
mu_arc <- mean(arc_eb)
tau2_arc <- var(arc_eb) - mean(var_i)
if(tau2_arc <= 0) tau2_arc <- 0.001
weights2 <- tau2_arc / (tau2_arc + var_i)
eb_est <- mu_arc + weights2 * (arc_eb - mu_arc)
meth6 <- sin(eb_est)^2

# Calculate MSEs
mse1 <- mean((meth1 - true_half)^2)
mse2 <- mean((meth2 - true_half)^2)
mse3 <- mean((meth3 - true_half)^2)
mse4 <- mean((meth4 - true_half)^2)
mse5 <- mean((meth5 - true_half)^2)
mse6 <- mean((meth6 - true_half)^2)

# Results
results <- data.frame(
  Method = c("Meth1: Average", "Meth2: Individual", "Meth3: EM_V1", 
             "Meth4: EM_V2", "Meth5: EB_V1", "Meth6: EB_V2"),
  MSE = c(mse1, mse2, mse3, mse4, mse5, mse6)
) %>%
  arrange(MSE) %>%
  mutate(Rank = row_number())

print(results)

# Plot
plot_data <- data.frame(
  true = rep(true_half, 6),
  pred = c(meth1, meth2, meth3, meth4, meth5, meth6),
  meth = rep(c("Meth1", "Meth2", "Meth3", "Meth4", "Meth5", "Meth6"), each = k)
)

p <- ggplot(plot_data, aes(x = true, y = pred, color = meth)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(
    title = "Pred vs True 2H Performance",
    x = "True 2H",
    y = "Predicted",
    color = "Method"
  ) +
  theme_minimal() +
  xlim(0, 1) + ylim(0, 1)

print(p)