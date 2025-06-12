#9.1.1 RIP
r <- 14     
n <- 56    
p_hat <- r / n

se <- sqrt(p_hat * (1 - p_hat) / n)
wald_lower <- p_hat - 1.96 * se
wald_upper <- p_hat + 1.96 * se

z <- 1.96
n_tilde <- n + z^2
p_tilde <- (r + 0.5 * z^2) / n_tilde
se_tilde <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
agresti_lower <- p_tilde - z * se_tilde
agresti_upper <- p_tilde + z * se_tilde

cat("Wald CI: [", round(wald_lower, 3), ",", round(wald_upper, 3), "]\n")
cat("Agresti-Coull CI: [", round(agresti_lower, 3), ",", round(agresti_upper, 3), "]\n")

skittles <- c(rep(1, r), rep(0, n - r))

set.seed(123)
boot_means <- replicate(1000, {
  sample_mean <- mean(sample(skittles, replace = TRUE))
  return(sample_mean)
})

boot_lower <- quantile(boot_means, 0.025)
boot_upper <- quantile(boot_means, 0.975)

cat("Bootstrap CI: [", round(boot_lower, 3), ",", round(boot_upper, 3), "]\n")

#9.2.1
nba <- read.csv2(file.choose())
names(nba) 
nba$FT_percent <- as.numeric(nba$FT.)
mean(nba$FT_percent, na.rm = TRUE)

set.seed(123)

boot_means <- replicate(1000, {
  sample_data <- sample(nba$FT_percent, replace = TRUE)
  mean(sample_data, na.rm = TRUE)
})

ci <- quantile(boot_means, c(0.025, 0.975))
cat("95% CI for mean FT%: [", round(ci[1], 3), ",", round(ci[2], 3), "]\n")

hist(boot_means, col = "skyblue",
     main = "Bootstrap Dstbn of Mean FT%",
     xlab = "Mean FT%", breaks = 20)
abline(v = ci, col = "red", lty = 2)
