data {
  int<lower=1> N_games;
  int<lower=1> N_teams;
  int<lower=2> N_seasons;

  real y[N_games];                   // score differential
  int<lower=1, upper=N_teams> H[N_games];  // home team
  int<lower=1, upper=N_teams> A[N_games];  // away team
  int<lower=1, upper=N_seasons> S[N_games]; // season index
}

parameters {
  real beta_0;                           // global intercept
  matrix[N_teams, N_seasons] beta;       // team strength
  real alpha[N_teams];                   // team-specific home field adv.
  real<lower=0> sigma_games;
  real<lower=0> sigma_teams;
  real<lower=0> sigma_seasons;
  real<lower=0> sigma_alpha;
  real<lower=0, upper=1> gamma;
}

model {
  // Likelihood
  for (i in 1:N_games) {
    y[i] ~ normal(
      beta_0 + alpha[H[i]] + beta[H[i], S[i]] - beta[A[i], S[i]],
      sigma_games
    );
  }

  // Priors
  #beta_0 ~ normal(0, 5);
  sigma_games ~ normal(0, 5);
  sigma_teams ~ normal(0, 5);
  sigma_seasons ~ normal(0, 5);
  sigma_alpha ~ normal(0, 5);
  gamma ~ uniform(0, 1);

  // Team strength priors
  for (j in 1:N_teams) {
    beta[j, 1] ~ normal(0, sigma_teams); // prior for first season
    for (s in 2:N_seasons) {
      beta[j, s] ~ normal(gamma * beta[j, s - 1], sigma_seasons);
    }
  }

  // Team-specific HFA priors
  for (j in 1:N_teams) {
    alpha[j] ~ normal(0, sigma_alpha);
  }
}
