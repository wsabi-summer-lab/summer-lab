data {
  int<lower=1> N_games;
  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  real y[N_games];
  int<lower=1, upper=N_teams> H[N_games];
  int<lower=1, upper=N_teams> A[N_games];
  int<lower=1, upper=N_seasons> S[N_games];
}

parameters {
  real alpha;                          // overall home field advantage
  real alpha_team[N_teams];           // team-specific HFA
  real<lower=0> sigma_HFA;
  real<lower=0> tau_HFA;

  real beta[N_teams, N_seasons];      // team strength
  real<lower=0> sigma_teams;
  real<lower=0> sigma_seasons;
  real<lower=0> sigma_games;
  real<lower=0, upper=1> gamma;
}

model {
  // Priors
  alpha ~ normal(0, tau_HFA);
  alpha_team ~ normal(alpha, sigma_HFA);

  sigma_HFA ~ normal(0, 5);
  tau_HFA ~ normal(0, 5);
  sigma_teams ~ normal(0, 5);
  sigma_seasons ~ normal(0, 5);
  sigma_games ~ normal(0, 5);
  gamma ~ uniform(0, 1);

  for (j in 1:N_teams) {
    beta[j, 1] ~ normal(0, sigma_teams);
    for (s in 2:N_seasons) {
      beta[j, s] ~ normal(gamma * beta[j, s-1], sigma_seasons);
    }
  }

  for (i in 1:N_games) {
    y[i] ~ normal(alpha_team[H[i]] + beta[H[i], S[i]] - beta[A[i], S[i]], sigma_games);
  }
}
