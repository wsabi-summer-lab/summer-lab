data {
  int<lower=1> N_games;
  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  real y[N_games];                          // Point differential
  int<lower=1, upper=N_teams> H[N_games];   // Home team
  int<lower=1, upper=N_teams> A[N_games];   // Away team
  int<lower=1, upper=N_seasons> S[N_games]; // Season
}

parameters {
  real beta_0;                              // Global intercept
  real betas[N_teams, N_seasons];           // Team-season strength

  real alpha;                               // Overall HFA
  vector[N_teams] alpha_team_raw;           // Raw team-specific HFA
  real<lower=0> sigma_HFA;                  // Std dev for team-specific HFA

  real<lower=0> sigma_games;                // Residual variance
  real<lower=0> sigma_teams;                // Across-team variance
  real<lower=0> sigma_seasons;              // Year-to-year variance
  real<lower=0, upper=1> gamma;             // AR smoothing
}

transformed parameters {
  vector[N_teams] alpha_team;
  alpha_team = alpha + sigma_HFA * alpha_team_raw;
}

model {
  // Game model
  for (i in 1:N_games) {
    y[i] ~ normal(
      beta_0 + alpha_team[H[i]] + betas[H[i], S[i]] - betas[A[i], S[i]],
      sigma_games
    );
  }

  // AR team strength
  for (j in 1:N_teams) {
    betas[j, 1] ~ normal(0, sigma_teams);
    for (s in 2:N_seasons) {
      betas[j, s] ~ normal(gamma * betas[j, s - 1], sigma_seasons);
    }
  }

  // Priors
  beta_0 ~ normal(0, 5);
  alpha ~ normal(0, 5);
  alpha_team_raw ~ normal(0, 1);          // standard normal for raw effects
  sigma_HFA ~ normal(0, 5);               // HFA spread
  sigma_games ~ normal(0, 5);
  sigma_teams ~ normal(0, 5);
  sigma_seasons ~ normal(0, 5);
  gamma ~ uniform(0, 1);
}



