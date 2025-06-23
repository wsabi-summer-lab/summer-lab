data {
  int<lower=1> N_games;             // number of games
  int<lower=1> N_teams;             // number of teams
  int<lower=2> N_seasons;           // number of seasons
  array[N_games] real y;                  // outcome vector (point differential)
  array[N_games] int<lower=1, upper=N_teams> H; // home team indices
  array[N_games] int<lower=1, upper=N_teams> A; // away team indices
  array[N_games] int<lower=1, upper=N_seasons> S; // season indices
}

parameters {
  real beta_0;                                  // overall home field advantage
  vector[N_teams] alpha;                        // team-specific HFA deviation
  real<lower=0> sigma_HFA;                      // team HFA variance
  real<lower=0> tau_HFA;                        // prior std on overall HFA

  matrix[N_teams, N_seasons] betas;             // team strengths
  real<lower=0> sigma_games;                    // game-level variance
  real<lower=0> sigma_teams;                    // team variance (initial season)
  real<lower=0> sigma_seasons;                  // season-to-season variance
  real<lower=0, upper=1> gamma;                 // AR(1) coefficient
}

model {
  // Priors for home field advantage
  beta_0 ~ normal(0, tau_HFA);
  alpha ~ normal(beta_0, sigma_HFA);
  sigma_HFA ~ normal(0, 5);
  tau_HFA ~ normal(0, 5);

  // Priors on team strengths
  for (j in 1:N_teams) {
    betas[j, 1] ~ normal(0, sigma_teams);
    for (s in 2:N_seasons) {
      betas[j, s] ~ normal(gamma * betas[j, s - 1], sigma_seasons);
    }
  }

  // Likelihood for each game
  for (i in 1:N_games) {
    y[i] ~ normal(
      alpha[H[i]] - alpha[A[i]] + betas[H[i], S[i]] - betas[A[i], S[i]],
      sigma_games
    );
  }

  // Priors on scale parameters
  sigma_games ~ normal(0, 5);
  sigma_teams ~ normal(0, 5);
  sigma_seasons ~ normal(0, 5);
  gamma ~ uniform(0, 1);
}
