data {
  int<lower=1>  N_games;                                 // # games
  int<lower=1>  N_teams;                                 // # teams
  int<lower=2>  N_seasons;                               // # seasons

  array[N_games]         real y;                         // point diff  (home − away)
  array[N_games] int<lower=1,upper=N_teams>   H;         // home-team index
  array[N_games] int<lower=1,upper=N_teams>   A;         // away-team index
  array[N_games] int<lower=1,upper=N_seasons> S;         // season index
}

parameters {
  //––– home-field-advantage hierarchy –––
  real                     mu_alpha;                     // global mean HFA
  vector[N_teams]          alpha;                        // team-specific HFA
  real<lower=0>            sigma_HFA;                    // sd across teams
  real<lower=0>            tau_HFA;                      // sd for mu_alpha prior

  //––– team strength state-space –––
  matrix[N_teams, N_seasons]   betas;                    // team strength coefficients
  real<lower=0,upper=1>        rho;                      // AR-1 coefficient
  real<lower=0>                sigma_games;              // game-level sd
  real<lower=0>                sigma_teams;              // sd across teams (season 1)
  real<lower=0>                sigma_seasons;            // sd across seasons
}

model {
  //-------------------
  //  Priors
  //-------------------
  // home-field
  alpha      ~ normal(mu_alpha, sigma_HFA);
  mu_alpha   ~ normal(0, tau_HFA);
  sigma_HFA  ~ normal(0, 5);           // half-Normal via <lower=0>
  tau_HFA    ~ normal(0, 5);           // half-Normal via <lower=0>

  // team-strength at season 1
  to_vector(betas[,1]) ~ normal(0, sigma_teams);

  // AR-1 evolution across seasons
  for (s in 2:N_seasons)
    betas[, s] ~ normal(rho * betas[, s-1], sigma_seasons);

  // hyper-priors
  sigma_games   ~ normal(0, 5);
  sigma_teams   ~ normal(0, 5);
  sigma_seasons ~ normal(0, 5);
  rho           ~ beta(1, 1);          // proper Uniform(0,1)

  //-------------------
  //  Likelihood
  //-------------------
  for (i in 1:N_games) {
    real mu_i = mu_alpha + alpha[H[i]]
                + betas[H[i], S[i]] - betas[A[i], S[i]];
    y[i] ~ normal(mu_i, sigma_games);
  }
}

generated quantities {
  // posterior-predictive draw and pointwise log-likelihood (optional)
  vector[N_games] y_rep;
  vector[N_games] log_lik;

  for (i in 1:N_games) {
    real mu_i = mu_alpha + alpha[H[i]]
                + betas[H[i], S[i]] - betas[A[i], S[i]];
    y_rep[i]   = normal_rng(mu_i, sigma_games);
    log_lik[i] = normal_lpdf(y[i] | mu_i, sigma_games);
  }
}