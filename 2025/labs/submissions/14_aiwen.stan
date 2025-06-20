data {
  int<lower=1> N_games;                   // Number of games
  int<lower=1> N_teams;                   // Number of teams
  int<lower=1> N_seasons;                // Number of seasons
  vector[N_games] y;                      // Home - Away score diff
  int<lower=1, upper=N_teams> home[N_games]; // Home team indices
  int<lower=1, upper=N_teams> away[N_games]; // Away team indices
  int<lower=1, upper=N_seasons> season[N_games]; // Season indices
}

parameters {
  vector[N_teams] team_strength;          // Team strength
  vector[N_teams] alpha_team;             // Team-specific HFA
  real alpha;                             // Global HFA mean
  real<lower=0> sigma_HFA;                // HFA team-level variance
  real<lower=0> tau_HFA;                  // Global HFA prior variance
  real<lower=0> sigma_game;               // Game-level residual SD

  vector[N_seasons] season_effect;        // Season-specific intercepts
}

model {
  // Priors
  alpha ~ normal(0, tau_HFA);
  tau_HFA ~ normal(0, 5);
  sigma_HFA ~ normal(0, 5);
  sigma_game ~ normal(0, 5);

  team_strength ~ normal(0, 5);
  alpha_team ~ normal(alpha, sigma_HFA);
  season_effect ~ normal(0, 5);

  // Likelihood
  for (i in 1:N_games) {
    y[i] ~ normal(season_effect[season[i]] + alpha_team[home[i]]
                  + team_strength[home[i]] - team_strength[away[i]],
                  sigma_game);
  }
}
