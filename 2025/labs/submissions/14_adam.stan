
// The input data is a vector 'y' of length 'N'.
data {
 int<lower = 1> N_games; // number of games 
 int<lower = 1> N_teams; // number of teams 
 int<lower = 2> N_seasons; // number of seasons

  real y[N_games]; // outcome vector (point differential) 
  int<lower = 1, upper = N_teams> H[N_games]; // vector of home team indices
  int<lower = 1, upper = N_teams> A[N_games]; // vector of away team indices
  int<lower = 1, upper = N_seasons> S[N_games]; // vector of season indices
}

// The parameters accepted by the model. 
parameters {
  real beta_0; // intercept (home field advantage)
  real betas[N_teams, N_seasons]; // team strength coefficients for each team-season
  real<lower = 0> sigma_games; // game-level variance in point differential
  real<lower = 0> sigma_teams; // variance across teams before the first season
  real<lower = 0> sigma_seasons; // a team’s variance across seasons upper
  real<lower = 0, upper = 1> gamma; // autoregressive parameter
  
  //HFA parameters
  real alpha_0;
  vector[N_teams] alpha_team_raw;
  real<lower = 0> sigma_HFA; 
 
}

transformed parameters {
  vector[N_teams] alpha_team;
  alpha_team = alpha_0 + sigma_HFA * alpha_team_raw;
}

// The model to be estimated. 

model {
 // game-level model 
 for(i in 1:N_games) {
    y[i] ~ normal(beta_0 + betas[H[i] , S[i]] + alpha_team[H[i]] - betas[A[i], S[i]], sigma_games);
  }
  // team-level priors
  for(j in 1:N_teams) {
    // initial season prior across teams 
    betas[j, 1] ~ normal(0, sigma_teams); 
    for (s in 2:N_seasons) {
      // auto-regressive model across seasons
      betas[j, s] ~ normal(gamma * betas[j, s-1], sigma_seasons);
  } }
  // priors
  
  sigma_games ~ normal(0, 5); 
  sigma_teams ~ normal(0, 5); 
  sigma_seasons ~ normal(0, 5); 
  gamma ~ uniform(0, 1);
  
  //HFA priors
  alpha_0 ~ normal(0, 5);
  alpha_team_raw ~ normal(0, 1);
  sigma_HFA ~ normal(0, 5);
}

