data {
int<lower = 1> N_games; // number of games 
int<lower = 1> N_teams; // number of teams 
int<lower = 2> N_seasons; // number of seasons
real y[N_games]; // outcome vector (point differential) 
int<lower = 1, upper = N_teams> H_index[N_games]; // vector of home
int<lower = 1, upper = N_teams> A_index[N_games]; // vector of away
int<lower = 1, upper = N_seasons> season[N_games]; // vector of season
}

parameters {

real beta_0;
real betas[N_teams, N_seasons];

real alpha;
vector[N_teams] alpha_team_raw;
real<lower=0> sigma_HFA; 

real<lower = 0> sigma_games;
real<lower = 0> sigma_teams;
real<lower = 0> sigma_seasons;
real<lower = 0, upper = 1> gamma;

}

transformed parameters {
  vector[N_teams] alpha_team;
  alpha_team = alpha + sigma_HFA * alpha_team_raw;
}

model {
// game-level model 
for(i in 1:N_games) {
y[i] ~ normal(beta_0 
                  + alpha_team[H_index[i]]
                  + betas[H_index[i], season[i]] 
                  - betas[A_index[i], season[i]], 
                  sigma_games);
}
// team-level priors
for(j in 1:N_teams) {
// initial season prior across teams 
  betas[j, 1] ~ normal(0, sigma_teams); 
  for (s in 2:N_seasons) {
    betas[j, s] ~ normal(gamma * betas[j, s-1], 
    sigma_seasons);
} }

// priors
sigma_games ~ normal(0, 5); 
sigma_teams ~ normal(0, 5); 
sigma_seasons ~ normal(0, 5); 
gamma ~ uniform(0, 1);

alpha ~ normal(0, 5);
alpha_team_raw ~ normal(0, 1);
sigma_HFA ~ normal(0, 5);
}
