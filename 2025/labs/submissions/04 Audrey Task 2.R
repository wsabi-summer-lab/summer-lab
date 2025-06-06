library(tidyverse)
install.packages("BradleyTerry2")
library(BradleyTerry2)
ncaa_data <- read.csv(file.choose())

ncaa_data$home_team <- ifelse(ncaa_data$WLoc == "H", ncaa_data$WTeamID,
                              ifelse(ncaa_data$WLoc == "A", ncaa_data$LTeamID, NA))

ncaa_data$away_team <- ifelse(ncaa_data$WLoc == "H", ncaa_data$LTeamID,
                              ifelse(ncaa_data$WLoc == "A", ncaa_data$WTeamID, NA))

ncaa_data$home_win  <- ifelse(ncaa_data$WLoc == "H", 1,
                              ifelse(ncaa_data$WLoc == "A", 0, NA))

ncaa_data <- ncaa_data[!is.na(ncaa_data$home_team), ]

season_data <- ncaa_data[ncaa_data$Season == 2023, ]
head(season_data)

bt_model <- glm(home_win ~ factor(home_team) + factor(away_team), 
                family = binomial, data = season_data)

summary(bt_model)
print(summary(bt_model))
team_lookup <- read.csv(file.choose())
head(team_lookup)


team_lookup[team_lookup$TeamName == "Purdue", ]
team_lookup[team_lookup$TeamName == "Connecticut", ]


purdue_id <- 1345
uconn_id  <- 1163

beta <- coef(bt_model)

beta_home <- beta[paste0("factor(home_team)", purdue_id)]
beta_away <- beta[paste0("factor(away_team)", uconn_id)]

beta_home <- ifelse(is.na(beta_home), 0, beta_home)
beta_away <- ifelse(is.na(beta_away), 0, beta_away)

logit_p <- beta_home - beta_away
p_purdue_win <- 1 / (1 + exp(-logit_p))

print(paste("Probability Purdue wins:", round(p_purdue_win, 3)))

#For Fn haha
purdue_id <- 1345
upenn_id  <- 1335

beta <- coef(bt_model)

beta_home <- beta[paste0("factor(home_team)", purdue_id)]
beta_away <- beta[paste0("factor(away_team)", upenn_id)]

beta_home <- ifelse(is.na(beta_home), 0, beta_home)
beta_away <- ifelse(is.na(beta_away), 0, beta_away)

logit_p <- beta_home - beta_away
p_purdue_win <- 1 / (1 + exp(-logit_p))

print(paste("Probability Purdue wins:", round(p_purdue_win, 3)))
print(paste("NAW PENN IS BETTER THAN PURDUE"))

