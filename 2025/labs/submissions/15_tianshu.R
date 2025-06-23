#############
### SETUP ###
#############

# install.packages(c("ggplot2", "glmnet", "tidyverse"))
# setwd("/Users/tianshufeng/Documents/GitHub/summer-lab/2025/labs/data")
library(ggplot2)
library(glmnet)
library(tidyverse)

# set seed
set.seed(15)

####################
### NBA LINEUPS ###
####################

# load NBA lineups data
nba_lineups = readRDS("../data/15_nba-lineups.rds")
# preview data
head(nba_lineups)

extract_names = function(s) {
  players <- strsplit(s, ",\\s*")[[1]]
  sub("^\\d+\\s+", "", players)
}

off_names = lapply(nba_lineups$lineup_team, extract_names)
def_names = lapply(nba_lineups$lineup_opp, extract_names)
all_players = sort(unique(unlist(c(off_names, def_names))))

X = matrix(0, nrow=nrow(nba_lineups), ncol=1 + 2 * length(all_players))
colnames(X) = c("Intercept", 
                 paste0(all_players, "_off"), 
                 paste0(all_players, "_def"))
X[, "Intercept"] = 1

for (i in seq_len(nrow(nba_lineups))) {
  X[i, paste0(off_names[[i]], "_off")] = 1
  X[i, paste0(def_names[[i]], "_off")] = 1
}

ols_model = lm(nba_lineups$pts_poss ~ ., data = as.data.frame(X))
summary(ols_model)

lambdas = 10^seq(-3, 3, by=0.2)
ridge_model = glmnet(x=X, y=nba_lineups$pts_poss, nfolds=5, alpha=0, lambda=lambdas,
                     family="gaussian", stadardize=FALSE)
ridge_best_lambda = ridge_model$lambda.min
plot(ridge_model)

# Plot comparing OLS and Ridge coefficients
ols_coefs <- coef(ols_model)
names(ols_coefs) <- gsub("^`|`$", "", names(ols_coefs))

ridge_coefs <- coef(ridge_model, s = ridge_best_lambda)  # returns a sparse matrix
ridge_coefs <- as.vector(ridge_coefs)
names(ridge_coefs) <- rownames(coef(ridge_model))

sample_players = sample(paste0(all_players, "_off"), 20)

compare_df <- data.frame(
  player = sample_players,
  ols = ols_coefs[sample_players],
  ridge = ridge_coefs[sample_players]
)

print(compare_df)

compare_long <- reshape2::melt(compare_df, id.vars = "player")

ggplot(compare_long, aes(x = player, y = value, color = variable)) +
  geom_point(size = 3) +
  geom_line(aes(group = player), color = "gray70", linetype = "dashed") +
  theme_minimal() +
  coord_flip() +
  labs(title = "OLS vs Ridge Coefficients for Sample Players",
       x = "Player",
       y = "Estimated Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))