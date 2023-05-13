# simulate correlated multivariate response data
# make one missing not at random
# see how well LVM can recover missing values

library(MASS)
library(tidyverse)
library(R2jags)
library(boral)
source('scripts/helpers/get_predictions.R')

num_variables <- 20 # number of response variables
sample_size <- 100 # sample size
corr <- c(0.5, 0.7, 0.3)
prop.na <- c(0.5, 0.3, 0.5)

df <- data.frame(corr = NA, prop.na = NA, sim = NA, r2 = NA, RMSE = NA) # df to store results
tmp2 <- list()
for(a in 1:length(corr)){

  correlation <- corr[a]
  tmp <- list()

  for(b in 1:length(prop.na)){

    prop <- prop.na[b]

for(d in 1:50){ # repeat simulation 100 times

# generate a random covariance matrix with varying correlations between -0.5 and 0.5

cov_matrix <- matrix(runif(num_variables^2, min=-correlation, max=correlation), nrow=num_variables, ncol=num_variables)
cov_matrix <- t(cov_matrix) %*% cov_matrix # make it a valid covariance matrix

# simulate data using the covariance matrix

data <- data.frame(mvrnorm(n=sample_size, mu=rep(0,num_variables), Sigma=cov_matrix))

# make one variable MNAR, i.e., only missing high values, where 50% are missing
# and make some others missing at random

data_mnar <- data %>%
  mutate(X10 = ifelse(X10 > quantile(X10, prop), NA, X10)) #%>%
  #mutate_at(vars(X2, X6, X20), ~ifelse(runif(length(.)) < 0.1, NA, .))

# lvm without missing data

i <- 10 # sets the number (~ half number of indicators)

m1 <- boral(as.matrix(data), family = 'normal',
              row.eff = "none",
              lv.control = list(num.lv = i),
              mcmc.control = list(n.burnin = 2000, n.iteration = 50000,
                                  n.thin = 5, seed = 123),
              save.model = TRUE)

# lvm with missing data

m2 <- boral(as.matrix(data_mnar), family = 'normal',
              row.eff = "none",
              lv.control = list(num.lv = i),
              mcmc.control = list(n.burnin = 2000, n.iteration = 50000,
                                  n.thin = 5, seed = 123),
              save.model = TRUE)

# extract predictions

pred <- get_predictions(x = m1, num_lv = 10)
pred2 <- get_predictions(x = m2, num_lv = 10)

# compare predictions

comp <- data.frame(x_pred = pred$X10, x_pred_mnar = pred2$X10,
                   x_raw = data$X10, x_mnar = data_mnar$X10)
#head(comp)

#ggplot() +
 # geom_point(data = filter(comp, !is.na(x_mnar)),
  #            aes(x = x_raw, y = x_pred_mnar), alpha = 0.75) +
  #geom_point(data = filter(comp, is.na(x_mnar)),
   #          aes(x = x_raw, y = x_pred_mnar), alpha = 0.75, col = 'red') +
  #ylab('Predicted values with 50% missing not at random') +
  #xlab('Observed values') +
  #theme_classic()

#ggsave('outputs/sim-mnar2.png', width = 6, height = 4)

df[d,1] <- correlation
df[d,2] <- prop
df[d,3] <- d
df[d,4] <- cor(comp$x_raw, comp$x_pred_mnar)^2 # R2 with mnar
df[d,5] <- sqrt(mean((comp$x_raw - comp$x_pred_mnar)^2)) # rmse with mnar
write.csv(df, paste0('outputs/sim_', corr[a], '_', prop.na[b], '.csv'), row.names = F)
}
    tmp[[b]] <- df
  }
  df2 <- do.call(rbind, tmp)
  tmp2[[a]] <- df2
}

final.df <- do.call(rbind, tmp2)
write.csv(final.df, 'outputs/sim-results.csv', row.names = F)
