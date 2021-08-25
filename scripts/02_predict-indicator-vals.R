# calculate predicted indicator values from MCMC chain and extract median

library(boral)

# data

m1 <- readRDS('outputs/models/mod-LV9-final.rds')
colnames <- read.csv('outputs/colnames-boral.csv')

# extract lv.coefs and lv scores from mcmc samples for LVs

samples <- get.mcmcsamples(m1)
dim(samples)

lv.coefs <- samples[,grep('lv.coefs', colnames(samples))]

lv.coefs.1 <- lv.coefs[,grep(',2]', colnames(lv.coefs))]
lv.coefs.2 <- lv.coefs[,grep(',3]', colnames(lv.coefs))]
lv.coefs.3 <- lv.coefs[,grep(',4]', colnames(lv.coefs))]
lv.coefs.4 <- lv.coefs[,grep(',5]', colnames(lv.coefs))]
lv.coefs.5 <- lv.coefs[,grep(',6]', colnames(lv.coefs))]
lv.coefs.6 <- lv.coefs[,grep(',7]', colnames(lv.coefs))]
lv.coefs.7 <- lv.coefs[,grep(',8]', colnames(lv.coefs))]
lv.coefs.8 <- lv.coefs[,grep(',9]', colnames(lv.coefs))]
lv.coefs.9 <- lv.coefs[,grep(',10]', colnames(lv.coefs))]

lv <- samples[,grep('lv', colnames(samples))]
lv <- lv[,-grep('coefs', colnames(lv))]

lv.1 <- lv[,grep(',1]', colnames(lv))]
lv.2 <- lv[,grep(',2]', colnames(lv))]
lv.3 <- lv[,grep(',3]', colnames(lv))]
lv.4 <- lv[,grep(',4]', colnames(lv))]
lv.5 <- lv[,grep(',5]', colnames(lv))]
lv.6 <- lv[,grep(',6]', colnames(lv))]
lv.7 <- lv[,grep(',7]', colnames(lv))]
lv.8 <- lv[,grep(',8]', colnames(lv))]
lv.9 <- lv[,grep(',9]', colnames(lv))]

# calculate predicted value for each MCMC sample

# 3D array 

nmcmc <- nrow(lv.coefs.1)
nsites <- ncol(lv.1)
nind <- ncol(lv.coefs.1)
X <- array(dim = c(nsites, nind, nmcmc))

system.time(
for (i in 1:nmcmc){ #loop over samples 

  x <- (lv.coefs.1[i,] %*% t(lv.1[i,])) + 
    (lv.coefs.2[i,] %*% t(lv.2[i,])) + 
    (lv.coefs.3[i,] %*% t(lv.3[i,])) + 
    (lv.coefs.4[i,] %*% t(lv.4[i,])) + 
    (lv.coefs.5[i,] %*% t(lv.5[i,])) + 
    (lv.coefs.6[i,] %*% t(lv.6[i,])) + 
    (lv.coefs.7[i,] %*% t(lv.7[i,])) + 
    (lv.coefs.8[i,] %*% t(lv.8[i,])) + 
    (lv.coefs.9[i,] %*% t(lv.9[i,]))
  X[,,i] <- t(x) #convention to have rows as sites
}
)

# summary stats

#Now apply over samples to get median and quantiles
system.time(Xmedian <- apply(X, c(1,2), median)) # takes a few minutes
Xlwr <- apply(X, c(1,2), quantile, probs = 0.025)
Xupr <- apply(X, c(1,2), quantile, probs = 0.975)

# save

df <- data.frame(Xmedian)
colnames(df) <- colnames$ynames

write.csv(df, 'outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final.csv', row.names = F)

# loop through and get every nth sample, for total of 100 samples
# for robustness index

ls <- list()
sequence <- seq(from = 1, to = dim(X)[3], by = dim(X)[3]/100)
system.time(
  for(i in 1:length(sequence)){
    X.sub <- X[,,i]
    df <- data.frame(X.sub)
    colnames(df) <- colnames$ynames
    df$mcmc_sample <- rep(sequence[i], nrow(df))
    ls[[i]] <- df
  })
df.full <- do.call(rbind, ls)
df.full2 <- data.frame(mcmc_sample = df.full$mcmc_sample, df.full[,-length(df.full)])

write.csv(df.full2, 'outputs/predicted-indicator-vals/predicted-ind-vals_100-mcmc.csv', row.names = F)


