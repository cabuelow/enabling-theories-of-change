get_predictions <- function(x, num_lv){

  samples <- get.mcmcsamples(x)
  dim(samples)

  lv.coefs <- samples[,grep('lv.coefs', colnames(samples))]

  tmp <- list()
  for(i in 1:num_lv){
    tmp[[i]] <- lv.coefs[,grep(paste0(',',i+1,']'), colnames(lv.coefs))]
  }

  lv <- samples[,grep('lv', colnames(samples))]
  lv <- lv[,-grep('coefs', colnames(lv))]

  tmp2 <- list()
  for(i in 1:num_lv){
    tmp2[[i]] <- lv[,grep(paste0(',',i,']'), colnames(lv))]
  }

  # calculate predicted value for each MCMC sample

  # 3D array

  nmcmc <- nrow(tmp[[1]])
  nsites <- ncol(tmp2[[1]])
  nind <- ncol(tmp[[1]])
  X <- array(dim = c(nsites, nind, nmcmc))

  system.time(
    for (i in 1:nmcmc){ #loop over samples

      # multiply each matrix in tmp and tmp2 and sum across num_lv
      x <- lapply(seq(num_lv), function(i) Reduce("+", lapply(seq_along(tmp), function(j) tmp[[j]][, i] %*% t(tmp2[[j]][, i]))))

      X[,,i] <- t(x) #convention to have rows as sites
    }
  )

  # summary stats

  #Now apply over samples to get median and quantiles
  system.time(Xmedian <- apply(X, c(1,2), median)) # takes a few minutes
  Xlwr <- apply(X, c(1,2), quantile, probs = 0.025)
  Xupr <- apply(X, c(1,2), quantile, probs = 0.975)

  df <- data.frame(Xmedian)
  return(df)
}
