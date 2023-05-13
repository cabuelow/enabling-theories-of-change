# latent variable model
# note that re-producing Buelow et al. 2023 will require R version 3.5.2 and below package versions
# alternatively, models produced in this R environment are available in the 'outputs/models' folder
# and can be used to reproduce all figures/results
# (devtools)
# install_version("boral", version = "1.7", repos = "http://cran.us.r-project.org")
# install_version("rjags", version = "4.9", repos = "http://cran.us.r-project.org")
# install_version("R2jags", version = "0.5.7", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(R2jags)
library(boral)
library(scales)
library(tmap)
library(sf)
sf_use_s2(FALSE)

# data

dat <- read.csv('data/master-df_final.csv') %>%
  dplyr::rename(iso_a3 = ISO_SOV1)
data('World')

# explore data

world.dat <- World %>%
  left_join(dat, by = 'iso_a3')

#tmap_mode('view')

#tm_shape(world.dat) +
 # tm_polygons('mean_annual_funds')

# plot data distributions

dat.long <- dat %>%
  pivot_longer(cols = c(WGI_GovEff_est:mean_annual_funds),
               names_to = 'indicator',
               values_to = 'val')

ggplot(dat.long) +
  geom_histogram(aes(x = val)) +
  facet_wrap('indicator', scales = 'free') +
  theme_classic()

# log transform continuous response variables

logtrans <- function(x) log(x + (min(x[x>0],na.rm = T)*0.33))

y <- dat %>%
  select(WGI_GovEff_est:mean_annual_funds) %>%
  mutate_at(vars(Bio_Engage_sc, ramsar.area.sc, MPA.ind.2020, NXA.ind.2020,
                 SDA.ind.2020, Fees, OECD_sc, NGO_Environmental,
                 WWT.ind.2020, mean_annual_funds), logtrans)

# z-score standardise to mean 0 SD 1
# exclude binomial data, counts, ranks, proportions, and WGI b/c already a standard normal variable

y.scale <- as.data.frame(scale(dplyr::select(y, -c(MEA_Membership, BEA_Membership,
                                                   Mitigation_blue_carbon, Adaptation_resilience,
                                                   NDC_Commit_binary,
                                                   WGI_GovEff_est, WGI_RQ_est, ramsar.manage)),
                                     center = TRUE, scale = TRUE))

# add those indicators that weren't z-score standardised back to response datafame

y.scale <- cbind(y.scale, dplyr::select(y, MEA_Membership, BEA_Membership,
                                           Mitigation_blue_carbon,Adaptation_resilience,
                                           NDC_Commit_binary,
                                           WGI_GovEff_est, WGI_RQ_est, ramsar.manage))

# calc percentage NAs in each indicator

miss <- colMeans(is.na(y.scale))*100
df <- data.frame(indicator = names(miss), percent_na = miss)
write.csv(df, 'outputs/percent-na-indicator.csv', row.names = F)

# make indicator matrix for latent variable model

y.mat <- as.matrix(y.scale)

# set appropriate distribution for each response variable
# i.e. normal, beta for proportions, binomial for binary, poisson for counts

ynames <- colnames(y.mat)
ynames
write.csv(data.frame(ynames), 'outputs/colnames-boral.csv', row.names = F) # save column names to use later

family <- rep("normal", ncol(y.scale)) # first set all as normal
family[c(12, 13)] <- "poisson" # then change the poission by column numbers (count)
family[c(14, 15, 16, 19)] <- "binomial" # then change the binomial by column numbers (binary)

# check correct

df <- data.frame(ynames, family)
df

# run model

i <- 9 # sets the number (~ half number of indicators)

system.time( # 12.25 mins
  m1 <- boral(y.mat, family = family,
              row.eff = "none",
              lv.control = list(num.lv = i),
              mcmc.control = list(n.burnin = 2000, n.iteration = 50000,
                                  n.thin = 5, seed = 123),
              save.model = TRUE
  ))

saveRDS(m1, paste0('outputs/models/mod-LV', i, '-final.rds'))

# plot Dunn-smyth residuals

pdf(paste0('outputs/diagnostics/DS-residuals-LV', i, '-final.pdf'))
plot(m1)
dev.off()

# gewecke's convergence diagostics,
# p < 0.05 indicates chain has not converged

gew.pvals <- 2*pnorm(abs(unlist(m1$geweke.diag[[1]])), lower.tail = FALSE)
df <- data.frame(pvalue = p.adjust(gew.pvals, method = "holm"))
df$coefficient <- row.names(df)
write.csv(df, paste0('outputs/diagnostics/gewecke-convergence-diagnostics-LV', i, '-final.csv'), row.names = F)

# check chain convergence with trace plots

pdf(paste0('outputs/diagnostics/mcmc-trace-LV', i, '-final.pdf'))
plot(get.mcmcsamples(m1))
dev.off()

# check autocorrelation

pdf(paste0('outputs/diagnostics/autocorrelation-LV', i, '-final.pdf'))
autocorr.plot(get.mcmcsamples(m1), lag.max = 15)
dev.off()

