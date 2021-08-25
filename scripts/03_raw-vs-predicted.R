# check that the raw and predicted standard normal indicator values are strongly positively correlated

library(tidyverse)
library(scales)

# data

resid.df <- read.csv('outputs/predicted-indicator-vals/predicted-ind-vals-LV9-final.csv')
dat <- read.csv('data/master-df_final.csv') %>%
  mutate(iso_a3 = ISO_SOV1)

# rescale predicted indicator values to smallest range

max <- min(apply(resid.df, 2, max))
min <- max(apply(resid.df, 2, min))

resid.df <- resid.df %>% 
  mutate_all(funs(rescale(., to = c(min, max))))

# log transform continuous response variables

logtrans <- function(x) log(x + (min(x[x>0],na.rm = T)*0.33))

y <- dat %>% 
  select(WGI_GovEff_est:mean_annual_funds) %>% 
  mutate_at(vars(Bio_Engage_sc, ramsar.area.sc, MPA.ind.2020, NXA.ind.2020,
                 SDA.ind.2020, Fees, OECD_sc, NGO_Environmental,
                 WWT.ind.2020, mean_annual_funds), logtrans)

# z-score standardise to mean 0 SD 1
# mean center and scale

y.scale <- as.data.frame(scale(dplyr::select(y, -c(MEA_Membership, BEA_Membership,
                                                   Mitigation_blue_carbon, Adaptation_resilience,
                                                   NDC_Commit_binary, 
                                                   WGI_GovEff_est, WGI_RQ_est, ramsar.manage)), 
                               center = TRUE, scale = TRUE)) # exclude binomial data, counts, ranks, proportions, and WGI b/c already a standard normal variable

# add those that weren't 

y.scale <- cbind(y.scale, dplyr::select(y, MEA_Membership, BEA_Membership,
                                        Mitigation_blue_carbon,Adaptation_resilience, 
                                        NDC_Commit_binary,
                                        WGI_GovEff_est, WGI_RQ_est, ramsar.manage)) # 'bind variables that weren't scaled to df


# plot residual indicator values against actual values

y.scale$ISO_SOV1 <- dat$ISO_SOV1
resid.df$ISO_SOV1 <- dat$ISO_SOV1
y <- y.scale

# wide to long

resid.df.long <- pivot_longer(resid.df, OECD_sc:ramsar.manage, 
                              names_to = 'Indicator', values_to = 'Val_resid')
resid.df.long$Indicator <- as.factor(resid.df.long$Indicator)

dat.ind.long <- pivot_longer(y, OECD_sc:ramsar.manage, 
                             names_to = 'Indicator', values_to = 'Val_raw')
dat.ind.long$Indicator <- as.factor(dat.ind.long$Indicator)

dat.plot <- data.frame(resid.df.long, dat.ind.long[,'Val_raw'])

# plot

a <- ggplot(dat.plot)+
  geom_point(aes(x = Val_raw, y = Val_resid)) +
  facet_wrap(.~Indicator, nrow = 10, ncol = 7, scales = 'free') +
  theme_classic()
a





